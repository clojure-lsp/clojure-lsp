(ns clojure-lsp.feature.file-management
  (:require
   [clojure-lsp.clojure-producer :as clojure-producer]
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [lsp4clj.protocols.logger :as logger]
   [lsp4clj.protocols.producer :as producer]
   [medley.core :as medley]))

(set! *warn-on-reflection* true)

(defn ^:private update-analysis [db uri new-analysis]
  (assoc-in db [:analysis (shared/uri->filename uri)] (lsp.kondo/normalize-analysis new-analysis)))

(defn ^:private update-findings [db uri new-findings]
  (assoc-in db [:findings (shared/uri->filename uri)] new-findings))

(defn did-open [uri text db allow-create-ns]
  (shared/logging-task
    :did-open
    (when-let [kondo-result (lsp.kondo/run-kondo-on-text! text uri db)]
      (swap! db (fn [state-db]
                  (-> state-db
                      (assoc-in [:documents uri] {:v 0 :text text :saved-on-disk false})
                      (update-analysis uri (:analysis kondo-result))
                      (update-findings uri (:findings kondo-result))
                      (assoc :kondo-config (:config kondo-result)))))
      (f.diagnostic/async-publish-diagnostics! uri db)))
  (when-let [new-ns (and allow-create-ns
                         (string/blank? text)
                         (contains? #{:clj :cljs :cljc} (shared/uri->file-type uri))
                         (not (get (:create-ns-blank-files-denylist @db) uri))
                         (shared/uri->namespace uri db))]
    (when (settings/get db [:auto-add-ns-to-new-files?] true)
      (let [new-text (format "(ns %s)" new-ns)
            changes [{:text-document {:version (get-in @db [:documents uri :v] 0) :uri uri}
                      :edits [{:range (shared/->range {:row 1 :end-row 999999 :col 1 :end-col 999999})
                               :new-text new-text}]}]]
        (async/>!! db/edits-chan (shared/client-changes changes db))))))

(defn ^:private find-changed-elems-by
  "Detect elements that changed number of occurrences."
  [signature-fn old-elems new-elems]
  (comment
    ;; increased
    (merge-with - {:a 2} {:a 1}) ;; => {:a 1}
    ;; decreased
    (merge-with - {:a 1} {:a 2}) ;; => {:a -1}
    ;; removed
    (merge-with - {} {:a 1}) ;; => {:a 1} ;; not {:a -1}, as you'd expect with removing, but at least it's not 0
    ;; added
    (merge-with - {:a 1} {}) ;; => {:a 1}
    ;; same
    (merge-with - {:a 1} {:a 1}) ;; => {:a 0}
    )
  (let [signature-with-elem (fn [elem]
                              (with-meta (signature-fn elem) {:elem elem}))
        old-counts (->> old-elems (map signature-with-elem) frequencies)
        new-counts (->> new-elems (map signature-with-elem) frequencies)]
    (->> (merge-with - new-counts old-counts)
         (medley/remove-vals zero?)
         keys
         (map (comp :elem meta)))))

(defn ^:private find-changed-var-definitions [old-local-analysis new-local-analysis]
  (let [old-var-defs (filter #(identical? :var-definitions (:bucket %)) old-local-analysis)
        new-var-defs (filter #(identical? :var-definitions (:bucket %)) new-local-analysis)
        definition-signature (juxt :ns :name :fixed-arities :defined-by)]
    (find-changed-elems-by definition-signature old-var-defs new-var-defs)))

(defn ^:private find-changed-var-usages
  [old-local-analysis new-local-analysis]
  (let [old-var-usages (filter #(identical? :var-usages (:bucket %)) old-local-analysis)
        new-var-usages (filter #(identical? :var-usages (:bucket %)) new-local-analysis)
        usage-signature (juxt :to :name)]
    (find-changed-elems-by usage-signature old-var-usages new-var-usages)))

(defn reference-filenames [filename old-local-analysis new-local-analysis db]
  (let [changed-var-definitions (find-changed-var-definitions old-local-analysis new-local-analysis)
        ;; TODO: can we easily remove usages of external namespaces (clojure.core, etc.) here?
        changed-var-usages (find-changed-var-usages old-local-analysis new-local-analysis)
        project-analysis (into {}
                               (q/filter-project-analysis-xf db)
                               (dissoc (:analysis @db) filename)) ;; don't notify self
        incoming-filenames (when (seq changed-var-definitions)
                             (let [def-signs (->> changed-var-definitions
                                                  (map q/var-definition-signatures)
                                                  (apply set/union))]
                               (into #{}
                                     (comp
                                       (mapcat val)
                                       (filter #(identical? :var-usages (:bucket %)))
                                       (filter #(contains? def-signs (q/var-usage-signature %)))
                                       (map :filename))
                                     project-analysis)))
        outgoing-filenames (when (seq changed-var-usages)
                             (let [usage-signs->langs (->> changed-var-usages
                                                              ;; TODO: do we really care if lang doesn't match?
                                                           (reduce (fn [result usage]
                                                                     (assoc result (q/var-usage-signature usage) (q/elem-langs usage)))
                                                                   {}))]
                               (into #{}
                                     (comp
                                       (mapcat val)
                                       (filter #(identical? :var-definitions (:bucket %)))
                                       (filter #(when-let [usage-langs (some usage-signs->langs (q/var-definition-signatures %))]
                                                  (some usage-langs (q/elem-langs %))))
                                          ;; TODO: this excludes "private calls", but they are counted in code lens, so maybe shouldn't exclude
                                       (remove :private)
                                       (map :filename))
                                     project-analysis)))]
    (set/union (or incoming-filenames #{})
               (or outgoing-filenames #{}))))

(defn ^:private notify-references [filename old-local-analysis new-local-analysis {:keys [db producer]}]
  (async/go
    (shared/logging-task
      :notify-references
      (let [filenames (shared/logging-task
                        :reference-files/find
                        (reference-filenames filename old-local-analysis new-local-analysis db))]
        (when (seq filenames)
          (logger/debug "Analyzing references for files:" filenames)
          (shared/logging-task
            :reference-files/analyze
            (crawler/analyze-reference-filenames! filenames db))
          (doseq [filename filenames]
            (f.diagnostic/sync-publish-diagnostics! (shared/filename->uri filename db) db))
          (producer/refresh-code-lens producer))))))

(defn ^:private offsets [lines line col end-line end-col]
  (loop [lines (seq lines)
         offset 0
         idx 0]
    (if (or (not lines)
            (= line idx))
      [(+ offset col line)
       (loop [lines lines
              offset offset
              idx idx]
         (if (or (not lines)
                 (= end-line idx))
           (+ offset end-col end-line)
           (recur (next lines)
                  (+ offset (count (first lines)))
                  (inc idx))))]
      (recur (next lines)
             (+ offset (count (first lines)))
             (inc idx)))))

(defn replace-text [original replacement line col end-line end-col]
  (let [lines (string/split original #"\n") ;; don't use OS specific line delimiter!
        [start-offset end-offset] (offsets lines line col end-line end-col)
        [prefix suffix] [(subs original 0 start-offset)
                         (subs original (min end-offset (count original)) (count original))]]
    (string/join [prefix replacement suffix])))

(defn ^:private handle-change
  "Handle a TextDocumentContentChangeEvent"
  [old-text change]
  (let [new-text (:text change)]
    (if-let [r (:range change)]
      (let [{{start-line :line
              start-character :character} :start
             {end-line :line
              end-character :character} :end} r]
        (replace-text old-text new-text start-line start-character end-line end-character))
      ;; If range and rangeLength are omitted the new text is considered to be
      ;; the full content of the document.
      new-text)))

(defn analyze-changes [{:keys [uri text version]} {:keys [producer db] :as components}]
  (shared/logging-task
    :analyze-file
    (loop [state-db @db]
      (when (>= version (get-in state-db [:documents uri :v] -1))
        (when-let [kondo-result (shared/logging-time
                                  (str "changes analyzed by clj-kondo took %s")
                                  (lsp.kondo/run-kondo-on-text! text uri db))]
          (let [filename (shared/uri->filename uri)
                old-local-analysis (get-in @db [:analysis filename])]
            (if (compare-and-set! db state-db (-> state-db
                                                  (update-analysis uri (:analysis kondo-result))
                                                  (update-findings uri (:findings kondo-result))
                                                  (update :processing-changes disj uri)
                                                  (assoc :kondo-config (:config kondo-result))))
              (do
                (f.diagnostic/sync-publish-diagnostics! uri db)
                (when (settings/get db [:notify-references-on-file-change] true)
                  (notify-references filename old-local-analysis (get-in @db [:analysis filename]) components))
                (clojure-producer/refresh-test-tree producer [uri]))
              (recur @db))))))))

(defn did-change [uri changes version db]
  (let [old-text (get-in @db [:documents uri :text])
        final-text (reduce handle-change old-text changes)]
    (swap! db (fn [state-db] (-> state-db
                                 (assoc-in [:documents uri :v] version)
                                 (assoc-in [:documents uri :text] final-text)
                                 (update :processing-changes conj uri))))
    (async/>!! db/current-changes-chan {:uri uri
                                        :text final-text
                                        :version version})))

(defn analyze-watched-created-files! [uris {:keys [db producer] :as components}]
  (shared/logging-task
    :analyze-created-files-in-watched-dir
    (let [filenames (map shared/uri->filename uris)
          result (shared/logging-time
                   "Created watched files analyzed, took %s"
                   (lsp.kondo/run-kondo-on-paths! filenames false components))
          analysis (->> (:analysis result)
                        lsp.kondo/normalize-analysis
                        (group-by :filename))]
      (swap! db (fn [state-db]
                  (-> state-db
                      (update :analysis merge analysis)
                      (assoc :kondo-config (:config result))
                      (update :findings merge (group-by :filename (:findings result))))))
      (f.diagnostic/publish-all-diagnostics! filenames db)
      (clojure-producer/refresh-test-tree producer uris))))

(defn did-change-watched-files [changes db]
  (doseq [{:keys [uri type]} changes]
    (case type
      :created (async/>!! db/created-watched-files-chan uri)
      ;; TODO Fix outdated changes overwriting newer changes.
      :changed nil #_(did-change uri
                                 [{:text (slurp filename)}]
                                 (inc (get-in @db [:documents uri :v] 0))
                                 db)
      :deleted (shared/logging-task
                 :delete-watched-file
                 (let [filename (shared/uri->filename uri)]
                   (swap! db (fn [state-db]
                               (-> state-db
                                   (shared/dissoc-in [:documents uri])
                                   (shared/dissoc-in [:analysis filename])
                                   (shared/dissoc-in [:findings filename])))))))))

(defn did-close [uri db]
  (shared/logging-task
    :did-close
    (let [filename (shared/uri->filename uri)
          source-paths (settings/get db [:source-paths])]
      (when (and (not (shared/external-filename? filename source-paths))
                 (not (shared/file-exists? (io/file filename))))
        (swap! db (fn [state-db] (-> state-db
                                     (shared/dissoc-in [:documents uri])
                                     (shared/dissoc-in [:analysis filename])
                                     (shared/dissoc-in [:findings filename]))))
        (f.diagnostic/publish-empty-diagnostics! uri db)))))

(defn force-get-document-text
  "Get document text from db, if document not found, tries to open the document"
  [uri db]
  (or (get-in @db [:documents uri :text])
      (do
        (did-open uri (slurp uri) db false)
        (get-in @db [:documents uri :text]))))

(defn did-save [uri db]
  (shared/logging-task
    :did-save
    (swap! db #(assoc-in % [:documents uri :saved-on-disk] true))))
