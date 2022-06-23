(ns clojure-lsp.feature.file-management
  (:require
   [clojure-lsp.clj-depend :as lsp.depend]
   [clojure-lsp.clojure-producer :as clojure-producer]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.feature.rename :as f.rename]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [lsp4clj.protocols.logger :as logger]
   [lsp4clj.protocols.producer :as producer]))

(set! *warn-on-reflection* true)

(defn create-ns-changes [uri text db]
  (when-let [new-ns (and (string/blank? text)
                         (contains? #{:clj :cljs :cljc} (shared/uri->file-type uri))
                         (not (get (:create-ns-blank-files-denylist db) uri))
                         (shared/uri->namespace uri db))]
    (when (settings/get db [:auto-add-ns-to-new-files?] true)
      (let [new-text (format "(ns %s)" new-ns)
            changes [{:text-document {:version (get-in db [:documents uri :v] 0) :uri uri}
                      :edits [{:range (shared/->range {:row 1 :end-row 999999 :col 1 :end-col 999999})
                               :new-text new-text}]}]]
        (shared/client-changes changes db)))))

(defn load-document! [uri text db*]
  (swap! db* #(assoc-in % [:documents uri] {:v 0 :text text :saved-on-disk false})))

(defn did-open [uri text db* allow-create-ns]
  (load-document! uri text db*)
  (let [kondo-result* (future (lsp.kondo/run-kondo-on-text! text uri db*))
        depend-result* (future (lsp.depend/analyze-filename! (shared/uri->filename uri) @db*))
        kondo-result @kondo-result*
        depend-result @depend-result*]
    (swap! db* (fn [state-db]
                 (-> state-db
                     (lsp.kondo/db-with-results kondo-result)
                     (lsp.depend/db-with-results depend-result))))
    (f.diagnostic/publish-diagnostics! uri @db*))
  (when allow-create-ns
    (when-let [create-ns-edits (create-ns-changes uri text @db*)]
      (async/put! db/edits-chan create-ns-edits))))

(defn ^:private set-xor [a b]
  (into (set/difference a b)
        (set/difference b a)))

(defn ^:private find-changed-elems-by
  "Detect elements that went from 0 to 1 occurrence, or from 1 to 0
  occurrences."
  [signature-fn old-elems new-elems]
  (let [signature-with-elem (fn [elem]
                              (with-meta (signature-fn elem) {:elem elem}))
        old-signs (->> old-elems (map signature-with-elem) (into #{}))
        new-signs (->> new-elems (map signature-with-elem) (into #{}))]
    (map (comp :elem meta)
         (set-xor old-signs new-signs))))

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

(defn reference-filenames [filename db-before db-after]
  (let [old-local-analysis (get-in db-before [:analysis filename])
        new-local-analysis (get-in db-after [:analysis filename])
        changed-var-definitions (find-changed-var-definitions old-local-analysis new-local-analysis)
        changed-var-usages (find-changed-var-usages old-local-analysis new-local-analysis)
        project-db (-> db-after
                       q/db-with-project-analysis
                       ;; don't notify self
                       (update :analysis dissoc filename))
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
                                     (:analysis project-db))))
        outgoing-filenames (when (seq changed-var-usages)
                             ;; If definition is in both a clj and cljs file, and this is a clj
                             ;; usage, we are careful to notify only the clj file. But, it wouldn't
                             ;; really hurt to notify both files. So, if it helps readability,
                             ;; maintenance, or symmetry with `incoming-filenames`, we could look
                             ;; at just signature, not signature and lang.
                             (let [usage-signs->langs (->> changed-var-usages
                                                           (reduce (fn [result usage]
                                                                     (assoc result (q/var-usage-signature usage) (q/elem-langs usage)))
                                                                   {}))]
                               (into #{}
                                     (comp
                                       (mapcat val)
                                       (filter #(identical? :var-definitions (:bucket %)))
                                       (filter #(when-let [usage-langs (some usage-signs->langs (q/var-definition-signatures %))]
                                                  (some usage-langs (q/elem-langs %))))
                                       (map :filename))
                                     (:analysis project-db))))]
    (set/union (or incoming-filenames #{})
               (or outgoing-filenames #{}))))

(defn analyze-reference-filenames! [filenames db*]
  (let [result (lsp.kondo/run-kondo-on-reference-filenames! filenames db*)]
    (swap! db* lsp.kondo/db-with-results result)))

(defn ^:private notify-references [filename db-before db-after {:keys [db* producer]}]
  (async/go
    (shared/logging-task
      :notify-references
      (let [filenames (shared/logging-task
                        :reference-files/find
                        (reference-filenames filename db-before db-after))]
        (when (seq filenames)
          (logger/debug "Analyzing references for files:" filenames)
          (shared/logging-task
            :reference-files/analyze
            (analyze-reference-filenames! filenames db*))
          (let [db @db*]
            (doseq [filename filenames]
              (f.diagnostic/publish-diagnostics! (shared/filename->uri filename db) db)))
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

(defn analyze-changes [{:keys [uri text version]} {:keys [producer db*] :as components}]
  (let [filename (shared/uri->filename uri)]
    (loop [state-db @db*]
      (when (>= version (get-in state-db [:documents uri :v] -1))
        (let [kondo-result* (future
                              (shared/logging-time
                                (str "changes analyzed by clj-kondo took %s")
                                (lsp.kondo/run-kondo-on-text! text uri db*)))
              depend-result* (future
                               (shared/logging-time
                                 (str "changes analyzed by clj-depend took %s")
                                 (lsp.depend/analyze-filename! filename state-db)))
              kondo-result @kondo-result*
              depend-result @depend-result*
              old-db @db*]
          (if (compare-and-set! db* state-db
                                (-> state-db
                                    (lsp.kondo/db-with-results kondo-result)
                                    (lsp.depend/db-with-results depend-result)
                                    (update :processing-changes disj uri)))
            (let [db @db*]
              (f.diagnostic/publish-diagnostics! uri db)
              (when (settings/get db [:notify-references-on-file-change] true)
                (notify-references filename old-db db components))
              (clojure-producer/refresh-test-tree producer [uri]))
            (recur @db*)))))))

(defn did-change [uri changes version db*]
  (let [old-text (get-in @db* [:documents uri :text])
        final-text (reduce handle-change old-text changes)]
    (swap! db* (fn [state-db] (-> state-db
                                  (assoc-in [:documents uri :v] version)
                                  (assoc-in [:documents uri :text] final-text)
                                  (update :processing-changes conj uri))))
    (async/put! db/current-changes-chan {:uri uri
                                         :text final-text
                                         :version version})))

(defn analyze-watched-created-files! [uris {:keys [db* producer]}]
  (let [filenames (map shared/uri->filename uris)
        result (shared/logging-time
                 "Created watched files analyzed, took %s"
                 (lsp.kondo/run-kondo-on-paths! filenames db* {:external? false} nil))]
    (swap! db* lsp.kondo/db-with-results result)
    (f.diagnostic/publish-all-diagnostics! filenames @db*)
    (clojure-producer/refresh-test-tree producer uris)))

(defn did-change-watched-files [changes db*]
  (doseq [{:keys [uri type]} changes]
    (case type
      :created (async/put! db/created-watched-files-chan uri)
      :changed (when (settings/get @db* [:compute-external-file-changes] true)
                 (shared/logging-task
                   :changed-watched-file
                   (did-change uri
                               [{:text (slurp (shared/uri->filename uri))}]
                               (get-in @db* [:documents uri :v] 0)
                               db*)))
      :deleted (shared/logging-task
                 :delete-watched-file
                 (let [filename (shared/uri->filename uri)]
                   (swap! db* (fn [state-db]
                                (-> state-db
                                    (shared/dissoc-in [:documents uri])
                                    (shared/dissoc-in [:analysis filename])
                                    (shared/dissoc-in [:findings filename])))))))))

(defn did-close [uri db*]
  (let [filename (shared/uri->filename uri)
        source-paths (settings/get @db* [:source-paths])]
    (when (and (not (shared/external-filename? filename source-paths))
               (not (shared/file-exists? (io/file filename))))
      (swap! db* (fn [state-db] (-> state-db
                                    (shared/dissoc-in [:documents uri])
                                    (shared/dissoc-in [:analysis filename])
                                    (shared/dissoc-in [:findings filename]))))
      (f.diagnostic/publish-empty-diagnostics! uri @db*))))

(defn force-get-document-text
  "Get document text from db, if document not found, tries to open the document"
  [uri db*]
  (or (get-in @db* [:documents uri :text])
      (do
        (did-open uri (slurp uri) db* false)
        (get-in @db* [:documents uri :text]))))

(defn did-save [uri db*]
  (swap! db* #(assoc-in % [:documents uri :saved-on-disk] true)))

(defn will-rename-files [files db]
  (->> files
       (keep (fn [{:keys [oldUri newUri]}]
               (let [old-filename (shared/uri->filename oldUri)
                     new-ns (shared/uri->namespace newUri db)
                     ns-definition (q/find-namespace-definition-by-filename db old-filename)]
                 (when ns-definition
                   (f.rename/rename-element oldUri new-ns db ns-definition :rename-file)))))
       (reduce #(shared/deep-merge %1 %2) {:document-changes []})))
