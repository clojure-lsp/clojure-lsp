(ns clojure-lsp.feature.file-management
  (:require
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [medley.core :as medley]
   [taoensso.timbre :as log]))

(defn ^:private update-analysis [db uri new-analysis]
  (assoc-in db [:analysis (shared/uri->filename uri)] (lsp.kondo/normalize-analysis new-analysis)))

(defn ^:private update-findings [db uri new-findings]
  (assoc-in db [:findings (shared/uri->filename uri)] new-findings))

(defn ^:private uri->namespace [uri db]
  (let [project-root-uri (:project-root-uri @db)
        source-paths (get-in @db [:settings :source-paths])
        in-project? (when project-root-uri
                      (string/starts-with? uri project-root-uri))
        file-type (shared/uri->file-type uri)
        filename (shared/uri->filename uri)]
    (when (and in-project? (not= :unknown file-type))
      (->> source-paths
           (some (fn [source-path]
                   (when (string/starts-with? filename source-path)
                     (some-> (shared/relativize-filepath filename source-path)
                             (->> (re-find #"^(.+)\.\S+$"))
                             (nth 1)
                             (string/replace (System/getProperty "file.separator") ".")
                             (string/replace #"_" "-")))))))))

(defn did-open [uri text db]
  (let [settings (get @db :settings {})]
    (when-let [kondo-result (lsp.kondo/run-kondo-on-text! text uri db)]
      (swap! db (fn [state-db]
                  (-> state-db
                      (assoc-in [:documents uri] {:v 0 :text text :saved-on-disk false})
                      (update-analysis uri (:analysis kondo-result))
                      (update-findings uri (:findings kondo-result))
                      (assoc :kondo-config (:config kondo-result)))))
      (f.diagnostic/async-lint-file! uri db))
    (when-let [new-ns (and (string/blank? text)
                           (contains? #{:clj :cljs :cljc} (shared/uri->file-type uri))
                           (uri->namespace uri db))]
      (when (get settings :auto-add-ns-to-new-files? true)
        (let [new-text (format "(ns %s)" new-ns)
              changes [{:text-document {:version (get-in @db [:documents uri :v] 0) :uri uri}
                        :edits [{:range (shared/->range {:row 1 :end-row 999999 :col 1 :end-col 999999})
                                 :new-text new-text}]}]]
          (async/>!! db/edits-chan (f.refactor/client-changes changes db)))))))

(defn ^:private find-changed-var-definitions [old-local-analysis new-local-analysis]
  (let [old-var-defs (filter #(identical? :var-definitions (:bucket %)) old-local-analysis)
        new-var-defs (filter #(identical? :var-definitions (:bucket %)) new-local-analysis)
        compare-fn (fn [other-var-defs {:keys [name fixed-arities]}]
                     (if-let [var-def (first (filter #(= name (:name %)) other-var-defs))]
                       (and (not (= fixed-arities (:fixed-arities var-def)))
                            (not= 'clojure.core/declare (:defined-by var-def)))
                       true))]
    (->> (concat
           (filter (partial compare-fn new-var-defs) old-var-defs)
           (filter (partial compare-fn old-var-defs) new-var-defs))
         (medley/distinct-by (juxt :name)))))

(defn ^:private find-changed-var-usages
  [old-local-analysis new-local-analysis]
  (let [old-var-usages (filter #(identical? :var-usages (:bucket %)) old-local-analysis)
        new-var-usages (filter #(identical? :var-usages (:bucket %)) new-local-analysis)
        compare-fn (fn [other-var-usages current-var-usages var-usage]
                     (= (count (filter #(= (:name var-usage) (:name %)) current-var-usages))
                        (count (filter #(= (:name var-usage) (:name %)) other-var-usages))))]
    (->> (concat
           (remove (partial compare-fn new-var-usages old-var-usages) old-var-usages)
           (remove (partial compare-fn old-var-usages new-var-usages) new-var-usages))
         (medley/distinct-by (juxt :name)))))

(defn ^:private notify-references [filename old-local-analysis new-local-analysis db]
  (async/go
    (let [project-analysis (q/filter-project-analysis (:analysis @db))
          changed-var-definitions (find-changed-var-definitions old-local-analysis new-local-analysis)
          references-filenames (->> changed-var-definitions
                                    (map #(q/find-references project-analysis % false))
                                    flatten
                                    (map :filename))
          changed-var-usages (find-changed-var-usages old-local-analysis new-local-analysis)
          definitions-filenames (->> changed-var-usages
                                     (map #(q/find-definition project-analysis %))
                                     (remove :private)
                                     (map :filename)
                                     (remove nil?))
          filenames (->> definitions-filenames
                         (concat references-filenames)
                         (remove #(= filename %))
                         set)]
      (when (seq filenames)
        (log/debug "Analyzing references for files:" filenames)
        (crawler/analyze-reference-filenames! filenames db)
        (doseq [filename filenames]
          (f.diagnostic/sync-lint-file! (shared/filename->uri filename db) db))))))

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

(defn analyze-changes [{:keys [uri text version]} db]
  (loop [state-db @db]
    (when (>= version (get-in state-db [:documents uri :v] -1))
      (when-let [kondo-result (lsp.kondo/run-kondo-on-text! text uri db)]
        (let [filename (shared/uri->filename uri)
              old-local-analysis (get-in @db [:analysis filename])]
          (if (compare-and-set! db state-db (-> state-db
                                                (update-analysis uri (:analysis kondo-result))
                                                (update-findings uri (:findings kondo-result))
                                                (assoc :processing-changes false)
                                                (assoc :kondo-config (:config kondo-result))))
            (do
              (f.diagnostic/sync-lint-file! uri db)
              (when (get-in @db [:settings :notify-references-on-file-change] false)
                (notify-references filename old-local-analysis (get-in @db [:analysis filename]) db)))
            (recur @db)))))))

(defn did-change [uri changes version db]
  (let [old-text (get-in @db [:documents uri :text])
        final-text (reduce handle-change old-text changes)]
    (swap! db (fn [state-db] (-> state-db
                                 (assoc-in [:documents uri :v] version)
                                 (assoc-in [:documents uri :text] final-text)
                                 (assoc :processing-changes true))))
    (async/>!! db/current-changes-chan {:uri uri
                                        :text final-text
                                        :version version})))

(defn did-close [uri db]
  (let [filename (shared/uri->filename uri)]
    (when-not (shared/file-exists? (io/file filename))
      (swap! db (fn [state-db] (-> state-db
                                   (shared/dissoc-in [:documents uri])
                                   (shared/dissoc-in [:analysis filename])
                                   (shared/dissoc-in [:findings filename]))))
      (f.diagnostic/clean! uri))))

(defn force-get-document-text
  "Get document text from db, if document not found, tries to open the document"
  [uri db]
  (or (get-in @db [:documents uri :text])
      (do
        (did-open uri (slurp uri) db)
        (get-in @db [:documents uri :text]))))
