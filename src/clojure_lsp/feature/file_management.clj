(ns clojure-lsp.feature.file-management
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.string :as string]
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
      (f.diagnostic/async-lint-file uri db))
    (when-let [new-ns (and (string/blank? text)
                           (uri->namespace uri db))]
      (when (get settings :auto-add-ns-to-new-files? true)
        (let [new-text (format "(ns %s)" new-ns)
              changes [{:text-document {:version (get-in @db [:documents uri :v] 0) :uri uri}
                        :edits [{:range (shared/->range {:row 1 :end-row 999999 :col 1 :end-col 999999})
                                 :new-text new-text}]}]]
          (async/>!! db/edits-chan (f.refactor/client-changes changes db)))))))

(defn ^:private find-changed-var-definitions [old-analysis new-analysis]
  (let [old-var-def (filter #(= :var-definitions (:bucket %)) old-analysis)
        new-var-def (filter #(= :var-definitions (:bucket %)) new-analysis)]
    (->> old-var-def
         (filter (fn [{:keys [name fixed-arities]}]
                   (let [var-def (first (filter #(= name (:name %)) new-var-def))]
                     (not (= fixed-arities (:fixed-arities var-def)))))))))

(defn ^:private find-references-uris [analysis elements db]
  (->> elements
       (map #(q/find-references analysis % false))
       flatten
       (map (comp #(shared/filename->uri % db) :filename))))

(defn ^:private notify-references [old-analysis new-analysis db]
  (let [changed-var-definitions (find-changed-var-definitions old-analysis new-analysis)
        references-uri (find-references-uris (:analysis @db) changed-var-definitions db)]
    (mapv
      (fn [uri]
        (when-let [text (get-in @db [:documents uri :text])]
          (log/debug "Analyzing reference" uri)
          (when-let [new-analysis (lsp.kondo/run-kondo-on-text! text uri db)]
            (swap! db (fn [db] (-> db
                                   (update-analysis uri (:analysis new-analysis))
                                   (update-findings uri (:findings new-analysis)))))
            (f.diagnostic/async-lint-file uri db))))
      references-uri)))

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
  (let [settings (:settings @db)
        notify-references? (get-in settings [:notify-references-on-file-change] false)]
    (loop [state-db @db]
      (when (>= version (get-in state-db [:documents uri :v] -1))
        (when-let [kondo-result (lsp.kondo/run-kondo-on-text! text uri db)]
          (let [filename (shared/uri->filename uri)
                old-analysis (get-in @db [:analysis filename])]
            (if (compare-and-set! db state-db (-> state-db
                                                  (update-analysis uri (:analysis kondo-result))
                                                  (update-findings uri (:findings kondo-result))
                                                  (assoc :processing-changes false)
                                                  (assoc :kondo-config (:config kondo-result))))
              (do
                (f.diagnostic/sync-lint-file uri db)
                (when notify-references?
                  (notify-references old-analysis (get-in @db [:analysis filename]) db)))
              (recur @db))))))))

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

(defn force-get-document-text
  "Get document text from db, if document not found, tries to open the document"
  [uri db]
  (or (get-in @db [:documents uri :text])
      (do
        (did-open uri (slurp uri) db)
        (get-in @db [:documents uri :text]))))
