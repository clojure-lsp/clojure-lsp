(ns clojure-lsp.feature.file-management
  (:require
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.string :as string]
   [taoensso.timbre :as log]))

(defn ^:private uri->namespace [uri]
  (let [project-root (:project-root @db/db)
        source-paths (get-in @db/db [:settings :source-paths])
        in-project? (when project-root
                      (string/starts-with? uri project-root))
        file-type (shared/uri->file-type uri)
        filename (shared/uri->filename uri)]
    (when (and in-project? (not= :unknown file-type))
      (->> source-paths
           (some (fn [source-path]
                   (when (string/starts-with? filename source-path)
                     (some-> filename
                             (subs (inc (count source-path))
                                   (- (count filename)
                                      (inc (count (name file-type)))))
                             (string/replace #"/" ".")
                             (string/replace #"_" "-")))))))))

(defn did-open [uri text]
  (let [settings (get @db/db :settings {})]
    (when-let [new-ns (and (string/blank? text)
                           (uri->namespace uri))]
      (when (get @db/db :auto-add-ns-to-new-files? true)
        (let [new-text (format "(ns %s)" new-ns)
              changes [{:text-document {:version (get-in @db/db [:documents uri :v] 0) :uri uri}
                        :edits [{:range (shared/->range {:row 1 :end-row 1 :col 1 :end-col 1})
                                 :new-text new-text}]}]]
          (async/put! db/edits-chan (f.refactor/client-changes changes)))))
    (when-let [kondo-result (crawler/run-kondo-on-text! text uri settings)]
      (swap! db/db (fn [state-db]
                     (-> state-db
                         (assoc-in [:documents uri] {:v 0 :text text :saved-on-disk false})
                         (crawler/update-analysis uri (:analysis kondo-result))
                         (crawler/update-findings uri (:findings kondo-result)))))
      (f.diagnostic/lint-file uri @db/db))))

(defn ^:private find-changed-var-definitions [old-analysis new-analysis]
  (let [old-var-def (filter #(= :var-definitions (:bucket %)) old-analysis)
        new-var-def (filter #(= :var-definitions (:bucket %)) new-analysis)]
    (->> old-var-def
         (filter (fn [{:keys [name fixed-arities]}]
                   (let [var-def (first (filter #(= name (:name %)) new-var-def))]
                     (not (= fixed-arities (:fixed-arities var-def)))))))))

(defn ^:private find-references-uris [analysis elements]
  (->> elements
       (map #(q/find-references analysis % false))
       flatten
       (map (comp shared/filename->uri :filename))))


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

(defn ^:private replace-text [original replacement line col end-line end-col]
  (let [lines (string/split original #"\n") ;; don't use OS specific line delimiter!
        [start-offset end-offset] (offsets lines line col end-line end-col)
        [prefix suffix] [(subs original 0 start-offset)
                         (subs original end-offset (count original))]]
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

(declare notify-references)

(defn update-and-notify
  "Should only be called from processing change channel"
  [{:keys [uri version text new-version?]}]
  (let [settings (get @db/db :settings {})
        notify-references? (get settings :settings false)
        filename (shared/uri->filename uri)
        old-analysis (get-in @db/db [:analysis filename])
        kondo-result (some-> text (crawler/run-kondo-on-text! uri settings))]
    (when kondo-result
      (when (loop [state-db @db/db]
              (when (>= version (get-in state-db [:documents uri :v] -1))
                (if (compare-and-set! db/db state-db (cond-> state-db
                                                       new-version? (assoc-in [:documents uri :v] version)
                                                       new-version? (assoc-in [:documents uri :text] text)
                                                       :always (crawler/update-analysis uri (:analysis kondo-result))
                                                       :always (crawler/update-findings uri (:findings kondo-result))))
                  true
                  (recur @db/db))))
        (f.diagnostic/lint-file uri @db/db)
        (when notify-references?
          (notify-references uri old-analysis (get-in @db/db [:analysis filename])))))))

(defn did-change [uri changes version]
  (let [old-text (get-in @db/db [:documents uri :text])
        final-text (reduce handle-change old-text changes)]
    (async/put! db/current-changes-chan {:uri uri :version version :text final-text :new-version? true})))

(defn reanalyze-uri [uri]
  (let [version (get-in @db/db [:documents uri :v] -1)
        text (get-in @db/db [:documents uri :text])]
    (async/put! db/current-changes-chan {:uri uri :version version :text text :new-version? false})))

(defn ^:private notify-references [uri old-analysis new-analysis]
  (let [changed-var-definitions (find-changed-var-definitions old-analysis new-analysis)
        references-uri (disj (set (find-references-uris (:analysis @db/db) changed-var-definitions)) uri)]
    (mapv reanalyze-uri references-uri)))

(defn force-get-document-text
  "Get document text from db, if document not found, tries to open the document"
  [uri]
  (or (get-in @db/db [:documents uri :text])
      (do
        (did-open uri (slurp uri))
        (get-in @db/db [:documents uri :text]))))
