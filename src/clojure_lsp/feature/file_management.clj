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
  (when-let [new-ns (and (string/blank? text)
                         (uri->namespace uri))]
    (when (get-in @db/db [:settings :auto-add-ns-to-new-files?] true)
      (let [new-text (format "(ns %s)" new-ns)
            changes [{:text-document {:version (get-in @db/db [:documents uri :v] 0) :uri uri}
                      :edits [{:range (shared/->range {:row 1 :end-row 1 :col 1 :end-col 1})
                               :new-text new-text}]}]]
        (async/put! db/edits-chan (f.refactor/client-changes changes)))))
  (when-let [result (crawler/run-kondo-on-text! text uri)]
    (swap! db/db (fn [state-db]
                   (-> state-db
                       (assoc-in [:documents uri] {:v 0 :text text :saved-on-disk false})
                       (crawler/update-analysis uri (:analysis result))
                       (crawler/update-findings uri (:findings result)))))
    (f.diagnostic/notify uri result)))

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

(defn ^:private notify-references [old-analysis new-analysis]
  (let [changed-var-definitions (find-changed-var-definitions old-analysis new-analysis)
        references-uri (find-references-uris (:analysis @db/db) changed-var-definitions)]
    (mapv
      (fn [uri]
        (when-let [text (get-in @db/db [:documents uri :text])]
          (log/debug "Analyzing reference" uri)
          (when-let [new-analysis (crawler/run-kondo-on-text! text uri)]
            (swap! db/db (fn [db] (-> db
                                      (crawler/update-analysis uri (:analysis new-analysis))
                                      (crawler/update-findings uri (:findings new-analysis)))))
            (f.diagnostic/notify uri new-analysis))))
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

(defn analyze-changes [{:keys [uri text version]}]
  (let [notify-references? (get-in @db/db [:settings :notify-references-on-file-change] false)]
    (loop [state-db @db/db]
      (when (>= version (get-in state-db [:documents uri :v] -1))
        (when-let [new-analysis (crawler/run-kondo-on-text! text uri)]
          (let [filename (shared/uri->filename uri)
                old-analysis (get-in @db/db [:analysis filename])]
            (if (compare-and-set! db/db state-db (-> state-db
                                                     (crawler/update-analysis uri (:analysis new-analysis))
                                                     (crawler/update-findings uri (:findings new-analysis))))
              (do
                (f.diagnostic/notify uri new-analysis)
                (when notify-references?
                  (notify-references old-analysis (get-in @db/db [:analysis filename]))))
              (recur @db/db))))))))

(defn did-change [uri changes version]
  (loop [state-db @db/db]
    (let [old-text (get-in state-db [:documents uri :text])
          final-text (reduce handle-change old-text changes)]

      (if (compare-and-set! db/db state-db (-> state-db
                                               (assoc-in [:documents uri :v] version)
                                               (assoc-in [:documents uri :text] final-text)))

        (async/put! db/current-changes-chan {:uri uri
                                             :text final-text
                                             :version version})
        (recur @db/db)))))

(defn force-get-document-text
  "Get document text from db, if document not found, tries to open the document"
  [uri]
  (or (get-in @db/db [:documents uri :text])
      (do
        (did-open uri (slurp uri))
        (get-in @db/db [:documents uri :text]))))
