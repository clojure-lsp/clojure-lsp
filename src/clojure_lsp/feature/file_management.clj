(ns clojure-lsp.feature.file-management
  (:require
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.string :as string]))

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

(defn force-get-document-text
  "Get document text from db, if document not found, tries to open the document"
  [uri]
  (or (get-in @db/db [:documents uri :text])
      (do
        (did-open uri (slurp uri))
        (get-in @db/db [:documents uri :text]))))
