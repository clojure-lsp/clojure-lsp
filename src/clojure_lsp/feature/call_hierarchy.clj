(ns clojure-lsp.feature.call-hierarchy
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.document-symbol :as f.document-symbol]
    [clojure-lsp.feature.file-management :as f.file-management]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.queries :as q]
    [clojure-lsp.refactor.edit :as edit]
    [clojure-lsp.shared :as shared]
    [clojure.string :as string]
    [rewrite-clj.zip :as z]
    [taoensso.timbre :as log]))

(defn ^:private element-by-uri->call-hierarchy-item
  [{uri :uri {:keys [name-row name-col arglist-strs deprecated] value :name :as element} :element}
   project-root-uri]
  (let [range (shared/->range element)
        project-file? (string/starts-with? uri "file://")
        detail (if project-file?
                 (-> (f.file-management/force-get-document-text uri)
                     (parser/loc-at-pos name-row name-col)
                     edit/find-namespace-name)
                 (shared/uri->relative-filepath uri project-root-uri))]
    {:name (if arglist-strs
             (str (name value) " " (some->> arglist-strs (remove nil?) (string/join " ")))
             (name value))
     :kind (f.document-symbol/element->symbol-kind element)
     :tags (cond-> [] deprecated (conj 1))
     :detail detail
     :uri uri
     :range range
     :selection-range range}))

(defn prepare [uri row col project-root-uri]
  (let [cursor-element (q/find-element-under-cursor (:analysis @db/db) (shared/uri->filename uri) row col)]
    [(element-by-uri->call-hierarchy-item {:uri uri :element cursor-element} project-root-uri)]))

(defn ^:private element->incoming-usage-by-uri
  [{:keys [name-row name-col filename]}]
  (let [uri (shared/filename->uri filename)
        zloc (-> (f.file-management/force-get-document-text uri)
                 (parser/loc-at-pos name-row name-col))
        parent-zloc (edit/find-function-definition-name-loc zloc)]
    (when parent-zloc
      (let [{parent-row :row parent-col :col} (-> parent-zloc z/node meta)]
        {:uri uri
         :element (q/find-element-under-cursor (:analysis @db/db) filename parent-row parent-col)}))))

(defn ^:private element->outgoing-usage-by-uri
  [element]
  (when-let [definition (q/find-definition (:analysis @db/db) element)]
    (let [def-filename (:filename definition)
          definition-uri (if (shared/plain-uri? def-filename)
                           def-filename
                           (shared/filename->uri def-filename))]
      {:uri definition-uri
       :element definition})))

(defn incoming [uri row col project-root-uri]
  (->> (q/find-references-from-cursor (:analysis @db/db) (shared/uri->filename uri) row col false)
       (map element->incoming-usage-by-uri)
       (remove nil?)
       (mapv (fn [element-by-uri]
               {:from-ranges []
                :from (element-by-uri->call-hierarchy-item element-by-uri project-root-uri)}))))

(defn outgoing [uri row col project-root-uri]
  (let [analysis (:analysis @db/db)
        filename (shared/uri->filename uri)
        zloc (-> (f.file-management/force-get-document-text uri)
                 (parser/loc-at-pos row col))
        {parent-row :row parent-col :col} (some-> (edit/find-function-definition-name-loc zloc)
                                                  z/node
                                                  meta)]
    (when (and parent-row parent-col)
      (let [definition (q/find-element-under-cursor analysis filename parent-row parent-col)]
        (->> (q/find-var-usages-under-form analysis
                                           filename
                                           (:name-row definition)
                                           (:name-col definition)
                                           (:end-row definition)
                                           (:end-col definition))
             (map element->outgoing-usage-by-uri)
             (remove nil?)
             (mapv (fn [element-by-uri]
                     {:from-ranges []
                      :to (element-by-uri->call-hierarchy-item element-by-uri project-root-uri)})))))))
