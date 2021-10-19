(ns clojure-lsp.feature.call-hierarchy
  (:require
   [clojure-lsp.feature.document-symbol :as f.document-symbol]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(defn ^:private element-by-uri->call-hierarchy-item
  [{uri :uri {:keys [ns filename name-row name-col arglist-strs deprecated] value :name :as element} :element} db]
  (let [range (shared/->range element)
        project-file? (string/starts-with? uri "file://")
        detail (if project-file?
                 (-> (f.file-management/force-get-document-text uri db)
                     (parser/loc-at-pos name-row name-col)
                     edit/find-namespace-name)
                 (or (some-> ns str)
                     filename))]
    {:name (if arglist-strs
             (str (name value) " " (some->> arglist-strs (remove nil?) (string/join " ")))
             (name value))
     :kind (f.document-symbol/element->symbol-kind element)
     :tags (cond-> [] deprecated (conj 1))
     :detail detail
     :uri uri
     :range range
     :selection-range range}))

(defn prepare [uri row col db]
  (let [cursor-element (q/find-element-under-cursor (:analysis @db) (shared/uri->filename uri) row col)]
    [(element-by-uri->call-hierarchy-item {:uri uri :element cursor-element} db)]))

(defn ^:private element->incoming-usage-by-uri
  [db {:keys [name-row name-col filename]}]
  (let [uri (shared/filename->uri filename db)
        zloc (-> (f.file-management/force-get-document-text uri db)
                 (parser/loc-at-pos name-row name-col))
        parent-zloc (edit/find-function-definition-name-loc zloc)]
    (when parent-zloc
      (let [{parent-row :row parent-col :col} (-> parent-zloc z/node meta)]
        {:uri uri
         :element (q/find-element-under-cursor (:analysis @db) filename parent-row parent-col)}))))

(defn ^:private element->outgoing-usage-by-uri
  [db element]
  (when-let [definition (q/find-definition (:analysis @db) element db)]
    (let [def-filename (:filename definition)
          definition-uri (if (shared/plain-uri? def-filename)
                           def-filename
                           (shared/filename->uri def-filename db))]
      {:uri definition-uri
       :element definition})))

(defn incoming [uri row col db]
  (->> (q/find-references-from-cursor (:analysis @db) (shared/uri->filename uri) row col false db)
       (map (partial element->incoming-usage-by-uri db))
       (remove nil?)
       (mapv (fn [element-by-uri]
               {:from-ranges []
                :from (element-by-uri->call-hierarchy-item element-by-uri db)}))))

(defn outgoing [uri row col db]
  (let [analysis (:analysis @db)
        filename (shared/uri->filename uri)
        zloc (-> (f.file-management/force-get-document-text uri db)
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
             (map (partial element->outgoing-usage-by-uri db))
             (remove nil?)
             (mapv (fn [element-by-uri]
                     {:from-ranges []
                      :to (element-by-uri->call-hierarchy-item element-by-uri db)})))))))
