(ns clojure-lsp.feature.call-hierarchy
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.document-symbol :as f.document-symbol]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]
   [clojure-lsp.queries :as q]))

(defn ^:private element-by-uri->call-hierarchy-item
  [{uri :uri {:keys [name-row name-col] value :name :as element} :element}
   project-root]
  (let [range (shared/->range element)
        ns-zloc (-> (get-in @db/db [:documents uri :text])
                    (parser/loc-at-pos name-row name-col)
                    edit/find-namespace-name)]
    {:name (name value)
     :kind (f.document-symbol/element->symbol-kind element)
     :tags [] ;; TODO whether usage is deprecated
     :detail (or ns-zloc
                 (shared/uri->project-related-path uri project-root))
     :uri uri
     :range range
     :selection-range range}))

(defn prepare [uri row col project-root]
  (let [cursor-element (q/find-element-under-cursor (:analysis @db/db) (shared/uri->filename uri) row col)]
    [(element-by-uri->call-hierarchy-item {:uri uri :element cursor-element} project-root)]))

(defn ^:private element-by-uri->incoming-usage-by-uri
  [{:keys [name-row name-col filename]}]
  (let [uri (shared/filename->uri filename)
        zloc (-> (get-in @db/db [:documents uri :text])
                 (parser/loc-at-pos name-row name-col))
        parent-zloc (edit/find-function-name zloc)]
    (when parent-zloc
      (let [{parent-row :row parent-col :col} (-> parent-zloc z/node meta)]
        {:uri uri
         :element (q/find-element-under-cursor (:analysis @db/db) filename parent-row parent-col)}))))

(defn incoming [uri row col project-root]
  (->> (q/find-references-from-cursor (:analysis @db/db) (shared/uri->filename uri) row col false)
       (map element-by-uri->incoming-usage-by-uri)
       (remove nil?)
       (mapv (fn [element-by-uri]
                 {:from-ranges []
                  :from (element-by-uri->call-hierarchy-item element-by-uri project-root)}))))
