(ns clojure-lsp.feature.call-hierarchy
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.document-symbol :as f.document-symbol]
    [clojure-lsp.feature.references :as f.references]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.refactor.edit :as edit]
    [clojure-lsp.shared :as shared]
    [rewrite-clj.zip :as z]))

(defn ^:private usage-by-uri->call-hierarchy-item
  [{uri :uri {:keys [kind] value :str :as usage} :usage}
   project-root]
  (let [range (shared/->range usage)
        entry-kind (or kind
                       (f.document-symbol/is-declaration? usage))]
    {:name value
     :kind (f.document-symbol/entry-kind->symbol-kind entry-kind)
     :tags [] ;; TODO whether usage is deprecated
     :detail (shared/uri->project-related-path uri project-root)
     :uri uri
     :range range
     :selection-range range}))

(defn prepare [uri row col local-env project-root]
  (let [file-type (shared/uri->file-type uri)
        cursor-usage (f.references/find-under-cursor row col local-env file-type)]
    [(usage-by-uri->call-hierarchy-item {:uri uri :usage cursor-usage} project-root)]))

(defn ^:private usage-by-uri->incoming-usage-by-uri
  [{{:keys [row col]} :usage uri :uri}]
  (let [file-type (shared/uri->file-type uri)
        zloc (-> (get-in @db/db [:documents uri :text])
                 (parser/loc-at-pos row col))
        local-env (get-in @db/db [:file-envs uri])
        parent-zloc (-> zloc edit/to-top z/down z/next)
        {parent-row :row parent-col :col} (-> parent-zloc z/node meta)]
    {:uri uri
     :usage (f.references/find-under-cursor parent-row parent-col local-env file-type)}))

(defn incoming [uri row col project-root]
  (->> (f.references/reference-usages uri row col)
       (map usage-by-uri->incoming-usage-by-uri)
       (mapv (fn [usage-by-uri]
                 {:from-ranges []
                  :from (usage-by-uri->call-hierarchy-item usage-by-uri project-root)}))))
