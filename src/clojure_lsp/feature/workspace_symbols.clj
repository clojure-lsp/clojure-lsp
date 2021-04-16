(ns clojure-lsp.feature.workspace-symbols
  (:require
    [anonimitoraf.clj-flx :as flx]
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.document-symbol :as f.document-symbol]
    [clojure-lsp.queries :as q]
    [clojure-lsp.shared :as shared]
    [clojure.string :as string]
    [taoensso.timbre :as log]))

(defn ^:private fuzzy-search [^String query col get-against]
  (let [query (string/lower-case query)]
    (->> (for [doc col]
           {:data doc
            :score (flx/score query (string/lower-case (name (get-against doc))))})
         (filter #(not (nil? (:score %))))
         (sort-by :score (comp - compare))
         (map :data))))

(defn ^:private fuzzy-filter
  [query elements]
  (if (string/blank? query)
    elements
    (fuzzy-search query elements :name)))

(defn workspace-symbols [query]
  (->> (:analysis @db/db)
       q/filter-project-analysis
       vals
       flatten
       (filter f.document-symbol/declaration?)
       (fuzzy-filter query)
       (mapv (fn [element]
               {:name (-> element :name name)
                :kind (f.document-symbol/element->symbol-kind element)
                :location {:uri (shared/filename->uri (:filename element))
                           :range (shared/->scope-range element)}}))))
