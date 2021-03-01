(ns clojure-lsp.feature.workspace-symbols
  (:require
   [clj-fuzzy.metrics :as fuzzy]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.document-symbol :as f.document-symbol]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]))

(defn ^:private fuzzy-search [^String query col get-against]
  (let [query (.toLowerCase query)]
    (->> (for [doc col]
           {:data doc
            :score (fuzzy/dice query (.toLowerCase (name (get-against doc))))})
         (filter #(< 0 (:score %)))
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
