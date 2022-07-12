(ns clojure-lsp.feature.workspace-symbols
  (:require
   [anonimitoraf.clj-flx :as flx]
   [clojure-lsp.feature.document-symbol :as f.document-symbol]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]))

(set! *warn-on-reflection* true)

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

(defn ^:private group-by-ord
  "Similar to `group-by` but returns a vector of the groups
  without the keys.
  Use this fn if the order of the groups needs to be preserved.
  The order of groups depends on the order of their respective
  first members."
  [f coll]
  (->> coll
       (reduce (fn [groups curr]
                 (let [group-key (f curr)
                       group-idx (->> groups
                                      (map-indexed (fn [idx g] {:idx idx :group g}))
                                      (filter #(= group-key (-> % :group :key)))
                                      first
                                      :idx)]
                   (if (nil? group-idx)
                     (conj groups {:key group-key :members [curr]})
                     (update-in groups [group-idx :members] conj curr))))
               [])
       (map :members)))

(defn workspace-symbols [query db]
  ;; TODO refactor to be a complete transducer
  (->> (q/internal-analysis db)
       (map val)
       (mapcat (fn [buckets]
                 (mapcat buckets f.document-symbol/declaration-buckets)))
       (fuzzy-filter query)
       (mapv (fn [element]
               {:name (-> element :name name)
                :kind (f.document-symbol/element->symbol-kind element)
                :location {:uri (shared/filename->uri (:filename element) db)
                           :range (shared/->scope-range element)}}))
       (group-by-ord (comp :uri :location))
       flatten
       (into [])))
