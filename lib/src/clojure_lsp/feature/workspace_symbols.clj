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
    (->> col
         (keep (fn [doc]
                 (when-let [score (flx/score query (string/lower-case (name (get-against doc))))]
                   {:data doc
                    :score score})))
         (sort-by :score (comp - compare))
         (map :data))))

(defn ^:private fuzzy-filter
  [query elements]
  (if (string/blank? query)
    elements
    (fuzzy-search query elements :name)))

(defn ^:private element->workspace-symbol [element]
  {:name (f.document-symbol/element->name element)
   :kind (f.document-symbol/element->symbol-kind element)
   :location {:uri (:uri element)
              :range (shared/->scope-range element)}})

(defn workspace-symbols [query db]
  ;; TODO refactor to be a complete transducer
  (->> (q/find-internal-definitions db)
       (fuzzy-filter query)
       (map element->workspace-symbol)
       ;; Group elements by uri, but otherwise preserve ordering by search score.
       (reduce (fn [{:keys [next-idx index-by-uri] :as result} symb]
                 (let [uri (:uri (:location symb))]
                   (if-let [idx (get index-by-uri uri)]
                     (update-in result [:members idx] conj symb)
                     (-> result
                         (update :members conj [symb])
                         (assoc-in [:index-by-uri uri] next-idx)
                         (update :next-idx inc)))))
               {:next-idx 0
                :index-by-uri {}
                :members []})
       :members
       (reduce into [])))
