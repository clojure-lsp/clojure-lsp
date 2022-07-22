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
   :location {:filename (:filename element) ;; will be replaced with uri
              :range (shared/->scope-range element)}})

(defn ^:private symbol-with-uri [symb uri]
  (-> symb
      (update :location dissoc :filename)
      (update :location assoc :uri uri)))

(defn workspace-symbols [query db]
  ;; TODO refactor to be a complete transducer
  (->> (q/find-internal-definitions db)
       (fuzzy-filter query)
       (map element->workspace-symbol)
       ;; Group elements by file, but otherwise preserve ordering by search score.
       ;; Also replace filename with uri.
       (reduce (fn [{:keys [next-idx filenames] :as result} symb]
                 (let [filename (:filename (:location symb))]
                   (if-let [{:keys [idx uri]} (get filenames filename)]
                     (update-in result [:members idx] conj (symbol-with-uri symb uri))
                     (let [uri (shared/filename->uri filename db)]
                       (-> result
                           (update :members conj [(symbol-with-uri symb uri)])
                           (assoc-in [:filenames filename] {:idx next-idx
                                                            :uri uri})
                           (update :next-idx inc))))))
               {:next-idx 0
                :filenames {}
                :members []})
       :members
       (reduce into [])))
