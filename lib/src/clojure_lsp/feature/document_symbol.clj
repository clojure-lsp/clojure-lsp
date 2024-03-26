(ns clojure-lsp.feature.document-symbol
  (:require
   [clojure-lsp.logger :as logger]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]))

(set! *warn-on-reflection* true)

(defn element->name [{elem-name :name symbol :symbol :keys [dispatch-val-str]}]
  (cond-> (or (some-> symbol str) (name elem-name))
    dispatch-val-str (str " " dispatch-val-str)))

(defn ^:private element->document-symbol [e]
  (shared/assoc-some
    {:name (element->name e)
     :kind (q/element->symbol-kind e)
     :range (shared/->scope-range e)
     :selection-range (shared/->range e)
     :tags (cond-> []
             (:deprecated e) (conj 1))}
    :detail (cond (:private e) "private"
                  (:reg e) (name (:reg e)))))

(defn ^:private symbol-order [{:keys [selection-range]}]
  [(:line (:start selection-range)) (:character (:start selection-range))])

(defn ^:private remove-first [item-to-remove coll]
  (if (empty? coll)
    ()  ; literal name for empty list
    (let [item (first coll)]
      (if (= item item-to-remove)
        (rest coll)
        (cons (first coll) (lazy-seq (remove-first item-to-remove (rest coll))))))))

(defn ^:private edn->element-tree [m keyword-elements* symbol-elements*]
  ;; TODO use tail recur for better performance
  (when (coll? m)
    (->> m
         (reduce
           (fn [acc entry]
             (cond
               (map? entry)
               (concat acc (edn->element-tree entry keyword-elements* symbol-elements*))

               (or (not (coll? entry))
                   (< (count entry) 2))
               acc

               :else
               (let [[k v] (vec entry)
                     element (or (if (keyword? k)
                                   (last (filter #(= (:name %) (name k)) @keyword-elements*))
                                   (last (filter #(= (:symbol %) k) @symbol-elements*)))
                                 ;; fallback to dumb element
                                 {:symbol (or (some-> k str) "nil") :row 0 :col 0 :end-row 0 :end-col 0})
                     _ (swap! keyword-elements* #(remove-first element %))
                     _ (swap! symbol-elements* #(remove-first element %))
                     document-symbol (element->document-symbol element)
                     kind (cond
                            (string? v) :string
                            (keyword? v) :field
                            (boolean? v) :boolean
                            (number? v) :number
                            (vector? v) :array
                            (list? v) :array
                            (set? v) :array
                            :else :struct)
                     document-symbol (assoc document-symbol :kind kind)]
                 (conj acc
                       (cond
                         (map? v)
                         (assoc document-symbol
                                :children
                                (edn->element-tree
                                  v
                                  keyword-elements*
                                  symbol-elements*))

                         (coll? v)
                         (shared/assoc-some
                           document-symbol
                           :children
                           (->> v
                                (keep
                                  #(edn->element-tree
                                     %
                                     keyword-elements*
                                     symbol-elements*))
                                flatten
                                seq))

                         :else
                         document-symbol)))))
           [])
         (sort-by symbol-order))))

(defn document-symbols [db uri]
  (if (identical? :edn (shared/uri->file-type uri))
    (try
      (some-> (parser/safe-zloc-of-file db uri)
              parser/safe-zloc-sexpr
              (edn->element-tree (atom (get-in db [:analysis uri :keyword-usages]))
                                 (atom (get-in db [:analysis uri :symbols]))))
      (catch Exception e
        (logger/error e)))
    (->> (q/find-element-definitions db uri)
         (map element->document-symbol)
         (sort-by symbol-order)
         vec)))
