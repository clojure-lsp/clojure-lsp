(ns clojure-lsp.feature.signature-help
  (:require
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared :refer [assoc-some]]
   [edamame.core :as edamame]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z])
  (:import
   [clojure.lang PersistentVector]))

(set! *warn-on-reflection* true)

(defn ^:private function-loc->arglist-nodes [zloc]
  (->> zloc
       z/up
       z/node
       n/children
       (remove n/whitespace-or-comment?)
       (drop 1)))

(defn ^:private get-active-parameter-index
  [signatures active-signature arglist-nodes cursor-row cursor-col]
  (let [params-count (-> (nth signatures active-signature)
                         :parameters
                         count)
        selected-arg (->> arglist-nodes
                          reverse
                          (filter (fn [node]
                                    (let [{:keys [row col]} (meta node)]
                                      (or (< row cursor-row)
                                          (and (= row cursor-row)
                                               (<= col cursor-col))))))
                          first)]
    (if selected-arg
      (let [index (.indexOf ^PersistentVector (vec arglist-nodes) selected-arg)]
        (if (> index (dec params-count))
          (dec params-count)
          index))
      0)))

(defn ^:private get-active-signature-index [{:keys [fixed-arities arglist-strs]} arglist-nodes]
  (let [arities (vec (sort-by max (if fixed-arities
                                    (if (= (count fixed-arities) (count arglist-strs))
                                      fixed-arities
                                      (conj fixed-arities (inc (apply max fixed-arities))))
                                    #{(count arglist-strs)})))
        args-count (count arglist-nodes)
        current-arity (first (filter #(= % args-count) arities))]
    (if current-arity
      (.indexOf ^PersistentVector arities current-arity)
      (if (>= args-count (count arities))
        (.indexOf ^PersistentVector arities (apply max arities))
        (.indexOf ^PersistentVector arities (apply min arities))))))

(defn ^:private arglist-str->parameters [arglist-str]
  (let [parameters (edamame/parse-string arglist-str {:auto-resolve #(symbol (str ":" %))})
        rest-args? (some #(= '& %) parameters)
        available-params (filter (complement #(= '& %)) parameters)
        params-count (dec (count available-params))]
    (->> available-params
         (map-indexed (fn [index arg]
                        (let [last-arg? (= index params-count)]
                          (if (and rest-args? last-arg?)
                            {:label (format "& %s" arg)}
                            {:label (str arg)})))))))

(defn ^:private definition->signature-informations [{:keys [arglist-strs] :as definition}]
  (map (fn [arglist-str]
         (-> {:label (format "(%s %s)" (-> definition :name str) arglist-str)
              :parameters (arglist-str->parameters arglist-str)}
             (assoc-some :documentation (:doc definition))))
       arglist-strs))

(defn signature-help [uri row col db*]
  (when-let [function-loc (some-> (f.file-management/force-get-document-text uri db*)
                                  parser/safe-zloc-of-string
                                  (parser/to-pos row col)
                                  edit/find-function-usage-name-loc)]
    (let [db @db*
          arglist-nodes (function-loc->arglist-nodes function-loc)
          filename (shared/uri->filename uri)
          function-meta (meta (z/node function-loc))
          definition (q/find-definition-from-cursor db filename (:row function-meta) (:col function-meta))
          signatures (definition->signature-informations definition)
          active-signature (get-active-signature-index definition arglist-nodes)]
      (when (seq signatures)
        {:signatures signatures
         :active-parameter (get-active-parameter-index signatures active-signature arglist-nodes row col)
         :active-signature active-signature}))))
