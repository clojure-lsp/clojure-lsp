(ns clojure-lsp.feature.signature-help
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.file-management :as f.file-management]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.queries :as q]
    [clojure-lsp.refactor.edit :as edit]
    [clojure-lsp.shared :as shared]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]
    [taoensso.timbre :as log])
  (:import
   [clojure.lang PersistentVector]))


(defn ^:private function-loc->arglist-nodes [zloc]
  (->> zloc
       z/up
       z/node
       n/children
       (remove n/whitespace-or-comment?)
       (drop 1)))

;; TODO Use it when we return parameters
;; (defn ^:private get-active-parameter-index [arglist-nodes cursor-row cursor-col]
;;   (let [selected-arg (->> arglist-nodes
;;                           reverse
;;                           (filter (fn [node]
;;                                     (let [{:keys [row col]} (meta node)]
;;                                       (or (< row cursor-row)
;;                                           (and (= row cursor-row)
;;                                                (<= col cursor-col))))))
;;                           first)]
;;     (max (.indexOf ^PersistentVector arglist-nodes selected-arg) 0)))

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

(defn ^:private definition->signature-informations [{:keys [arglist-strs] :as definition}]
  (map (fn [arg]
         (-> {:label (format "(%s %s)" (-> definition :name str) arg)}
             (shared/assoc-some :documentation (:doc definition))))
       arglist-strs))

(defn signature-help [uri row col]
  (let [filename (shared/uri->filename uri)
        zloc (-> (f.file-management/force-get-document-text uri)
                 (parser/loc-at-pos row col))
        function-loc (edit/find-function-usage-name zloc)]
    (when function-loc
      (let [arglist-nodes (function-loc->arglist-nodes function-loc)
            function-meta (meta (z/node function-loc))
            definition (q/find-definition-from-cursor (:analysis @db/db) filename (:row function-meta) (:col function-meta))
            signatures (definition->signature-informations definition)]
        (when (seq signatures)
          {:signatures signatures
           :active-signature (get-active-signature-index definition arglist-nodes)})))))
