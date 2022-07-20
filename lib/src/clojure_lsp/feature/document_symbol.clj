(ns clojure-lsp.feature.document-symbol
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]))

(set! *warn-on-reflection* true)

(defn element->symbol-kind [el]
  (cond
    (#{:namespace-usages :namespace-definitions} (:bucket el)) :namespace
    (or (:macro el)
        (:fixed-arities el)) :function
    (#{:var-definitions :var-usages} (:bucket el)) :variable
    :else :null))

(defn element->name [{elem-name :name :keys [dispatch-val]}]
  (cond-> (name elem-name)
    dispatch-val (str " " dispatch-val)))

(defn ^:private element->document-symbol [e]
  (shared/assoc-some
    {:name (element->name e)
     :kind (element->symbol-kind e)
     :range (shared/->scope-range e)
     :selection-range (shared/->range e)
     :tags (cond-> []
             (:deprecated e) (conj 1))}
    :detail (when (:private e)
              "private")))

(defn ^:private symbol-order [{:keys [selection-range]}]
  [(:line (:start selection-range)) (:character (:start selection-range))])

(defn document-symbols [db filename]
  (let [namespace-definition (q/find-namespace-definition-by-filename db filename)]
    [{:name (or (some-> namespace-definition :name name)
                filename)
      :kind (element->symbol-kind namespace-definition)
      :range shared/full-file-range
      :selection-range (if namespace-definition
                         (shared/->scope-range namespace-definition)
                         shared/full-file-range)
      :children (->> (concat (q/find-var-definitions db filename true)
                             (q/find-defmethods db filename))
                     (map element->document-symbol)
                     (sort-by symbol-order)
                     vec)}]))
