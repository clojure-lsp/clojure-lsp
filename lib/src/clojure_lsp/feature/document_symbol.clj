(ns clojure-lsp.feature.document-symbol
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]))

(set! *warn-on-reflection* true)

(def declaration-buckets #{:namespace-definitions :var-definitions})

(defn element->symbol-kind [el]
  (cond
    (#{:namespace-usages :namespace-definitions} (:bucket el)) :namespace
    (or (:macro el)
        (:fixed-arities el)) :function
    (#{:var-definitions :var-usages} (:bucket el)) :variable
    :else :null))

(defn ^:private element-symbol [e]
  (shared/assoc-some
    {:kind (element->symbol-kind e)
     :range (shared/->scope-range e)
     :selection-range (shared/->range e)
     :tags (cond-> []
             (:deprecated e) (conj 1))}
    :detail (when (:private e)
              "private")))

(defn ^:private var-defs [db filename]
  (->> (q/find-var-definitions db filename true)
       (map (fn [e]
              (assoc (element-symbol e)
                     :name (-> e :name name))))))

(defn ^:private defmethods [db filename]
  (->> (q/find-defmethods db filename)
       (map (fn [e]
              (assoc (element-symbol e)
                     :name (-> e :name name))))))

(defn document-symbols [db filename]
  (let [namespace-definition (q/find-namespace-definition-by-filename db filename)]
    [{:name (or (some-> namespace-definition :name name)
                filename)
      :kind (element->symbol-kind namespace-definition)
      :range shared/full-file-range
      :selection-range (if namespace-definition
                         (shared/->scope-range namespace-definition)
                         shared/full-file-range)
      :children (->> (concat (var-defs db filename)
                             (defmethods db filename))
                     (sort-by #(shared/position->line-column (:start (:selection-range %))))
                     vec)}]))
