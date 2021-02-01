(ns clojure-lsp.feature.document-symbol
  (:require
    [taoensso.timbre :as log]))

(defn declaration? [e]
  (#{:namespace-definitions :var-definitions} (:bucket e)))

(defn element->symbol-kind [el]
  (cond
    (#{:namespace-usages :namespace-definitions} (:bucket el)) :namespace
    (or (:macro el)
        (:fixed-arities el)) :function
    (#{:var-definitions :var-usages} (:bucket el)) :variable
    :else :null))
