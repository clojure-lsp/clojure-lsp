(ns clojure-lsp.feature.document-symbol)

(defn is-declaration? [e]
  (and (get-in e [:tags :declare])
       (or (get-in e [:tags :local])
         (get-in e [:tags :public]))))

(defn element->symbol-kind [el]
  (cond
    (#{:namespace-usages} (:bucket el)) :namespace
    (or (:macro el)
        (:fixed-arities el)) :function
    (#{:var-definitions :var-usages} (:bucket el)) :variable
    :else :null))
