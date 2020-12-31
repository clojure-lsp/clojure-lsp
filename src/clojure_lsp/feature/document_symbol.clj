(ns clojure-lsp.feature.document-symbol)

(defn is-declaration? [e]
  (and (get-in e [:tags :declare])
       (or (get-in e [:tags :local])
         (get-in e [:tags :public]))))

(defn entry-kind->symbol-kind [k]
  (case k
    :module :namespace
    :function :function
    :declaration :variable
    :null))
