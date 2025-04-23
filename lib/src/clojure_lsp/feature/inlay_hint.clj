(ns clojure-lsp.feature.inlay-hint
  (:require
   [clojure-lsp.logger :as logger]
   [clojure-lsp.queries :as q]))

(defn inlay-hint [uri db]
  (try
    (into []
          (comp
            (filter :args)
            (remove #(:external? (q/find-definition db %)))
            (mapcat (fn [{:keys [args]}]
                      (keep (fn [{:keys [row col end-row end-col name]}]
                              (when (and name row col)
                                {:label (str "∥" name)
                                 :position {:line (dec end-row) :character (dec end-col)}
                                 :kind :parameter
                                 :padding-left true})) args))))
          (q/find-var-usages db uri))
    (catch Exception e
      (logger/error e))))
