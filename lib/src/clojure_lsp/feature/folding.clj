(ns clojure-lsp.feature.folding
  (:require
   [clojure-lsp.queries :as q]))

(set! *warn-on-reflection* true)

(defn folding-range [uri db]
  (->> (q/find-element-definitions db uri)
       (keep (fn [{:keys [row col end-row end-col] :as a}]
               (when (and row col end-row end-col)
                 {:start-line (dec row)
                  :start-character (dec col)
                  :end-line (dec end-row)
                  :end-character (dec end-col)
                  :kind :region})))))
