(ns clojure-lsp.feature.folding
  (:require
   [clojure-lsp.queries :as q]))

(set! *warn-on-reflection* true)

(defn folding-range [uri db]
  (->> (q/find-element-definitions db uri)
       (mapv (fn [{:keys [name-row name-col name-end-row name-end-col]}]
               {:start-line name-row
                :start-character name-col
                :end-line name-end-row
                :end-character name-end-col
                :kind :region}))))
