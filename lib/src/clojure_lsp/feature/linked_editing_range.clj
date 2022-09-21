(ns clojure-lsp.feature.linked-editing-range
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]))

(set! *warn-on-reflection* true)

(defn ranges [uri row col db]
  (let [elements (q/find-references-from-cursor db uri row col true)
        same-file-references-only? (= 1 (count (keys (group-by :uri elements))))]
    (if same-file-references-only?
      {:ranges (->> elements
                    (map shared/->range))}
      (if (seq elements)
        {:error {:code :invalid-params
                 :message "There are references on other files for this symbol"}}
        {:error {:code :invalid-params
                 :message "No symbol found"}}))))
