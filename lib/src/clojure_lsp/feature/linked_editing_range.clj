(ns clojure-lsp.feature.linked-editing-range
  (:require
   [clojure-lsp.queries :as q]
   [lsp4clj.shared :as shared]))

(set! *warn-on-reflection* true)

(defn ranges [uri row col db]
  (let [filename (shared/uri->filename uri)
        elements (q/find-references-from-cursor (:analysis @db) filename row col true db)
        same-file-references-only? (= 1 (count (keys (group-by :filename elements))))]
    (if same-file-references-only?
      {:ranges (->> elements
                    (map shared/->range))}
      (if (seq elements)
        {:error {:code :invalid-params
                 :message "There are references on other files for this symbol"}}
        {:error {:code :invalid-params
                 :message "No symbol found"}}))))
