(ns clojure-lsp.feature.linked-editing-range
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]))

(set! *warn-on-reflection* true)

(defn ranges [uri row col db]
  (let [filename (shared/uri->filename uri)
        elements (q/find-references-from-cursor (:analysis @db) filename row col true db)
        same-file-references-only? (= 1 (count (keys (group-by :filename elements))))]
    (when same-file-references-only?
      {:ranges (->> elements
                    (map shared/->range))})))
