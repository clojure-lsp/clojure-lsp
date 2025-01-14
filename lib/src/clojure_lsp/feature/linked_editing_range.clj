(ns clojure-lsp.feature.linked-editing-range
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]))

(set! *warn-on-reflection* true)

(defn ranges [uri row col db]
  (when-let [element (q/find-element-under-cursor db uri row col)]
    (when (= :namespace-alias (:bucket element))
      (when-let [elements (q/find-references db element true)]
        {:ranges (->> elements
                      (map shared/->range))}))))
