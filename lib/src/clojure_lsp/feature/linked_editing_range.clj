(ns clojure-lsp.feature.linked-editing-range
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]))

(set! *warn-on-reflection* true)

(defn ^:private alias-range [alias-length {:keys [name-row name-col]}]
  {:start (shared/row-col->position name-row name-col)
   :end (shared/row-col->position name-row (+ name-col alias-length))})

(defn ranges [uri row col db]
  (when-let [element (q/find-element-under-cursor db uri row col)]
    (when (= :namespace-alias (:bucket element))
      (when-let [elements (q/find-references db element true)]
        (let [alias-length (count (str (:alias element)))]
          {:ranges (->> elements
                        (map (partial alias-range alias-length)))})))))
