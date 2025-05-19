(ns clojure-lsp.feature.selection-range-test
  (:require
   [clojure-lsp.feature.selection-range :as f.selection-range]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest]]))

(deftest selection-ranges-test
  (let [[[row col]] (h/load-code-and-locs
                      (h/code "(ns foo)"
                              "(def something (con|cat [] [1]))"))]
    (h/assert-submaps
      [{:range
        {:start {:line 1 :character 16} :end {:line 1 :character 22}}
        :parent
        {:range
         {:start {:line 1 :character 16} :end {:line 1 :character 29}}
         :parent
         {:range
          {:start {:line 1 :character 15} :end {:line 1 :character 30}}
          :parent
          {:range
           {:start {:line 1 :character 1} :end {:line 1 :character 30}}
           :parent
           {:range
            {:start {:line 1 :character 0} :end {:line 1 :character 31}}}}}}}]
      (f.selection-range/selection-ranges
        h/default-uri
        [[row col]]
        (h/components)))))
