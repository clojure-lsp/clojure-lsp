(ns clojure-lsp.feature.linked-editing-range-test
  (:require
   [clojure-lsp.feature.linked-editing-range :as f.linked-editing-range]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest testing]]))

(h/reset-components-before-test)

(deftest ranges
  (h/load-code-and-locs
    (h/code "(ns foo)"
            ":foo"
            "(defn foo [a] a)"
            "(defn some-fn [a] a)"
            "(foo 1)"
            "some-fn"
            "foo"))
  (h/load-code-and-locs
    (h/code "(ns bar"
            " (:require [foo :as f]))"
            "(f/some-fn 3)"
            "") "file:///b.clj")
  (testing "when all references are on the same file"
    (h/assert-submap
      {:ranges [{:start {:line 2, :character 6}, :end {:line 2, :character 9}}
                {:start {:line 4, :character 1}, :end {:line 4, :character 4}}
                {:start {:line 6, :character 0}, :end {:line 6, :character 3}}]}
      (f.linked-editing-range/ranges h/default-uri 5 2 (h/db))))
  (testing "when some reference is on another file"
    (h/assert-submap
      {:error {:code :invalid-params
               :message "There are references on other files for this symbol"}}
      (f.linked-editing-range/ranges h/default-uri 6 2 (h/db)))))
