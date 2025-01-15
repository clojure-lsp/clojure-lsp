(ns clojure-lsp.feature.linked-editing-range-test
  (:require
   [clojure-lsp.feature.linked-editing-range :as f.linked-editing-range]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(deftest ranges
  (let [[[alias-r alias-c]
         [ref-r ref-c]
         [other-r other-c]]
        (h/load-code-and-locs
          (h/code "(ns foo"
                  "  (:require [bar :as b|az]))"
                  "(def xyz 2)"
                  "(ba|z/some-fn xy|z)"
                  "(baz/some-other-fn 4)"))]
    (testing "aliases are linked"
      (h/assert-submap
        {:ranges [{:start {:line 1, :character 21}, :end {:line 1, :character 24}}
                  {:start {:line 3, :character 1}, :end {:line 3, :character 4}}
                  {:start {:line 4, :character 1}, :end {:line 4, :character 4}}]}
        (f.linked-editing-range/ranges h/default-uri alias-r alias-c (h/db))))
    (testing "but not in the other direction"
      (is (nil? (f.linked-editing-range/ranges h/default-uri ref-r ref-c (h/db)))))
    (testing "and not for other symbols"
      (is (nil? (f.linked-editing-range/ranges h/default-uri other-r other-c (h/db)))))))
