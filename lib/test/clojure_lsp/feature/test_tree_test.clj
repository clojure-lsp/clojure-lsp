(ns clojure-lsp.feature.test-tree-test
  (:require
   [clojure-lsp.feature.test-tree :as f.test-tree]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest testing]]))

(h/reset-db-after-test)

(deftest tree-test
  (testing "valid test tree"
    (h/load-code-and-locs (h/code "(ns foo.bar (:require [clojure.test :refer :all]))"
                                  "(deftest some-test"
                                  "  (testing \"foo\""
                                  "    (+ 1 2))"
                                  "  (testing \"bar baz\""
                                  "    (testing \"qux\" 123)"
                                  "    (testing \"last one\""
                                  "      (+ 2 3)))"
                                  ")"))
    (h/assert-submap
      {:uri "file:///a.clj"
       :tree
       {:name "foo.bar"
        :range {:start {:line 0 :character 0}
                :end {:line 0 :character 50}}
        :name-range {:start {:line 0 :character 4}
                     :end {:line 0 :character 11}}
        :kind :namespace
        :children
        [{:name "some-test"
          :range {:start {:line 1 :character 0}
                  :end {:line 8 :character 1}}
          :name-range {:start {:line 1 :character 9}
                       :end {:line 1 :character 18}}
          :kind :deftest
          :children
          [{:name "foo"
            :range {:start {:line 2 :character 2}
                    :end {:line 3 :character 12}}
            :name-range {:start {:line 2 :character 3}
                         :end {:line 2 :character 10}}
            :kind :testing}
           {:name "bar baz"
            :range {:start {:line 4 :character 2}
                    :end {:line 7 :character 15}}
            :name-range {:start {:line 4 :character 3}
                         :end {:line 4 :character 10}}
            :kind :testing
            :children
            [{:name "qux"
              :range {:start {:line 5 :character 4}
                      :end {:line 5 :character 23}}
              :name-range {:start {:line 5 :character 5}
                           :end {:line 5 :character 12}}
              :kind :testing}
             {:name "last one"
              :range {:start {:line 6 :character 4}
                      :end {:line 7 :character 14}}
              :name-range {:start {:line 6 :character 5}
                           :end {:line 6 :character 12}}
              :kind :testing}]}]}]}}
      (f.test-tree/tree "file:///a.clj" (h/db)))))
