(ns foo
  (:require
    [clojure.test :refer [deftest testing is]]))

(deftest foo
  (testing "bar 1"
    (is true))
  (testing "bar 2"
    (testing "bar 3"
      (is true))
    (testing "bar 4"
      (is true))))

;; clojure/textDocument/testTree
{:uri "..."
 :tests [{:name "foo"
          :range {:start {:line 1 :character 1} :end {:line 8 :character 18}}
          :name-range {:start {:line 1 :character 10} :end {:line 1 :character 12}}
          :kind :deftest
          :children [{:name "bar 1"
                      :range {:start {:line 2 :character 3} :end {:line 3 :character 14}}
                      :name-range {:start {:line 2 :character 13} :end {:line 2 :character 17}}
                      :kind :testing}
                     {:name "bar 2"
                      :range {:start {:line 4 :character 3} :end {:line 8 :character 17}}
                      :name-range {:start {:line 4 :character 13} :end {:line 4 :character 17}}
                      :kind :testing
                      :children [{:name "bar 3"
                                  :range {:start {:line 5 :character 5} :end {:line 6 :character 16}}
                                  :name-range {:start {:line 5 :character 15} :end {:line 5 :character 19}}
                                  :kind :testing}
                                 {:name "bar 4"
                                  :range {:start {:line 7 :character 5} :end {:line 8 :character 16}}
                                  :name-range {:start {:line 7 :character 15} :end {:line 7 :character 19}}
                                  :kind :testing}]}]}]}
