(ns clojure-lsp.features.semantic-tokens-test
  (:require
    [clojure-lsp.feature.semantic-tokens :as semantic-tokens]
    [clojure-lsp.test-helper :as h]
    [clojure.string :as clojure.string]
    [clojure.test :refer [deftest is testing]]))

(defn ^:private ->token
  [usage token-type]
  (#'semantic-tokens/element->absolute-token usage token-type))

(def refered-usage-a
  {:name-row 7
   :name-end-row 7
   :name-col 4
   :name-end-col 7
   :bucket :var-usages})

(def refered-usage-b
  {:name-row 7
   :name-end-row 7
   :name-col 11
   :name-end-col 14
   :bucket :var-definitions})

(def refered-usage-c
  {:name-row 9
   :name-end-row 9
   :name-col 3
   :name-end-col 6
   :bucket :locals})

(def refered-usages
  [refered-usage-a
   refered-usage-b
   refered-usage-c])

(def refered-tokens
  (map #(->token % :function) refered-usages))

(defn code [& strings] (clojure.string/join "\n" strings))

(deftest usage->absolute-token
  (is (= [6 3 3 1 -1]
         (#'semantic-tokens/element->absolute-token refered-usage-a
                                                  :function))))

(deftest absolute-token->relative-token
  (testing "without previous token"
    (is (= [6 3 3 1 -1]
           (#'semantic-tokens/absolute-token->relative-token refered-tokens
                                                             0
                                                             (->token refered-usage-a :function)))))
  (testing "same line token"
    (is (= [0 7 3 1 -1]
           (#'semantic-tokens/absolute-token->relative-token refered-tokens
                                                             1
                                                             (->token refered-usage-b :function)))))

  (testing "other line token"
    (is (= [2 2 3 1 -1]
           (#'semantic-tokens/absolute-token->relative-token refered-tokens
                                                             2
                                                             (->token refered-usage-c :function))))))

(deftest full-tokens
  (testing "testing tokens order"
    (h/load-code-and-locs
      (code "(ns some.ns"
            "  (:require [foo.bar :as f :refer [baz]]))"
            ""
            "(def bla 2)"
            "baz"
            "f/bar"
            ""
            "bla"
            "(comment)"))
    (is (= [3 1 3 2 -1
            5 1 7 2 -1]
           (semantic-tokens/full-tokens "file:///a.clj"))))
  (testing "testing macro refered tokens"
    (h/load-code-and-locs
      (code "(ns some.ns (:require [clojure.test :refer [deftest]]))"
            "(deftest some-test 1)"))
    (is (= [1 1 7 2 -1]
           (semantic-tokens/full-tokens "file:///a.clj"))))
  (testing "testing macro core tokens"
    (h/load-code-and-locs (code "(comment 1)"))
    (is (= [0 1 7 2 -1]
           (semantic-tokens/full-tokens "file:///a.clj")))))

(deftest range-tokens
  (testing "testing tokens only for range"
    (h/load-code-and-locs
      (code "(ns some.ns (:require [foo.bar :refer [baz]]))"
            "(def bla baz)"
            "baz"
            "(comment)"
            "baz"))
    (is (= [3 1 7 2 -1]
           (semantic-tokens/range-tokens "file:///a.clj" {:name-row 3 :name-col 0 :name-end-row 4 :name-end-col 0})))))
