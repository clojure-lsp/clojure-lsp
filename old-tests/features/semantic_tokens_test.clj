(ns clojure-lsp.features.semantic-tokens-test
  (:require [clojure-lsp.parser :as parser]
            [clojure-lsp.feature.semantic-tokens :as semantic-tokens]
            [clojure.string :as clojure.string]
            [clojure.test :refer [deftest is testing]]))

(defn ^:private ->token
  [usage token-type]
  (#'semantic-tokens/usage->absolute-token usage token-type))

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

(defn long-str [& strings] (clojure.string/join "\n" strings))

(deftest usage->absolute-token
  (is (= [6 3 3 1 -1]
         (#'semantic-tokens/usage->absolute-token refered-usage-a
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
    (let [code (long-str "(ns some.ns"
                         "  (:require [foo.bar :as f :refer [baz]]))"
                         ""
                         "(def bla 2)"
                         "baz"
                         "f/bar"
                         ""
                         "bla"
                         "(comment)")
          usages (parser/find-usages code :clj {})]
      (is (= [4 0 3 1 -1
              1 0 1 0 -1
              0 2 3 1 -1
              2 0 3 1 -1
              1 1 7 2 -1]
             (semantic-tokens/full-tokens usages)))))
  (testing "testing user refered tokens"
    (let [code (long-str "(ns some.ns (:require [foo.bar :refer [baz]]))"
                         "(def bla baz)")
          usages (parser/find-usages code :clj {})]
      (is (= [1 9 3 1 -1]
             (semantic-tokens/full-tokens usages)))))
  (testing "testing macro refered tokens"
    (let [code (long-str "(ns some.ns (:require [clojure.test :refer [deftest]]))"
                         "(deftest some-test 1)")
          usages (parser/find-usages code :clj {})]
      (is (= [1 1 7 2 -1]
             (semantic-tokens/full-tokens usages)))))
  (testing "testing macro core tokens"
    (let [code (long-str "(comment 1)")
          usages (parser/find-usages code :clj {})]
      (is (= [0 1 7 2 -1]
             (semantic-tokens/full-tokens usages)))))
  (testing "testing function declared tokens"
    (let [code (long-str "(def foo 1)"
                         "foo")
          usages (parser/find-usages code :clj {})]
      (is (= [1 0 3 1 -1]
             (semantic-tokens/full-tokens usages)))))
  (testing "testing type alias for function tokens"
    (let [code (long-str "(ns some.ns (:require [foo.bar :as fb]))"
                         "fb/some-foo-bar")
          usages (parser/find-usages code :clj {})]
      (is (= [1 0 2 0 -1
              0 3 12 1 -1]
             (semantic-tokens/full-tokens usages))))))

(deftest range-tokens
  (testing "testing tokens only for range"
    (let [code (long-str "(ns some.ns (:require [foo.bar :refer [baz]]))"
                         "(def bla baz)"
                         "baz"
                         "(comment)"
                         "baz")
          range {:row 3 :col 0 :end-row 4 :end-col 0}
          usages (parser/find-usages code :clj {})]
      (is (= [2 0 3 1 -1
              1 1 7 2 -1]
             (semantic-tokens/range-tokens usages range))))))
