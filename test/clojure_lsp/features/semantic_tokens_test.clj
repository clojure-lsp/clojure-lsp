(ns clojure-lsp.features.semantic-tokens-test
  (:require [clojure-lsp.parser :as parser]
            [clojure-lsp.features.semantic-tokens :as semantic-tokens]
            [clojure.string :as clojure.string]
            [clojure.test :refer [deftest is testing]]))

(defn ^:private ->token
  [usage token-type]
  (#'semantic-tokens/usage->absolute-token usage token-type))

(def refered-usage-a
  {:row 7
   :end-row 7
   :col 4
   :end-col 7
   :file-type :clj
   :sym 'foo.bar/match?
   :str "baz"
   :argc 2})

(def refered-usage-b
  {:row 7
   :end-row 7
   :col 11
   :end-col 14
   :file-type :clj
   :sym 'foo.bar/match?
   :str "baz"
   :argc 2})

(def refered-usage-c
  {:row 9
   :end-row 9
   :col 3
   :end-col 6
   :file-type :clj
   :sym 'foo.bar/baz
   :str "baz"
   :argc 2})

(def refered-usages
  [refered-usage-a
   refered-usage-b
   refered-usage-c])

(def refered-tokens
  (map #(->token % :function) refered-usages))

(defn long-str [& strings] (clojure.string/join "\n" strings))

(deftest usage->absolute-token
  (is (= [6 3 3 0 -1]
         (#'semantic-tokens/usage->absolute-token refered-usage-a
                                                  :function))))

(deftest absolute-token->relative-token
  (testing "without previous token"
    (is (= [6 3 3 0 -1]
           (#'semantic-tokens/absolute-token->relative-token refered-tokens
                                                             0
                                                             (->token refered-usage-a :function)))))
  (testing "same line token"
    (is (= [0 7 3 0 -1]
           (#'semantic-tokens/absolute-token->relative-token refered-tokens
                                                             1
                                                             (->token refered-usage-b :function)))))

  (testing "other line token"
    (is (= [2 2 3 0 -1]
           (#'semantic-tokens/absolute-token->relative-token refered-tokens
                                                             2
                                                             (->token refered-usage-c :function))))))

(deftest full
  (testing "testing tokens order"
    (let [code (long-str "(ns some.ns (:require [foo.bar :refer [baz]]))"
                         "(def bla baz)"
                         "baz"
                         "(comment)"
                         "baz")
          usages (parser/find-usages code :clj {})]
      (is (= [1 9 3 0 -1
              1 0 3 0 -1
              1 1 7 1 -1
              1 0 3 0 -1]
             (semantic-tokens/full usages)))))
  (testing "testing user refered tokens"
    (let [code (long-str "(ns some.ns (:require [foo.bar :refer [baz]]))"
                         "(def bla baz)")
          usages (parser/find-usages code :clj {})]
      (is (= [1 9 3 0 -1]
             (semantic-tokens/full usages)))))
  (testing "testing macro refered tokens"
    (let [code (long-str "(ns some.ns (:require [clojure.test :refer [deftest]]))"
                         "(deftest some-test 1)")
          usages (parser/find-usages code :clj {})]
      (is (= [1 1 7 1 -1]
             (semantic-tokens/full usages)))))
  (testing "testing macro core tokens"
    (let [code (long-str "(comment 1)")
          usages (parser/find-usages code :clj {})]
      (is (= [0 1 7 1 -1]
             (semantic-tokens/full usages))))))
