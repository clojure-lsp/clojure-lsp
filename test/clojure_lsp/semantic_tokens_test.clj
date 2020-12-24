(ns clojure-lsp.semantic-tokens-test
  (:require [clojure-lsp.parser :as parser]
            [clojure-lsp.semantic-tokens :as semantic-tokens]
            [clojure.string :as clojure.string]
            [clojure.test :refer :all]))

(defn ^:private ->token
  [usage token-type]
  (#'semantic-tokens/usage->absolute-token usage token-type))

(def function-usage-a
  {:row 7
   :end-row 7
   :col 4
   :end-col 7
   :file-type :clj
   :sym 'foo.bar/baz
   :str "baz"
   :argc 2})

(def function-usage-b
  {:row 7
   :end-row 7
   :col 11
   :end-col 14
   :file-type :clj
   :sym 'foo.bar/baz
   :str "baz"
   :argc 2})

(def function-usage-c
  {:row 9
   :end-row 9
   :col 3
   :end-col 6
   :file-type :clj
   :sym 'foo.bar/baz
   :str "baz"
   :argc 2})

(def function-usages
  [function-usage-a
   function-usage-b
   function-usage-c])

(def function-tokens
  (map #(->token % :function) function-usages))

(defn long-str [& strings] (clojure.string/join "\n" strings))

(deftest usage->absolute-token
  (is (= [6 3 3 0 -1]
         (#'semantic-tokens/usage->absolute-token function-usage-a
                                                  :function))))

(deftest absolute-token->relative-token
  (testing "without previous token"
    (is (= [6 3 3 0 -1]
           (#'semantic-tokens/absolute-token->relative-token function-tokens
                                                             0
                                                             (->token function-usage-a :function)))))
  (testing "same line token"
    (is (= [0 4 3 0 -1]
           (#'semantic-tokens/absolute-token->relative-token function-tokens
                                                             1
                                                             (->token function-usage-b :function)))))

  (testing "other line token"
    (is (= [2 2 3 0 -1]
           (#'semantic-tokens/absolute-token->relative-token function-tokens
                                                             2
                                                             (->token function-usage-c :function))))))

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
              2 0 3 0 -1]
             (semantic-tokens/full usages)))))
  (testing "testing refered tokens"
    (let [code (long-str "(ns some.ns (:require [foo.bar :refer [baz]]))"
                         "(def bla baz)")
          usages (parser/find-usages code :clj {})]
      (is (= [1 9 3 0 -1]
             (semantic-tokens/full usages))))))
