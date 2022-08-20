(ns clojure-lsp.features.semantic-tokens-test
  (:require
   [clojure-lsp.feature.semantic-tokens :as semantic-tokens]
   [clojure-lsp.test-helper :as h]
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

(deftest usage->absolute-token
  (is (= [6 3 3 2 0]
         (#'semantic-tokens/element->absolute-token refered-usage-a
                                                    :function))))

(deftest absolute-token->relative-token
  (testing "without previous token"
    (is (= [6 3 3 2 0]
           (#'semantic-tokens/absolute-token->relative-token [nil
                                                              (->token refered-usage-a :function)]))))
  (testing "same line token"
    (is (= [0 7 3 2 0]
           (#'semantic-tokens/absolute-token->relative-token [(->token refered-usage-a :function)
                                                              (->token refered-usage-b :function)]))))

  (testing "other line token"
    (is (= [2 2 3 2 0]
           (#'semantic-tokens/absolute-token->relative-token [(->token refered-usage-b :function)
                                                              (->token refered-usage-c :function)])))))

(deftest full-tokens
  (let [components (h/make-components)
        assert-full-tokens (fn [code tokens]
                             (h/load-code code h/default-uri components)
                             (is (= tokens
                                    (semantic-tokens/full-tokens h/default-uri (h/db components)))))]
    (testing "tokens order"
      (assert-full-tokens
        (h/code "(ns some.ns"
                "  (:require [foo.bar :as f :refer [baz]]))"
                ""
                "(def bla 2)"
                "baz"
                "f/bar"
                ""
                "bla"
                "(comment)")
        [0 4 7 0 0
         1 3 8 4 0
         0 18 3 4 0
         0 6 6 4 0
         0 8 3 2 0
         2 1 3 3 0
         0 4 3 2 1
         1 0 3 2 0
         1 0 1 1 0
         0 1 1 8 0
         0 1 3 2 0
         2 0 3 2 0
         1 1 7 3 0]))
    (testing "macro refered tokens"
      (assert-full-tokens
        (h/code "(ns some.ns (:require [clojure.test :refer [deftest]]))"
                "(deftest some-test 1)")
        [0 4 7 0 0
         0 9 8 4 0
         0 23 6 4 0
         0 8 7 3 0
         1 1 7 3 0
         0 8 9 2 1]))
    (testing "macro core tokens"
      (assert-full-tokens
        (h/code "(comment 1)")
        [0 1 7 3 0]))
    (testing "function definition tokens"
      (assert-full-tokens
        (h/code "(def foo 1)"
                "foo")
        [0 1 3 3 0
         0 4 3 2 1
         1 0 3 2 0]))
    (testing "variable with defaultLibrary modifier tokens"
      (assert-full-tokens
        (h/code "(def *anything*) *anything*")
        [0 1 3 3 0
         0 4 10 2 1
         0 12 10 6 2]))
    (testing "type alias for function tokens"
      (assert-full-tokens
        (h/code "(ns some.ns (:require [foo.bar :as fb]))"
                "fb/some-foo-bar")
        [0 4 7 0 0
         0 9 8 4 0
         0 18 3 4 0
         1 0 2 1 0
         0 2 1 8 0
         0 1 12 2 0]))
    (testing "type alias for macro tokens"
      (assert-full-tokens
        (h/code "(ns some.ns (:require [clojure.test :as test]))"
                "test/deftest")
        [0 4 7 0 0
         0 9 8 4 0
         0 23 3 4 0
         1 0 4 1 0
         0 4 1 8 0
         0 1 7 3 0]))
    #_(testing "java classes for function tokens"
        (assert-full-tokens-and-locs
          (h/code "(ns some.ns)"
                  "^java.lang.String \"\""
                  "String"
                  "^String \"\"")
          [1 1 16 1 0
           1 0 6 4 0
           1 1 6 4 0]))
  ;; TODO need more java kondo analysis
    #_(testing "java static methods for method + class tokens"
        (assert-full-tokens-and-locs
          (h/code "(ns some.ns)"
                  "(Character/isUpperCase \"some-string\")")
          [1 1 21 1 0]))
    (testing "java methods for method tokens"
      (assert-full-tokens
        (h/code "(ns some.ns)"
                "(.equals \"some-string\" \"other-string\")")
        [0 4 7 0 0
         1 1 7 7 0]))
    (testing "keywords for keyword tokens"
      (assert-full-tokens
        (h/code "(ns some.ns (:require [foo :as foo]))"
                ":foo"
                ":some.ns/foo"
                "::foo"
                "#:some.ns{:foo 1 :bar/foo 2}"
                "::foo/something")
        [0 4 7 0 0
         0 9 8 4 0
         0 14 3 4 0
         1 0 4 4 0
         1 0 8 1 0
         0 8 1 8 0
         0 1 3 4 0
         1 0 5 4 0
         1 10 4 4 0
         0 7 4 1 0
         0 4 1 8 0
         0 1 3 4 0
         1 0 5 1 0
         0 5 1 8 0
         0 1 9 4 0]))
    (testing "locals destructuring for variable tokens"
      (assert-full-tokens
        (h/code "(fn [{:keys [foo bar]}])")
        [0 1 2 3 0
         0 5 5 4 0
         0 0 5 4 0
         0 7 3 6 0
         0 4 3 6 0]))
    (testing "defrecord/deftype"
      (assert-full-tokens
        (h/code "(defrecord Something []"
                "  Otherthing"
                "(some-method [this] 123))")
        [0 1 9 3 0
         0 10 9 2 1
         0 0 9 2 1
         0 0 9 2 1
         2 1 11 7 4
         0 13 4 6 0]))))

(deftest range-tokens
  (testing "tokens only for range"
    (let [components (h/make-components)]
      (h/load-code
        (h/code "(ns some.ns (:require [foo.bar :refer [baz]]))"
                "(def bla baz)"
                "baz"
                "(comment)"
                "baz") h/default-uri components)
      (is (= [2 0 3 2 0
              1 1 7 3 0]
             (semantic-tokens/range-tokens h/default-uri
                                           {:name-row 3 :name-col 0 :name-end-row 4 :name-end-col 0}
                                           (h/db components)))))))
