(ns clojure-lsp.features.semantic-tokens-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.semantic-tokens :as semantic-tokens]
   [clojure-lsp.test-helper :as h]
   [clojure.string :as clojure.string]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

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
  (is (= [6 3 3 2 0]
         (#'semantic-tokens/element->absolute-token refered-usage-a
                                                    :function))))

(deftest absolute-token->relative-token
  (testing "without previous token"
    (is (= [6 3 3 2 0]
           (#'semantic-tokens/absolute-token->relative-token refered-tokens
                                                             0
                                                             (->token refered-usage-a :function)))))
  (testing "same line token"
    (is (= [0 7 3 2 0]
           (#'semantic-tokens/absolute-token->relative-token refered-tokens
                                                             1
                                                             (->token refered-usage-b :function)))))

  (testing "other line token"
    (is (= [2 2 3 2 0]
           (#'semantic-tokens/absolute-token->relative-token refered-tokens
                                                             2
                                                             (->token refered-usage-c :function))))))

(deftest full-tokens
  (testing "tokens order"
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
    (is (= [0 4 7 0 0
            1 35 3 2 0
            2 1 3 3 0
            0 4 3 2 1
            1 0 3 2 0
            1 0 1 1 0
            0 1 1 8 0
            0 1 3 2 0
            2 0 3 2 0
            1 1 7 3 0]
           (semantic-tokens/full-tokens (h/file-uri "file:///a.clj") db/db))))
  (testing "macro refered tokens"
    (h/load-code-and-locs
      (code "(ns some.ns (:require [clojure.test :refer [deftest]]))"
            "(deftest some-test 1)"))
    (is (= [0 4 7 0 0
            0 40 7 3 0
            1 1 7 3 0
            0 8 9 2 1]
           (semantic-tokens/full-tokens (h/file-uri "file:///a.clj") db/db))))
  (testing "macro core tokens"
    (h/load-code-and-locs (code "(comment 1)"))
    (is (= [0 1 7 3 0]
           (semantic-tokens/full-tokens (h/file-uri "file:///a.clj") db/db))))
  (testing "function definition tokens"
    (h/load-code-and-locs (code "(def foo 1)"
                                "foo"))
    (is (= [0 1 3 3 0
            0 4 3 2 1
            1 0 3 2 0]
           (semantic-tokens/full-tokens (h/file-uri "file:///a.clj") db/db))))
  (testing "variable with defaultLibrary modifier tokens"
    (h/load-code-and-locs (code "(def *anything*) *anything*"))
    (is (= [0 1 3 3 0
            0 4 10 2 1
            0 12 10 6 2]
           (semantic-tokens/full-tokens (h/file-uri "file:///a.clj") db/db))))
  (testing "type alias for function tokens"
    (h/load-code-and-locs (code "(ns some.ns (:require [foo.bar :as fb]))"
                                "fb/some-foo-bar"))
    (is (= [0 4 7 0 0
            1 0 2 1 0
            0 2 1 8 0
            0 1 12 2 0]
           (semantic-tokens/full-tokens (h/file-uri "file:///a.clj") db/db))))
  (testing "type alias for macro tokens"
    (h/load-code-and-locs (code "(ns some.ns (:require [clojure.test :as test]))"
                                "test/deftest"))
    (is (= [0 4 7 0 0
            1 0 4 1 0
            0 4 1 8 0
            0 1 7 3 0]
           (semantic-tokens/full-tokens (h/file-uri "file:///a.clj") db/db))))
  #_(testing "java classes for function tokens"
      (h/load-code-and-locs (code "(ns some.ns)"
                                  "^java.lang.String \"\""
                                  "String"
                                  "^String \"\""))
      (is (= [1 1 16 1 0
              1 0 6 4 0
              1 1 6 4 0]
             (semantic-tokens/full-tokens (h/file-uri "file:///a.clj") db/db))))
  ;; TODO need more java kondo analysis
  #_(testing "java static methods for method + class tokens"
      (h/load-code-and-locs (code "(ns some.ns)"
                                  "(Character/isUpperCase \"some-string\")"))
      (is (= [1 1 21 1 0]
             (semantic-tokens/full-tokens (h/file-uri "file:///a.clj") db/db))))
  (testing "java methods for method tokens"
    (h/load-code-and-locs (code "(ns some.ns)"
                                "(.equals \"some-string\" \"other-string\")"))
    (is (= [0 4 7 0 0
            1 1 7 7 0]
           (semantic-tokens/full-tokens (h/file-uri "file:///a.clj") db/db))))
  (testing "keywords for keyword tokens"
    (h/load-code-and-locs (code "(ns some.ns (:require [foo :as foo]))"
                                ":foo"
                                ":some.ns/foo"
                                "::foo"
                                "#:some.ns{:foo 1 :bar/foo 2}"
                                "::foo/something"))
    (is (= [0 4 7 0 0
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
            0 1 9 4 0]
           (semantic-tokens/full-tokens (h/file-uri "file:///a.clj") db/db))))
  (testing "locals destructuring for variable tokens"
    (h/load-code-and-locs (code "(fn [{:keys [foo bar]}])"))
    (is (= [0 1 2 3 0
            0 5 5 4 0
            0 0 5 4 0
            0 7 3 6 0
            0 4 3 6 0]
           (semantic-tokens/full-tokens (h/file-uri "file:///a.clj") db/db)))))

(deftest range-tokens
  (testing "tokens only for range"
    (h/load-code-and-locs
      (code "(ns some.ns (:require [foo.bar :refer [baz]]))"
            "(def bla baz)"
            "baz"
            "(comment)"
            "baz"))
    (is (= [2 0 3 2 0
            1 1 7 3 0]
           (semantic-tokens/range-tokens (h/file-uri "file:///a.clj") {:name-row 3 :name-col 0 :name-end-row 4 :name-end-col 0} db/db)))))
