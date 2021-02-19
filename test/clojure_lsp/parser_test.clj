(ns clojure-lsp.parser-test
  (:require
    [clojure-lsp.parser :as parser]
    [clojure.test :refer [deftest is testing]]
    [rewrite-clj.zip :as z]
    [taoensso.timbre :as log]))

(deftest safe-zloc-of-string
  (is (= "(ns foo) foo/bar" (z/string (z/up (#'parser/safe-zloc-of-string "(ns foo) foo/bar")))))
  (is (= "(ns foo) foo/ (+ 1 2)" (z/string (#'parser/safe-zloc-of-string "(ns foo) foo/ (+ 1 2)"))))
  (is (= "(ns foo) foo/\n(+ 1 2)" (z/string (#'parser/safe-zloc-of-string "(ns foo) foo/\n(+ 1 2)"))))
  (is (= "(ns foo) (foo/)" (z/string (#'parser/safe-zloc-of-string "(ns foo) (foo/)")))))

(deftest find-loc-at-pos-test
  (testing "valid code"
    (is (= nil (z/sexpr (parser/loc-at-pos "  foo  " 1 1))))
    (is (= 'foo (z/sexpr (parser/loc-at-pos "  foo  " 1 3))))
    (is (= 'foo (z/sexpr (parser/loc-at-pos "  foo  " 1 5))))
    (is (= nil (z/sexpr (parser/loc-at-pos "  foo  " 1 6)))))
  (testing "invalid code"
    (is (= "foo/" (z/string (parser/loc-at-pos "(ns foo)  (foo/)  " 1 12))))
    (is (= "foo/" (z/string (parser/loc-at-pos "(ns foo)  foo/ " 1 11))))
    (is (= "foo/" (z/string (parser/loc-at-pos "(ns foo)  foo/\n" 1 11))))))

(deftest find-top-forms-test
  (let [code "(a) (b c d)"]
    (is (= '[(a) (b c d)]
           (->> {:row 1 :col 2 :end-row 1 :end-col (count code)}
                (parser/find-top-forms-in-range code)
                (map z/sexpr))))))
