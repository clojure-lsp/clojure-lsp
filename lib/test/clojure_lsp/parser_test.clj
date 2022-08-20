(ns clojure-lsp.parser-test
  (:require
   [clojure-lsp.parser :as parser]
   [clojure.test :refer [are deftest is testing]]
   [rewrite-clj.zip :as z]))

(deftest safe-zloc-of-string
  (are [s] (= s (z/root-string (parser/safe-zloc-of-string s)))
    "(ns foo) foo/bar"
    "(ns foo) foo/ (+ 1 2)"
    "(ns foo) foo/\n(+ 1 2)"
    "(ns foo) (foo/)"
    "(ns foo) :\n"
    "(ns foo) : "
    "(ns foo) (:)"
    "(ns foo)\n:\n"
    "(ns foo) :foo/ (+ 1 2)"
    "(ns foo) (:foo/) (+ 1 2)"
    "(ns foo)\n:foo/\n"
    "(ns foo) ::foo/ (+ 1 2)"
    "(ns foo) (::foo/) (+ 1 2)"
    "(ns foo)\n::foo/\n"))

(defn to-pos [s row col]
  (-> s parser/safe-zloc-of-string (parser/to-pos row col) z/string))

(deftest to-pos-test
  (testing "complete code"
    (let [zloc (parser/safe-zloc-of-string "  foo  ")]
      (is (= nil (z/string (parser/to-pos zloc 1 1))))
      (is (= 'foo (z/sexpr (parser/to-pos zloc 1 3))))
      (is (= 'foo (z/sexpr (parser/to-pos zloc 1 5))))
      (is (= "  " (z/string (parser/to-pos zloc 1 6))))))
  (testing "incomplete code"
    (testing "entire line"
      (are [col expected] (= expected (to-pos "(ns foo) foo/ (+ 1 2)" 1 col))
        1  "(ns foo)"
        2  "ns"
        3  "ns"
        4  " "
        5  "foo"
        6  "foo"
        7  "foo"
        8  "(ns foo)"
        9  " "
        10 "foo/"
        11 "foo/"
        12 "foo/"
        13 "foo/"
        14 " "
        15 "(+ 1 2)"
        16 "+"
        17 " "
        18 "1"
        19 " "
        20 "2"
        21 "(+ 1 2)"))
    (is (= "foo/" (to-pos "foo/ " 1 1)))
    (is (= "foo/" (to-pos "foo/\n" 1 1)))
    (is (= "foo/" (to-pos "(foo/)" 1 2)))
    (is (= ":" (to-pos ": " 1 1)))
    (is (= ":" (to-pos ":\n" 1 1)))
    (is (= ":" (to-pos "(:)" 1 2)))
    (is (= ":" (to-pos "[:]" 1 2)))
    (is (= ":" (to-pos "{:}" 1 2)))
    (is (= ":" (to-pos ";; TODO: foo\n{:}" 2 2)))
    (is (= ":" (to-pos ";; TODO: foo\n: " 2 1)))
    (is (= ":" (to-pos "\n:\n" 2 1)))
    (is (= ":foo/" (to-pos ":foo/ " 1 1)))
    (is (= ":foo/" (to-pos ":foo/\n" 1 1)))
    (is (= ":foo/" (to-pos "(:foo/)" 1 2)))
    (is (= ":foo/" (to-pos "\n:foo/\n" 2 1)))
    (is (= "::foo/" (to-pos "::foo/ " 1 1)))
    (is (= "::foo/" (to-pos "::foo/\n" 1 1)))
    (is (= "::foo/" (to-pos "(::foo/)" 1 2)))
    (is (= "::foo/" (to-pos "\n::foo/\n" 2 1)))
    (testing "doesn't impact later nodes' positions"
      (is (= "a" (to-pos "foo/ a" 1 6)))
      (is (= "a" (to-pos ": a" 1 3)))
      (is (= "a" (to-pos ":foo/ a" 1 7)))
      (is (= "a" (to-pos "::foo/ a" 1 8))))))

(deftest lein-file->edn
  (testing "simple defproject on root"
    (is (= {:foo 1
            :bar 2}
           (parser/lein-zloc->edn (z/of-string "(defproject my-project \"0.1.2\" :foo 1 :bar 2)")))))
  (testing "simple defproject with multiple code before"
    (is (= {:foo 1
            :bar 2}
           (parser/lein-zloc->edn (z/of-string (str "(def otherthing 2)\n"
                                                    "(defproject my-project \"0.1.2\" :foo 1 :bar 2)\n"
                                                    "(def something 1)")))))))
