(ns clojure-lsp.parser-test
  (:require
   [clojure-lsp.parser :as parser]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]))

(h/reset-db-after-test)

(deftest safe-zloc-of-string
  (is (= "(ns foo) foo/bar" (z/root-string (parser/safe-zloc-of-string "(ns foo) foo/bar"))))
  (is (= "(ns foo) foo/ (+ 1 2)" (z/root-string (parser/safe-zloc-of-string "(ns foo) foo/ (+ 1 2)"))))
  (is (= "(ns foo) foo/\n(+ 1 2)" (z/root-string (parser/safe-zloc-of-string "(ns foo) foo/\n(+ 1 2)"))))
  (is (= "(ns foo) (foo/)" (z/root-string (parser/safe-zloc-of-string "(ns foo) (foo/)"))))
  (is (= "(ns foo) :\n" (z/root-string (parser/safe-zloc-of-string "(ns foo) :\n"))))
  (is (= "(ns foo) : " (z/root-string (parser/safe-zloc-of-string "(ns foo) : "))))
  (is (= "(ns foo) (:)" (z/root-string (parser/safe-zloc-of-string "(ns foo) (:)"))))
  (is (= "(ns foo)\n:\n" (z/root-string (parser/safe-zloc-of-string "(ns foo)\n:\n"))))
  (is (= "(ns foo) :foo/ (+ 1 2)" (z/root-string (parser/safe-zloc-of-string "(ns foo) :foo/ (+ 1 2)"))))
  (is (= "(ns foo) (:foo/) (+ 1 2)" (z/root-string (parser/safe-zloc-of-string "(ns foo) (:foo/) (+ 1 2)"))))
  (is (= "(ns foo)\n:foo/\n" (z/root-string (parser/safe-zloc-of-string "(ns foo)\n:foo/\n"))))
  (is (= "(ns foo) ::foo/ (+ 1 2)" (z/root-string (parser/safe-zloc-of-string "(ns foo) ::foo/ (+ 1 2)"))))
  (is (= "(ns foo) (::foo/) (+ 1 2)" (z/root-string (parser/safe-zloc-of-string "(ns foo) (::foo/) (+ 1 2)"))))
  (is (= "(ns foo)\n::foo/\n" (z/root-string (parser/safe-zloc-of-string "(ns foo)\n::foo/\n")))))

(deftest to-loc
  (is (= "foo/" (-> "(ns foo) foo/ (+ 1 2)" parser/safe-zloc-of-string (parser/to-pos 1 10) z/string)))
  (is (= "foo/" (-> "(ns foo) foo/ (+ 1 2)" parser/safe-zloc-of-string (parser/to-pos 1 11) z/string)))
  (is (= "foo/" (-> "(ns foo) foo/ (+ 1 2)" parser/safe-zloc-of-string (parser/to-pos 1 12) z/string)))
  (is (= "foo/" (-> "(ns foo) foo/ (+ 1 2)" parser/safe-zloc-of-string (parser/to-pos 1 13) z/string)))
  (is (= "foo/" (-> "(ns foo) foo/\n(+ 1 2)" parser/safe-zloc-of-string (parser/to-pos 1 11) z/string)))
  (is (= "foo/" (-> "(ns foo) (foo/)" parser/safe-zloc-of-string (parser/to-pos 1 12) z/string)))
  (is (= ":" (-> "(ns foo) :\n" parser/safe-zloc-of-string (parser/to-pos 1 10) z/string)))
  (is (= ":" (-> "(ns foo) : " parser/safe-zloc-of-string (parser/to-pos 1 10) z/string)))
  (is (= ":" (-> "(ns foo) (:)" parser/safe-zloc-of-string (parser/to-pos 1 11) z/string)))
  (is (= ":" (-> "(ns foo)\n:\n" parser/safe-zloc-of-string (parser/to-pos 2 1) z/string)))
  (is (= ":foo/" (-> "(ns foo) :foo/ (+ 1 2)" parser/safe-zloc-of-string (parser/to-pos 1 11) z/string)))
  (is (= ":foo/" (-> "(ns foo) (:foo/) (+ 1 2)" parser/safe-zloc-of-string (parser/to-pos 1 11) z/string)))
  (is (= ":foo/" (-> "(ns foo)\n:foo/\n" parser/safe-zloc-of-string (parser/to-pos 2 1) z/string)))
  (is (= "::foo/" (-> "(ns foo) ::foo/ (+ 1 2)" parser/safe-zloc-of-string (parser/to-pos 1 12) z/string)))
  (is (= "::foo/" (-> "(ns foo) (::foo/) (+ 1 2)" parser/safe-zloc-of-string (parser/to-pos 1 12) z/string)))
  (is (= "::foo/" (-> "(ns foo)\n::foo/\n" parser/safe-zloc-of-string (parser/to-pos 2 1) z/string)))
  (testing "incomplete code doesn't impact later nodes' positions"
    (is (= "a" (-> "(ns foo) foo/ a b" parser/safe-zloc-of-string (parser/to-pos 1 15) z/string)))
    (is (= "a" (-> "(ns foo) : a b c" parser/safe-zloc-of-string (parser/to-pos 1 12) z/string)))
    (is (= "a" (-> "(ns foo) :foo/ a b c" parser/safe-zloc-of-string (parser/to-pos 1 16) z/string)))
    (is (= "a" (-> "(ns foo) ::foo/ a b c" parser/safe-zloc-of-string (parser/to-pos 1 17) z/string)))))

(deftest to-pos-test
  (testing "valid code"
    (let [zloc (parser/zloc-of-string "  foo  ")]
      (is (= nil (z/string (parser/to-pos zloc 1 1))))
      (is (= 'foo (z/sexpr (parser/to-pos zloc 1 3))))
      (is (= 'foo (z/sexpr (parser/to-pos zloc 1 5))))
      (is (= "  " (z/string (parser/to-pos zloc 1 6))))))
  (testing "invalid code"
    (is (= "foo/" (-> (parser/zloc-of-string "(ns foo)  (foo/)  ")
                      (parser/to-pos 1 12)
                      z/string)))
    (is (= "foo/" (-> (parser/zloc-of-string "(ns foo)  foo/ ")
                      (parser/to-pos 1 11)
                      z/string)))
    (is (= "foo/" (-> (parser/zloc-of-string "(ns foo)  foo/\n")
                      (parser/to-pos 1 11)
                      z/string)))))

(deftest find-top-forms-test
  (let [code "(a) (b c d)"]
    (is (= '[(a) (b c d)]
           (->> {:row 1 :col 2 :end-row 1 :end-col (count code)}
                (parser/find-top-forms-in-range code)
                (map z/sexpr))))))

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
