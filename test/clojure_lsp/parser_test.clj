(ns clojure-lsp.parser-test
  (:require
    [clojure-lsp.parser :as parser]
    [clojure.test :refer [deftest is]]
    [taoensso.timbre :as log]
    [rewrite-clj.zip :as z]))

(deftest find-loc-at-pos-test
  (is (= nil (z/sexpr (parser/loc-at-pos "  foo  " 1 1))))
  (is (= 'foo (z/sexpr (parser/loc-at-pos "  foo  " 1 3))))
  (is (= 'foo (z/sexpr (parser/loc-at-pos "  foo  " 1 5))))
  (is (= nil (z/sexpr (parser/loc-at-pos "  foo  " 1 6)))))

(deftest find-top-forms-test
  (let [code "(a) (b c d)"]
    (is (= '[(a) (b c d)]
           (->> {:row 1 :col 2 :end-row 1 :end-col (count code)}
                (parser/find-top-forms-in-range code)
                (map z/sexpr))))))
