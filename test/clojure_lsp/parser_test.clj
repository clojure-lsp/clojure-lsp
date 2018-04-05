(ns clojure-lsp.parser-test
  (:require [clojure.test :refer :all]
            [clojure-lsp.handlers :as handlers]
            [clojure-lsp.parser :as parser]
            [clojure.string :as string]))

(defn syms [code]
  (->> code
       (parser/find-references)
       (:usages)
       (map :sym)
       (set)))

(defn scoped-str [code scope-bounds]
  (subs code (dec (:col scope-bounds)) (dec (:end-col scope-bounds))))

(deftest find-references-test
  (is (= '#{clojure.core/ns clojure.core/def foo.bar/qux}
         (syms "(ns foo.bar) (def qux qux)")))
  (testing "clojure.core/defn"
    (testing "single-arity"
      (let [code "(defn a [b] b a)"
            usages (:usages (parser/find-references code))
            bound-ref (nth usages 2)
            usage-ref (nth usages 3)]
        (is (not= "user" (namespace (:sym bound-ref))))
        (is (= "b" (name (:sym bound-ref))))
        (is (= #{:declare :param} (:tags bound-ref)))
        (is (= "[b] b a)" (scoped-str code (:scope-bounds bound-ref))))
        (is (= (:sym bound-ref) (:sym usage-ref)))
        (is (= (:scope-bounds bound-ref) (:scope-bounds usage-ref)))))
    (testing "multi-arity"
      (let [code "(defn a ([b] b) ([b c] b))"
            usages (:usages (parser/find-references code))
            def-ref (second usages)
            bound-ref (nth usages 2)
            usage-ref (nth usages 3)]
        (is (= 'user/a (:sym def-ref)))
        (is (not= "user" (namespace (:sym bound-ref))))
        (is (= "b" (name (:sym bound-ref))))
        (is (= #{:declare :param} (:tags bound-ref)))
        (is (= "[b] b)" (scoped-str code (:scope-bounds bound-ref))))
        (is (= (:sym bound-ref) (:sym usage-ref)))
        (is (= (:scope-bounds bound-ref) (:scope-bounds usage-ref))))))
  (testing "clojure.core/let"
    (let [code "(let [#_#_x 1 a 2 b 3] a)"
          a-valid (string/index-of code "3")
          end-scope (inc (count code))
          usages (:usages (parser/find-references code))
          bound-ref (second usages)
          usage-ref (nth usages 3)]
      (is (not= "user" (namespace (:sym bound-ref))))
      (is (= "a" (name (:sym bound-ref))))
      (is (= #{:declare :param} (:tags bound-ref)))
      (is (= " 3] a)" (scoped-str code (:scope-bounds bound-ref))))
      (is (= (:sym bound-ref) (:sym usage-ref)))
      (is (= (:scope-bounds bound-ref) (:scope-bounds usage-ref)))))
  (testing "clojure.core/for"
    (let [code "(for [a 1 :let [b 2]] [a b])"
          usages (:usages (parser/find-references code))
          bound-ref (second usages)
          usage-ref (nth usages 3)
          b-bound (nth usages 2)
          b-usage (nth usages 4)]
      (is (not= "user" (namespace (:sym bound-ref))))
      (is (= "a" (name (:sym bound-ref))))
      (is (= #{:declare :param} (:tags bound-ref)))
      (is (= " [b 2]] [a b])" (scoped-str code (:scope-bounds bound-ref))))
      (is (= (:sym bound-ref) (:sym usage-ref)))
      (is (= (:scope-bounds bound-ref) (:scope-bounds usage-ref)))
      (is (= (get-in bound-ref [:scope-bounds :end-col]) (get-in b-bound [:scope-bounds :end-col])))
      (is (= (:sym b-bound) (:sym b-usage)))
      (is (= (:scope-bounds b-bound) (:scope-bounds b-usage))))))
