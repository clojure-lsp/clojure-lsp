(ns clojure-lsp.parser-test
  (:require [clojure.test :refer :all]
            [rewrite-clj.zip :as z]
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

(deftest parse-destructuring-test
  (is (= '(a b c d e) (keys (parser/parse-destructuring (z/of-string "[a {:keys [b c] :as d} e]") {} (volatile! {}) {})))))

(deftest parse-bindings-test
  (let [context (volatile! {})]
    (is (= 1 (count (parser/parse-bindings (z/of-string "[a @db]") context {} {}))))
    (is (= 2 (count (:usages @context))))))

(deftest find-references-test
  (testing "simple stuff"
    (is (= '#{clojure.core/ns clojure.core/def foo.bar/qux}
           (syms "(ns foo.bar) (def qux qux)")))
    (is (= 1 (count (syms "(:id user)"))))
    (is (= 2 (count (syms "{:x id :y (:id user)}")))))
  (testing "clojure.core/defn"
    (testing "single-arity"
      (let [code "(defn a [b] b a)"
            usages (:usages (parser/find-references code))
            bound-ref (nth usages 2)
            usage-ref (nth usages 3)]
        (is (= 'user/a (:sym (nth usages 1))))
        (is (= #{:declare :public} (:tags (nth usages 1))))
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
      (is (= (:scope-bounds b-bound) (:scope-bounds b-usage)))))
  (testing "#(dispatch-macro)"
    (let [code "#(% %1 %2 %&)"
          usages (:usages (parser/find-references code))]
      (is (= [] (filter (fn [usage] (contains? (:tags usage) :unknown)) usages)))))
  (testing "destructuring"
    (let [code "(let [{:keys [a] b :b} {}] a b)"
          usages (:usages (parser/find-references code))
          bound-ref (second usages)
          usage-ref (nth usages 3)
          b-bound (nth usages 2)
          b-usage (nth usages 4)]
      (is (= #{:declare :param} (:tags bound-ref)))
      (is (= (namespace (:sym usage-ref)) (namespace (:sym bound-ref))))
      (is (= #{:declare :param} (:tags b-bound)))
      (is (= (namespace (:sym b-usage)) (namespace (:sym b-bound)))))
    (let [code "(fn [y {:keys [a] b :b}] a b)"
          usages (:usages (parser/find-references code))
          bound-ref (nth usages 2)
          usage-ref (nth usages 4)
          b-bound (nth usages 3)
          b-usage (nth usages 5)]
      (is (= #{:declare :param} (:tags bound-ref)))
      (is (= (namespace (:sym usage-ref)) (namespace (:sym bound-ref))))
      (is (= #{:declare :param} (:tags b-bound)))
      (is (= (namespace (:sym b-usage)) (namespace (:sym b-bound)))))))
