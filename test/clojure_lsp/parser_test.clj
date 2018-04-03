(ns clojure-lsp.parser-test
  (:require [clojure.test :refer :all]
            [clojure-lsp.handlers :as handlers]
            [clojure-lsp.parser :as parser]))

(defn syms [code]
  (->> code
       (parser/find-references)
       (:usages)
       (map :sym)
       (set)))

(deftest qualify-ident-test
  (is (= 'foo.bar/qux (parser/qualify-ident 'qux {:ns 'foo.bar :publics #{'qux}} {})))
  (is (= 'foo.bar/qux (parser/qualify-ident 'qux {:ns 'foo.bar :refers {'qux 'foo.bar}} {}))))

(deftest find-references-test
  (is (= '#{clojure.core/ns clojure.core/def foo.bar/qux}
         (syms "(ns foo.bar) (def qux qux)")))
  (testing "clojure.core/let"
    (let [usages (:usages (parser/find-references "(let [a 1 b 2] a)"))
          bound-ref (second usages)
          usage-ref (nth usages 3)]
      (is (not= "user" (namespace (:sym bound-ref))))
      (is (= "a" (name (:sym bound-ref))))
      (is (= #{:declare :param} (:tags bound-ref)))
      (is (= :before (handlers/check-bounds 1 11 (:scope-bounds bound-ref))))
      (is (= :within (handlers/check-bounds 1 12 (:scope-bounds bound-ref))))
      (is (= :within (handlers/check-bounds 1 18 (:scope-bounds bound-ref))))
      (is (= :after (handlers/check-bounds 1 19 (:scope-bounds bound-ref))))
      (is (= (:sym bound-ref) (:sym usage-ref)))
      (is (= (:scope-bounds bound-ref) (:scope-bounds usage-ref))))))
