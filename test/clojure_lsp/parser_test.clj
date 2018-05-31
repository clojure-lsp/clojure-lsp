(ns clojure-lsp.parser-test
  (:require [clojure.test :refer :all]
            [rewrite-clj.zip :as z]
            [clojure-lsp.handlers :as handlers]
            [clojure.tools.logging :as log]
            [clojure-lsp.parser :as parser]
            [clojure.string :as string]
            [clojure-lsp.db :as db]))

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
    (is (= 0 (count (parser/parse-bindings (z/of-string "[]") context {} {}))))
    (is (= 1 (count (parser/parse-bindings (z/of-string "[a @db]") context {} {}))))
    (is (= 2 (count (:usages @context))))))

(deftest parse-params-test
  (let [context (volatile! {})]
    (is (= 0 (count (parser/parse-params (z/of-string "[]") context {}))))))

(deftest qualify-ident-test
  (let [context (volatile! {})]
    (is (= {:sym 'clojure.core/for :tags #{:norename}} (parser/qualify-ident 'for context {})))
    (is (= {:sym 'java.lang.Exception :tags #{:norename}} (parser/qualify-ident 'Exception context {})))
    (is (= {:sym 'java.lang.Exception :tags #{:norename}} (parser/qualify-ident 'Exception. context {})))
    (is (= {:sym '.getMessage :tags #{:method :norename}} (parser/qualify-ident '.getMessage context {})))
    (is (= {:sym 'java.lang.Thread/sleep :tags #{:method :norename}} (parser/qualify-ident 'Thread/sleep context {})))))

(deftest find-references-simple-test
  (testing "simple stuff"
    (is (= '#{clojure.core/ns clojure.core/def foo.bar/qux}
           (syms "(ns foo.bar) (def qux qux)")))
    (is (= 1 (count (syms "(:id user)"))))
    (is (= 2 (count (syms "{:x id :y (:id user)}")))))
  (testing "#(dispatch-macro)"
    (let [code "#(% %1 %2 %&)"
          usages (:usages (parser/find-references code))]
      (is (= [] (filter (fn [usage] (contains? (:tags usage) :unknown)) usages))))))

(deftest find-references-defn-test
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
  (testing "private"
    (let [code "(defn- a [b] b a)"
          usages (:usages (parser/find-references code))
          bound-ref (nth usages 2)
          usage-ref (nth usages 3)]
      (is (= 'user/a (:sym (nth usages 1))))
      (is (= #{:declare :local} (:tags (nth usages 1))))
      (is (not= "user" (namespace (:sym bound-ref))))
      (is (= "b" (name (:sym bound-ref))))
      (is (= #{:declare :param} (:tags bound-ref)))
      (is (= "[b] b a)" (scoped-str code (:scope-bounds bound-ref))))
      (is (= (:sym bound-ref) (:sym usage-ref)))
      (is (= (:scope-bounds bound-ref) (:scope-bounds usage-ref)))))
  (testing "private meta"
    (let [code "(defn ^:private a [b] b a)"
          usages (:usages (parser/find-references code))
          bound-ref (nth usages 2)
          usage-ref (nth usages 3)]
      (is (= 'user/a (:sym (nth usages 1))))
      (is (= #{:declare :local} (:tags (nth usages 1))))
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

(deftest find-references-let-test
  (testing "let"
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
      (is (= (:scope-bounds b-bound) (:scope-bounds b-usage))))
    (let [code "(for [:when true :let [a 0] b [] :let [c 0] :when true] a b c)"
          usages (:usages (parser/find-references code))
          a-bound (nth usages 1)
          a-usage (nth usages 4)
          b-bound (nth usages 2)
          b-usage (nth usages 5)
          c-bound (nth usages 3)
          c-usage (nth usages 6)]
      (is (not= "user" (namespace (:sym a-bound))))
      (is (= "a" (name (:sym a-bound))))
      (is (= #{:declare :param} (:tags a-bound)))
      (is (= " :when true] a b c)" (scoped-str code (:scope-bounds c-bound))))
      (is (= (:sym a-bound) (:sym a-usage)))
      (is (= (:scope-bounds a-bound) (:scope-bounds a-usage)))
      (is (= (:sym b-bound) (:sym b-usage)))
      (is (= (:scope-bounds b-bound) (:scope-bounds b-usage)))
      (is (= (:sym c-bound) (:sym c-usage)))
      (is (= (:scope-bounds c-bound) (:scope-bounds c-usage))))))

(deftest find-references-destructuring-test
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
    (is (= 6 (count usages)))
    (is (= #{:declare :param} (:tags bound-ref)))
    (is (= (namespace (:sym usage-ref)) (namespace (:sym bound-ref))))
    (is (= #{:declare :param} (:tags b-bound)))
    (is (= (namespace (:sym b-usage)) (namespace (:sym b-bound)))))
  (let [code "(fn myname [y {:keys [a] b :b}] a b)"
        usages (:usages (parser/find-references code))
        name-ref (nth usages 1)
        bound-ref (nth usages 3)
        usage-ref (nth usages 5)
        b-bound (nth usages 4)
        b-usage (nth usages 6)]
    (is (= 7 (count usages)))
    (is (not= "user" (namespace (:sym name-ref))))
    (is (= #{:declare} (:tags name-ref)))
    (is (= #{:declare :param} (:tags bound-ref)))
    (is (= (namespace (:sym usage-ref)) (namespace (:sym bound-ref))))
    (is (= #{:declare :param} (:tags b-bound)))
    (is (= (namespace (:sym b-usage)) (namespace (:sym b-bound))))))

(deftest find-references-ns-test
  (testing "refer all"
    (reset! db/db {:file-envs {"a.clj" {:ns 'clojure.test :usages [{:sym 'clojure.test/deftest :tags #{:declare :public}}]}}})
    (let [code "(ns foo.bar (:require [clojure.test :refer :all])) (deftest hi)"
          {:keys [usages imports]} (parser/find-references code)
          deftest-ref (nth usages 1)
          hi-ref (nth usages 2)]
      (is (= 'clojure.test/deftest (:sym deftest-ref)))))
  (testing "refers"
    (let [code "(ns foo.bar (:require [clojure.test :refer [deftest]])) (deftest hi)"
          {:keys [usages imports]} (parser/find-references code)
          deftest-ref (nth usages 1)
          hi-ref (nth usages 2)]
      (is (= 'clojure.test/deftest (:sym deftest-ref)))
      #_
      (is (not= #{:unknown} (:tags hi-ref)))))
  (testing "import"
    (let [code "(ns foo.bar (:import java.util.jar.JarFile (java.io File))) (java.util.jar.JarFile.) (File.) (File/static :a) (JarFile.)"
          {:keys [usages imports]} (parser/find-references code)
          jar-file-ref (nth usages 1)
          file-ref (nth usages 2)
          file-static-ref (nth usages 3)
          unknown-ref (nth usages 4)]
      (is (= 'java.util.jar.JarFile (:sym jar-file-ref)))
      (is (= 'java.io.File (:sym file-ref)))
      (is (= 'java.io.File/static (:sym file-static-ref)))
      (is (not= 'java.util.jar.JarFile (:sym unknown-ref)))
      (is (= #{:unknown} (:tags unknown-ref))))))

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
