(ns clojure-lsp.feature.restructure-keys-test
  (:require
   [clojure-lsp.feature.restructure-keys :as f.restructure-keys]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(defn ^:private can-restructure-zloc? [zloc]
  (f.restructure-keys/can-restructure-keys? zloc h/default-uri (h/db)))

(defn ^:private restructure-zloc [zloc]
  (f.restructure-keys/restructure-keys zloc h/default-uri (h/db)))

(defn ^:private as-string [changes]
  (h/changes->code changes (h/db)))

(defmacro ^:private assert-cannot-restructure [code]
  `(let [zloc# (h/load-code-and-zloc ~code)]
     (is (not (can-restructure-zloc? zloc#))
         (as-string (restructure-zloc zloc#)))))

(defmacro ^:private assert-restructures [restructured-code original-code]
  `(let [original# ~original-code
         zloc# (h/load-code-and-zloc original#)
         expected# ~restructured-code]
     (is (can-restructure-zloc? zloc#) original#)
     (is (= expected#
            (as-string (restructure-zloc zloc#)))
         original#)))

(defmacro assert-restructures-variations [restructured-code & original-codes]
  `(do ~@(map (fn [original-code]
                `(assert-restructures ~restructured-code ~original-code))
              original-codes)))

(deftest should-not-restructure
  (assert-cannot-restructure "{|:a 1}")
  (assert-cannot-restructure "{:a |1}")
  (assert-cannot-restructure "(|def foo)")
  (assert-cannot-restructure "(def |foo)")
  (assert-cannot-restructure "|")
  (assert-cannot-restructure "|;; comment")
  (assert-cannot-restructure "(let [loc' loc] (tangent |loc'))")
  (assert-cannot-restructure "|{:a 1}"))

(deftest should-restructure-local
  (testing "restructuring keys"
    (assert-restructures-variations
      (h/code "(defn foo [element] (:a element))")
      (h/code "(defn foo [|{:keys [a]}] a)")
      (h/code "(defn foo [|{:keys [:a]}] a)"))
    (assert-restructures-variations
      (h/code "(defn foo [element] (:prefix/a element))")
      (h/code "(defn foo [|{:keys [prefix/a]}] a)")
      (h/code "(defn foo [|{:keys [:prefix/a]}] a)")
      (h/code "(defn foo [|{:prefix/keys [a]}] a)")
      (h/code "(defn foo [|{:prefix/keys [:a]}] a)")
      (h/code "(defn foo [|#:prefix{:keys [a]}] a)")
      (h/code "(defn foo [|#:prefix{:keys [:a]}] a)"))
    (assert-restructures-variations
      (h/code "(defn foo [element] (::prefix/a element))")
      (h/code "(defn foo [|{:keys [::prefix/a]}] a)")
      (h/code "(defn foo [|{::prefix/keys [a]}] a)")
      (h/code "(defn foo [|{::prefix/keys [:a]}] a)")
      (h/code "(defn foo [|#::prefix{:keys [a]}] a)")
      (h/code "(defn foo [|#::prefix{:keys [:a]}] a)"))
    (assert-restructures-variations
      (h/code "(defn foo [element] (::a element))")
      (h/code "(defn foo [|{:keys [::a]}] a)")
      (h/code "(defn foo [|{::keys [a]}] a)")
      (h/code "(defn foo [|{::keys [:a]}] a)")
      (h/code "(defn foo [|#::{:keys [a]}] a)")
      (h/code "(defn foo [|#::{:keys [:a]}] a)"))
    (testing "overriding implied ns"
      (assert-restructures-variations
        (h/code "(defn foo [element] (:override/a element))")
        (h/code "(defn foo [|{:prefix/keys [override/a]}] a)")
        (h/code "(defn foo [|{:prefix/keys [:override/a]}] a)")
        (h/code "(defn foo [|#:prefix{:keys [override/a]}] a)")
        (h/code "(defn foo [|#:prefix{:keys [:override/a]}] a)")
        (h/code "(defn foo [|#::prefix{:keys [override/a]}] a)")
        (h/code "(defn foo [|#::prefix{:keys [:override/a]}] a)")
        (h/code "(defn foo [|#::{:keys [override/a]}] a)")
        (h/code "(defn foo [|#::{:keys [:override/a]}] a)"))
      (assert-restructures-variations
        (h/code "(defn foo [element] (::override/a element))")
        (h/code "(defn foo [|{:prefix/keys [::override/a]}] a)")
        (h/code "(defn foo [|#:prefix{:keys [::override/a]}] a)")
        (h/code "(defn foo [|#::prefix{:keys [::override/a]}] a)")
        (h/code "(defn foo [|#::{:keys [::override/a]}] a)"))
      (assert-restructures-variations
        (h/code "(defn foo [element] (::a element))")
        (h/code "(defn foo [|{:prefix/keys [::a]}] a)")
        (h/code "(defn foo [|#:prefix{:keys [::a]}] a)")
        (h/code "(defn foo [|#:prefix{:keys [::a]}] a)")
        (h/code "(defn foo [|#::{:keys [::a]}] a)"))
      (assert-restructures-variations
        (h/code "(defn foo [element] (:a element))")
        (h/code "(defn foo [|#:prefix{:_/keys [a]}] a)")
        (h/code "(defn foo [|#:prefix{:_/keys [:a]}] a)")
        (h/code "(defn foo [|#::prefix{:_/keys [a]}] a)")
        (h/code "(defn foo [|#::prefix{:_/keys [:a]}] a)")
        (h/code "(defn foo [|#::{:_/keys [a]}] a)")
        (h/code "(defn foo [|#::{:_/keys [:a]}] a)"))))
  (testing "restructuring symbols"
    (assert-restructures-variations
      (h/code "(defn foo [element] ('a element))")
      (h/code "(defn foo [|{:syms [a]}] a)")
      (h/code "(defn foo [|{:syms [:a]}] a)"))
    (assert-restructures-variations
      (h/code "(defn foo [element] ('prefix/a element))")
      (h/code "(defn foo [|{:syms [prefix/a]}] a)")
      (h/code "(defn foo [|{:syms [:prefix/a]}] a)")
      (h/code "(defn foo [|{:prefix/syms [a]}] a)")
      (h/code "(defn foo [|{:prefix/syms [:a]}] a)")
      (h/code "(defn foo [|#:prefix{:syms [a]}] a)")
      (h/code "(defn foo [|#:prefix{:syms [:a]}] a)"))
    (testing "overriding implied ns"
      (assert-restructures-variations
        (h/code "(defn foo [element] ('override/a element))")
        (h/code "(defn foo [|{:prefix/syms [override/a]}] a)")
        (h/code "(defn foo [|{:prefix/syms [:override/a]}] a)")
        (h/code "(defn foo [|#:prefix{:syms [override/a]}] a)")
        (h/code "(defn foo [|#:prefix{:syms [:override/a]}] a)")
        (h/code "(defn foo [|#::prefix{:syms [override/a]}] a)")
        (h/code "(defn foo [|#::prefix{:syms [:override/a]}] a)")
        (h/code "(defn foo [|#::{:syms [override/a]}] a)")
        (h/code "(defn foo [|#::{:syms [:override/a]}] a)"))
      (assert-restructures-variations
        (h/code "(defn foo [element] ('a element))")
        (h/code "(defn foo [|#:prefix{:_/syms [a]}] a)")
        (h/code "(defn foo [|#:prefix{:_/syms [:a]}] a)")
        (h/code "(defn foo [|#::prefix{:_/syms [a]}] a)")
        (h/code "(defn foo [|#::prefix{:_/syms [:a]}] a)")
        (h/code "(defn foo [|#::{:_/syms [a]}] a)")
        (h/code "(defn foo [|#::{:_/syms [:a]}] a)"))))
  (testing "restructures from usage"
    (assert-restructures (h/code "(defn foo [element] (:a element))")
                         (h/code "(defn foo [{:keys [a]}] |a)")))
  (testing "restructures elements of different type"
    (assert-restructures (h/code "(defn foo [element] (+ ('a element) (:b element)))")
                         (h/code "(defn foo [|{:syms [a], :keys [b]}] (+ a b))")))
  (testing "uses :as for element name"
    (assert-restructures (h/code "(defn foo [loc] (:a loc))")
                         (h/code "(defn foo [|{:keys [a], :as loc}] a)"))
    (assert-restructures (h/code "(defn foo [loc] (:prefix/a loc))")
                         (h/code "(defn foo [|#:prefix{:keys [a], :as loc}] a)"))
    (assert-restructures (h/code "(defn foo [loc] (::a loc))")
                         (h/code "(defn foo [|#::{:keys [a], :as loc}] a)"))
    (assert-restructures (h/code "(defn foo [loc] (+ (:a loc) (tangent loc)))")
                         (h/code "(defn foo [|{:keys [a], :as loc}] (+ a (tangent loc)))")))
  (testing "doesn't shadow other locals with element name"
    (assert-restructures (h/code "(defn foobar"
                                 "  [{element :top-a"
                                 "    element-1 :top-b}]"
                                 "  (:b element-1) (:a element))")
                         (h/code "(defn foobar"
                                 "  [{element :top-a"
                                 "    |{:keys [b]} :top-b}]"
                                 "  b (:a element))"))
    (assert-restructures (h/code "(defn foobar"
                                 "  [{element-1 :top-a"
                                 "    element-2 :top-b}]"
                                 "  (:b element-2) (:a element-1))")
                         (h/code "(defn foobar"
                                 "  [{element-1 :top-a"
                                 "    |{:keys [b]} :top-b}]"
                                 "  b (:a element-1))"))
    (assert-restructures (h/code "(defn foobar"
                                 "  [{element :top-a}]"
                                 "  (let [element-1 :top-b]"
                                 "    (:b element-1) (:a element)))")
                         (h/code "(defn foobar"
                                 "  [{element :top-a}]"
                                 "  (let [|{:keys [b]} :top-b]"
                                 "    b (:a element)))")))
  (testing "uses :or for default values"
    (assert-restructures (h/code "(defn foo [element] (get element :a 1))")
                         (h/code "(defn foo [|{:keys [a] :or {a 1}}] a)"))

    (assert-restructures (h/code "(defn foo [element] (get element :prefix/a 1))")
                         (h/code "(defn foo [|#:prefix{:keys [a] :or {a 1}}] a)"))
    (assert-restructures (h/code "(defn foo [element] (get element ::a 1))")
                         (h/code "(defn foo [|#::{:keys [a] :or {a 1}}] a)")))
  (testing "of named keys that resolve to symbols"
    (assert-restructures (h/code "(defn foo [element] (:a element))")
                         (h/code "(defn foo [|{a :a}] a)")))
  (testing "of named keys that resolve to further destructuring"
    (assert-restructures (h/code "(defn foo [{[a] :a}] a)")
                         (h/code "(defn foo [|{[a] :a}] a)"))
    (assert-restructures (h/code "(defn foo [{{:keys [a]} :a}] a)")
                         (h/code "(defn foo [|{{:keys [a]} :a}] a)"))
    (assert-restructures (h/code "(defn foo [{[a] :a :as element}] (+ a (:b element)))")
                         (h/code "(defn foo [|{[a] :a :keys [b]}] (+ a b))"))
    (assert-restructures (h/code "(defn foo [#:prefix{[a] :a :as element}] (+ a (:prefix/b element)))")
                         (h/code "(defn foo [|#:prefix{[a] :a :keys [b]}] (+ a b))"))
    (assert-restructures (h/code "(defn foo [{[b] :b :or {b 2} :as element}] (+ (get element :a 1) b))")
                         (h/code "(defn foo [|{:keys [a], [b] :b, :or {a 1 b 2}}] (+ a b))")))
  (testing "of pathological maps"
    ;; This map establishes a local inside of it, so our heuristics suggest it
    ;; should be restructurable. Rather than make the heuristics smarter to
    ;; avoid offering the code action, we're satisified with not breaking the
    ;; map.
    (assert-restructures (h/code "{:a (let [x 1] (inc x))}")
                         (h/code "|{:a (let [x 1] (inc x))}"))))
