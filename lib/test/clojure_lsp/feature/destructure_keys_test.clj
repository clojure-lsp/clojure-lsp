(ns clojure-lsp.feature.destructure-keys-test
  (:require
   [clojure-lsp.feature.destructure-keys :as f.destructure-keys]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(defn ^:private can-destructure-zloc? [zloc]
  (f.destructure-keys/can-destructure-keys? zloc h/default-uri (h/db)))

(defn ^:private destructure-zloc [zloc]
  (f.destructure-keys/destructure-keys zloc h/default-uri (h/db)))

(defn ^:private as-string [changes]
  (h/changes->code changes (h/db)))

(defmacro ^:private assert-cannot-destructure [code]
  `(let [zloc# (h/load-code-and-zloc ~code)]
     (is (not (can-destructure-zloc? zloc#))
         (as-string (destructure-zloc zloc#)))))

(defmacro ^:private assert-destructures [destructured-code original-code]
  `(let [original# ~original-code
         zloc# (h/load-code-and-zloc original#)
         expected# ~destructured-code]
     (is (can-destructure-zloc? zloc#) original#)
     (is (= expected#
            (as-string (destructure-zloc zloc#)))
          original#)))

(deftest should-not-destructure
  (assert-cannot-destructure "{|:a 1}")
  (assert-cannot-destructure "{:a |1}")
  (assert-cannot-destructure "(|def foo)")
  (assert-cannot-destructure "(def |foo)")
  (assert-cannot-destructure "(let [{:keys [a]} loc] (+ (:x |a)))")
  (assert-cannot-destructure "(let [{:syms [a]} loc] (+ (:x |a)))")
  (assert-cannot-destructure "(let [{:strs [a]} loc] (+ (:x |a)))")
  (assert-cannot-destructure "(let [{:nsed/keys [a]} loc] (+ (:x |a)))")
  (assert-cannot-destructure "|")
  (assert-cannot-destructure "|;; comment"))

(deftest should-destructure-local
  (testing "destructures keys"
    (assert-destructures (h/code "(let [{:keys [a b]} loc] (+ a b))")
                         (h/code "(let [|loc' loc] (+ (:a loc') (:b loc')))")))
  (testing "doesn't duplicate"
    (assert-destructures (h/code "(let [{:keys [a]} loc] (+ a a))")
                         (h/code "(let [|loc' loc] (+ (:a loc') (:a loc')))")))
  (testing "destructures from usage"
    (assert-destructures (h/code "(let [{:keys [a b]} loc] (+ a b))")
                         (h/code "(let [loc' loc] (+ (:a |loc') (:b loc')))")))
  (testing "destructures symbols"
    (assert-destructures (h/code "(let [{:syms [a b]} loc] (+ a b))")
                         (h/code "(let [|loc' loc] (+ ('a loc') ('b loc')))")))
  (testing "destructures keys and symbols"
    (assert-destructures (h/code "(let [{:syms [a], :keys [b]} loc] (+ a b))")
                         (h/code "(let [|loc' loc] (+ ('a loc') (:b loc')))")))
  (testing "destructures qualified keys"
    (assert-destructures (h/code "(let [{:keys [loc/a loc/b]} loc] (+ a b))")
                         (h/code "(let [|loc' loc] (+ (:loc/a loc') (:loc/b loc')))")))
  (testing "destructures qualified symbols"
    (assert-destructures (h/code "(let [{:syms [loc/a loc/b]} loc] (+ a b))")
                         (h/code "(let [|loc' loc] (+ ('loc/a loc') ('loc/b loc')))")))
  (testing "destructures auto-resolved aliased-ns keys"
    (assert-destructures (h/code "(let [{:keys [::loc/a ::loc/b]} loc] (+ a b))")
                         (h/code "(let [|loc' loc] (+ (::loc/a loc') (::loc/b loc')))")))
  (testing "destructures auto-resolved current-ns keys"
    (assert-destructures (h/code "(let [{:keys [::a ::b]} loc] (+ a b))")
                         (h/code "(let [|loc' loc] (+ (::a loc') (::b loc')))")))
  (testing "doesn't duplicate auto-resolved aliased-ns keys"
    (assert-destructures (h/code "(let [{:keys [::loc/a]} loc] (+ a a))")
                         (h/code "(let [|loc' loc] (+ (::loc/a loc') (::loc/a loc')))")))
  (testing "doesn't duplicate auto-resolved current-ns keys"
    (assert-destructures (h/code "(let [{:keys [::a]} loc] (+ a a))")
                         (h/code "(let [|loc' loc] (+ (::a loc') (::a loc')))")))
  (testing "maintains :as if necessary"
    (assert-destructures (h/code "(let [{:keys [a b], :as loc'} loc] (+ a b (tangent loc')))")
                         (h/code "(let [|loc' loc] (+ (:a loc') (:b loc') (tangent loc')))"))
    (assert-destructures (h/code "(let [{:keys [a b], :as loc'} loc]"
                                 "  (+ a b)"
                                 "  loc')")
                         (h/code "(let [|loc' loc]"
                                 "  (+ (:a loc') (:b loc'))"
                                 "  loc')")))
  (testing "makes changes only if necessary"
    (assert-destructures "(let [loc' loc] (tangent loc'))"
                         "(let [loc' loc] (tangent |loc'))")))

(deftest should-extend-prior-map-destructuring
  (testing "when local is :as"
    (assert-destructures (h/code "(let [{:keys [a b c]} loc] (+ a b c))")
                         (h/code "(let [{:keys [a b] :as x} loc] (+ a b (:c |x)))")))
  (testing "when local is :as and needs to be maintained"
    (assert-destructures (h/code "(let [{:keys [a b c], :as x} loc] (+ a b c (tangent x)))")
                         (h/code "(let [{:keys [a b] :as x} loc] (+ a b (:c |x) (tangent x)))")))
  (testing "when local is destructured by name"
    (assert-destructures (h/code "(let [{{:keys [x]} :a} loc] (+ x))")
                         (h/code "(let [{a :a} loc] (+ (:x |a)))")))
  (testing "when exotic keys are present"
    (assert-destructures (h/code "(let [{:nsed/keys [a b], :keys [c]} loc] (+ a b c))")
                         (h/code "(let [{:nsed/keys [a b] :as x} loc] (+ a b (:c |x)))"))
    (assert-destructures (h/code "(let [{a :a, b :b, :keys [c]} loc] (+ a b c))")
                         (h/code "(let [{a :a b :b :as x} loc] (+ a b (:c |x)))"))))
