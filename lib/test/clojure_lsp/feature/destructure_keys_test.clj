(ns clojure-lsp.feature.destructure-keys-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.destructure-keys :as f.destructure-keys]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(defn can-destructure-zloc? [zloc]
  (f.destructure-keys/can-destructure-keys? zloc h/default-uri @db/db*))

(defn can-destructure-code? [code]
  (can-destructure-zloc? (h/load-code-and-zloc code)))

(deftest can-destructure-keys?
  (is (not (can-destructure-code? "{:a |1}")))
  (is (not (can-destructure-code? "(def |foo {:a 1})"))))

(defn destructure-zloc [zloc]
  (f.destructure-keys/destructure-keys zloc h/default-uri @db/db*))

(defn- as-string [changes]
  (h/changes->code changes @db/db*))

(defmacro ^:private assert-destructures [destructured-code original-code]
  `(let [zloc# (h/load-code-and-zloc ~original-code)
         expected# ~destructured-code]
     (is (can-destructure-zloc? zloc#))
     (is (= expected#
            (as-string (destructure-zloc zloc#))))))

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
                                 "  loc')"))))
