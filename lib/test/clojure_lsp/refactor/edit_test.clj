(ns clojure-lsp.refactor.edit-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(h/reset-db-after-test)

(deftest find-last-by-pos
  (is (= "foo/bar" (z/string (edit/find-last-by-pos (z/of-string "(ns foo) (foo/bar 1)")
                                                    {:row 1 :col 11 :end-row 1 :end-col 11}))))
  (is (= "1" (z/string (edit/find-last-by-pos (z/of-string "(ns foo) 1 #(+ 1 2) 3")
                                              {:row 1 :col 10 :end-row 1 :end-col 10}))))
  (is (= "3" (z/string (edit/find-last-by-pos (z/of-string "(ns foo) 1 #(+ 1 2) 3")
                                              {:row 1 :col 21 :end-row 1 :end-col 21}))))
  (is (= "1" (z/string (edit/find-last-by-pos (z/of-string "(ns foo) 1 #?(+ 1 2) 3")
                                              {:row 1 :col 10 :end-row 1 :end-col 10}))))
  (is (= "3" (z/string (edit/find-last-by-pos (z/of-string "(ns foo) 1 #?(+ 1 2) 3")
                                              {:row 1 :col 22 :end-row 1 :end-col 22}))))
  (is (= "some" (z/string (edit/find-last-by-pos (z/of-string "(ns foo) some (def other {:foo/bar 1})")
                                                 {:row 1 :col 10 :end-row 1 :end-col 13}))))
  (is (= "some" (z/string (edit/find-last-by-pos (z/of-string "(ns foo) some (def other #:foo{:bar 1})")
                                                 {:row 1 :col 10 :end-row 1 :end-col 13})))))

(deftest find-op-test
  (let [code "(foo ((x) [a] (b {c d})))"
        val-finder (fn [v] #(z/find-next-value %1 z/next v))
        op-from-val (fn [finder]
                      (-> code
                          z/of-string
                          finder
                          edit/find-op
                          z/sexpr))]
    (is (= 'foo (op-from-val identity)))
    (is (= 'foo (op-from-val (val-finder 'foo))))
    (is (= '(x) (op-from-val (val-finder 'a))))
    (is (= 'b (op-from-val (val-finder 'd))))
    (is (= 'x (op-from-val (val-finder 'x))))
    (is (= '(x) (op-from-val (comp z/right (val-finder 'foo)))))))

(deftest find-ops-up-test
  (let [code "(foo ((x) [a] (b {c d})))"
        val-finder (fn [v] #(z/find-next-value %1 z/next v))
        op-from-val (fn [finder ops]
                      (-> code
                          z/of-string
                          finder
                          (edit/find-ops-up (str ops))
                          z/sexpr))]
    (testing "find top op"
      (is (= 'foo (op-from-val (val-finder 'a) 'foo)))
      (is (= 'foo (op-from-val (val-finder 'foo) 'foo)))
      (is (= 'foo (op-from-val (val-finder 'd) 'foo)))
      (is (= 'foo (op-from-val (val-finder 'x) 'foo)))
      (is (= 'foo (op-from-val (comp z/right (val-finder 'foo)) 'foo))))
    (testing "find inner op"
      (is (= nil (op-from-val (val-finder 'a) 'b)))
      (is (= nil (op-from-val (val-finder 'foo) 'b)))
      (is (= nil (op-from-val (val-finder 'x) 'b)))
      (is (= nil (op-from-val (comp z/right (val-finder 'foo)) 'b)))
      (is (= 'b (op-from-val (val-finder 'd) 'b)))
      (is (= 'b (op-from-val (val-finder 'b) 'b)))
      (is (= 'b (op-from-val (comp z/up (val-finder 'b)) 'b)))))
  (testing "when sexpr is invalid"
    (is (= '->
           (-> "(-> {:a (some) (find-ops-up)})"
               z/of-string
               (z/find-next-value z/next 'some)
               (edit/find-ops-up "->")
               z/sexpr)))))

(deftest find-namespace-name
  (testing "without ns on file"
    (is (nil? (-> "(foo ((x) [a] (b {c |d})))"
                  h/zloc-from-code
                  edit/find-namespace-name))))
  (testing "with ns on file"
    (is (= "some.foo.bar"
           (-> (h/code "(ns some.foo.bar (require [some.foo :as s]))"
                       "(foo ((x) [a] (b {c |d})))")
               h/zloc-from-code
               edit/find-namespace-name)))))

(defn ^:private assert-function-name [code]
  (h/clean-db!)
  (h/load-code-and-locs code)
  (let [zloc (-> code z/of-string (z/find-next-value z/next 'd))]
    (is (= "foo"
           (z/string (edit/find-var-definition-name-loc zloc "/a.clj" db/db))))))

(deftest find-function-definition-name
  (testing "defn"
    (testing "simple" (assert-function-name "(defn foo [] (let [a 1] d))"))
    (testing "with meta map" (assert-function-name "(defn ^{:asd :ds} foo [] (let [a 1] d))"))
    (testing "with meta" (assert-function-name "(defn ^:private foo [] (let [a 1] d))")))
  (testing "defn-"
    (testing "simple" (assert-function-name "(defn- foo [] (let [a 1] d))"))
    (testing "with meta map" (assert-function-name "(defn- ^{:asd :ds} foo [] (let [a 1] d))"))
    (testing "with meta" (assert-function-name "(defn- ^:private foo [] (let [a 1] d))")))
  (testing "def"
    (testing "simple" (assert-function-name "(def foo (let [a 1] d))"))
    (testing "with meta map" (assert-function-name "(def ^{:asd :ds} foo (let [a 1] d))"))
    (testing "with meta" (assert-function-name "(def ^:private foo (let [a 1] d))")))
  (testing "defmacro"
    (testing "simple" (assert-function-name "(defmacro foo [] (let [a 1] d))"))
    (testing "with meta map" (assert-function-name "(defmacro ^{:asd :ds} foo [] (let [a 1] d))"))
    (testing "with meta" (assert-function-name "(defmacro ^:private foo [] (let [a 1] d))")))
  (testing "defmulti"
    (testing "simple" (assert-function-name "(defmulti foo [] (let [a 1] d))"))
    (testing "with meta map" (assert-function-name "(defmulti ^{:asd :ds} foo [] (let [a 1] d))"))
    (testing "with meta" (assert-function-name "(defmulti ^:private foo [] (let [a 1] d))")))
  (testing "defonce"
    (testing "simple" (assert-function-name "(defonce foo (let [a 1] d))"))
    (testing "with meta map" (assert-function-name "(defonce ^{:asd :ds} foo (let [a 1] d))"))
    (testing "with meta" (assert-function-name "(defonce ^:private foo (let [a 1] d))")))
  (testing "s/def"
    (testing "simple" (assert-function-name "(ns a (:require [schema.core :as s])) (s/def foo :- s/String (let [a 1] d))"))
    (testing "with meta map" (assert-function-name "(ns a (:require [schema.core :as s])) (s/def ^{:asd :ds} foo :- s/String (let [a 1] d))"))
    (testing "with meta" (assert-function-name "(ns a (:require [schema.core :as s])) (s/def ^:private foo :- s/String (let [a 1] d))")))
  (testing "s/defn"
    (testing "simple" (assert-function-name "(ns a (:require [schema.core :as s])) (s/defn foo :- s/String [] (let [a 1] d))"))
    (testing "with meta map" (assert-function-name "(ns a (:require [schema.core :as s])) (s/defn ^{:asd :ds} foo :- s/String [] (let [a 1] d))"))
    (testing "with meta" (assert-function-name "(ns a (:require [schema.core :as s])) (s/defn ^:private foo :- s/String [] (let [a 1] d))")))
  (testing "deftype"
    (testing "simple" (assert-function-name "(deftype foo [] Some (^void bar [] (let [d 1] d)))"))
    (testing "Implementing nothing" (assert-function-name "(deftype foo [] (^void bar [] (let [d 1] d)))"))
    (testing "with meta" (assert-function-name "(deftype ^:private foo [] Some (^void bar [] (let [d 1] d)))")))
  (testing "defrecord"
    (testing "simple" (assert-function-name "(defrecord foo [] Some (^void bar [] (let [a 1] d)))"))
    (testing "Implementing nothing" (assert-function-name "(defrecord foo [] (^void bar [] (let [a 1] d)))"))
    (testing "with meta map" (assert-function-name "(defrecord ^{:asd :ds} foo [] Some (^void bar [] (let [a 1] d)))"))
    (testing "with meta" (assert-function-name "(defrecord ^:private foo [] Some (^void bar [] (let [a 1] d)))")))
  (testing "deftest"
    (testing "simple" (assert-function-name "(ns a (:require [clojure.test :refer :all])) (deftest foo (is (let [a 1] d)))"))
    (testing "with meta map" (assert-function-name "(ns a (:require [clojure.test :refer :all])) (deftest ^{:pending true} foo (is (let [a 1] d)))"))))

(deftest find-function-usage-name
  (let [zloc (h/zloc-from-code "(defn foo [] (let [a 1] |d))")]
    (is (= "let" (z/string (edit/find-function-usage-name-loc zloc)))))
  (let [zloc (h/zloc-from-code "(defn foo [] (let [a 1] (and 1 |d)))")]
    (is (= "and" (z/string (edit/find-function-usage-name-loc zloc))))))

(deftest inside-refer?
  (let [zloc (h/zloc-from-code "(ns a (:require [clojure.test :refer [|deftes]]))")]
    (is (edit/inside-refer? zloc)))
  (let [zloc (h/zloc-from-code "(ns a (:require [clojure.test :refer |[]]))")]
    (is (edit/inside-refer? zloc)))
  (let [zloc (h/zloc-from-code "(ns a (:require [clojure.test :refer [deftest |is]]))")]
    (is (edit/inside-refer? zloc)))
  (let [zloc (h/zloc-from-code "(ns a (:require [clojure.test :as |test]))")]
    (is (not (edit/inside-refer? zloc))))
  (let [zloc (h/zloc-from-code "(require '[clojure.test :refer [|testing]])")]
    (is (edit/inside-refer? zloc))))

(deftest find-refer-ns
  (let [zloc (h/zloc-from-code "(ns a (:require [clojure.test :refer [|deftes]]))")]
    (is (= 'clojure.test (z/sexpr (edit/find-refer-ns zloc)))))
  (let [zloc (h/zloc-from-code "(ns a (:require [clojure.test :refer |[]]))")]
    (is (= 'clojure.test (z/sexpr (edit/find-refer-ns zloc)))))
  (let [zloc (h/zloc-from-code "(ns a (:require [clojure.test :refer [deftest |is]]))")]
    (is (= 'clojure.test (z/sexpr (edit/find-refer-ns zloc)))))
  (let [zloc (h/zloc-from-code "(ns a (:require [clojure.test :as |test]))")]
    (is (not (edit/find-refer-ns zloc))))
  (let [zloc (h/zloc-from-code "(require '[clojure.test :refer [|testing]])")]
    (is (= 'clojure.test (z/sexpr (edit/find-refer-ns zloc))))))
