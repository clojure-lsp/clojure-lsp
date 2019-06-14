(ns clojure-lsp.refactor.edit-test
  (:require
    [clojure-lsp.refactor.edit :as edit]
    [clojure.test :refer :all]
    [clojure.tools.logging :as log]
    [rewrite-clj.zip :as z]))

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
                          (edit/find-ops-up ops)
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
      (is (= 'b (op-from-val (comp z/up (val-finder 'b)) 'b))))))

