(ns clojure-lsp.refactor.transform-test
  (:require
   [clojure.test :refer :all]
   [clojure-lsp.refactor.transform :as transform]
   [rewrite-clj.zip :as z]))

(deftest paredit-test
  (let [zloc (transform/raise (z/find-value (z/of-string "(a (b))") z/next 'b))]
    (is (= 'b (z/sexpr zloc)))
    (is (= '(a b) (z/sexpr (z/up zloc))))
    (is (= "b" (-> zloc transform/raise z/root-string))))
 (let [zloc (transform/wrap-around (z/find-value (z/of-string "(a (b))") z/next 'a) :list)]
    (is (= '(a) (z/sexpr zloc)))
    (is (= '((a) (b)) (z/sexpr (z/up zloc))))
    (is (= "((a) (b))" (z/root-string zloc)))))

(deftest thread-test
  (let [zloc (z/of-string "(remove nil? (filter :id (map doit xs)))")]
    (let [result (transform/thread-last zloc)]
      (is (= '->> (z/sexpr (z/down result))))
      (is (= "(->> (filter :id (map doit xs)) (remove nil?))" (z/root-string result))))
    (let [result (transform/thread-last-all zloc)]
      (is (= '->> (z/sexpr (z/down result))))
      (is (= "(->> xs (map doit) (filter :id) (remove nil?))" (z/root-string result)))))
  (let [zloc (z/of-string "(assoc (dissoc (update m :xs reverse) :bye) :hello :world)")]
    (let [result (transform/thread-first zloc)]
      (is (= '-> (z/sexpr (z/down result))))
      (is (= "(-> (dissoc (update m :xs reverse) :bye) (assoc :hello :world))" (z/root-string result))))
    (let [result (transform/thread-first-all zloc)]
      (is (= '-> (z/sexpr (z/down result))))
      (is (= "(-> m (update :xs reverse) (dissoc :bye) (assoc :hello :world))" (z/root-string result))))))
