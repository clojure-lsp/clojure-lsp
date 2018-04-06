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
    (let [[{:keys [loc]}] (transform/thread-last zloc)]
      (is (= '->> (z/sexpr (z/down loc))))
      (is (= "(->> (filter :id (map doit xs)) (remove nil?))" (z/root-string loc))))
    (let [[{:keys [loc]}] (transform/thread-last-all zloc)]
      (is (= '->> (z/sexpr (z/down loc))))
      (is (= "(->> xs (map doit) (filter :id) (remove nil?))" (z/root-string loc)))))
  (let [zloc (z/of-string "(assoc (dissoc (update m :xs reverse) :bye) :hello :world)")]
    (let [[{:keys [loc]}] (transform/thread-first zloc)]
      (is (= '-> (z/sexpr (z/down loc))))
      (is (= "(-> (dissoc (update m :xs reverse) :bye) (assoc :hello :world))" (z/root-string loc))))
    (let [[{:keys [loc]}] (transform/thread-first-all zloc)]
      (is (= '-> (z/sexpr (z/down loc))))
      (is (= "(-> m (update :xs reverse) (dissoc :bye) (assoc :hello :world))" (z/root-string loc))))))
