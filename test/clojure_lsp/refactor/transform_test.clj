(ns clojure-lsp.refactor.transform-test
  (:require
   [clojure.test :refer :all]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.refactor.transform :as transform]
   [rewrite-clj.zip :as z]))

(deftest paredit-test
  (let [zloc (edit/raise (z/find-value (z/of-string "(a (b))") z/next 'b))]
    (is (= 'b (z/sexpr zloc)))
    (is (= '(a b) (z/sexpr (z/up zloc))))
    (is (= "b" (-> zloc edit/raise z/root-string))))
 (let [zloc (edit/wrap-around (z/find-value (z/of-string "(a (b))") z/next 'a) :list)]
    (is (= '(a) (z/sexpr zloc)))
    (is (= '((a) (b)) (z/sexpr (z/up zloc))))
    (is (= "((a) (b))" (z/root-string zloc)))))

(deftest thread-test
  (let [zloc (z/of-string "(remove nil? (filter :id (map (comp now doit) xs)))")]
    (let [[{:keys [loc]}] (transform/thread-last zloc)]
      (is (= '->> (z/sexpr (z/down loc))))
      (is (= "(->> (filter :id (map (comp now doit) xs)) (remove nil?))" (z/root-string loc))))
    (let [[{:keys [loc]}] (transform/thread-last-all zloc)]
      (is (= '->> (z/sexpr (z/down loc))))
      (is (= "(->> xs (map (comp now doit)) (filter :id) (remove nil?))" (z/root-string loc)))))
  (let [zloc (z/of-string "(assoc (dissoc (update m :xs reverse) :bye) :hello :world)")]
    (let [[{:keys [loc]}] (transform/thread-first zloc)]
      (is (= '-> (z/sexpr (z/down loc))))
      (is (= "(-> (dissoc (update m :xs reverse) :bye) (assoc :hello :world))" (z/root-string loc))))
    (let [[{:keys [loc]}] (transform/thread-first-all zloc)]
      (is (= '-> (z/sexpr (z/down loc))))
      (is (= "(-> m (update :xs reverse) (dissoc :bye) (assoc :hello :world))" (z/root-string loc)))))
  (let [zloc (z/of-string "(interpose (spaces) (foo))")]
    (let [[{:keys [loc]} :as result] (transform/thread-last-all zloc)]
      (is (= '->> (z/sexpr (z/down loc))))
      (is (= "(->> (foo) (interpose (spaces)))" (z/root-string loc))))))

(deftest move-to-let-test
  (let [zloc (z/up (z/find-value (z/of-string "(let [a 1] (inc a))") z/next 'inc))]
    (let [[{:keys [loc]}] (transform/move-to-let zloc 'b)]
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= (str "(let [a 1" \newline
                  "      b (inc a)] b)") (z/root-string loc)))))
  (let [zloc (z/up (z/find-value (z/of-string "(let [a 1] (thing (inc a)))") z/next 'inc))]
    (let [[{:keys [loc]}] (transform/move-to-let zloc 'b)]
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= (str "(let [a 1" \newline
                  "      b (inc a)] (thing b))") (z/root-string loc)))))
  (let [zloc (z/up (z/find-value (z/of-string "(let [a 1] a) (inc b)") z/next 'inc))]
    (let [[{:keys [loc]}] (transform/move-to-let zloc 'b)]
      (is (nil? loc))))
  (let [zloc (z/of-string "(let [as [{:a :a}]] b)")]
    (let [[{:keys [loc]}] (transform/move-to-let (z/rightmost (z/down zloc)) 'b)]
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= (str "(let [as [{:a :a}]\n      b b] b)") (z/root-string loc)))))
  (let [zloc (z/find-value (z/of-string "(let [a (inc 1)] a (inc 1))") z/next 1)]
    (let [[{:keys [loc]}] (transform/move-to-let zloc 'b)]
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= (str "(let [b 1"\newline
                  "      a (inc b)] a (inc b))") (z/root-string loc)))))
  (let [zloc (z/up (z/find-value (z/of-string "(let [a (inc 1)] a (inc 1))") z/next 1))]
    (let [[{:keys [loc]}] (transform/move-to-let zloc 'b)]
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= (str "(let [b (inc 1)"\newline
                  "      a b] a b)") (z/root-string loc))))))

(deftest introduce-let-test
  (let [zloc (z/of-string "(inc a)")]
    (let [[{:keys [loc range]}] (transform/introduce-let zloc 'b)]
      (is (some? range))
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= (str "(let [b (inc a)]\n b)") (z/root-string loc)))
      (let [[{:keys [loc range]}] (transform/introduce-let (z/rightmost (z/down  (z/of-string (z/root-string loc)))) 'c)]
        (is (some? range))
        (is (= 'let (z/sexpr (z/down loc))))
        (is (= (str "(let [b (inc a)\n c b]\n c)") (z/root-string loc)))))))
