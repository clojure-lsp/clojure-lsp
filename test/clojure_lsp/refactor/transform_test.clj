(ns clojure-lsp.refactor.transform-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.handlers :as handlers]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.refactor.edit :as edit]
    [clojure-lsp.refactor.transform :as transform]
    [clojure.string :as string]
    [clojure.test :refer :all]
    [clojure.tools.logging :as log]
    [rewrite-cljc.zip :as z]))

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
    (let [[{:keys [loc]}] (transform/thread-last zloc nil)]
      (is (= '->> (z/sexpr (z/down loc))))
      (is (= (string/join "\n" ["(->> (filter :id (map (comp now doit) xs))"
                                "     (remove nil?))"])
             (z/root-string loc))))
    (let [[{:keys [loc]}] (transform/thread-last-all zloc nil)]
      (is (= '->> (z/sexpr (z/down loc))))
      (is (= (string/join "\n" ["(->> xs"
                                "     (map (comp now doit))"
                                "     (filter :id)"
                                "     (remove nil?))"])
             (z/root-string loc)))))
  (let [zloc (z/of-string "(assoc (dissoc (update m :xs reverse) :bye) :hello :world)")]
    (let [[{:keys [loc]}] (transform/thread-first zloc nil)]
      (is (= '-> (z/sexpr (z/down loc))))
      (is (= (string/join "\n" ["(-> (dissoc (update m :xs reverse) :bye)"
                                "    (assoc :hello :world))"])
             (z/root-string loc))))
    (let [[{:keys [loc]}] (transform/thread-first-all zloc nil)]
      (is (= '-> (z/sexpr (z/down loc))))
      (is (= (string/join "\n" ["(-> m"
                                "    (update :xs reverse)"
                                "    (dissoc :bye)"
                                "    (assoc :hello :world))"])
             (z/root-string loc)))))
  (let [zloc (z/of-string "(interpose (spaces) (foo))")
        [{:keys [loc]} :as _result] (transform/thread-last-all zloc nil)]
    (is (= '->> (z/sexpr (z/down loc))))
    (is (= "(->> (foo)\n     (interpose (spaces)))" (z/root-string loc))))
  (let [zloc (z/of-string "[:a :b]")
        result (transform/thread-last-all zloc nil)]
    (is (nil? result)))
  (let [zloc (z/of-string "(get-in foo [:a :b])")
        [{:keys [loc]} :as _result] (transform/thread-last-all zloc nil)]
    (is (= '->> (z/sexpr (z/down loc))))
    (is (= "(->> [:a :b]\n     (get-in foo))" (z/root-string loc)))))

(deftest move-to-let-test
  (let [zloc (z/rightmost (z/down (z/of-string "(let [a 1] a)")))
        [{:keys [loc]}] (transform/move-to-let zloc nil 'b)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [a 1" \newline
                "      b a] b)")
           (z/root-string loc))))
  (let [zloc (z/up (z/find-value (z/of-string "(let [a 1] (inc a))") z/next 'inc))
        [{:keys [loc]}] (transform/move-to-let zloc nil 'b)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [a 1" \newline
                "      b (inc a)] b)") (z/root-string loc))))
  (let [zloc (z/up (z/find-value (z/of-string "(let [a 1] (thing (inc a)))") z/next 'inc))
        [{:keys [loc]}] (transform/move-to-let zloc nil 'b)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [a 1" \newline
                "      b (inc a)] (thing b))") (z/root-string loc))))
  (let [zloc (z/up (z/find-value (z/of-string "(let [a 1] a) (inc b)") z/next 'inc))
        [{:keys [loc]}] (transform/move-to-let zloc nil 'b)]
    (is (nil? loc)))
  (let [zloc (z/of-string "(let [as [{:a :a}]] b)")
        [{:keys [loc]}] (transform/move-to-let (z/rightmost (z/down zloc)) nil 'b)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [as [{:a :a}]\n      b b] b)") (z/root-string loc))))
  (let [zloc (z/find-value (z/of-string "(let [a (inc 1)] a (inc 1))") z/next 1)
        [{:keys [loc]}] (transform/move-to-let zloc nil 'b)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [b 1" \newline
                "      a (inc b)] a (inc b))") (z/root-string loc))))
  (let [zloc (z/up (z/find-value (z/of-string "(let [a (inc 1)] a (inc 1))") z/next 1))
        [{:keys [loc]}] (transform/move-to-let zloc nil 'b)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [b (inc 1)" \newline
                "      a b] a b)") (z/root-string loc))))
  (let [zloc (z/find-value (z/of-string "(let [x 1] y)") z/next 'y)
        [{:keys [loc]}] (transform/move-to-let zloc nil 'x)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [x 1" \newline
                "      x y] x)")
           (z/root-string loc))))
  (let [zloc (z/find-value (z/of-string "(let [[_] 1 a (x)] a)") z/next 'x)
        [{:keys [loc]}] (transform/move-to-let zloc nil 'x)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [[_] 1 x x" \newline
                "      a (x)] a)")
           (z/root-string loc))))
  (let [zloc (z/find-value (z/of-string "(let [] a)") z/next 'a)
        [{:keys [loc]}] (transform/move-to-let zloc nil 'x)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [x a] x)")
           (z/root-string loc)))))

(deftest introduce-let-test
  (let [zloc (z/of-string "(inc a)")
        [{:keys [loc range]}] (transform/introduce-let zloc nil 'b)]
    (is (some? range))
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [b (inc a)]\n  b)") (z/root-string loc)))
    (let [[{:keys [loc range]}] (transform/introduce-let (z/rightmost (z/down  (z/of-string (z/root-string loc)))) nil 'c)]
      (is (some? range))
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= (str "(let [b (inc a)\n c b]\n  c)") (z/root-string loc))))))

(deftest expand-let-test
  (testing "simple"
    (let [zloc (-> (z/of-string "(+ 1 (let [a 1] a) 2)") (z/find-value z/next 'let))
          [{:keys [loc range]}] (transform/expand-let zloc nil)]
      (is (some? range))
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= (str "(let [a 1]\n (+ 1 a 2))") (z/root-string loc)))))
  (testing "in fn literal"
    (let [code "#(a (b c))"
          zloc (-> (z/of-string code) (z/find-value z/next 'b) (z/up))
          [{:keys [loc]}] (transform/expand-let zloc nil)]
      (is (nil? (z/root-string loc))))))

(def ns-to-clean
  (str "(ns foo.bar\n"
       " (:require\n"
       "   [foo  :as f] [bar :as b] baz [z] ))\n"
       "(s/defn func []\n"
       "  (f/some))"))

(deftest clean-ns-test
  (with-redefs [slurp (constantly ns-to-clean)]
    (testing "without keep-require-at-start?"
      (reset! db/db {:settings {"keep-require-at-start?" false}})
      (let [zloc (-> (z/of-string ns-to-clean) z/down z/right z/right)
            [{:keys [loc range]}] (transform/clean-ns zloc "file://a.clj")]
        (is (some? range))
        (is (= (str "(ns foo.bar\n"
                    " (:require\n"
                    "   [foo  :as f]\n"
                    "   [z]\n"
                    "   baz))\n"
                    "(s/defn func []\n"
                    "  (f/some))")
               (z/root-string loc))))))
  (with-redefs [slurp (constantly ns-to-clean)]
    (testing "with keep-require-at-start?"
      (reset! db/db {:settings {"keep-require-at-start?" true}})
      (let [zloc (-> (z/of-string ns-to-clean) z/down z/right z/right)
            [{:keys [loc range]}] (transform/clean-ns zloc "file://a.clj")]
        (is (some? range))
        (is (= (str "(ns foo.bar\n"
                    " (:require [foo  :as f]\n"
                    "           [z]\n"
                    "           baz))\n"
                    "(s/defn func []\n"
                    "  (f/some))")
               (z/root-string loc)))))))

(deftest add-missing-libspec
  (reset! db/db {:file-envs
                 {"file://a.clj" (parser/find-usages "(ns a (:require [foo.s :as s]))" :clj {})}})
  (let [zloc (-> (z/of-string "(ns foo) s/thing") z/rightmost)
        [{:keys [loc range]}] (transform/add-missing-libspec zloc nil)]
    (is (some? range))
    (is (= '(ns foo (:require [foo.s :as s])) (z/sexpr loc)))))

(deftest unwind-thread-test
  (let [zloc (z/of-string "(-> a b (c) d)")
        [{loc1 :loc :keys [range]}] (transform/unwind-thread zloc nil)
        [{loc2 :loc}] (transform/unwind-thread loc1 nil)
        [{loc3 :loc}] (transform/unwind-thread loc2 nil)]
    (is (some? range))
    (is (= "(-> (b a) (c) d)" (z/string loc1)))
    (is (= "(-> (c (b a)) d)" (z/string loc2)))
    (is (= "(d (c (b a)))" (z/string loc3)))))

(deftest unwind-all-test
  (let [zloc (z/of-string "(->> (a) b (c x y) d)")
        [{:keys [loc range]}] (transform/unwind-all zloc nil)]
    (is (some? range))
    (is (= "(d (c x y (b (a))))" (z/string loc)))))

(deftest extract-function-test
  (let [code "(defn a [b] (let [c 1] (b c)))"
        usages (parser/find-usages code :clj {})
        zloc (z/find-value (z/of-string code) z/next 'let)
        results (transform/extract-function
                  zloc
                  nil
                  "foo"
                  (parser/usages-in-form zloc usages))]
    (is (= (z/string (:loc (first results))) "(defn foo [b]\n  (let [c 1] (b c)))"))
    (is (= (z/string (:loc (last results))) "(foo b)"))))

(deftest inline-symbol
  (testing "simple let"
    (let [code "(let [something 1] something something)"
          usages (parser/find-usages code :clj {})]
      (reset! db/db {:documents {"file://a.clj" {:text code}}
                     :file-envs {"file://a.clj" usages}})
      (let [zloc (z/find-value (z/of-string code) z/next 'something)
            pos (meta (z/node zloc))
            definition (handlers/definition-usage "file://a.clj" (:row pos) (:col pos))
            references (handlers/reference-usages "file://a.clj" (:row pos) (:col pos))
            results (transform/inline-symbol zloc "file://a.clj" definition references)
            a-results (get results "file://a.clj")]
        (is (map? results))
        (is (= 1 (count results)))
        (is (= 3 (count a-results)))
        (is (= [nil "1" "1"] (map (comp z/string :loc) a-results))))))
  (testing "def in another file"
    (let [a-code "(ns a) (def something (1 * 60))"
          b-code "(ns b (:require a)) (inc a/something)"
          a-usages (parser/find-usages a-code :clj {})
          b-usages (parser/find-usages b-code :clj {})]
      (reset! db/db {:documents {"file://a.clj" {:text a-code}
                                 "file://b.clj" {:text b-code}}
                     :file-envs {"file://a.clj" a-usages
                                 "file://b.clj" b-usages}})
      (let [zloc (z/find-value (z/of-string b-code) z/next 'a/something)
            pos (meta (z/node zloc))
            definition (handlers/definition-usage "file://b.clj" (:row pos) (:col pos))
            references (handlers/reference-usages "file://b.clj" (:row pos) (:col pos))
            results (transform/inline-symbol zloc "file://b.clj" definition references)
            a-results (get results "file://a.clj")
            b-results (get results "file://b.clj")]
        (is (map? results))
        (is (= 2 (count results)))
        (is (= [nil] (map (comp z/string :loc) a-results)))
        (is (= ["(1 * 60)"] (map (comp z/string :loc) b-results)))))))

(deftest cycle-privacy-test
  (testing "without-setting"
    (reset! db/db {:settings {}})
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(defn a [])") z/next 'a)
                              (transform/cycle-privacy nil))]
      (is (= (z/string loc) "defn-")))
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(defn- a [])") z/next 'a)
                              (transform/cycle-privacy nil))]
      (is (= (z/string loc) "defn")))
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(def a [])") z/next 'a)
                              (transform/cycle-privacy nil))]
      (is (= (z/string loc) "^:private a")))
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(def ^:private a [])") z/next 'a)
                              (transform/cycle-privacy nil))]
      (is (= (z/string loc) "a"))))
  (testing "with-setting"
    (reset! db/db {:settings {"use-metadata-for-privacy?" true}})
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(defn a [])") z/next 'a)
                              (transform/cycle-privacy nil))]
      (is (= (z/string loc) "^:private a")))
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(defn ^:private a [])") z/next 'a)
                              (transform/cycle-privacy nil))]
      (is (= (z/string loc) "a")))))
