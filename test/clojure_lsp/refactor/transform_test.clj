(ns clojure-lsp.refactor.transform-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.references :as f.references]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.refactor.edit :as edit]
    [clojure-lsp.refactor.transform :as transform]
    [clojure.string :as string]
    [clojure.test :refer :all]
    [rewrite-clj.zip :as z]
    [clojure-lsp.test-helper :as h]
    [clojure-lsp.queries :as q]
    [clojure-lsp.shared :as shared]))

(defn code [& strings] (string/join "\n" strings))

(deftest add-missing-libspec
  (testing "aliases"
     (testing "known namespaces in project"
      (h/load-code-and-locs "(ns a (:require [foo.s :as s]))")
      (let [zloc (-> (z/of-string "(ns foo) s/thing") z/rightmost)
            [{:keys [loc range]}] (transform/add-missing-libspec "file:///a.clj"zloc)]
        (is (some? range))
        (is (= '(ns foo (:require [foo.s :as s])) (z/sexpr loc)))))
    (testing "common ns aliases"
      (reset! db/db {})
      (let [zloc (-> (z/of-string "(ns foo) set/subset?") z/rightmost)
            [{:keys [loc range]}] (transform/add-missing-libspec "file:///a.clj" zloc)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.set :as set])) (z/sexpr loc)))))
    (testing "with keep-require-at-start?"
      (testing "we add first require without spaces"
        (reset! db/db {:settings {:keep-require-at-start? true}})
        (let [zloc (-> (z/of-string "(ns foo) set/subset?") z/rightmost)
              [{:keys [loc range]}] (transform/add-missing-libspec "file:///a.clj" zloc)]
          (is (some? range))
          (is (= (code "(ns foo "
                       "  (:require [clojure.set :as set]))") (z/string loc)))))
      (testing "next requires follow the same pattern"
        (reset! db/db {:settings {:keep-require-at-start? true}})
        (let [zloc (-> (z/of-string "(ns foo \n  (:require [foo :as bar])) set/subset?") z/rightmost)
              [{:keys [loc range]}] (transform/add-missing-libspec "file:///a.clj" zloc)]
          (is (some? range))
          (is (= (code "(ns foo "
                       "  (:require [foo :as bar]"
                       "            [clojure.set :as set]))") (z/string loc)))))))
  (testing "common refers"
    (testing "when require doesn't exists"
      (reset! db/db {})
      (let [zloc (-> (z/of-string "(ns foo) deftest") z/rightmost)
            [{:keys [loc range]}] (transform/add-missing-libspec "file:///a.clj" zloc)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.test :refer [deftest]])) (z/sexpr loc)))))
    (testing "when already exists another require"
      (reset! db/db {})
      (let [zloc (-> (z/of-string "(ns foo (:require [clojure.set :refer [subset?]])) deftest") z/rightmost)
            [{:keys [loc range]}] (transform/add-missing-libspec "file:///a.clj" zloc)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.set :refer [subset?]]
                          [clojure.test :refer [deftest]])) (z/sexpr loc)))))
    (testing "when already exists that ns with another refer"
      (reset! db/db {})
      (let [zloc (-> (z/of-string "(ns foo (:require [clojure.test :refer [deftest]])) testing") z/rightmost)
            [{:keys [loc range]}] (transform/add-missing-libspec "file:///a.clj" zloc)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.test :refer [deftest testing]])) (z/sexpr loc)))))
    (testing "we don't add existing refers"
      (reset! db/db {})
      (let [zloc (-> (z/of-string "(ns foo (:require [clojure.test :refer [testing]])) testing") z/rightmost)]
        (is (= nil (transform/add-missing-libspec "file:///a.clj" zloc)))))
    (testing "we can add multiple refers"
      (reset! db/db {})
      (let [zloc (-> (z/of-string "(ns foo (:require [clojure.test :refer [deftest testing]])) is") z/rightmost)
            [{:keys [loc range]}] (transform/add-missing-libspec "file:///a.clj" zloc)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.test :refer [deftest testing is]])) (z/sexpr loc)))))))

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
      (is (= (string/join "\n" ["(->> (filter :id (map (comp now doit) xs))"
                                "     (remove nil?))"])
             (z/root-string loc))))
    (let [[{:keys [loc]}] (transform/thread-last-all zloc)]
      (is (= '->> (z/sexpr (z/down loc))))
      (is (= (string/join "\n" ["(->> xs"
                                "     (map (comp now doit))"
                                "     (filter :id)"
                                "     (remove nil?))"])
             (z/root-string loc)))))
  (let [zloc (z/of-string "(assoc (dissoc (update m :xs reverse) :bye) :hello :world)")]
    (let [[{:keys [loc]}] (transform/thread-first zloc)]
      (is (= '-> (z/sexpr (z/down loc))))
      (is (= (string/join "\n" ["(-> (dissoc (update m :xs reverse) :bye)"
                                "    (assoc :hello :world))"])
             (z/root-string loc))))
    (let [[{:keys [loc]}] (transform/thread-first-all zloc)]
      (is (= '-> (z/sexpr (z/down loc))))
      (is (= (string/join "\n" ["(-> m"
                                "    (update :xs reverse)"
                                "    (dissoc :bye)"
                                "    (assoc :hello :world))"])
             (z/root-string loc)))))
  (let [zloc (z/of-string "(interpose (spaces) (foo))")
        [{:keys [loc]} :as _result] (transform/thread-last-all zloc)]
    (is (= '->> (z/sexpr (z/down loc))))
    (is (= "(->> (foo)\n     (interpose (spaces)))" (z/root-string loc))))
  (let [zloc (z/of-string "[:a :b]")
        result (transform/thread-last-all zloc)]
    (is (nil? result)))
  (let [zloc (z/of-string "(get-in foo [:a :b])")
        [{:keys [loc]} :as _result] (transform/thread-last-all zloc)]
    (is (= '->> (z/sexpr (z/down loc))))
    (is (= "(->> [:a :b]\n     (get-in foo))" (z/root-string loc)))))

(deftest move-to-let-test
  (let [zloc (z/rightmost (z/down (z/of-string "(let [a 1] a)")))
        [{:keys [loc]}] (transform/move-to-let zloc 'b)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [a 1" \newline
                "      b a] b)")
           (z/root-string loc))))
  (let [zloc (z/up (z/find-value (z/of-string "(let [a 1] (inc a))") z/next 'inc))
        [{:keys [loc]}] (transform/move-to-let zloc 'b)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [a 1" \newline
                "      b (inc a)] b)") (z/root-string loc))))
  (let [zloc (z/up (z/find-value (z/of-string "(let [a 1] (thing (inc a)))") z/next 'inc))
        [{:keys [loc]}] (transform/move-to-let zloc 'b)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [a 1" \newline
                "      b (inc a)] (thing b))") (z/root-string loc))))
  (let [zloc (z/up (z/find-value (z/of-string "(let [a 1] a) (inc b)") z/next 'inc))
        [{:keys [loc]}] (transform/move-to-let zloc 'b)]
    (is (nil? loc)))
  (let [zloc (z/of-string "(let [as [{:a :a}]] b)")
        [{:keys [loc]}] (transform/move-to-let (z/rightmost (z/down zloc)) 'b)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [as [{:a :a}]\n      b b] b)") (z/root-string loc))))
  (let [zloc (z/find-value (z/of-string "(let [a (inc 1)] a (inc 1))") z/next 1)
        [{:keys [loc]}] (transform/move-to-let zloc 'b)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [b 1" \newline
                "      a (inc b)] a (inc b))") (z/root-string loc))))
  (let [zloc (z/up (z/find-value (z/of-string "(let [a (inc 1)] a (inc 1))") z/next 1))
        [{:keys [loc]}] (transform/move-to-let zloc 'b)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [b (inc 1)" \newline
                "      a b] a b)") (z/root-string loc))))
  (let [zloc (z/find-value (z/of-string "(let [x 1] y)") z/next 'y)
        [{:keys [loc]}] (transform/move-to-let zloc 'x)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [x 1" \newline
                "      x y] x)")
           (z/root-string loc))))
  (let [zloc (z/find-value (z/of-string "(let [[_] 1 a (x)] a)") z/next 'x)
        [{:keys [loc]}] (transform/move-to-let zloc 'x)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [[_] 1 x x" \newline
                "      a (x)] a)")
           (z/root-string loc))))
  (let [zloc (z/find-value (z/of-string "(let [] a)") z/next 'a)
        [{:keys [loc]}] (transform/move-to-let zloc 'x)]
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [x a] x)")
           (z/root-string loc)))))

(deftest introduce-let-test
  (let [zloc (z/of-string "(inc a)")
        [{:keys [loc range]}] (transform/introduce-let zloc 'b)]
    (is (some? range))
    (is (= 'let (z/sexpr (z/down loc))))
    (is (= (str "(let [b (inc a)]\n  b)") (z/root-string loc)))
    (let [[{:keys [loc range]}] (transform/introduce-let (z/rightmost (z/down (z/of-string (z/root-string loc)))) 'c)]
      (is (some? range))
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= (str "(let [b (inc a)\n c b]\n  c)") (z/root-string loc))))))

(deftest expand-let-test
  (testing "simple"
    (let [zloc (-> (z/of-string "(+ 1 (let [a 1] a) 2)") (z/find-value z/next 'let))
          [{:keys [loc range]}] (transform/expand-let zloc)]
      (is (some? range))
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= (str "(let [a 1]\n (+ 1 a 2))") (z/root-string loc)))))
  (testing "in fn literal"
    (let [code "#(a (b c))"
          zloc (-> (z/of-string code) (z/find-value z/next 'b) (z/up))
          [{:keys [loc]}] (transform/expand-let zloc)]
      (is (nil? (z/root-string loc)))))
  (testing "in fn without args"
    (let [code "(def foo (fn [] (let [a 1] a) 2))"
          zloc (-> code z/of-string (z/find-value z/next 'let))
          [{:keys [loc range]}] (transform/expand-let zloc)]
      (is (some? range))
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= (str "(def foo (let [a 1]\n          (fn [] a 2)))") (z/root-string loc)))))
  (testing "in fn with args"
    (let [code "(def foo (fn [bar] (let [a 1] a) 2))"
          zloc (-> code z/of-string (z/find-value z/next 'let))
          [{:keys [loc range]}] (transform/expand-let zloc)]
      (is (some? range))
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= (str "(def foo (let [a 1]\n          (fn [bar] a 2)))") (z/root-string loc))))))

(deftest find-missing-import
  (testing "when usage is a java constructor"
    (let [zloc (-> (z/of-string "(ns a) Date.") z/rightmost)
          full-package (transform/find-missing-import zloc)]
      (is (= 'java.util.Date full-package))))
  (testing "when usage is a java constructor"
    (let [zloc (-> (z/of-string "(ns a) Date/parse") z/rightmost)
          full-package (transform/find-missing-import zloc)]
      (is (= 'java.util.Date full-package)))))

(deftest unwind-thread-test
  (let [zloc (z/of-string "(-> a b (c) d)")
        [{loc1 :loc :keys [range]}] (transform/unwind-thread zloc)
        [{loc2 :loc}] (transform/unwind-thread loc1)
        [{loc3 :loc}] (transform/unwind-thread loc2)]
    (is (some? range))
    (is (= "(-> (b a) (c) d)" (z/string loc1)))
    (is (= "(-> (c (b a)) d)" (z/string loc2)))
    (is (= "(d (c (b a)))" (z/string loc3)))))

(deftest unwind-all-test
  (let [zloc (z/of-string "(->> (a) b (c x y) d)")
        [{:keys [loc range]}] (transform/unwind-all zloc)]
    (is (some? range))
    (is (= "(d (c x y (b (a))))" (z/string loc)))))

(deftest cycle-privacy-test
  (testing "without-setting"
    (reset! db/db {:settings {}})
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(defn a [])") z/next 'a)
                              transform/cycle-privacy)]
      (is (= (z/string loc) "defn-")))
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(defn- a [])") z/next 'a)
                              transform/cycle-privacy)]
      (is (= (z/string loc) "defn")))
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(def a [])") z/next 'a)
                              transform/cycle-privacy)]
      (is (= (z/string loc) "^:private a")))
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(def ^:private a [])") z/next 'a)
                              transform/cycle-privacy)]
      (is (= (z/string loc) "a"))))
  (testing "with-setting"
    (reset! db/db {:settings {:use-metadata-for-privacy? true}})
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(defn a [])") z/next 'a)
                              transform/cycle-privacy)]
      (is (= (z/string loc) "^:private a")))
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(defn ^:private a [])") z/next 'a)
                              transform/cycle-privacy)]
      (is (= (z/string loc) "a")))))
