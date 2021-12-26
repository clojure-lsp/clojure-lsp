(ns clojure-lsp.refactor.transform-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.refactor.transform :as transform]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(h/reset-db-after-test)

(deftest find-other-colls
  (testing "map"
    (is (= [:vector :set :list]
           (transform/find-other-colls (z/of-string "{:a 1}")))))
  (testing "set"
    (is (= [:list :map :vector]
           (transform/find-other-colls (z/of-string "#{:a 1}")))))
  (testing "list"
    (is (= [:map :vector :set]
           (transform/find-other-colls (z/of-string "(1 2 3)")))))
  (testing "vector"
    (is (= [:set :list :map]
           (transform/find-other-colls (z/of-string "[1 2 3]")))))
  (testing "commented code"
    (is (= nil
           (transform/find-other-colls (z/of-string "#_{:a 1}")))))
  (testing "invalid code"
    (is (= [:vector :set :list]
           (transform/find-other-colls (z/of-string "{:a }"))))))

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
    (let [[{:keys [loc]}] (transform/thread-last zloc db/db)]
      (is (= '->> (z/sexpr (z/down loc))))
      (is (= (string/join "\n" ["(->> (filter :id (map (comp now doit) xs))"
                                "     (remove nil?))"])
             (z/root-string loc))))
    (let [[{:keys [loc]}] (transform/thread-last-all zloc db/db)]
      (is (= '->> (z/sexpr (z/down loc))))
      (is (= (string/join "\n" ["(->> xs"
                                "     (map (comp now doit))"
                                "     (filter :id)"
                                "     (remove nil?))"])
             (z/root-string loc)))))
  (let [zloc (z/of-string "(assoc (dissoc (update m :xs reverse) :bye) :hello :world)")]
    (let [[{:keys [loc]}] (transform/thread-first zloc db/db)]
      (is (= '-> (z/sexpr (z/down loc))))
      (is (= (string/join "\n" ["(-> (dissoc (update m :xs reverse) :bye)"
                                "    (assoc :hello :world))"])
             (z/root-string loc))))
    (let [[{:keys [loc]}] (transform/thread-first-all zloc db/db)]
      (is (= '-> (z/sexpr (z/down loc))))
      (is (= (string/join "\n" ["(-> m"
                                "    (update :xs reverse)"
                                "    (dissoc :bye)"
                                "    (assoc :hello :world))"])
             (z/root-string loc)))))
  (let [zloc (z/of-string "(interpose (spaces) (foo))")
        [{:keys [loc]} :as _result] (transform/thread-last-all zloc db/db)]
    (is (= '->> (z/sexpr (z/down loc))))
    (is (= "(->> (foo)\n     (interpose (spaces)))" (z/root-string loc))))
  (let [zloc (z/of-string "[:a :b]")
        result (transform/thread-last-all zloc db/db)]
    (is (nil? result)))
  (let [zloc (z/of-string "(get-in foo [:a :b])")
        [{:keys [loc]} :as _result] (transform/thread-last-all zloc db/db)]
    (is (= '->> (z/sexpr (z/down loc))))
    (is (= "(->> [:a :b]\n     (get-in foo))" (z/root-string loc))))
  (testing "Removing unecessary parens when 1 arg"
    (h/clean-db!)
    (let [zloc (z/of-string "(bar (foo [1 2]))")
          [{:keys [loc]}] (transform/thread-last-all zloc db/db)]
      (is (= '->> (z/sexpr (z/down loc))))
      (is (= "(->> [1 2]\n     foo\n     bar)" (z/root-string loc)))))
  (testing "Not removing unecessary parens when 1 arg"
    (h/clean-db!)
    (swap! db/db shared/deep-merge {:settings {:keep-parens-when-threading? true}})
    (let [zloc (z/of-string "(bar (foo [1 2]))")
          [{:keys [loc]}] (transform/thread-last-all zloc db/db)]
      (is (= '->> (z/sexpr (z/down loc))))
      (is (= "(->> [1 2]\n     (foo)\n     (bar))" (z/root-string loc))))))

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
      (is (= (str "(def foo (let [a 1]\n          (fn [bar] a 2)))") (z/root-string loc)))))
  (testing "with list in front of let"
    (let [code "(+ (* 3 3) (let [x 4] (* x x)))"
          zloc (-> code z/of-string (z/find-value z/next 'let))
          [{:keys [loc range]}] (transform/expand-let zloc)]
      (is (some? range))
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= "(let [x 4]\n (+ (* 3 3) (* x x)))" (z/root-string loc)))))
  (testing "with list in front of let and more than an expr in let body"
    (let [code "(+ (* 3 3) (let [x 4] (something 1)\n (* x x)))"
          zloc (-> code z/of-string (z/find-value z/next 'let))
          [{:keys [loc range]}] (transform/expand-let zloc)]
      (is (some? range))
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= "(let [x 4]\n (+ (* 3 3) (something 1)\n (* x x)))"
             (z/root-string loc)))))
  (testing "with inner let one level after outer let"
    (let [code "(let [x 5] (when x (let [y 2] y)))"
          zloc (-> code z/of-string (z/find-value z/next 'y))
          [{:keys [loc range]}] (transform/expand-let zloc)]
      (is (some? range))
      (is (= 'let (z/sexpr (z/down loc))))
      (is (= "(let [x 5\n y 2] (when x y))"
             (z/root-string loc))))))

(deftest unwind-thread-test
  (testing "from thread position"
    (let [zloc (z/of-string "(-> a b (c) d)")
          [{loc1 :loc :keys [range]}] (transform/unwind-thread zloc)
          [{loc2 :loc}] (transform/unwind-thread loc1)
          [{loc3 :loc}] (transform/unwind-thread loc2)]
      (is (some? range))
      (is (= "(-> (b a) (c) d)" (z/string loc1)))
      (is (= "(-> (c (b a)) d)" (z/string loc2)))
      (is (= "(d (c (b a)))" (z/string loc3)))))
  (testing "from inner calls position"
    (let [zloc (-> (z/of-string "(-> a b (c) d)")
                   (z/find-next-value z/next 'd))
          [{loc1 :loc :keys [range]}] (transform/unwind-thread zloc)
          [{loc2 :loc}] (transform/unwind-thread loc1)
          [{loc3 :loc}] (transform/unwind-thread loc2)]
      (is (some? range))
      (is (= "(-> (b a) (c) d)" (z/string loc1)))
      (is (= "(-> (c (b a)) d)" (z/string loc2)))
      (is (= "(d (c (b a)))" (z/string loc3)))))
  (testing "from inner calls position"
    (let [zloc (-> (z/of-string "(a (-> a b (c) d))")
                   (z/find-next-value z/next 'c))
          [{loc1 :loc :keys [range]}] (transform/unwind-thread zloc)
          [{loc2 :loc}] (transform/unwind-thread loc1)
          [{loc3 :loc}] (transform/unwind-thread loc2)]
      (is (some? range))
      (is (= "(-> (b a) (c) d)" (z/string loc1)))
      (is (= "(-> (c (b a)) d)" (z/string loc2)))
      (is (= "(d (c (b a)))" (z/string loc3))))))

(deftest unwind-all-test
  (let [zloc (z/of-string "(->> (a) b (c x y) d)")
        [{:keys [loc range]}] (transform/unwind-all zloc)]
    (is (some? range))
    (is (= "(d (c x y (b (a))))" (z/string loc)))))

(deftest cycle-privacy-test
  (testing "without-setting"
    (swap! db/db shared/deep-merge {:settings {}})
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(defn a [])") z/next 'a)
                              (transform/cycle-privacy db/db))]
      (is (= (z/string loc) "defn-")))
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(defn- a [])") z/next 'a)
                              (transform/cycle-privacy db/db))]
      (is (= (z/string loc) "defn")))
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(def a [])") z/next 'a)
                              (transform/cycle-privacy db/db))]
      (is (= (z/string loc) "^:private a")))
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(def ^:private a [])") z/next 'a)
                              (transform/cycle-privacy db/db))]
      (is (= (z/string loc) "a"))))
  (testing "with-setting"
    (swap! db/db shared/deep-merge {:settings {:use-metadata-for-privacy? true}})
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(defn a [])") z/next 'a)
                              (transform/cycle-privacy db/db))]
      (is (= (z/string loc) "^:private a")))
    (let [[{:keys [loc]}] (-> (z/find-value (z/of-string "(defn ^:private a [])") z/next 'a)
                              (transform/cycle-privacy db/db))]
      (is (= (z/string loc) "a")))))

(deftest change-coll-test
  (testing "when loc is not a coll"
    (is (= [] (-> (z/of-string "\"some string\"")
                  (transform/change-coll "map")))))
  (testing "when loc is a list"
    (is (= "{some-fun 1 2}" (-> (z/of-string "(some-fun 1 2)") (transform/change-coll "map") first :loc z/string)))
    (is (= "#{some-fun 1 2}" (-> (z/of-string "(some-fun 1 2)") (transform/change-coll "set") first :loc z/string)))
    (is (= "[some-fun 1 2]" (-> (z/of-string "(some-fun 1 2)") (transform/change-coll "vector") first :loc z/string))))
  (testing "when loc is a map"
    (is (= "#{:some-fun 1}" (-> (z/of-string "{:some-fun 1}") (transform/change-coll "set") first :loc z/string)))
    (is (= "[:some-fun 1]" (-> (z/of-string "{:some-fun 1}") (transform/change-coll "vector") first :loc z/string)))
    (is (= "(:some-fun 1)" (-> (z/of-string "{:some-fun 1}") (transform/change-coll "list") first :loc z/string))))
  (testing "when loc is a set"
    (is (= "[:some-fun 1]" (-> (z/of-string "#{:some-fun 1}") (transform/change-coll "vector") first :loc z/string)))
    (is (= "(:some-fun 1)" (-> (z/of-string "#{:some-fun 1}") (transform/change-coll "list") first :loc z/string)))
    (is (= "{:some-fun 1}" (-> (z/of-string "#{:some-fun 1}") (transform/change-coll "map") first :loc z/string))))
  (testing "when loc is a vector"
    (is (= "(:some-fun 1)" (-> (z/of-string "[:some-fun 1]") (transform/change-coll "list") first :loc z/string)))
    (is (= "{:some-fun 1}" (-> (z/of-string "[:some-fun 1]") (transform/change-coll "map") first :loc z/string)))
    (is (= "#{:some-fun 1}" (-> (z/of-string "[:some-fun 1]") (transform/change-coll "set") first :loc z/string)))))

(defn ^:private update-map [m f]
  (into {} (for [[k v] m] [k (f v)])))

(deftest extract-function-test
  (testing "simple extract"
    (h/clean-db!)
    (let [code "(defn a [b] (let [c 1] (b c)))"
          zloc (z/find-value (z/of-string code) z/next 'let)
          _ (h/load-code-and-locs code)
          results (transform/extract-function
                    zloc
                    (h/file-uri "file:///a.clj")
                    "foo"
                    db/db)]
      (is (= "\n(defn foo [b]\n  (let [c 1] (b c)))\n" (z/string (:loc (first results)))))
      (is (= "(foo b)" (z/string (:loc (last results)))))))
  (testing "multiple locals extract"
    (h/clean-db!)
    (let [code (h/code "(let [a 1 b 2 c 3]"
                       "  (+ 1 a))")
          zloc (-> (z/of-string code)
                   (z/find-value z/next '+)
                   z/up)
          _ (h/load-code-and-locs code)
          results (transform/extract-function
                    zloc
                    (h/file-uri "file:///a.clj")
                    "foo"
                    db/db)]
      (is (= "\n(defn foo [a]\n  (+ 1 a))\n" (z/string (:loc (first results)))))
      (is (= "(foo a)" (z/string (:loc (last results)))))))
  (testing "with comments above origin function"
    (h/clean-db!)
    (let [code (h/code "(ns foo)"
                       ";; {:something true}"
                       "(defn a [b] (let [c 1] (b c)))")
          zloc (z/find-value (z/of-string code) z/next 'let)
          _ (h/load-code-and-locs code)
          results (transform/extract-function
                    zloc
                    (h/file-uri "file:///a.clj")
                    "foo"
                    db/db)
          results (map #(update % :loc z/string) results)]
      (h/assert-submaps
        [{:loc "\n(defn foo [b]\n  (let [c 1] (b c)))\n"
          :range {:row 2 :col 1 :end-row 2 :end-col 1}}
         {:loc "(foo b)"
          :range {:row 3 :col 13 :end-row 3 :end-col 30}}]
        results)))
  (testing "with comments above origin function with spaces"
    (h/clean-db!)
    (let [code (h/code "(ns foo)"
                       ""
                       ""
                       "#_{:something true}"
                       "(defn a [b] (let [c 1] (b c)))")
          zloc (z/find-value (z/of-string code) z/next 'let)
          _ (h/load-code-and-locs code)
          results (transform/extract-function
                    zloc
                    (h/file-uri "file:///a.clj")
                    "foo"
                    db/db)
          results (map #(update % :loc z/string) results)]
      (h/assert-submaps
        [{:loc "\n(defn foo [b]\n  (let [c 1] (b c)))\n"
          :range {:row 2 :col 1 :end-row 2 :end-col 1}}
         {:loc "(foo b)"
          :range {:row 5 :col 13 :end-row 5 :end-col 30}}]
        results)))
  (testing "with comments above origin function with multi line comments"
    (h/clean-db!)
    (let [code (h/code "(ns foo)"
                       ""
                       ";; {:something true}"
                       ";; other comment"
                       "(defn a [b] (let [c 1] (b c)))")
          zloc (z/find-value (z/of-string code) z/next 'let)
          _ (h/load-code-and-locs code)
          results (transform/extract-function
                    zloc
                    (h/file-uri "file:///a.clj")
                    "foo"
                    db/db)
          results (map #(update % :loc z/string) results)]
      (h/assert-submaps
        [{:loc "\n(defn foo [b]\n  (let [c 1] (b c)))\n"
          :range {:row 2 :col 1 :end-row 2 :end-col 1}}
         {:loc "(foo b)"
          :range {:row 5 :col 13 :end-row 5 :end-col 30}}]
        results))))

(deftest create-function-test
  (testing "function on same file"
    (testing "creating with no args"
      (h/clean-db!)
      (let [code "(defn a [b] (my-func))"
            zloc (z/find-value (z/of-string code) z/next 'my-func)
            _ (h/load-code-and-locs code)
            results (transform/create-function zloc "file:///a.clj" db/db)]
        (is (= "(defn- my-func []\n  )" (z/string (:loc (first results)))))
        (is (= "\n\n" (z/string (:loc (last results)))))))
    (testing "creating with no args from the function definition"
      (h/clean-db!)
      (let [code "(defn a [b] (my-func))"
            zloc (z/up (z/find-value (z/of-string code) z/next 'my-func))
            _ (h/load-code-and-locs code)
            results (transform/create-function zloc "file:///a.clj" db/db)]
        (is (= "(defn- my-func []\n  )" (z/string (:loc (first results)))))
        (is (= "\n\n" (z/string (:loc (last results)))))))
    (testing "creating with 1 known arg"
      (h/clean-db!)
      (let [code "(defn a [b] (my-func b))"
            zloc (z/find-value (z/of-string code) z/next 'my-func)
            _ (h/load-code-and-locs code)
            results (transform/create-function zloc "file:///a.clj" db/db)]
        (is (= "(defn- my-func [b]\n  )" (z/string (:loc (first results)))))
        (is (= "\n\n" (z/string (:loc (last results)))))))
    (testing "creating with 1 known arg and a unknown arg"
      (h/clean-db!)
      (let [code "(defn a [b] (my-func b (+ 1 2)))"
            zloc (z/find-value (z/of-string code) z/next 'my-func)
            _ (h/load-code-and-locs code)
            results (transform/create-function zloc "file:///a.clj" db/db)]
        (is (= "(defn- my-func [b arg2]\n  )" (z/string (:loc (first results)))))
        (is (= "\n\n" (z/string (:loc (last results)))))))
    (testing "creating from a thread first macro with single arg"
      (h/clean-db!)
      (let [code "(-> b my-func)"
            zloc (z/find-value (z/of-string code) z/next 'my-func)
            _ (h/load-code-and-locs code)
            results (transform/create-function zloc "file:///a.clj" db/db)]
        (is (= "(defn- my-func [b]\n  )" (z/string (:loc (first results)))))
        (is (= "\n\n" (z/string (:loc (last results)))))))
    (testing "creating from a thread first macro with multiple args"
      (h/clean-db!)
      (let [code "(-> b (my-func a 3))"
            zloc (z/find-value (z/of-string code) z/next 'my-func)
            _ (h/load-code-and-locs code)
            results (transform/create-function zloc "file:///a.clj" db/db)]
        (is (= "(defn- my-func [b a arg2]\n  )" (z/string (:loc (first results)))))
        (is (= "\n\n" (z/string (:loc (last results)))))))
    (testing "creating from a thread last macro with multiple args"
      (h/clean-db!)
      (let [code "(->> b (my-func a 3))"
            zloc (z/find-value (z/of-string code) z/next 'my-func)
            _ (h/load-code-and-locs code)
            results (transform/create-function zloc "file:///a.clj" db/db)]
        (is (= "(defn- my-func [a arg2 b]\n  )" (z/string (:loc (first results)))))
        (is (= "\n\n" (z/string (:loc (last results))))))))
  (testing "on other files"
    (testing "when namespace is already required and exists"
      (h/clean-db!)
      (h/load-code-and-locs "(ns bar)" "file:///bar.clj")
      (let [code "(ns foo (:require [bar :as b])) (b/something)"
            zloc (z/find-value (z/of-string code) z/next 'b/something)
            _ (h/load-code-and-locs code)
            {:keys [changes-by-uri]} (transform/create-function zloc "file:///a.clj" db/db)
            result (update-map changes-by-uri (fn [v] (map #(update % :loc z/string) v)))]
        (is (= {(h/file-uri "file:///a.clj")
                []
                (h/file-uri "file:///bar.clj")
                [{:loc "(defn something []\n  )"
                  :range {:row 999999 :col 1
                          :end-row 999999 :end-col 1}}
                 {:loc "\n"
                  :range {:row 999999 :col 1
                          :end-row 999999 :end-col 1}}]}
               result))))
    (testing "when namespace is not required and not exists"
      (h/clean-db!)
      (swap! db/db shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src")
                                                                 (h/file-path "/project/test")}}})
      (let [code "(ns foo (:require [bar :as b])) (b/something)"
            zloc (z/find-value (z/of-string code) z/next 'b/something)
            _ (h/load-code-and-locs code "file:///project/src/foo.clj")
            {:keys [changes-by-uri resource-changes]} (transform/create-function zloc "file:///project/src/foo.clj" db/db)
            result (update-map changes-by-uri (fn [v] (map #(update % :loc z/string) v)))]
        (is (= [{:kind "create"
                 :uri (h/file-uri "file:///project/src/bar.clj")
                 :options {:overwrite? false, :ignore-if-exists? true}}]
               resource-changes))
        (is (= {(h/file-uri "file:///project/src/foo.clj")
                [{:range {:row 1 :col 1 :end-row 1 :end-col 32}
                  :loc "(ns foo (:require [bar :as b]\n                  [something :as b]))"}]
                (h/file-uri "file:///project/src/bar.clj")
                [{:loc "(ns b)\n"
                  :range {:row 1 :col 1
                          :end-row 1 :end-col 1}}
                 {:loc "(defn something []\n  )"
                  :range {:row 999999 :col 1
                          :end-row 999999 :end-col 1}}
                 {:loc "\n"
                  :range {:row 999999 :col 1
                          :end-row 999999 :end-col 1}}]}
               result))))))

(deftest can-create-test?
  (testing "when on multiples functions"
    (swap! db/db shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src")
                                                               (h/file-path "/project/test")}}})
    (let [code (h/code "(ns foo)"
                       "(defn bar []"
                       "  2)"
                       "(defn baz []"
                       "  3)"
                       "(defn zaz []"
                       "  4)")]
      (h/load-code-and-locs code "file:///project/src/foo.clj")
      (is (= {:source-paths #{"/project/src" "/project/test"},
              :current-source-path "/project/src",
              :function-name-loc "bar"}
             (update (transform/can-create-test? (-> (z/of-string code)
                                                     (z/find-value z/next '2))
                                                 "file:///project/src/foo.clj"
                                                 db/db)
                     :function-name-loc z/string))))))

(deftest create-test-test
  (testing "when only one available source-path besides current"
    (testing "when the test file doesn't exists for clj file"
      (swap! db/db shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src") (h/file-path "/project/test")}}
                                      :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                                      :project-root-uri (h/file-uri "file:///project")})
      (let [code "(ns some.ns) (defn foo [b] (+ 1 2))"
            zloc (z/find-value (z/of-string code) z/next 'foo)
            _ (h/load-code-and-locs code "file:///project/src/some/ns.clj")
            {:keys [changes-by-uri resource-changes]} (transform/create-test zloc "file:///project/src/some/ns.clj" db/db)
            results-to-assert (update-map changes-by-uri (fn [v] (map #(update % :loc z/string) v)))]
        (is (= [{:kind "create"
                 :uri (h/file-uri "file:///project/test/some/ns_test.clj")
                 :options {:overwrite? false :ignore-if-exists? true}}]
               resource-changes))
        (h/assert-submap
          {(h/file-uri "file:///project/test/some/ns_test.clj")
           [{:loc "(ns some.ns-test\n  (:require\n   [clojure.test :refer [deftest is]]\n   [some.ns :as subject]))\n\n(deftest foo-test\n  (is (= true\n         (subject/foo))))",
             :range {:row 1 :col 1 :end-row 4 :end-col 27}}]}
          results-to-assert)))
    (testing "when the test file doesn't exists for cljs file"
      (swap! db/db shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src") (h/file-path "/project/test")}}
                                      :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                                      :project-root-uri (h/file-uri "file:///project")})
      (let [code "(ns some.ns) (defn foo [b] (+ 1 2))"
            zloc (z/find-value (z/of-string code) z/next 'foo)
            _ (h/load-code-and-locs code "file:///project/src/some/ns.cljs")
            {:keys [changes-by-uri resource-changes]} (transform/create-test zloc "file:///project/src/some/ns.cljs" db/db)
            results-to-assert (update-map changes-by-uri (fn [v] (map #(update % :loc z/string) v)))]
        (is (= [{:kind "create"
                 :uri (h/file-uri "file:///project/test/some/ns_test.cljs")
                 :options {:overwrite? false :ignore-if-exists? true}}]
               resource-changes))
        (h/assert-submap
          {(h/file-uri "file:///project/test/some/ns_test.cljs")
           [{:loc "(ns some.ns-test\n  (:require\n   [cljs.test :refer [deftest is]]\n   [some.ns :as subject]))\n\n(deftest foo-test\n  (is (= true\n         (subject/foo))))",
             :range {:row 1 :col 1 :end-row 4 :end-col 27}}]}
          results-to-assert)))
    (testing "when the test file exists for clj file"
      (swap! db/db shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src")
                                                                 (h/file-path "/project/test")}}
                                      :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                                      :project-root-uri (h/file-uri "file:///project")})
      (let [test-code (h/code "(ns some.ns-test)"
                              "(deftest some-other-test)")]
        (with-redefs [shared/file-exists? (constantly true)
                      shared/slurp-filename (constantly test-code)]
          (let [_ (h/load-code-and-locs test-code "file:///project/test/some/ns_test.clj")
                code "(ns some.ns) (defn foo [b] (+ 1 2))"
                zloc (z/find-value (z/of-string code) z/next 'foo)
                _ (h/load-code-and-locs code "file:///project/src/some/ns.clj")
                {:keys [changes-by-uri resource-changes]} (transform/create-test zloc "file:///project/src/some/ns.clj" db/db)
                results-to-assert (update-map changes-by-uri (fn [v] (map #(update % :loc z/string) v)))]
            (is (= nil resource-changes))
            (h/assert-submap
              {(h/file-uri "file:///project/test/some/ns_test.clj")
               [{:loc "\n(deftest foo-test\n  (is (= 1 1)))",
                 :range {:row 3 :col 1 :end-row 5 :end-col 1}}]}
              results-to-assert)))))
    (testing "when the current source path is already a test"
      (swap! db/db shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src") (h/file-path "/project/test")}}
                                      :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                                      :project-root-uri (h/file-uri "file:///project")})
      (let [code "(ns some.ns-test) (deftest foo [b] (+ 1 2))"
            zloc (z/find-value (z/of-string code) z/next 'foo)
            _ (h/load-code-and-locs code "file:///project/test/some/ns_test.clj")
            {:keys [changes-by-uri resource-changes]} (transform/create-test zloc "file:///project/test/some/ns_test.clj" db/db)]
        (is (= nil resource-changes))
        (is (= nil changes-by-uri))))))

(deftest inline-symbol
  (testing "simple let"
    (h/clean-db!)
    (h/load-code-and-locs "(let [something 1] something something)")
    (let [results (:changes-by-uri (transform/inline-symbol (h/file-uri "file:///a.clj") 1 7 db/db))
          a-results (get results (h/file-uri "file:///a.clj"))]
      (is (map? results))
      (is (= 1 (count results)))
      (is (= 3 (count a-results)))
      (h/assert-submaps
        [{:loc nil :range {:row 1 :col 7 :end-row 1 :end-col 18}}
         {:loc "1" :range {:row 1 :col 20 :end-row 1 :end-col 29}}
         {:loc "1" :range {:row 1 :col 30 :end-row 1 :end-col 39}}]
        (map #(-> %
                  (update :loc z/string)
                  (update :range select-keys [:row :col :end-row :end-col])) a-results))))
  (testing "multiple binding let"
    (h/clean-db!)
    (h/load-code-and-locs "(let [something 1 other 2] something other something)")
    (let [results (:changes-by-uri (transform/inline-symbol (h/file-uri "file:///a.clj") 1 7 db/db))
          a-results (get results (h/file-uri "file:///a.clj"))]
      (is (map? results))
      (is (= 1 (count results)))
      (is (= 3 (count a-results)))
      (h/assert-submaps
        [{:loc nil :range {:row 1 :col 7 :end-row 1 :end-col 18}}
         {:loc "1" :range {:row 1 :col 28 :end-row 1 :end-col 37}}
         {:loc "1" :range {:row 1 :col 44 :end-row 1 :end-col 53}}]
        (map #(-> %
                  (update :loc z/string)
                  (update :range select-keys [:row :col :end-row :end-col])) a-results))))
  (testing "def in another file"
    (h/clean-db!)
    (let [[[pos-l pos-c]] (h/load-code-and-locs "(ns a) (def |something (1 * 60))")
          _ (h/load-code-and-locs "(ns b (:require a)) (inc a/something)" (h/file-uri "file:///b.clj"))
          results (:changes-by-uri (transform/inline-symbol (h/file-uri "file:///a.clj") pos-l pos-c db/db))
          a-results (get results (h/file-uri "file:///a.clj"))
          b-results (get results (h/file-uri "file:///b.clj"))]
      (is (map? results))
      (is (= 2 (count results)))
      (is (= [nil] (map (comp z/string :loc) a-results)))
      (is (= ["(1 * 60)"] (map (comp z/string :loc) b-results))))))

(deftest suppress-diagnostic
  (testing "when op has no spaces"
    (swap! db/db shared/deep-merge {:client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
    (let [code (h/code "(ns bla)"
                       ""
                       "(defn foo [b]"
                       "  (+ 1 2))"
                       "(foo 1)")
          zloc (z/find-value (z/of-string code) z/next 'b)
          _ (h/load-code-and-locs code)
          results (transform/suppress-diagnostic zloc "unused-var")
          results-to-assert (map #(update % :loc z/string) results)]
      (h/assert-submaps
        [{:loc "#_{:clj-kondo/ignore [:unused-var]}\n"
          :range {:row 3 :col 1 :end-row 3 :end-col 1}}]
        results-to-assert)))
  (testing "when op has spaces"
    (swap! db/db shared/deep-merge {:client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
    (let [code (h/code "(ns bla)"
                       ""
                       "(defn foo [b]"
                       "  (+ c 1 2))"
                       "(foo 1)")
          zloc (z/find-value (z/of-string code) z/next 'c)
          _ (h/load-code-and-locs code)
          results (transform/suppress-diagnostic zloc "unresolved-var")
          results-to-assert (map #(update % :loc z/string) results)]
      (h/assert-submaps
        [{:loc "#_{:clj-kondo/ignore [:unresolved-var]}\n  "
          :range {:row 4 :col 3 :end-row 4 :end-col 3}}]
        results-to-assert)))
  (testing "when diagnostic is from clojure-lsp"
    (swap! db/db shared/deep-merge {:client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
    (let [code (h/code "(ns bla)"
                       ""
                       "(def foo 1)")
          zloc (z/find-value (z/of-string code) z/next 'foo)
          _ (h/load-code-and-locs code)
          results (transform/suppress-diagnostic zloc "clojure-lsp/unused-public-var")
          results-to-assert (map #(update % :loc z/string) results)]
      (h/assert-submaps
        [{:loc "#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}\n"
          :range {:row 3 :col 1 :end-row 3 :end-col 1}}]
        results-to-assert))))
