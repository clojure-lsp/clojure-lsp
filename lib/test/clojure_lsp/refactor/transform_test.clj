(ns clojure-lsp.refactor.transform-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.refactor.transform :as transform]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.string :as string]
   [clojure.test :refer [are deftest is testing]]
   [rewrite-clj.zip :as z]))

(h/reset-db-after-test)

(defn as-strings [results]
  (map (comp z/string :loc) results))

(defn- as-root-string [[{:keys [loc]}]]
  (z/root-string loc))

(defn- as-string [[{:keys [loc]}]]
  (z/string loc))

(deftest find-other-colls
  (testing "map"
    (is (= [:vector :set :list]
           (transform/find-other-colls (z/of-string "{:a 1}")))))
  (testing "set"
    (is (= [:vector :list :map]
           (transform/find-other-colls (z/of-string "#{:a 1}")))))
  (testing "list"
    (is (= [:vector :set :map]
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
  (let [zloc (edit/raise (h/zloc-from-code "(a (|b))"))]
    (is (= 'b (z/sexpr zloc)))
    (is (= '(a b) (z/sexpr (z/up zloc))))
    (is (= "b" (-> zloc edit/raise z/root-string))))
  (let [zloc (edit/wrap-around (h/zloc-from-code "(|a (b))") :list)]
    (is (= '(a) (z/sexpr zloc)))
    (is (= '((a) (b)) (z/sexpr (z/up zloc))))
    (is (= "((a) (b))" (z/root-string zloc)))))

(defn- thread-first [code]
  (as-root-string (transform/thread-first (h/zloc-from-code code) @db/db*)))

(defn- thread-first-all [code]
  (as-root-string (transform/thread-first-all (h/zloc-from-code code) @db/db*)))

(defn- thread-last [code]
  (as-root-string (transform/thread-last (h/zloc-from-code code) @db/db*)))

(defn- thread-last-all [code]
  (as-root-string (transform/thread-last-all (h/zloc-from-code code) @db/db*)))

(deftest thread-test
  (let [code "|(remove nil? (filter :id (map (comp now doit) xs)))"]
    (is (= (h/code "(->> (filter :id (map (comp now doit) xs))"
                   "     (remove nil?))")
           (thread-last code)))
    (is (= (h/code "(->> xs"
                   "     (map (comp now doit))"
                   "     (filter :id)"
                   "     (remove nil?))")
           (thread-last-all code))))
  (let [code "|(assoc (dissoc (update m :xs reverse) :bye) :hello :world)"]
    (is (= (h/code "(-> (dissoc (update m :xs reverse) :bye)"
                   "    (assoc :hello :world))")
           (thread-first code)))
    (is (= (h/code "(-> m"
                   "    (update :xs reverse)"
                   "    (dissoc :bye)"
                   "    (assoc :hello :world))")
           (thread-first-all code))))
  (is (= (h/code "(->> (foo)"
                 "     (interpose (spaces)))")
         (thread-last-all "|(interpose (spaces) (foo))")))
  (is (nil? (thread-last-all "|[:a :b]")))
  (is (= (h/code "(->> [:a :b]"
                 "     (get-in foo))")
         (thread-last-all "|(get-in foo [:a :b])")))
  (testing "Removing unecessary parens when 1 arg"
    (h/clean-db!)
    (is (= (h/code "(->> [1 2]"
                   "     foo"
                   "     bar)")
           (thread-last-all "|(bar (foo [1 2]))"))))
  (testing "Not removing unecessary parens when 1 arg"
    (h/clean-db!)
    (swap! db/db* shared/deep-merge {:settings {:keep-parens-when-threading? true}})
    (is (= (h/code "(->> [1 2]"
                   "     (foo)"
                   "     (bar))")
           (thread-last-all "|(bar (foo [1 2]))")))))

(defn- move-to-let [code new-sym]
  (as-root-string (transform/move-to-let (h/load-code-and-zloc code) "file:///a.clj" @db/db* new-sym)))

(deftest move-to-let-test
  (is (= (h/code "(let [a 1"
                 "      b a] b)")
         (move-to-let "(let [a 1] |a)" 'b)))
  (is (= (h/code "(let [a 1"
                 "      b (inc a)] b)")
         (move-to-let "(let [a 1] |(inc a))" 'b)))
  (is (= (h/code "(let [a 1"
                 "      b (inc a)] (thing b))")
         (move-to-let "(let [a 1] (thing |(inc a)))" 'b)))
  (is (= (h/code "(let [a 1] a) (let [b (inc b)]"
                 "                b)")
         (move-to-let "(let [a 1] a) |(inc b)" 'b)))
  (is (= (h/code "(let [as [{:a :a}]"
                 "      b b] b)")
         (move-to-let "(let [as [{:a :a}]] |b)" 'b)))
  (is (= (h/code "(let [b 1"
                 "      a (inc b)] a (inc b))")
         (move-to-let "(let [a (inc |1)] a (inc 1))" 'b)))
  (is (= (h/code "(let [b (inc 1)"
                 "      a b] a b)")
         (move-to-let "(let [a |(inc 1)] a (inc 1))" 'b)))
  (is (= (h/code "(let [x 1"
                 "      x y] x)")
         (move-to-let "(let [x 1] |y)" 'x)))
  (is (= (h/code "(let [[_] 1 x x"
                 "      a (x)] a)")
         (move-to-let "(let [[_] 1 a (|x)] a)" 'x)))
  (is (= "(let [x a] x)"
         (move-to-let "(let [] |a)" 'x)))
  (is (= (h/code "(let [a 1"
                 "      x 2]"
                 "  (+ a"
                 ""
                 "     ;; comment"
                 "     x))")
         (-> (h/code "(let [a 1]"
                     "  (+ a"
                     ""
                     "     |;; comment"
                     "     2))")
             (move-to-let 'x))))
  (is (= (h/code "(let [a 1"
                 "      x 2]"
                 "  (+ a"
                 ""
                 "     ;; comment"
                 "     x))")
         (-> (h/code "(let [a 1]"
                     "  (+ a"
                     ""
                     " |    ;; comment"
                     "     2))")
             (move-to-let 'x))))
  (is (= (h/code "(doseq [x xs] (let [a x] a))")
         (move-to-let "(doseq [x xs] (let [] |x))" 'a)))
  (is (= (h/code "(let [] (when-let [x xs] (let [a x]"
                 "                           a)))")
         (move-to-let "(let [] (when-let [x xs] |x))" 'a)))
  (is (= (h/code "(let [] (doseq [x xs] (let [a x]"
                 "                        a)))")
         (move-to-let "(let [] (doseq [x xs] |x))" 'a)))
  (is (= (h/code "(do (let [a x]"
                 "      a))")
         (move-to-let "(do |x)" 'a)))
  (is (nil? (-> (h/code "(let [a 1]"
                        "  (+ a"
                        "     2"
                        "     |;; comment"
                        "))")
                (move-to-let 'x))))
  (is (nil? (transform/move-to-let nil "file:///a.clj" @db/db* 'x))))

(defn- introduce-let [code new-sym]
  (as-root-string (transform/introduce-let (h/zloc-from-code code) new-sym)))

(deftest introduce-let-test
  (testing "simple"
    (is (= (h/code "(let [b (inc a)]"
                   "  b)")
           (-> (h/code "|(inc a)")
               (introduce-let 'b))))
    (is (= (h/code "(let [b (inc a)"
                   " c b]"
                   "  c)")
           (-> (h/code "(let [b (inc a)]"
                       "  |b)")
               (introduce-let 'c)))))
  (testing "from comment"
    (is (= (h/code "foo"
                   ";; comment"
                   ""
                   "(let [b (inc a)]"
                   "  b)"
                   "(inc c)")
           (-> (h/code "foo"
                       "|;; comment"
                       ""
                       "(inc a)"
                       "(inc c)")
               (introduce-let 'b)))))
  (testing "from whitespace"
    (is (= (h/code "foo"
                   ";; comment"
                   ""
                   "(let [b (inc a)]"
                   "  b)")
           (-> (h/code "foo"
                       ";; comment"
                       "|"
                       "(inc a)")
               (introduce-let 'b)))))
  (testing "trailing whitespace or comments"
    (is (nil? (introduce-let (h/code "(inc 1)" "(inc 2)|") 'b)))
    (is (nil? (introduce-let (h/code "(inc 1)" "(inc 2)" "|") 'b)))
    (is (nil? (introduce-let (h/code "(inc 1)" "(inc 2)" "| ;; comment") 'b)))
    (is (nil? (introduce-let (h/code "(inc 1)" "(inc 2)" "|;; comment") 'b)))
    (is (nil? (introduce-let (h/code "(inc 1)" "(inc 2)" "|;; comment" "") 'b))))
  (is (nil? (transform/introduce-let nil 'b))))

(defn- expand-let [code]
  (as-root-string (transform/expand-let (h/load-code-and-zloc code) "file:///a.clj" @db/db*)))

(deftest expand-let-test
  (testing "simple"
    (is (= (h/code "(let [a 1]"
                   " (+ 1 a 2))")
           (expand-let "(+ 1 (|let [a 1] a) 2)"))))
  (testing "in fn literal"
    (is (nil? (expand-let "#(a |(b c))"))))
  (testing "in fn without args"
    (is (= (h/code "(def foo (let [a 1]"
                   "          (fn [] a 2)))")
           (expand-let "(def foo (fn [] (|let [a 1] a) 2))"))))
  (testing "in fn with args"
    (is (= (h/code "(def foo (let [a 1]"
                   "          (fn [bar] a 2)))")
           (expand-let "(def foo (fn [bar] (|let [a 1] a) 2))"))))
  (testing "with list in front of let"
    (is (= (h/code "(let [x 4]"
                   " (+ (* 3 3) (* x x)))")
           (expand-let "(+ (* 3 3) (|let [x 4] (* x x)))"))))
  (testing "with list in front of let and more than an expr in let body"
    (is (= (h/code "(let [x 4]"
                   " (+ (* 3 3) (something 1)"
                   " (* x x)))")
           (-> (h/code "(+ (* 3 3) (|let [x 4] (something 1)"
                       " (* x x)))")
               expand-let))))
  (testing "with inner let one level after outer let"
    (is (= (h/code "(let [x 5"
                   " y 2] (when x y))")
           (expand-let "(let [x 5] (when x (let [|y 2] y)))"))))
  (testing "within coll"
    (is (= (h/code "(do (let [f 1]"
                   "     [f]))")
           (expand-let "(do [(let [f 1] |f)])")))))

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

(defn cycle-privacy [code]
  (as-string (transform/cycle-privacy (h/zloc-from-code code) @db/db*)))

(deftest cycle-privacy-test
  (testing "without-setting"
    (swap! db/db* shared/deep-merge {:settings {}})
    (is (= "defn-" (cycle-privacy "(defn |a [])")))
    (is (= "defn" (cycle-privacy "(defn- |a [])")))
    (is (= "^:private a" (cycle-privacy "(def |a [])")))
    (is (= "a" (cycle-privacy "(def ^:private |a [])"))))
  (testing "with-setting"
    (swap! db/db* shared/deep-merge {:settings {:use-metadata-for-privacy? true}})
    (is (= "^:private a" (cycle-privacy "(defn |a [])")))
    (is (= "a" (cycle-privacy "(defn ^:private |a [])")))))

(defn demote-fn [code]
  (as-string (transform/demote-fn (h/zloc-from-code code))))

(deftest demote-fn-test
  (testing "fn to literal"
    (are [expected code] (= expected (demote-fn code))
      ;; basic params
      "#()"                    "|(fn [])"
      "#(+ 1 2)"               "|(fn [] (+ 1 2))"
      "#(+ 1 %)"               "|(fn [element] (+ 1 element))"
      "#(+ 1 %1 %2)"           "|(fn [element1 element2] (+ 1 element1 element2))"
      ;; vararg
      "#(+ 1 %&)"              "|(fn [& args] (+ 1 args))"
      "#(+ 1 % %&)"            "|(fn [element & args] (+ 1 element args))"
      "#(+ 1 %1 %2 %&)"        "|(fn [element1 element2 & args] (+ 1 element1 element2 args))"
      ;; implicit do
      "#(do (prn {}) (+ 3 4))" "|(fn [] (prn {}) (+ 3 4))"
      ;; with comment
      (h/code "#(;; comment"
              "   + 3 4)")
      (h/code "|(fn []"
              "   ;; comment"
              "   (+ 3 4))")
      ;; unused param
      "#(+ 1 %1 %3)"           "|(fn [element1 _ element3] (+ 1 element1 element3))"
      ;; reordered params
      "#(+ 1 %2 %1)"           "|(fn [element1 element2] (+ 1 element2 element1))"
      ;; duplicate param
      "#(+ 1 % %)"             "|(fn [element1] (+ 1 element1 element1))"
      ;; subsequent fn
      "#(+ 1 %)"               "|(fn [element1] (+ 1 element1)) (fn [element1 element2] (+ 1 element1 element2))"
      ;; named function
      "#(+ 1 2)"               "|(fn named [] (+ 1 2))"
      ;; from inside
      "#(+ 1 2)"               "(|fn [] (+ 1 2))"
      "#(+ 1 2)"               "(fn |[] (+ 1 2))"
      "#(+ 1 2)"               "(fn [] (+ 1| 2))")))

(deftest can-demote-fn-test
  (are [code] (not (transform/can-demote-fn? (h/zloc-from-code code)))
    ;; non fns
    "(+ |1 2)"
    "|(+ 1 2)"
    ;; destructured param
    "|(fn [{:keys [element1 element2]}] (+ 1 element1 element1))"
    ;; multi-arity fn
    "|(fn ([a] (inc a)) ([a b] (+ a b)))"))

(defn promote-fn
  ([code] (promote-fn code nil))
  ([code provided-name]
   (as-strings (transform/promote-fn (h/load-code-and-zloc code) h/default-uri @db/db* provided-name))))

(deftest promote-fn-test
  (testing "literal to fn"
    (h/clean-db!)
    (are [expected fn-literal] (= [expected] (promote-fn fn-literal))
      ;; basic params
      "(fn [])"                                                      "|#()"
      "(fn [] (+ 1 2))"                                              "|#(+ 1 2)"
      "(fn [element] (+ 1 element))"                                 "|#(+ 1 %)"
      "(fn [element1] (+ 1 element1))"                               "|#(+ 1 %1)"
      "(fn [element1 element2] (+ 1 element1 element2))"             "|#(+ 1 %1 %2)"
      ;; mixed numbering styles
      "(fn [element] (+ 1 element element))"                         "|#(+ 1 % %1)"
      "(fn [element element2] (+ 1 element element2))"               "|#(+ 1 % %2)"
      ;; vararg
      "(fn [& args] (+ 1 args))"                                     "|#(+ 1 %&)"
      "(fn [element & args] (+ 1 element args))"                     "|#(+ 1 % %&)"
      "(fn [element1 element2 & args] (+ 1 element1 element2 args))" "|#(+ 1 %1 %2 %&)"
      ;; implicit do
      "(fn [] (prn {}) (+ 3 4))"                                     "|#(do (prn {}) (+ 3 4))"
      ;; with comment
      (h/code "(fn [] ;; comment"
              "   (+ 3 4))")
      (h/code "|#(;; comment"
              "   + 3 4)")
      ;; unused param
      "(fn [element1 _ element3] (+ 1 element1 element3))"           "|#(+ 1 %1 %3)"
      ;; reordered params
      "(fn [element1 element2] (+ 1 element2 element1))"             "|#(+ 1 %2 %1)"
      ;; duplicate param
      "(fn [element1] (+ 1 element1 element1))"                      "|#(+ 1 %1 %1)"
      ;; subsequent literals
      "(fn [element] (+ 1 element))"                                 "|#(+ 1 %) #(+ 2 %1 %2)"
      ;; from inside
      "(fn [] (+ 1 2))"                                              "#|(+ 1 2)"
      "(fn [] (+ 1 2))"                                              "#(+ 1| 2)"))
  (testing "fn to defn"
    (h/clean-db!)
    (are [expected-defn expected-replacement code]
         (= [expected-defn expected-replacement] (promote-fn code))
      ;; basic params
      "\n(defn- new-function [])\n"                                  "new-function" "|(fn [])"
      "\n(defn- new-function [] (+ 1 2))\n"                          "new-function" "|(fn [] (+ 1 2))"
      "\n(defn- new-function [element] (+ 1 element))\n"             "new-function" "|(fn [element] (+ 1 element))"
      ;; destructured param
      "\n(defn- new-function [{:keys [element]}] (+ 1 element))\n"   "new-function" "|(fn [{:keys [element]}] (+ 1 element))"
      ;; vararg
      "\n(defn- new-function [element & args] (+ 1 element args))\n" "new-function" "|(fn [element & args] (+ 1 element args))"
      ;; multi-arity
      "\n(defn- new-function ([x] (+ 1 x)) ([x y] (+ 1 x y)))\n"     "new-function" "|(fn ([x] (+ 1 x)) ([x y] (+ 1 x y)))"
      ;; from inside
      "\n(defn- new-function [] (+ 1 2))\n"                          "new-function" "(|fn [] (+ 1 2))"))
  (testing "fn to defn with locals"
    (h/clean-db!)
    (are [expected-defn expected-replacement code]
         (= [expected-defn expected-replacement] (promote-fn code))
      ;; basic params
      "\n(defn- new-function [a] a)\n"                                   "#(new-function a)"          "(let [a 1] |(fn [] a))"
      "\n(defn- new-function [a] (+ 1 2 a))\n"                           "#(new-function a)"          "(let [a 1] |(fn [] (+ 1 2 a)))"
      "\n(defn- new-function [a element] (+ 1 element a))\n"             "#(new-function a %1)"       "(let [a 1] |(fn [element] (+ 1 element a)))"
      ;; destructured param
      "\n(defn- new-function [a {:keys [element]}] (+ 1 element a))\n"   "#(new-function a %1)"       "(let [a 1] |(fn [{:keys [element]}] (+ 1 element a)))"
      ;; multiple, and repeated locals
      "\n(defn- new-function [a b] (+ 1 a b b))\n"                       "#(new-function a b)"        "(let [a 1 b 2] |(fn [] (+ 1 a b b)))"
      "\n(defn- new-function [a b x y] (+ 1 x y a b b))\n"               "#(new-function a b %1 %2)"  "(let [a 1 b 2] |(fn [x y] (+ 1 x y a b b)))"
      ;; vararg
      "\n(defn- new-function [a element & args] (+ 1 a element args))\n" "#(new-function a %1 %&)"    "(let [a 1] |(fn [element & args] (+ 1 a element args)))"
      ;; multi-arity
      "\n(defn- new-function ([a x] (+ 1 x a)) ([a x y] (+ 1 x y a)))\n" "(partial new-function a)"   "(let [a 1] |(fn ([x] (+ 1 x a)) ([x y] (+ 1 x y a))))"))
  (testing "naming"
    (h/clean-db!)
    ;; previously named fn
    (is (= ["\n(defn- previously-named [])\n" "previously-named"] (promote-fn "|(fn previously-named [])")))
    ;; provided name
    (is (= ["\n(defn- my-name [])\n" "my-name"] (promote-fn "|(fn [])" "my-name")))
    (is (= ["(fn my-name [])"] (promote-fn "|#()" "my-name"))))
  (testing "when nested"
    (h/clean-db!)
    ;; literal within fn
    (is (= ["(fn [element1] (+ element1))"] (promote-fn "(fn [a] (+ a #(+ |%1)))")))
    ;; fn within literal
    (is (= ["\n(defn- new-function [a] a)\n" "new-function"] (promote-fn "#(+ %1 (fn [a] |a))")))
    ;; fn within literal and with locals, replaced with partial
    (is (= ["\n(defn- new-function [a x] (+ x a))\n" "(partial new-function a)"] (promote-fn "(let [a 1] #(map |(fn [x] (+ x a)) [1 2 3]))")))
    (is (= ["\n(defn- new-function [a b x] (+ x a b))\n" "(partial new-function a b)"] (promote-fn "(let [a 1 b 2] #(map |(fn [x] (+ x a b)) [1 2 3]))"))))
  (testing "with metadata privacy"
    (swap! db/db* shared/deep-merge {:settings {:use-metadata-for-privacy? true}})
    (is (= ["\n(defn ^:private new-function [])\n" "new-function"] (promote-fn "|(fn [])")))
    (is (= ["\n(defn ^:private new-function [a] a)\n" "#(new-function a)"] (promote-fn "(let [a 1] |(fn [] a))")))))

(deftest can-promote-fn-test
  (are [code] (not (transform/can-promote-fn? (h/zloc-from-code code)))
    ;; non fn or literal
    "|a"
    "(+ |1 2)"
    "|(+ 1 2)"))

(defn change-coll [code coll-type]
  (as-string (transform/change-coll (z/of-string code) coll-type)))

(deftest change-coll-test
  (testing "when loc is not a coll"
    (is (nil? (transform/change-coll (z/of-string "\"some string\"") "map"))))
  (testing "when loc is a list"
    (is (= "{some-fun 1 2}" (change-coll "(some-fun 1 2)" "map")))
    (is (= "#{some-fun 1 2}" (change-coll "(some-fun 1 2)" "set")))
    (is (= "[some-fun 1 2]" (change-coll "(some-fun 1 2)" "vector"))))
  (testing "when loc is a map"
    (is (= "#{:some-fun 1}" (change-coll "{:some-fun 1}" "set")))
    (is (= "[:some-fun 1]" (change-coll "{:some-fun 1}" "vector")))
    (is (= "(:some-fun 1)" (change-coll "{:some-fun 1}" "list"))))
  (testing "when loc is a set"
    (is (= "[:some-fun 1]" (change-coll "#{:some-fun 1}" "vector")))
    (is (= "(:some-fun 1)" (change-coll "#{:some-fun 1}" "list")))
    (is (= "{:some-fun 1}" (change-coll "#{:some-fun 1}" "map"))))
  (testing "when loc is a vector"
    (is (= "(:some-fun 1)" (change-coll "[:some-fun 1]" "list")))
    (is (= "{:some-fun 1}" (change-coll "[:some-fun 1]" "map")))
    (is (= "#{:some-fun 1}" (change-coll "[:some-fun 1]" "set")))))

(deftest cycle-coll-test
  (is (= ["(some-fun 1 2 3)"
          "{some-fun 1 2 3}"
          "[some-fun 1 2 3]"
          "#{some-fun 1 2 3}"
          "(some-fun 1 2 3)"]
         (->> "(some-fun 1 2 3)"
              (iterate #(as-string (transform/cycle-coll (z/of-string %))))
              (take 5))))
  (is (= "[some-fun 1 2]"
         (-> "|{some-fun 1 2}"
             h/zloc-from-code
             transform/cycle-coll
             as-string)))
  (is (nil? (transform/cycle-coll (h/zloc-from-code ";; |comment")))))

(defn ^:private update-map [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn- extract-function
  ([code new-fn-name] (extract-function code new-fn-name h/default-uri))
  ([code new-fn-name filepath]
   (let [file-uri (h/file-uri filepath)
         zloc     (h/load-code-and-zloc code file-uri)]
     (transform/extract-function zloc
                                 file-uri
                                 new-fn-name
                                 @db/db*))))

(deftest extract-function-test
  (testing "simple extract"
    (are [filepath] (do
                      (h/clean-db!)
                      (= [(h/code ""
                                  "(defn- foo [b]"
                                  "  (let [c 1] (b c)))"
                                  "")
                          (h/code "(foo b)")]
                         (as-strings
                           (extract-function "(defn a [b] (|let [c 1] (b c)))"
                                             "foo"
                                             filepath))))
      "file:///a.clj"
      "file:///a.cljc"
      "file:///a.cljs"))
  (testing "multiple locals extract"
    (is (= [(h/code ""
                    "(defn- foo [a]"
                    "  (+ 1 a))"
                    "")
            (h/code "(foo a)")]
           (-> (h/code "(let [a 1 b 2 c 3]"
                       "  |(+ 1 a))")
               (extract-function "foo")
               as-strings))))
  (testing "with-setting"
    (h/clean-db!)
    (swap! db/db* shared/deep-merge {:settings {:use-metadata-for-privacy? true}})
    (is (= [(h/code ""
                    "(defn ^:private foo []"
                    "  (inc 1))"
                    "")
            (h/code "(foo)")]
           (-> "(|inc 1)"
               (extract-function "foo")
               as-strings))))
  (testing "after local usage"
    (h/clean-db!)
    (is (= [(h/code ""
                    "(defn- foo [b c]"
                    "  (+ 2 b c))"
                    "")
            (h/code "(foo b c)")]
           (-> "(defn a [b] (let [c 1] (+ 2 b |c)))"
               (extract-function "foo")
               as-strings))))
  (testing "On multi-arity function"
    (h/clean-db!)
    (is (= [(h/code ""
                    "(defn- foo [a b]"
                    "  (= a b))"
                    "")
            (h/code "(foo a b)")]
           (-> (h/code "(defn mytest"
                       "  ([a b]"
                       "   (if |(= a b)"
                       "      1"
                       "      2)))")
               (extract-function "foo")
               as-strings))))
  (testing "from comment"
    (h/clean-db!)
    (is (= [(h/code ""
                    "(defn- foo [a]"
                    "  (+ 1 a))"
                    "")
            (h/code "(foo a)")]
           (-> (h/code "(let [a 1 b 2 c 3]"
                       "  ;; |comment"
                       ""
                       "  (+ 1 a))")
               (extract-function "foo")
               as-strings))))
  (testing "from whitespace"
    (h/clean-db!)
    (is (= [(h/code ""
                    "(defn- foo [a]"
                    "  (+ 1 a))"
                    "")
            (h/code "(foo a)")]
           (-> (h/code "(let [a 1 b 2 c 3]"
                       "|  ;; comment"
                       ""
                       "  (+ 1 a))")
               (extract-function "foo")
               as-strings))))
  (testing "from trailing comment"
    (h/clean-db!)
    (is (= [(h/code ""
                    "(defn- foo []"
                    "  (let [a 1 b 2 c 3]"
                    "  (+ 1 a)"
                    "  ;; comment"
                    "))"
                    "")
            (h/code "(foo)")]
           (-> (h/code "(let [a 1 b 2 c 3]"
                       "  (+ 1 a)"
                       "  |;; comment"
                       ")")
               (extract-function "foo")
               as-strings))))
  (testing "with comments above origin function"
    (h/clean-db!)
    (h/assert-submaps
      [{:loc   (h/code ""
                       "(defn- foo [b]"
                       "  (let [c 1] (b c)))"
                       "")
        :range {:row 2 :col 1 :end-row 2 :end-col 1}}
       {:loc   "(foo b)"
        :range {:row 3 :col 13 :end-row 3 :end-col 30}}]
      (-> (h/code "(ns foo)"
                  ";; {:something true}"
                  "(defn a [b] (|let [c 1] (b c)))")
          (extract-function "foo")
          h/with-strings)))
  (testing "with comments above origin function with spaces"
    (h/clean-db!)
    (h/assert-submaps
      [{:loc   (h/code ""
                       "(defn- foo [b]"
                       "  (let [c 1] (b c)))"
                       "")
        :range {:row 2 :col 1 :end-row 2 :end-col 1}}
       {:loc   "(foo b)"
        :range {:row 5 :col 13 :end-row 5 :end-col 30}}]
      (-> (h/code "(ns foo)"
                  ""
                  ""
                  "#_{:something true}"
                  "(defn a [b] (|let [c 1] (b c)))")
          (extract-function "foo")
          h/with-strings)))
  (testing "with comments above origin function with multi line comments"
    (h/clean-db!)
    (h/assert-submaps
      [{:loc   (h/code ""
                       "(defn- foo [b]"
                       "  (let [c 1] (b c)))"
                       "")
        :range {:row 2 :col 1 :end-row 2 :end-col 1}}
       {:loc   "(foo b)"
        :range {:row 5 :col 13 :end-row 5 :end-col 30}}]
      (-> (h/code "(ns foo)"
                  ""
                  ";; {:something true}"
                  ";; other comment"
                  "(defn a [b] (|let [c 1] (b c)))")
          (extract-function "foo")
          h/with-strings)))
  (testing "from end of file"
    (h/clean-db!)
    (is (nil? (transform/extract-function nil (h/file-uri "file:///a.clj") "foo" @db/db*)))
    (h/assert-submaps
      []
      (extract-function "|;; comment"
                        "foo"))))

(defn- create-function [code]
  (transform/create-function (h/load-code-and-zloc code) "file:///a.clj" @db/db*))

(deftest create-function-test
  (testing "function on same file"
    (testing "creating with no args"
      (h/clean-db!)
      (is (= (h/code ""
                     "(defn- my-func []"
                     "  )"
                     "")
             (-> "(defn a [b] (|my-func))"
                 create-function
                 as-string))))
    (testing "creating with 1 known arg"
      (h/clean-db!)
      (is (= (h/code ""
                     "(defn- my-func [b]"
                     "  )"
                     "")
             (-> "(defn a [b] (|my-func b))"
                 create-function
                 as-string))))
    (testing "creating with 1 known arg and a unknown arg"
      (h/clean-db!)
      (is (= (h/code ""
                     "(defn- my-func [b arg2]"
                     "  )"
                     "")
             (-> "(defn a [b] (|my-func b (+ 1 2)))"
                 create-function
                 as-string))))
    (testing "creating from a fn call of other function"
      (h/clean-db!)
      (is (= (h/code ""
                     "(defn- my-func [arg1]"
                     "  )"
                     "")
             (-> "(defn a [b] (remove |my-func [1 2 3 4]))"
                 create-function
                 as-string))))
    (testing "creating from a fn call of other function nested"
      (h/clean-db!)
      (is (= (h/code ""
                     "(defn- my-func [arg1 arg2]"
                     "  )"
                     "")
             (-> "(defn a [b] (remove (partial |my-func 2) [1 2 3 4]))"
                 create-function
                 as-string))))
    (testing "creating from an anonymous function"
      (h/clean-db!)
      (is (= (h/code ""
                     "(defn- my-func [arg1 element]"
                     "  )"
                     "")
             (-> "(defn a [b] (remove #(|my-func 2 %) [1 2 3 4]))"
                 create-function
                 as-string))))
    (testing "creating from an anonymous function with many args"
      (h/clean-db!)
      (is (= (h/code ""
                     "(defn- my-func [arg1 b element3 element2]"
                     "  )"
                     "")
             (-> "(defn a [b] (#(|my-func 2 b %3 %2) 1 2 3 4))"
                 create-function
                 as-string))))
    (testing "creating from a thread first macro with single arg"
      (h/clean-db!)
      (is (= (h/code ""
                     "(defn- my-func [b]"
                     "  )"
                     "")
             (-> "(-> b |my-func (+ 1 2) (+ 2 3))"
                 create-function
                 as-string))))
    (testing "creating from a thread first macro with multiple args"
      (h/clean-db!)
      (is (= (h/code ""
                     "(defn- my-func [b a arg3]"
                     "  )"
                     "")
             (-> "(-> b (|my-func a 3) (+ 1 2))"
                 create-function
                 as-string))))
    (testing "creating from a thread last macro with multiple args"
      (h/clean-db!)
      (is (= (h/code ""
                     "(defn- my-func [a arg2 b]"
                     "  )"
                     "")
             (-> "(->> b (|my-func a 3))"
                 create-function
                 as-string)))))
  (testing "on other files"
    (testing "when namespace is already required and exists"
      (h/clean-db!)
      (h/load-code-and-locs "(ns bar)" "file:///bar.clj")
      (let [{:keys [changes-by-uri]} (create-function "(ns foo (:require [bar :as b])) (|b/something)")
            result (update-map changes-by-uri h/with-strings)]
        (is (= {(h/file-uri "file:///bar.clj")
                [{:loc (h/code "(defn something []"
                               "  )")
                  :range {:row 999999 :col 1
                          :end-row 999999 :end-col 1}}
                 {:loc (h/code ""
                               "")
                  :range {:row 999999 :col 1
                          :end-row 999999 :end-col 1}}]}
               result))))
    (testing "when namespace is not required and not exists"
      (h/clean-db!)
      (swap! db/db* shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src")
                                                                  (h/file-path "/project/test")}}})
      (let [zloc (h/load-code-and-zloc "(ns foo (:require [bar :as b])) (|b/something)"
                                       "file:///project/src/foo.clj")
            {:keys [changes-by-uri resource-changes]} (transform/create-function zloc "file:///project/src/foo.clj" @db/db*)
            result (update-map changes-by-uri h/with-strings)]
        (is (= [{:kind "create"
                 :uri (h/file-uri "file:///project/src/bar.clj")
                 :options {:overwrite false, :ignore-if-exists true}}]
               resource-changes))
        (is (= {(h/file-uri "file:///project/src/foo.clj")
                [{:range {:row 1 :col 1 :end-row 1 :end-col 32}
                  :loc (h/code "(ns foo (:require [bar :as b]"
                               "                  [something :as b]))")}]
                (h/file-uri "file:///project/src/bar.clj")
                [{:loc (h/code "(ns b)"
                               "")
                  :range {:row 1 :col 1
                          :end-row 1 :end-col 1}}
                 {:loc (h/code "(defn something []"
                               "  )")
                  :range {:row 999999 :col 1
                          :end-row 999999 :end-col 1}}
                 {:loc (h/code ""
                               "")
                  :range {:row 999999 :col 1
                          :end-row 999999 :end-col 1}}]}
               result))))
    (testing "when namespace is not required and not exists not calling it as function"
      (h/clean-db!)
      (swap! db/db* shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src")
                                                                  (h/file-path "/project/test")}}})
      (let [zloc (h/load-code-and-zloc "(ns foo (:require [bar :as b])) |b/something"
                                       "file:///project/src/foo.clj")
            {:keys [changes-by-uri resource-changes]} (transform/create-function zloc "file:///project/src/foo.clj" @db/db*)
            result (update-map changes-by-uri h/with-strings)]
        (is (= [{:kind "create"
                 :uri (h/file-uri "file:///project/src/bar.clj")
                 :options {:overwrite false, :ignore-if-exists true}}]
               resource-changes))
        (is (= {(h/file-uri "file:///project/src/foo.clj")
                [{:range {:row 1 :col 1 :end-row 1 :end-col 32}
                  :loc (h/code "(ns foo (:require [bar :as b]"
                               "                  [something :as b]))")}]
                (h/file-uri "file:///project/src/bar.clj")
                [{:loc (h/code "(ns b)"
                               "")
                  :range {:row 1 :col 1
                          :end-row 1 :end-col 1}}
                 ;; TODO we should not add a arg
                 {:loc (h/code "(defn something [arg1]"
                               "  )")
                  :range {:row 999999 :col 1
                          :end-row 999999 :end-col 1}}
                 {:loc (h/code "" "")
                  :range {:row 999999 :col 1
                          :end-row 999999 :end-col 1}}]}
               result))))))

(defn- extract-to-def
  [code new-def-name]
  (let [zloc (h/zloc-from-code code)]
    (transform/extract-to-def zloc new-def-name)))

(deftest extract-to-def-test
  (h/clean-db!)
  (is (= [(h/code ""
                  "(def ^:private foo"
                  "  {:a 1})"
                  "")
          (h/code "foo")]
         (-> "|{:a 1}"
             (extract-to-def "foo")
             as-strings)))
  (h/clean-db!)
  (is (= [(h/code ""
                  "(def ^:private new-value"
                  "  {:a 1})"
                  "")
          (h/code "new-value")]
         (-> "|{:a 1}"
             (extract-to-def nil)
             as-strings))))

(deftest can-create-test?
  (testing "when on multiples functions"
    (swap! db/db* shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src")
                                                                (h/file-path "/project/test")}}})
    (let [zloc (h/load-code-and-zloc (h/code "(ns foo)"
                                             "(defn bar []"
                                             "  |2)"
                                             "(defn baz []"
                                             "  3)"
                                             "(defn zaz []"
                                             "  4)")
                                     "file:///project/src/foo.clj")]
      (is (= {:source-paths #{"/project/src" "/project/test"},
              :current-source-path "/project/src",
              :function-name-loc "bar"}
             (update (transform/can-create-test? zloc
                                                 "file:///project/src/foo.clj"
                                                 @db/db*)
                     :function-name-loc z/string))))))

(deftest create-test-test
  (testing "when only one available source-path besides current"
    (testing "when the test file doesn't exists for clj file"
      (swap! db/db* shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src") (h/file-path "/project/test")}}
                                       :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                                       :project-root-uri (h/file-uri "file:///project")})
      (let [zloc (h/load-code-and-zloc "(ns some.ns) (defn |foo [b] (+ 1 2))"
                                       "file:///project/src/some/ns.clj")
            {:keys [changes-by-uri resource-changes]} (transform/create-test zloc "file:///project/src/some/ns.clj" @db/db* h/components)
            results-to-assert (update-map changes-by-uri h/with-strings)]
        (is (= [{:kind "create"
                 :uri (h/file-uri "file:///project/test/some/ns_test.clj")
                 :options {:overwrite false :ignore-if-exists true}}]
               resource-changes))
        (h/assert-submap
          {(h/file-uri "file:///project/test/some/ns_test.clj")
           [{:loc (h/code "(ns some.ns-test"
                          "  (:require"
                          "   [clojure.test :refer [deftest is]]"
                          "   [some.ns :as subject]))"
                          ""
                          "(deftest foo-test"
                          "  (is (= true"
                          "         (subject/foo))))"),
             :range {:row 1 :col 1 :end-row 8 :end-col 26}}]}
          results-to-assert)))
    (testing "when the test file doesn't exists for cljs file"
      (swap! db/db* shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src") (h/file-path "/project/test")}}
                                       :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                                       :project-root-uri (h/file-uri "file:///project")})
      (let [zloc (h/load-code-and-zloc "(ns some.ns) (defn |foo [b] (+ 1 2))"
                                       "file:///project/src/some/ns.cljs")
            {:keys [changes-by-uri resource-changes]} (transform/create-test zloc "file:///project/src/some/ns.cljs" @db/db* h/components)
            results-to-assert (update-map changes-by-uri h/with-strings)]
        (is (= [{:kind "create"
                 :uri (h/file-uri "file:///project/test/some/ns_test.cljs")
                 :options {:overwrite false :ignore-if-exists true}}]
               resource-changes))
        (h/assert-submap
          {(h/file-uri "file:///project/test/some/ns_test.cljs")
           [{:loc (h/code "(ns some.ns-test"
                          "  (:require"
                          "   [cljs.test :refer [deftest is]]"
                          "   [some.ns :as subject]))"
                          ""
                          "(deftest foo-test"
                          "  (is (= true"
                          "         (subject/foo))))"),
             :range {:row 1 :col 1 :end-row 8 :end-col 26}}]}
          results-to-assert)))
    (testing "when the test file exists for clj file"
      (swap! db/db* shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src")
                                                                  (h/file-path "/project/test")}}
                                       :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                                       :project-root-uri (h/file-uri "file:///project")})
      (let [test-code (h/code "(ns some.ns-test)"
                              "(deftest some-other-test)")]
        (with-redefs [shared/file-exists? #(not (string/ends-with? % "config.edn"))
                      shared/slurp-filename (constantly test-code)]
          (h/load-code-and-locs test-code "file:///project/test/some/ns_test.clj")
          (let [zloc (h/load-code-and-zloc "(ns some.ns) (defn |foo [b] (+ 1 2))"
                                           "file:///project/src/some/ns.clj")
                {:keys [changes-by-uri resource-changes]} (transform/create-test zloc "file:///project/src/some/ns.clj" @db/db* h/components)
                results-to-assert (update-map changes-by-uri h/with-strings)]
            (is (= nil resource-changes))
            (h/assert-submap
              {(h/file-uri "file:///project/test/some/ns_test.clj")
               [{:loc (h/code ""
                              "(deftest foo-test"
                              "  (is (= 1 1)))"),
                 :range {:row 3 :col 1 :end-row 5 :end-col 1}}]}
              results-to-assert)))))
    (testing "when the current source path is already a test"
      (swap! db/db* shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src") (h/file-path "/project/test")}}
                                       :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                                       :project-root-uri (h/file-uri "file:///project")})
      (let [zloc (h/load-code-and-zloc "(ns some.ns-test) (deftest |foo [b] (+ 1 2))"
                                       "file:///project/test/some/ns_test.clj")
            {:keys [changes-by-uri resource-changes]} (transform/create-test zloc "file:///project/test/some/ns_test.clj" @db/db* h/components)]
        (is (= nil resource-changes))
        (is (= nil changes-by-uri))))))

(deftest inline-symbol
  (testing "simple let"
    (h/clean-db!)
    (h/load-code-and-locs "(let [something 1] something something)")
    (let [results (:changes-by-uri (transform/inline-symbol (h/file-uri "file:///a.clj") 1 7 @db/db*))
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
    (let [results (:changes-by-uri (transform/inline-symbol (h/file-uri "file:///a.clj") 1 7 @db/db*))
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
          results (:changes-by-uri (transform/inline-symbol (h/file-uri "file:///a.clj") pos-l pos-c @db/db*))
          a-results (get results (h/file-uri "file:///a.clj"))
          b-results (get results (h/file-uri "file:///b.clj"))]
      (is (map? results))
      (is (= 2 (count results)))
      (is (= [nil] (map (comp z/string :loc) a-results)))
      (is (= ["(1 * 60)"] (map (comp z/string :loc) b-results)))))
  (testing "invalid location"
    (let [[[pos-l pos-c]] (h/load-code-and-locs "|;; comment")]
      (is (nil? (transform/inline-symbol (h/file-uri "file:///a.clj") pos-l pos-c @db/db*))))))

(defn suppress-diagnostic [code diagnostic-code]
  (h/with-strings (transform/suppress-diagnostic (h/zloc-from-code code) diagnostic-code)))

(deftest suppress-diagnostic-test
  (testing "when op has no spaces"
    (swap! db/db* shared/deep-merge {:client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
    (h/assert-submaps
      [{:loc   (h/code "#_{:clj-kondo/ignore [:unused-var]}"
                       "")
        :range {:row 3 :col 1 :end-row 3 :end-col 1}}]
      (suppress-diagnostic (h/code "(ns bla)"
                                   ""
                                   "(defn foo [|b]"
                                   "  (+ 1 2))"
                                   "(foo 1)")
                           "unused-var")))
  (testing "when op has spaces"
    (swap! db/db* shared/deep-merge {:client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
    (h/assert-submaps
      [{:loc   (h/code "#_{:clj-kondo/ignore [:unresolved-var]}"
                       "  ")
        :range {:row 4 :col 3 :end-row 4 :end-col 3}}]
      (suppress-diagnostic (h/code "(ns bla)"
                                   ""
                                   "(defn foo [b]"
                                   "  (+ |c 1 2))"
                                   "(foo 1)")
                           "unresolved-var")))
  (testing "when diagnostic is from clojure-lsp"
    (swap! db/db* shared/deep-merge {:client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
    (h/assert-submaps
      [{:loc   (h/code "#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}"
                       "")
        :range {:row 3 :col 1 :end-row 3 :end-col 1}}]
      (suppress-diagnostic (h/code "(ns bla)"
                                   ""
                                   "(def |foo 1)")
                           "clojure-lsp/unused-public-var")))
  (testing "when outside of form"
    (swap! db/db* shared/deep-merge {:client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
    (h/assert-submaps
      [{:loc   (h/code "#_{:clj-kondo/ignore [:unresolved-symbol]}"
                       "")
        :range {:row 1 :col 1 :end-row 1 :end-col 1}}]
      (suppress-diagnostic (h/code "|zzz")
                           "unresolved-symbol"))))
