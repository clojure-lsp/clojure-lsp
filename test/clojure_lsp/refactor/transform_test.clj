(ns clojure-lsp.refactor.transform-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.refactor.edit :as edit]
    [clojure-lsp.refactor.transform :as transform]
    [clojure-lsp.test-helper :as h]
    [clojure.string :as string]
    [clojure.test :refer [deftest testing is]]
    [rewrite-clj.zip :as z]))

(defn code [& strings] (string/join "\n" strings))

(deftest resolve-best-alias-suggestion
  (testing "alias not exists"
    (is (= #{"foo"} (#'transform/resolve-best-alias-suggestions "foo" '#{bar})))
    (is (= #{"string"} (#'transform/resolve-best-alias-suggestions "clojure.string" '#{foo bar})))
    (is (= #{"json"} (#'transform/resolve-best-alias-suggestions "clojure.data.json" '#{foo bar}))))
  (testing "alias already exists"
    (is (= #{"foo"} (#'transform/resolve-best-alias-suggestions "foo" '#{foo bar})))
    (is (= #{"string" "clojure.string"} (#'transform/resolve-best-alias-suggestions "clojure.string" '#{foo bar string})))
    (is (= #{"json" "data.json"} (#'transform/resolve-best-alias-suggestions "clojure.data.json" '#{foo bar json})))
    (is (= #{"impl" "edn.impl"} (#'transform/resolve-best-alias-suggestions "clojure.data.edn.impl" '#{foo bar json impl edn.impl}))))
  (testing "core ns"
    (is (= #{"medley"} (#'transform/resolve-best-alias-suggestions "medley.core" '#{})))
    (is (= #{"bar"} (#'transform/resolve-best-alias-suggestions "foo.bar.core" '#{})))
    (is (= #{"bar"} (#'transform/resolve-best-alias-suggestions "foo.bar.core" '#{bar})))))

(deftest add-missing-libspec
  (testing "aliases"
    (testing "known namespaces in project"
      (h/load-code-and-locs "(ns a (:require [foo.s :as s]))")
      (let [zloc (-> (z/of-string "(ns foo) s/thing") z/rightmost)
            [{:keys [loc range]}] (transform/add-missing-libspec zloc)]
        (is (some? range))
        (is (= '(ns foo (:require [foo.s :as s])) (z/sexpr loc)))))
    (testing "common ns aliases"
      (reset! db/db {})
      (let [zloc (-> (z/of-string "(ns foo) set/subset?") z/rightmost)
            [{:keys [loc range]}] (transform/add-missing-libspec zloc)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.set :as set])) (z/sexpr loc)))))
    (testing "with keep-require-at-start?"
      (testing "we add first require without spaces"
        (reset! db/db {:settings {:keep-require-at-start? true}})
        (let [zloc (-> (z/of-string "(ns foo) set/subset?") z/rightmost)
              [{:keys [loc range]}] (transform/add-missing-libspec zloc)]
          (is (some? range))
          (is (= (code "(ns foo "
                       "  (:require [clojure.set :as set]))") (z/string loc)))))
      (testing "next requires follow the same pattern"
        (reset! db/db {:settings {:keep-require-at-start? true}})
        (let [zloc (-> (z/of-string "(ns foo \n  (:require [foo :as bar])) set/subset?") z/rightmost)
              [{:keys [loc range]}] (transform/add-missing-libspec zloc)]
          (is (some? range))
          (is (= (code "(ns foo "
                       "  (:require [foo :as bar]"
                       "            [clojure.set :as set]))") (z/string loc)))))))
  (testing "common refers"
    (testing "when require doesn't exists"
      (reset! db/db {})
      (let [zloc (-> (z/of-string "(ns foo) deftest") z/rightmost)
            [{:keys [loc range]}] (transform/add-missing-libspec zloc)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.test :refer [deftest]])) (z/sexpr loc)))))
    (testing "when already exists another require"
      (reset! db/db {})
      (let [zloc (-> (z/of-string "(ns foo (:require [clojure.set :refer [subset?]])) deftest") z/rightmost)
            [{:keys [loc range]}] (transform/add-missing-libspec zloc)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.set :refer [subset?]]
                          [clojure.test :refer [deftest]])) (z/sexpr loc)))))
    (testing "when already exists that ns with another refer"
      (reset! db/db {})
      (let [zloc (-> (z/of-string "(ns foo (:require [clojure.test :refer [deftest]])) testing") z/rightmost)
            [{:keys [loc range]}] (transform/add-missing-libspec zloc)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.test :refer [deftest testing]])) (z/sexpr loc)))))
    (testing "we don't add existing refers"
      (reset! db/db {})
      (let [zloc (-> (z/of-string "(ns foo (:require [clojure.test :refer [testing]])) testing") z/rightmost)]
        (is (= nil (transform/add-missing-libspec zloc)))))
    (testing "we can add multiple refers"
      (reset! db/db {})
      (let [zloc (-> (z/of-string "(ns foo (:require [clojure.test :refer [deftest testing]])) is") z/rightmost)
            [{:keys [loc range]}] (transform/add-missing-libspec zloc)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.test :refer [deftest testing is]])) (z/sexpr loc)))))))

(deftest add-import-to-namespace-test
  (testing "when there is no :import form"
    (reset! db/db {})
    (let [zloc (-> (z/of-string "(ns foo.bar) Date.") (z/find-value z/next 'Date.))
          [{:keys [loc range]}] (transform/add-import-to-namespace zloc "java.util.Date")]
      (is (some? range))
      (is (= (code "(ns foo.bar "
                   "  (:import"
                   "    java.util.Date))") (z/root-string loc)))))
  (testing "when there is no :import form with keep-require-at-start?"
    (reset! db/db {:settings {:keep-require-at-start? true}})
    (let [zloc (-> (z/of-string "(ns foo.bar) Date.") (z/find-value z/next 'Date.))
          [{:keys [loc range]}] (transform/add-import-to-namespace zloc "java.util.Date")]
      (is (some? range))
      (is (= (code "(ns foo.bar "
                   "  (:import java.util.Date))") (z/root-string loc)))))
  (testing "when there is a :import form already"
    (reset! db/db {})
    (let [zloc (-> (z/of-string (code "(ns foo.bar "
                                      "  (:import "
                                      "    java.util.Calendar)) Date.")) (z/find-value z/next 'Date.))
          [{:keys [loc range]}] (transform/add-import-to-namespace zloc "java.util.Date")]
      (is (some? range))
      (is (= (code "(ns foo.bar "
                   "  (:import "
                   "    java.util.Calendar"
                   "    java.util.Date))") (z/root-string loc)))))
  (testing "when there is already that :import imported"
    (reset! db/db {})
    (let [zloc (-> (z/of-string (code "(ns foo.bar "
                                      "  (:import "
                                      "    java.util.Date)) Date.")) (z/find-value z/next 'Date.))]
      (is (= nil
             (transform/add-import-to-namespace zloc "java.util.Date")))))
  (testing "when there is only a :require form"
    (reset! db/db {})
    (let [zloc (-> (z/of-string (code "(ns foo.bar"
                                      "  (:require"
                                      "    [foo.baz :as baz])) Date.")) (z/find-value z/next 'Date.))
          [{:keys [loc range]}] (transform/add-import-to-namespace zloc "java.util.Date")]
      (is (some? range))
      (is (= (code "(ns foo.bar"
                   "  (:require"
                   "    [foo.baz :as baz]) "
                   "  (:import"
                   "    java.util.Date))") (z/root-string loc)))))
  (testing "when there is a :require form and :import form"
    (reset! db/db {})
    (let [zloc (-> (z/of-string (code "(ns foo.bar"
                                      "  (:require"
                                      "    [foo.baz :as baz])"
                                      "  (:import"
                                      "    java.util.Calendar)) Date.")) (z/find-value z/next 'Date.))
          [{:keys [loc range]}] (transform/add-import-to-namespace zloc "java.util.Date")]
      (is (some? range))
      (is (= (code "(ns foo.bar"
                   "  (:require"
                   "    [foo.baz :as baz])"
                   "  (:import"
                   "    java.util.Calendar"
                   "    java.util.Date))") (z/root-string loc))))))

(deftest add-common-import-to-namespace-test
  (testing "when we known the import"
    (let [zloc (-> (z/of-string "(ns foo.bar) Date.") (z/find-value z/next 'Date.))
          [{:keys [loc range]}] (transform/add-common-import-to-namespace zloc)]
      (is (some? range))
      (is (= (code "(ns foo.bar "
                   "  (:import"
                   "    java.util.Date))") (z/root-string loc)))))
  (testing "when we don't known the import"
    (let [zloc (-> (z/of-string "(ns foo.bar) MyClass.") (z/find-value z/next 'MyClass.))]
      (is (= nil (transform/add-common-import-to-namespace zloc))))))

(defn test-clean-ns
  ([db input-code expected-code]
   (test-clean-ns db input-code expected-code true))
  ([db input-code expected-code in-form]
   (reset! db/db db)
   (h/load-code-and-locs input-code)
   (let [zloc (when in-form
                (-> (z/of-string input-code) z/down z/right z/right))
         [{:keys [loc range]}] (transform/clean-ns zloc (h/file-uri "file:///a.clj"))]
     (is (some? range))
     (is (= expected-code
            (z/root-string loc))))))

(deftest clean-ns-test
  (testing "without keep-require-at-start?"
    (test-clean-ns {:settings {:keep-require-at-start? false}}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] [bar :refer [b]] baz [z] ))"
                         "(s/defn func []"
                         "  (f/some))")
                   (code "(ns foo.bar"
                         " (:require"
                         "   baz"
                         "   [foo  :as f]"
                         "   [z]))"
                         "(s/defn func []"
                         "  (f/some))")))
  (testing "with keep-require-at-start?"
    (test-clean-ns {:settings {:keep-require-at-start? true}}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] [bar :as b] baz [z] ))"
                         "(s/defn func []"
                         "  (f/some))")
                   (code "(ns foo.bar"
                         " (:require baz"
                         "           [foo  :as f]"
                         "           [z]))"
                         "(s/defn func []"
                         "  (f/some))")))
  (testing "with first require as unused"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] [bar :as b] baz [z] ))"
                         "(defn func []"
                         "  (b/some))")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :as b]"
                         "   baz"
                         "   [z]))"
                         "(defn func []"
                         "  (b/some))")))
  (testing "with single unused require on ns"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] ))"
                         "(defn func []"
                         "  (b/some))")
                   (code "(ns foo.bar)"
                         "(defn func []"
                         "  (b/some))")))
  (testing "with single used require on ns"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] ))"
                         "(defn func []"
                         "  (f/some))")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f]))"
                         "(defn func []"
                         "  (f/some))")))
  (testing "with multiple unused requires on ns"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f]"
                         "   [bar :as b]))")
                   (code "(ns foo.bar)")))
  (testing "with refer at require"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] [bar :refer [some]] baz [z] ))"
                         "(defn func []"
                         "  (f/some))")
                   (code "(ns foo.bar"
                         " (:require"
                         "   baz"
                         "   [foo  :as f]"
                         "   [z]))"
                         "(defn func []"
                         "  (f/some))")))
  (testing "with refer as single require"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :refer [some]]))")
                   (code "(ns foo.bar)"))
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :refer :all]))")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :refer :all]))")))
  (testing "in any form"
    (let [to-clean (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] [bar :refer [some]] baz [z] ))"
                         ""
                         "(defn func []"
                         "  (f/some))")]
      (test-clean-ns {:documents {(h/file-uri "file:///a.clj") {:text to-clean}}}
                     to-clean
                     (code "(ns foo.bar"
                           " (:require"
                           "   baz"
                           "   [foo  :as f]"
                           "   [z]))"
                           ""
                           "(defn func []"
                           "  (f/some))")
                     false)))
  (testing "with first require as a refer"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :refer [some] ] [foo :as f]))"
                         ""
                         "(defn func []"
                         "  (some))")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :refer [some] ]))"
                         ""
                         "(defn func []"
                         "  (some))")))
  (testing "with first require as a refer with alias"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :as b :refer [some] ] [foo :as f]))"
                         ""
                         "(defn func []"
                         "  b/some"
                         "  (some))")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :as b :refer [some] ]))"
                         ""
                         "(defn func []"
                         "  b/some"
                         "  (some))")))
  (testing "unused refer from multiple refers"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :refer [some other] ]))"
                         "(some)")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :refer [some] ]))"
                         "(some)")))
  (testing "unused middle refer from multiple refers"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :refer [some other baz another] ]))"
                         "(some)"
                         "(another)"
                         "(baz)")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :refer [another baz some] ]))"
                         "(some)"
                         "(another)"
                         "(baz)")))
  (testing "unused refer and alias"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :refer [some] ]"
                         "   [baz :as b]))")
                   (code "(ns foo.bar)")))
  (testing "unsorted used refer"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [some :refer [foo bar baz]]))"
                         "   foo bar baz")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [some :refer [bar baz foo]]))"
                         "   foo bar baz")))
  (testing "single unused full package import"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import"
                         "  java.util.Date))")
                   (code "(ns foo.bar)")))
  (testing "single unused package import"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Date]))")
                   (code "(ns foo.bar)")))
  (testing "unused full package imports"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import "
                         "  java.util.Date java.util.Calendar java.util.List))"
                         "Calendar.")
                   (code "(ns foo.bar"
                         " (:import"
                         "  java.util.Calendar))"
                         "Calendar.")))
  (testing "unused package imports"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import "
                         "  [java.util Date Calendar List Map]))"
                         "Calendar."
                         "Map.")
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Calendar Map]))"
                         "Calendar."
                         "Map.")))
  (testing "unused package imports with keep-at-start?"
    (test-clean-ns {:settings {:keep-require-at-start? true}}
                   (code "(ns foo.bar"
                         " (:import [java.util Date Calendar List Map]))"
                         "Calendar."
                         "Map.")
                   (code "(ns foo.bar"
                         " (:import [java.util Calendar Map]))"
                         "Calendar."
                         "Map.")))
  (testing "unused package imports with single import"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Date List]"
                         "  java.util.Calendar))"
                         "Calendar."
                         "List.")
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util List]"
                         "  java.util.Calendar))"
                         "Calendar."
                         "List.")))
  (testing "unused package imports spacing"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Date"
                         "             Calendar"
                         "             List]))"
                         "Date."
                         "List.")
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Date"
                         "             List]))"
                         "Date."
                         "List."))
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Date"
                         "             List]))"
                         "Date."
                         "List.")
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Date"
                         "             List]))"
                         "Date."
                         "List."))
    (testing "sorts according to symbols not brackets"
      (test-clean-ns {}
                     (code "(ns foo.bar"
                           " (:import"
                           "  [zebra import1]"
                           "  apple))"
                           "import1."
                           "apple.")
                     (code "(ns foo.bar"
                           " (:import"
                           "  apple"
                           "  [zebra import1]))"
                           "import1."
                           "apple.")))))

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

(deftest extract-function-test
  (testing "simple extract"
    (reset! db/db {})
    (let [code "(defn a [b] (let [c 1] (b c)))"
          zloc (z/find-value (z/of-string code) z/next 'let)
          _ (h/load-code-and-locs code)
          results (transform/extract-function
                    zloc
                    (h/file-uri "file:///a.clj")
                    "foo")]
      (is (= "(defn foo [b]\n  (let [c 1] (b c)))" (z/string (:loc (first results)))))
      (is (= "(foo b)" (z/string (:loc (last results)))))))
  (testing "multiple locals extract"
    (reset! db/db {})
    (let [code (code "(let [a 1 b 2 c 3]"
                     "  (+ 1 a))")
          zloc (-> (z/of-string code)
                   (z/find-value z/next '+)
                   z/up)
          _ (h/load-code-and-locs code)
          results (transform/extract-function
                    zloc
                    (h/file-uri "file:///a.clj")
                    "foo")]
      (is (= "(defn foo [a]\n  (+ 1 a))" (z/string (:loc (first results)))))
      (is (= "(foo a)" (z/string (:loc (last results))))))))

(deftest create-function-test
  (testing "creating with no args"
    (reset! db/db {})
    (let [code "(defn a [b] (my-func))"
          zloc (z/find-value (z/of-string code) z/next 'my-func)
          _ (h/load-code-and-locs code)
          results (transform/create-function zloc)]
      (is (= "(defn- my-func\n  []\n  )" (z/string (:loc (first results)))))
      (is (= "\n\n" (z/string (:loc (last results)))))))
  (testing "creating with no args from the function definition"
    (reset! db/db {})
    (let [code "(defn a [b] (my-func))"
          zloc (z/up (z/find-value (z/of-string code) z/next 'my-func))
          _ (h/load-code-and-locs code)
          results (transform/create-function zloc)]
      (is (= "(defn- my-func\n  []\n  )" (z/string (:loc (first results)))))
      (is (= "\n\n" (z/string (:loc (last results)))))))
  (testing "creating with 1 known arg"
    (reset! db/db {})
    (let [code "(defn a [b] (my-func b))"
          zloc (z/find-value (z/of-string code) z/next 'my-func)
          _ (h/load-code-and-locs code)
          results (transform/create-function zloc)]
      (is (= "(defn- my-func\n  [b]\n  )" (z/string (:loc (first results)))))
      (is (= "\n\n" (z/string (:loc (last results)))))))
  (testing "creating with 1 known arg and a unknown arg"
    (reset! db/db {})
    (let [code "(defn a [b] (my-func b (+ 1 2)))"
          zloc (z/find-value (z/of-string code) z/next 'my-func)
          _ (h/load-code-and-locs code)
          results (transform/create-function zloc)]
      (is (= "(defn- my-func\n  [b arg2]\n  )" (z/string (:loc (first results)))))
      (is (= "\n\n" (z/string (:loc (last results))))))))

(deftest inline-symbol
  (testing "simple let"
    (reset! db/db {})
    (h/load-code-and-locs "(let [something 1] something something)")
    (let [results (transform/inline-symbol (h/file-uri "file:///a.clj") 1 7)
          a-results (get results (h/file-uri "file:///a.clj"))]
      (is (map? results))
      (is (= 1 (count results)))
      (is (= 3 (count a-results)))
      (is (= [nil "1" "1"] (map (comp z/string :loc) a-results)))))
  (testing "def in another file"
    (reset! db/db {})
    (let [[[pos-l pos-c]] (h/load-code-and-locs "(ns a) (def |something (1 * 60))")
          _ (h/load-code-and-locs "(ns b (:require a)) (inc a/something)" (h/file-uri "file:///b.clj"))
          results (transform/inline-symbol (h/file-uri "file:///a.clj") pos-l pos-c)
          a-results (get results (h/file-uri "file:///a.clj"))
          b-results (get results (h/file-uri "file:///b.clj"))]
      (is (map? results))
      (is (= 2 (count results)))
      (is (= [nil] (map (comp z/string :loc) a-results)))
      (is (= ["(1 * 60)"] (map (comp z/string :loc) b-results))))))
