(ns clojure-lsp.handlers-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.parser :as parser]
   [clojure.string :as string]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]
   [clojure-lsp.crawler :as crawler]))

(deftest test-rename
  (reset! db/db {:file-envs
                 {"file://a.clj" (parser/find-usages "(ns a) (def bar ::bar) (def ^:m baz 1)" :clj {})
                  "file://b.clj" (parser/find-usages "(ns b (:require [a :as aa])) (def x aa/bar) ::aa/bar :aa/bar" :clj {})
                  "file://c.clj" (parser/find-usages "(ns c (:require [a :as aa])) (def x aa/bar) ^:xab aa/baz" :clj {})}})
  (testing "on symbol without namespace"
    (let [changes (:changes (handlers/rename "file://a.clj" 1 13 "foo"))]
      (is (= 1 (count (get changes "file://a.clj"))))
      (is (= 1 (count (get changes "file://b.clj"))))
      (is (= "foo" (get-in changes ["file://a.clj" 0 :new-text])))
      (is (= "aa/foo" (get-in changes ["file://b.clj" 0 :new-text])))))
  (testing "on symbol with metadata namespace"
    (let [changes (:changes (handlers/rename "file://a.clj" 1 33 "qux"))]
      (is (= 1 (count (get changes "file://a.clj"))))
      (is (= 1 (count (get changes "file://c.clj"))))
      (is (= "qux" (get-in changes ["file://a.clj" 0 :new-text])))
      (is (= [32 35]
             [(get-in changes ["file://a.clj" 0 :range :start :character])
              (get-in changes ["file://a.clj" 0 :range :end :character])]))
      (is (= "aa/qux" (get-in changes ["file://c.clj" 0 :new-text])))
      (is (= [50 56]
             [(get-in changes ["file://c.clj" 0 :range :start :character])
              (get-in changes ["file://c.clj" 0 :range :end :character])]))))
  (testing "on ::keyword"
    (let [changes (:changes (handlers/rename "file://a.clj" 1 17 "foo"))]
      (is (= 1 (count (get changes "file://a.clj"))))
      (is (= 1 (count (get changes "file://b.clj"))))
      (is (= "::foo" (get-in changes ["file://a.clj" 0 :new-text])))
      (is (= "::aa/foo" (get-in changes ["file://b.clj" 0 :new-text])))))
  (testing "on symbol with namespace adds existing namespace"
    (is (= "foo" (get-in (handlers/rename "file://b.clj" 1 38 "foo")
                         [:changes "file://a.clj" 0 :new-text])))
    (is (= "aa/foo" (get-in (handlers/rename "file://b.clj" 1 38 "foo")
                            [:changes "file://b.clj" 0 :new-text]))))
  (testing "on symbol with namespace removes passed-in namespace"
    (is (= "foo" (get-in (handlers/rename "file://b.clj" 1 38 "aa/foo")
                         [:changes "file://a.clj" 0 :new-text])))
    (is (= "aa/foo" (get-in (handlers/rename "file://b.clj" 1 38 "aa/foo")
                            [:changes "file://b.clj" 0 :new-text]))))
  (testing "on alias changes namespaces inside file"
    (let [changes (:changes (handlers/rename "file://b.clj" 1 25 "xx"))]
      (is (= 0 (count (get changes "file://a.clj"))))
      (is (= 0 (count (get changes "file://c.clj"))))
      (is (= 3 (count (get changes "file://b.clj"))))
      (is (= "xx" (get-in changes ["file://b.clj" 0 :new-text])))
      (is (= "xx/bar" (get-in changes ["file://b.clj" 1 :new-text]))))))

(deftest test-rename-simple-keywords
  (reset! db/db {:file-envs
                 {"file://a.cljc" (parser/find-usages ":a (let [{:keys [a]} {}] a)" :cljc {})}})
  (testing "should not rename plain keywords"
    (let [changes (:changes (handlers/rename "file://a.cljc" 1 1 "b"))]
      (is (= nil changes))))

  (testing "should rename local in destructure not keywords"
    (let [changes (:changes (handlers/rename "file://a.cljc" 1 18 "b"))]
      (is (= [18 26] (mapv (comp inc :character :start :range) (get-in changes ["file://a.cljc"])))))))

(deftest test-find-diagnostics
  (testing "wrong arity"
    (testing "for argument destructuring"
      (reset! db/db {:file-envs {"file://a.clj" (parser/find-usages "(defn foo ([x] x) ([x y] (x y)))
                                                                     (defn bar [y & rest] ((foo y y y) (bar rest)))
                                                                     (defn baz [{x :x y :y :as long}
                                                                                {:keys [k v] :as short}
                                                                                [_ a b]]
                                                                       (x y k v a b long short))
                                                                     (baz :broken :brokken [nil :ok :okay])
                                                                     (baz {bar baz foo :no?})
                                                                     (bar)
                                                                     (bar {:a [:b]})
                                                                     (bar :one-fish :two-fish :red-fish :blue-fish)
                                                                     [foo]
                                                                     {foo 1 2 3}
                                                                     [foo 1 (foo 5 6 7)]
                                                                     (foo)
                                                                     (foo 1)
                                                                     (foo 1 ['a 'b])
                                                                     (foo 1 2 3 {:k 1 :v 2})" :clj {})}})
      (let [usages (crawler/find-diagnostics #{} "file://a.clj" (get-in @db/db [:file-envs "file://a.clj"]))]
        (is (= ["No overload foo for 3 arguments"
                "No overload baz for 1 argument"
                "No overload bar for 0 arguments"
                "No overload foo for 3 arguments"
                "No overload foo for 0 arguments"
                "No overload foo for 4 arguments"]
               (map :message usages)))))
    (testing "for threading macros"
      (reset! db/db {:file-envs {"file://a.clj" (parser/find-usages "(defn foo ([x] x) ([x y z] (z x y)))
                                                                     (defn bar [] :bar)
                                                                     (defn baz [arg & rest] (apply arg rest))
                                                                     (->> :test
                                                                       (foo)
                                                                       (foo 1)
                                                                       (bar))
                                                                     (-> 1
                                                                       (baz)
                                                                       (->> (baz)
                                                                            (foo 1 2))
                                                                       (baz :p :q :r)
                                                                       bar)
                                                                     (cond-> 0
                                                                      int? (bar :a :b)
                                                                      false (foo)
                                                                      :else (baz 3))" :clj {})}})
      (let [usages (crawler/find-diagnostics #{} "file://a.clj" (get-in @db/db [:file-envs "file://a.clj"]))]
        (is (= ["No overload foo for 2 arguments"
                "No overload bar for 1 argument"
                "No overload bar for 1 argument"
                "No overload bar for 3 arguments"]
               (map :message usages)))))
    (testing "with annotations"
      (reset! db/db {:file-envs {"file://a.clj" (parser/find-usages "(defn foo {:added \"1.0\"} [x] (inc x))
                                                                     (defn ^:private bar ^String [^Class x & rest] (str x rest))
                                                                     (foo foo)
                                                                     (foo foo foo)
                                                                     (bar :a)
                                                                     (bar :a :b)" :clj {})}})
      (let [usages (crawler/find-diagnostics #{} "file://a.clj" (get-in @db/db [:file-envs "file://a.clj"]))]
        (is (= ["No overload foo for 2 arguments"]
               (map :message usages)))))
    (testing "for meta arglists"
      (reset! db/db {:file-envs {"file://a.clj" (parser/find-usages "(def
                                                                       ^{:doc \"Don't use this\"
                                                                         :arglists '([z])
                                                                         :added \"1.17\"
                                                                         :static true}
                                                                       foo nil)
                                                                     (foo)
                                                                     (foo (foo :a :b))" :clj {})}})
      (let [usages (crawler/find-diagnostics #{} "file://a.clj" (get-in @db/db [:file-envs "file://a.clj"]))]
        (is (= ["No overload foo for 0 arguments"
                "No overload foo for 2 arguments"]
               (map :message usages)))))
    (testing "for schema defs"
      (reset! db/db {:file-envs {"file://a.clj" (parser/find-usages "(ns user (:require [schema.core :as s]))
                                                                     (s/defn foo :- s/Str
                                                                       [x :- Long y :- Long]
                                                                       (str x y))
                                                                     (foo)
                                                                     (foo 1 2)
                                                                     (foo 1)" :clj {})}})
      (let [usages (crawler/find-diagnostics #{} "file://a.clj" (get-in @db/db [:file-envs "file://a.clj"]))]
        (is (= ["Unused namespace: user"
                "No overload foo for 0 arguments"
                "No overload foo for 1 argument"]
               (map :message usages)))))
    (testing "for ignore-arity? macros"
      (let [usages  (parser/find-usages "(ns user (:require [schema.core :as s])) (s/defn foo [x :- int?] x) (foo 1 2 3)" :clj
                                        {'schema.core/defn [{:element :declaration
                                                             :signature [{:pred :keyword} {:pred :follows-constant :constant :-} {:pred :string} {:pred :map}]
                                                             :signature-style :typed
                                                             :ignore-arity? true}
                                                            {:element :sub-elements
                                                             :match-patterns [[:any :keyword :any] [:param :element :element]
                                                                              [:any] [:param]]}
                                                            :bound-elements]})
            _ (reset! db/db {:file-envs {"file://a.clj" usages}})
            diagnostics (crawler/find-diagnostics #{} "file://a.clj" (get-in @db/db [:file-envs "file://a.clj"]))]
        (is (= [] (mapv :message (drop 1 diagnostics)))))))
  (testing "unused symbols"
    (reset! db/db {:file-envs
                   {"file://a.clj" (parser/find-usages "(ns a) (def bar ::bar)" :clj {})
                    "file://b.clj" (parser/find-usages "(ns b
                                                          (:require [a :as a]
                                                            [c :as c]
                                                            [d :as d-alias]
                                                            [e :as e-alias]
                                                            [clojure.spec.alpha :as s]
                                                            [schema.core :as sc]))
                                                        (s/fdef wat)
                                                        (sc/defn over :- s/Int
                                                          ([a :- s/Int] a)
                                                          ([a :- s/Int b :- s/Int] (+ a b)))
                                                        (over 1)
                                                        (over 2 :a)
                                                        (def x a/bar)
                                                        (declare y)
                                                        (defn y [])
                                                        :a/bar
                                                        (let [{:keys [::d-alias/bar] ::e-alias/keys [foo] ::f/keys [qux]} {}]
                                                          [bar qux foo])" :clj {})}})
    (let [usages (crawler/find-diagnostics #{} "file://b.clj" (get-in @db/db [:file-envs "file://b.clj"]))]
      (is (= ["Unknown namespace: f"
              "Unused alias: c"
              "Unused namespace: b"
              "Unused declaration: x"
              "Unused declaration: y"
              "Unknown forward declaration: wat"]
             (map :message usages))))))

(deftest test-completion
  (let [db-state {:file-envs
                  {"file://a.cljc" (parser/find-usages
                                     (str "(ns alpaca.ns (:require [user :as alpaca]))\n"
                                          "(def barr)\n"
                                          "(def bazz)")
                                     :clj
                                     {})
                   "file://b.clj" (parser/find-usages
                                    (str "(ns user)\n"
                                         "(def alpha)\n"
                                         "alp\n"
                                         "ba")
                                    :clj
                                    {})
                   "file://c.cljs" (parser/find-usages
                                    (str "(ns alpaca.ns)\n"
                                         "(def baff)\n")
                                    :cljs
                                    {})
                   "file://d.clj" (parser/find-usages
                                    (str "(ns d (:require [alpaca.ns :as alpaca]))")
                                    :clj
                                    {})}}]
    (testing "complete-a"
      (reset! db/db db-state)
      (is (= [{:label "alpha" :data "user/alpha"}
              {:label "alpaca" :detail "user"}
              {:label "alpaca" :detail "alpaca.ns"}
              {:label "alpaca.ns" :detail "alpaca.ns"}]
             (handlers/completion "file://b.clj" 3 18))))
    (testing "complete-ba"
      (reset! db/db db-state)
			(is (= [{:label "bases" :data "clojure.core/bases"}]
             (handlers/completion "file://b.clj" 4 3))))
    (testing "complete-alph"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'user/alph :str "alph"}))
      (is (= [{:label "alpha" :data "user/alpha"}]
             (handlers/completion "file://b.clj" 3 3))))
    (testing "complete-alpaca"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'alpaca :str "alpaca"}))
      (is (= [{:label "alpaca" :detail "user"}
              {:label "alpaca" :detail "alpaca.ns"}
              {:label "alpaca.ns" :detail "alpaca.ns"}
              {:label "alpaca/barr" :detail "alpaca.ns" :data "alpaca.ns/barr"}
              {:label "alpaca/bazz" :detail "alpaca.ns" :data "alpaca.ns/bazz"}]
             (handlers/completion "file://b.clj" 3 18))))
    (testing "complete-core-stuff"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'freq :str "freq"}))
      (is (= [{:label "frequencies" :data "clojure.core/frequencies"}]
             (handlers/completion "file://b.clj" 3 18)))
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'Sys :str "Sys"}))
      (is (= [{:label "System" :data "java.lang.System"}]
             (handlers/completion "file://b.clj" 3 18))))
    (testing "resolving completion item"
      (reset! db/db db-state)
      (let [{:keys [label data documentation]} (handlers/resolve-completion-item "alpha" "user/alpha")]
        (and (is (= label "alpha"))
             (is (= data "user/alpha"))
             (is (string/includes? documentation data)))))))

(deftest test-range-formatting
    (reset! db/db {:documents {"file://a.clj" {:text "(a  )\n(b c d)"}}})
  (is (= [{:range {:start {:line 0 :character 0}
                   :end {:line 0 :character 5}}
           :new-text "(a)"}]
         (handlers/range-formatting "file://a.clj" {:row 1 :col 1 :end-row 1 :end-col 4}))))
