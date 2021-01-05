(ns clojure-lsp.handlers-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.parser :as parser]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]])
  (:import
   (org.eclipse.lsp4j
     Diagnostic
     DiagnosticSeverity
     Range
     Position)))

(deftest did-close
  (reset! db/db {:documents {"file://a.clj" {:text "(ns a)"}
                             "file://b.clj" {:text "(ns b)"}}
                 :file-envs {"file://a.clj" (parser/find-usages "(ns a)" :clj {})
                             "file://b.clj" (parser/find-usages "(ns b)" :clj {})}})
  (testing "should remove references to file"
    (handlers/did-close "file://a.clj")
    (is (= {:documents {"file://b.clj" {:text "(ns b)"}}
            :file-envs {"file://a.clj" (parser/find-usages "(ns a)" :clj {})
                        "file://b.clj" (parser/find-usages "(ns b)" :clj {})}}
           @db/db))))

(deftest hover
  (testing "with show-docs-arity-on-same-line? disabled"
    (testing "plain text without docs"
      (reset! db/db {:file-envs {"file://a.clj" (parser/find-usages (str "(ns a)\n"
                                                                         "(defn foo [x] x)\n"
                                                                         "(foo 1)") :clj {})}})
      (is (= {:range    {:start {:line      2
                                 :character 1}
                         :end   {:line 2 :character 4}}
              :contents ["a/foo \n[x]\nfile://a.clj\n"]}
             (handlers/hover "file://a.clj" 3 3))))
    (testing "plain text with docs"
      (reset! db/db {:file-envs {"file://a.clj" (parser/find-usages (str "(ns a)\n"
                                                                         "(defn foo \"Some cool docs :foo\" [x] x)\n"
                                                                         "(foo 1)") :clj {})}})
      (is (= {:range    {:start {:line      2
                                 :character 1}
                         :end   {:line 2 :character 4}}
              :contents ["a/foo \n[x]\nfile://a.clj\n\n----\nSome cool docs :foo\n"]}
             (handlers/hover "file://a.clj" 3 3))))
    (testing "markdown content with function args"
      (reset! db/db {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}
                     :file-envs           {"file://a.clj" (parser/find-usages (str "(ns a)\n"
                                                                                   "(defn foo [x] x)\n"
                                                                                   "(foo 1)") :clj {})}})
      (is (= {:range    {:start {:line 2 :character 1}
                         :end   {:line 2 :character 4}}
              :contents {:kind  "markdown"
                         :value "```clojure\na/foo \n```\n```clojure\n[x]\n```\n*file://a.clj*\n"}}
             (handlers/hover "file://a.clj" 3 3))))
    (testing "markdown content with no function args"
      (reset! db/db {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}
                     :file-envs           {"file://a.clj" (parser/find-usages (str "(ns a)\n"
                                                                                   "(defn foo [] 1)\n"
                                                                                   "(foo)") :clj {})}})
      (is (= {:range    {:start {:line 2 :character 1}
                         :end   {:line 2 :character 4}}
              :contents {:kind  "markdown"
                         :value "```clojure\na/foo \n```\n```clojure\n[]\n```\n*file://a.clj*\n"}}
             (handlers/hover "file://a.clj" 3 2)))))
  (testing "with show-docs-arity-on-same-line? enabled"
    (testing "plain text"
      (reset! db/db {:settings  {:show-docs-arity-on-same-line? true}
                     :file-envs {"file://a.clj" (parser/find-usages (str "(ns a)\n"
                                                                         "(defn foo [x] x)\n"
                                                                         "(foo 1)") :clj {})}})
      (is (= {:range    {:start {:line      2
                                 :character 1}
                         :end   {:line 2 :character 4}}
              :contents ["a/foo [x]\nfile://a.clj\n"]}
             (handlers/hover "file://a.clj" 3 3))))
    (testing "markdown content with function args"
      (reset! db/db {:settings            {:show-docs-arity-on-same-line? true}
                     :client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}
                     :file-envs           {"file://a.clj" (parser/find-usages (str "(ns a)\n"
                                                                                   "(defn foo [x] x)\n"
                                                                                   "(foo 1)") :clj {})}})
      (is (= {:range    {:start {:line 2 :character 1}
                         :end   {:line 2 :character 4}}
              :contents {:kind  "markdown"
                         :value "```clojure\na/foo [x]\n```\n*file://a.clj*\n"}}
             (handlers/hover "file://a.clj" 3 3))))
    (testing "markdown content with no function args"
      (reset! db/db {:settings            {:show-docs-arity-on-same-line? true}
                     :client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}
                     :file-envs           {"file://a.clj" (parser/find-usages (str "(ns a)\n"
                                                                                   "(defn foo [] 1)\n"
                                                                                   "(foo)") :clj {})}})
      (is (= {:range    {:start {:line 2 :character 1}
                         :end   {:line 2 :character 4}}
              :contents {:kind  "markdown"
                         :value "```clojure\na/foo []\n```\n*file://a.clj*\n"}}
             (handlers/hover "file://a.clj" 3 2))))
    (testing "markdown content with docs"
      (reset! db/db {:settings            {:show-docs-arity-on-same-line? true}
                     :client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}
                     :file-envs           {"file://a.clj" (parser/find-usages (str "(ns a)\n"
                                                                                   "(defn foo \"Some cool docstring :foo :bar\" [] 1)\n"
                                                                                   "(foo)") :clj {})}})
      (is (= {:range    {:start {:line 2 :character 1}
                         :end   {:line 2 :character 4}}
              :contents {:kind  "markdown"
                         :value "```clojure\na/foo []\n```\n*file://a.clj*\n\n----\n```clojure\nSome cool docstring :foo :bar\n```\n"}}
             (handlers/hover "file://a.clj" 3 2))))))

(deftest document-symbol
  (reset! db/db {:file-envs
                 {"file://a.clj" (parser/find-usages "(ns a) (def bar ::bar) (def ^:m baz 1)" :clj {})}})
  (is (= 1
         (count (handlers/document-symbol "file://a.clj")))))

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
      (is (= "xx/bar" (get-in changes ["file://b.clj" 1 :new-text])))))
  (testing "on a namespace"
    (reset! db/db {:project-root "file:///my-project"
                   :settings {:source-paths #{"src" "test"}}
                   :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                   :file-envs {"file:///my-project/src/foo/bar_baz.clj" (parser/find-usages "(ns foo.bar-baz)" :clj {})}})
    (is (= {:document-changes
            [{:text-document {:version 0
                              :uri "file:///my-project/src/foo/bar_baz.clj"}
              :edits [{:range
                       {:start {:line 0 :character 4}
                        :end {:line 0 :character 15}}
                       :new-text "foo.baz-qux"
                       :text-document {:version 0
                                       :uri "file:///my-project/src/foo/bar_baz.clj"}}]}
             {:kind "rename"
              :old-uri "file:///my-project/src/foo/bar_baz.clj"
              :new-uri "file:///my-project/src/foo/baz_qux.clj"}]}
           (handlers/rename "file:///my-project/src/foo/bar_baz.clj" 1 5 "foo.baz-qux")))
    (is (empty? (get @db/db :file-envs)))))

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
      (let [code "(defn foo ([x] x) ([x y] (x y)))
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
                  (foo 1 2 3 {:k 1 :v 2})"]
        (reset! db/db {:file-envs {"file://a.clj" (parser/find-usages code :clj {})} :project-root "file:///"})
        (let [usages (f.diagnostic/find-diagnostics "file://a.clj" code (get-in @db/db [:file-envs "file://a.clj"]) #{})]
          (is (= ["user/foo is called with 3 args but expects 1 or 2"
                  "user/baz is called with 1 arg but expects 3"
                  "user/bar is called with 0 args but expects 1 or more"
                  "user/foo is called with 3 args but expects 1 or 2"
                  "user/foo is called with 0 args but expects 1 or 2"
                  "user/foo is called with 4 args but expects 1 or 2"]
                 (map :message usages))))))
    (testing "for threading macros"
      (let [code "(defn foo ([x] x) ([x y z] (z x y)))
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
                    :else (baz 3))
                  (doto 1
                    (foo)
                    (foo 1)
                    (bar))"]
        (reset! db/db {:file-envs {"file://a.clj" (parser/find-usages code :clj {})} :project-root "file:///"})
        (let [usages (f.diagnostic/find-diagnostics "file://a.clj" code (get-in @db/db [:file-envs "file://a.clj"]) #{})]
          (is (= ["user/foo is called with 2 args but expects 1 or 3"
                  "user/bar is called with 1 arg but expects 0"
                  "user/bar is called with 1 arg but expects 0"
                  "user/bar is called with 3 args but expects 0"
                  "user/foo is called with 2 args but expects 1 or 3"
                  "user/bar is called with 1 arg but expects 0"]
                 (map :message usages))))))
    (testing "with annotations"
      (let [code "(defn foo {:added \"1.0\"} [x] (inc x))
                  (defn ^:private bar ^String [^Class x & rest] (str x rest))
                  (foo foo)
                  (foo foo foo)
                  (bar :a)
                  (bar :a :b)"]
        (reset! db/db {:file-envs {"file://a.clj" (parser/find-usages code :clj {})} :project-root "file:///"})
        (let [usages (f.diagnostic/find-diagnostics "file://a.clj" code (get-in @db/db [:file-envs "file://a.clj"]) #{})]
          (is (= ["user/foo is called with 2 args but expects 1"]
                 (map :message usages))))))
    ;; Waiting for kondo implement this support: https://github.com/borkdude/clj-kondo/issues/912
    #_(testing "for meta arglists"
      (let [code "(def
                    ^{:doc \"Don't use this\"
                      :arglists '([z])
                      :added \"1.17\"
                      :static true}
                    foo nil)
                  (foo)
                  (foo (foo :a :b))"]
        (reset! db/db {:file-envs {"file://a.clj" (parser/find-usages code :clj {})} :project-root "file:///"})
        (let [usages (f.diagnostic/find-diagnostics "file://a.clj" code (get-in @db/db [:file-envs "file://a.clj"]) #{})]
          (is (= ["No overload for 'foo' with 0 arguments"
                  "No overload for 'foo' with 2 arguments"]
                 (map :message usages))))))
    (testing "for schema defs"
      (let [code "(ns user (:require [schema.core :as s]))
                  (s/defn foo :- s/Str
                    [x :- Long y :- Long]
                    (str x y))
                  (foo)
                  (foo 1 2)
                  (foo 1)"]
        (reset! db/db {:file-envs {"file://a.clj" (parser/find-usages code :clj {})} :project-root "file:///"})
        (let [usages (f.diagnostic/find-diagnostics "file://a.clj" code (get-in @db/db [:file-envs "file://a.clj"]) #{})]
          (is (= ["Unused namespace: user"
                  "user/foo is called with 0 args but expects 2"
                  "user/foo is called with 1 arg but expects 2"]
                 (map :message usages)))))))
  (testing "unused symbols"
    (let [code-b "(ns b
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
                    [bar qux foo])"]
      (reset! db/db {:file-envs
                     {"file://a.clj" (parser/find-usages "(ns a) (def bar ::bar)" :clj {})
                      "file://b.clj" (parser/find-usages code-b :clj {})}
                    :project-root "file:///"})
      (let [usages (f.diagnostic/find-diagnostics "file://b.clj" code-b (get-in @db/db [:file-envs "file://b.clj"]) #{})]
        (is (= ["Unused namespace: b"
                "Unused declaration: x"
                "Unused declaration: y"
                "Unknown forward declaration: wat"
                "namespace c is required but never used"
                "Unresolved namespace f. Are you missing a require?"]
               (map :message usages))))))
  (testing "custom unused namespace declaration"
    (let [code "(ns foo.bar)"]
      (reset! db/db {:file-envs {"file://foo/bar.clj" (parser/find-usages code :clj {})}
                     :project-root "file:///"})
      (let [usages (f.diagnostic/find-diagnostics "file://foo/bar.clj" code (get-in @db/db [:file-envs "file://foo/bar.clj"]) #{"foo"})]
        (is (empty?
               (map :message usages)))))))

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
                                    {})
                   "file://e.clj" (parser/find-usages
                                    (str "(ns e (:require [alpaca.ns :refer [ba]]))")
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
    (testing "complete-within-refering"
      (reset! db/db db-state)
      (is (= [{:label "barr" :detail "alpaca.ns/barr" :data "alpaca.ns/barr"}
              {:label "bazz" :detail "alpaca.ns/bazz" :data "alpaca.ns/bazz"}]
             (handlers/completion "file://e.clj" 1 38))))
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

(deftest test-formatting
  (reset! db/db {:documents {"file://a.clj" {:text "(a  )\n(b c d)"}}})
  (is (= "(a)\n(b c d)"
         (:new-text (first (handlers/formatting "file://a.clj"))))))

(deftest test-formatting-noop
  (reset! db/db {:documents {"file://a.clj" {:text "(a)\n(b c d)"}}})
  (let [r (handlers/formatting "file://a.clj")]
    (is (empty? r))
    (is (vector? r))))

(deftest test-range-formatting
  (reset! db/db {:documents {"file://a.clj" {:text "(a  )\n(b c d)"}}})
  (is (= [{:range {:start {:line 0 :character 0}
                   :end {:line 0 :character 5}}
           :new-text "(a)"}]
         (handlers/range-formatting "file://a.clj" {:row 1 :col 1 :end-row 1 :end-col 4}))))

(deftest test-code-actions
  (let [references-code   (str "(ns some-ns)\n"
                      "(def foo)")
        b-code   (str "(ns other-ns (:require [some-ns :as sns]))\n"
                      "(def bar 1)\n"
                      "(defn baz []\n"
                      "  bar)")
        c-code   (str "(ns another-ns)\n"
                      "(def bar ons/bar)\n"
                      "(def foo sns/foo)\n"
                      "(deftest some-test)")
        db-state {:documents {"file://a.clj" {:text references-code}
                              "file://b.clj" {:text b-code}
                              "file://c.clj" {:text c-code}}
                  :file-envs {"file://a.clj" (parser/find-usages references-code :clj {})
                              "file://b.clj" (parser/find-usages b-code :clj {})
                              "file://c.clj" (parser/find-usages c-code :clj {})}}]
    (testing "Add missing namespace"
      (testing "when it has not unresolved-namespace diagnostic"
        (reset! db/db db-state)
        (is (not-any? #(string/starts-with? (:title %) "Add missing")
                      (handlers/code-actions "file://c.clj" [] 1 9))))

      (testing "when it has unresolved-namespace but cannot find namespace"
        (reset! db/db db-state)
        (let [unknown-ns-diagnostic (Diagnostic. (Range. (Position. 1 10) (Position. 1 16)) "Unknown namespace" DiagnosticSeverity/Error "some source" "unresolved-namespace")]
          (is (not-any? #(string/starts-with? (:title %) "Add missing")
                        (handlers/code-actions "file://c.clj" [unknown-ns-diagnostic] 1 10)))))

      (testing "when it has unresolved-namespace and can find namespace"
        (reset! db/db db-state)
        (let [unknown-ns-diagnostic (Diagnostic. (Range. (Position. 2 10) (Position. 2 16)) "Unknown namespace" DiagnosticSeverity/Error "some source" "unresolved-namespace")]
          (is (some #(= (:title %) "Add missing 'some-ns' namespace")
                    (handlers/code-actions "file://c.clj" [unknown-ns-diagnostic] 2 10)))))
      (testing "when it has unresolved-symbol and it's a known refer"
        (reset! db/db db-state)
        (let [unknown-symbol-diagnostic (Diagnostic. (Range. (Position. 3 1) (Position. 3 8)) "Unresolved symbol deftest" DiagnosticSeverity/Error "some source" "unresolved-symbol")]
          (is (some #(= (:title %) "Add missing 'clojure.test' namespace")
                    (handlers/code-actions "file://c.clj" [unknown-symbol-diagnostic] 3 1)))))
      (testing "when it has unresolved-symbol but it's not a known refer"
        (reset! db/db db-state)
        (let [unknown-symbol-diagnostic (Diagnostic. (Range. (Position. 3 10) (Position. 3 18)) "Unresolved symbol some-test" DiagnosticSeverity/Error "some source" "unresolved-symbol")]
          (is (not-any? #(string/starts-with? (:title %) "Add missing")
                        (handlers/code-actions "file://c.clj" [unknown-symbol-diagnostic] 3 10))))))
    (testing "Inline symbol"
      (testing "when in not a let/def symbol"
        (reset! db/db db-state)
        (is (not-any? #(= (:title %) "Inline symbol")
                      (handlers/code-actions "file://b.clj" [] 3 7))))
      (testing "when in let/def symbol"
        (reset! db/db db-state)
        (is (some #(= (:title %) "Inline symbol")
                  (handlers/code-actions "file://b.clj" [] 3 4)))))
    (testing "Cycle privacy"
      (testing "when non function location"
        (reset! db/db db-state)
        (is (not-any? #(= (:title %) "Cycle privacy")
                      (handlers/code-actions "file://a.clj" [] 0 4))))
      (testing "when on function location"
        (reset! db/db db-state)
        (is (some #(= (:title %) "Cycle privacy")
                  (handlers/code-actions "file://a.clj" [] 1 4)))))
    (testing "Extract function"
      (testing "when non function location"
        (reset! db/db db-state)
        (is (not-any? #(= (:title %) "Extract function")
                      (handlers/code-actions "file://a.clj" [] 0 4))))
      (testing "when on function location"
        (reset! db/db db-state)
        (is (some #(= (:title %) "Extract function")
                  (handlers/code-actions "file://a.clj" [] 1 4)))))
    (testing "clean namespace"
      (testing "without workspace edit client capability"
        (reset! db/db db-state)
        (is (not-any? #(= (:title %) "Clean namespace")
                      (handlers/code-actions "file://b.clj" [] 1 1))))

      (testing "with workspace edit client capability"
        (reset! db/db (assoc-in db-state [:client-capabilities :workspace :workspace-edit] true))
        (is (some #(= (:title %) "Clean namespace")
                  (handlers/code-actions "file://b.clj" [] 1 1)))))))

(deftest test-code-lens
  (let [references-code (str "(ns some-ns)\n"
                             "(def foo 1)\n"
                             "(defn- foo2 []\n"
                             " foo)\n"
                             "(defn bar [a b]\n"
                             "  (+ a b (foo2)))\n"
                             "(s/defn baz []\n"
                             "  (bar 2 3))\n")
        db-state        {:file-envs {"file://a.clj" (parser/find-usages references-code :clj {})}}]
    (testing "references lens"
      (reset! db/db db-state)
      (is (= '({:range
                {:start {:line 0 :character 4} :end {:line 0 :character 11}}
                :data ["file://a.clj" 1 5]}
               {:range
                {:start {:line 1 :character 5} :end {:line 1 :character 8}}
                :data ["file://a.clj" 2 6]}
               {:range
                {:start {:line 2 :character 7} :end {:line 2 :character 11}}
                :data ["file://a.clj" 3 8]}
               {:range
                {:start {:line 4 :character 6} :end {:line 4 :character 9}}
                :data ["file://a.clj" 5 7]})
             (handlers/code-lens "file://a.clj"))))))

(deftest test-code-lens-resolve
  (let [references-code (str "(ns some-ns)\n"
                             "(def foo 1)\n"
                             "(defn- foo2 []\n"
                             " foo)\n"
                             "(defn bar [a b]\n"
                             "  (+ a b (foo2)))\n"
                             "(s/defn baz []\n"
                             "  (bar 2 3))\n")
        db-state        {:file-envs {"file://a.clj" (parser/find-usages references-code :clj {})}}]
    (testing "references"
      (testing "empty lens"
        (reset! db/db db-state)
        (is (= {:range   {:start {:line      1
                                  :character 5}
                          :end   {:line      1
                                  :character 12}}
                :command {:title   "0 references"
                          :command "code-lens-references"
                          :arguments ["file://a.clj" 1 5]}}
               (handlers/code-lens-resolve {:start {:line 1 :character 5} :end {:line 1 :character 12}}
                                           ["file://a.clj" 1 5]))))
      (testing "some lens"
        (reset! db/db db-state)
        (is (= {:range   {:start {:line      2
                                  :character 7}
                          :end   {:line      2
                                  :character 11}}
                :command {:title   "1 references"
                          :command "code-lens-references"
                          :arguments ["file://a.clj" 3 8]}}
               (handlers/code-lens-resolve {:start {:line 2 :character 7} :end {:line 2 :character 11}}
                                           ["file://a.clj" 3 8])))))))
