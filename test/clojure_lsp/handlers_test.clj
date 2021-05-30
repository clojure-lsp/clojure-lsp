(ns clojure-lsp.handlers-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.handlers :as handlers]
    [clojure-lsp.test-helper :as h]
    [clojure.core.async :as async]
    [clojure.string :as string]
    [clojure.test :refer [deftest is testing]]
    [taoensso.timbre :as log]))

(h/reset-db-after-test)

(defn code [& strings] (clojure.string/join "\n" strings))

(deftest did-open
  (reset! db/db {})
  (testing "opening a existing file"
    (let [_ (h/load-code-and-locs "(ns a) (when)")
          diagnostics (h/diagnostics-or-timeout)]
      (is (some? (get-in @db/db [:analysis (h/file-path "/a.clj")])))
      (h/assert-submaps
        [{:code "missing-body-in-when"}
         {:code "invalid-arity"}]
        diagnostics)))
  (testing "opening a new file adding the ns"
    (swap! db/db merge {:settings {:auto-add-ns-to-new-files? true
                                   :source-paths #{(h/file-path "/project/src")}}
                        :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                        :project-root-uri (h/file-uri "file:///project")})
    (alter-var-root #'db/edits-chan (constantly (async/chan 1)))
    (let [_ (h/load-code-and-locs "" (h/file-uri "file:///project/src/foo/bar.clj"))
          changes (:document-changes (h/edits-or-timeout))]
      (h/assert-submaps
        [{:edits [{:range {:start {:line 0, :character 0}
                           :end {:line 0, :character 0}}
                   :new-text "(ns foo.bar)"}]}]
        changes)
      (is (some? (get-in @db/db [:analysis (h/file-path "/project/src/foo/bar.clj")]))))))

(deftest document-symbol
  (let [code "(ns a) (def bar ::bar) (def ^:m baz 1)"
        result [{:name "a"
                 :kind :namespace
                 :range {:start {:line 0 :character 0} :end {:line 999999 :character 999999}}
                 :selection-range {:start {:line 0 :character 0} :end {:line 0 :character 5}}
                 :children [{:name "bar"
                             :kind :variable
                             :range {:start {:line 0 :character 7} :end {:line 0 :character 22}}
                             :selection-range {:start {:line 0 :character 12} :end {:line 0 :character 15}}}
                            {:name "baz"
                             :kind :variable
                             :range {:start {:line 0 :character 23} :end {:line 0 :character 38}}
                             :selection-range {:start {:line 0 :character 32} :end {:line 0 :character 35}}}]}]]
    (testing "clj files"
      (h/load-code-and-locs code)
      (h/assert-submaps result
                        (handlers/document-symbol {:textDocument (h/file-uri "file:///a.clj")})))
    (testing "cljs files"
      (h/load-code-and-locs code (h/file-uri "file:///b.cljs"))
      (h/assert-submaps result
                        (handlers/document-symbol {:textDocument (h/file-uri "file:///b.cljs")})))
    (testing "cljc files"
      (h/load-code-and-locs code (h/file-uri "file:///c.cljc"))
      (h/assert-submaps result
                        (handlers/document-symbol {:textDocument (h/file-uri "file:///c.cljc")})))))

(deftest document-highlight
  (let [[bar-start] (h/load-code-and-locs "(ns a) (def |bar ::bar) (def ^:m baz 1)")]
    (h/assert-submaps
      [{:range {:start {:line 0 :character 12} :end {:line 0 :character 15}}}]
      (handlers/document-highlight {:textDocument (h/file-uri "file:///a.clj")
                                    :position (h/->position bar-start)}))))

(deftest references
  (testing "simple single reference"
    (let [[bar-def-pos] (h/load-code-and-locs "(ns a) (def |bar 1)")
          _ (h/load-code-and-locs "(ns b (:require [a :as foo])) (foo/bar)" (h/file-uri "file:///b.clj"))]
      (h/assert-submaps
        [{:uri (h/file-uri "file:///b.clj")
          :range {:start {:line 0 :character 31} :end {:line 0 :character 38}}}]
        (handlers/references {:textDocument (h/file-uri "file:///a.clj")
                              :position (h/->position bar-def-pos)}))))
  (testing "when including declaration"
    (let [[bar-def-pos] (h/load-code-and-locs "(ns a) (def |bar 1)")
          _ (h/load-code-and-locs "(ns b (:require [a :as foo])) (foo/bar)" (h/file-uri "file:///b.clj"))]
      (h/assert-submaps
        [{:uri (h/file-uri "file:///a.clj")
          :range {:start {:line 0 :character 12} :end {:line 0 :character 15}}}
         {:uri (h/file-uri "file:///b.clj")
          :range {:start {:line 0 :character 31} :end {:line 0 :character 38}}}]
        (handlers/references {:textDocument (h/file-uri "file:///a.clj")
                              :position (h/->position bar-def-pos)
                              :context {:includeDeclaration true}})))))

(deftest test-rename
  (let [[abar-start abar-stop
         akw-start akwbar-start akwbar-stop
         abaz-start abaz-stop] (h/load-code-and-locs (code "(ns a.aa)"
                                                           "(def |bar| |::|bar|)"
                                                           "(def ^:m |baz| 1)") (h/file-uri "file:///a.clj"))
        [balias-start balias-stop
         ba1-start _ba1-stop
         bbar-start bbar-stop
         ba2-kw-start ba2-kw-stop] (h/load-code-and-locs (code "(ns b.bb (:require [a.aa :as |aa|]))"
                                                               "(def x |aa|/|bar|)"
                                                               "|::aa/bar|"
                                                               ":aa/bar") (h/file-uri "file:///b.clj"))
        [cbar-start cbar-stop
         cbaz-start cbaz-stop] (h/load-code-and-locs (code "(ns c.cc (:require [a.aa :as aa]))"
                                                           "(def x aa/|bar|)"
                                                           "^:xab aa/|baz|") (h/file-uri "file:///c.clj"))
        [d-name-kw-start d-name-kw-stop] (h/load-code-and-locs (code "(ns d.dd)"
                                                                     "(def name |::name|)") (h/file-uri "file:///d.clj"))
        [kw-aliased-start kw-aliased-stop
         kw-unaliased-start kw-unaliased-stop] (h/load-code-and-locs (code "(ns e.ee (:require [d.dd :as dd]))"
                                                                           "(def name |::dd/name|)"
                                                                           "(def other-name |:d.dd/name|)") (h/file-uri "file:///e.clj"))
        [main-uname-kw-start main-uname-kw-end] (h/load-code-and-locs (code "(ns main (:require [user :as u]))"
                                                                            "(def name |::u/name|)") (h/file-uri "file:///main.cljc"))
        [uname-kw-start uname-kw-end] (h/load-code-and-locs (code "(ns user)"
                                                                  "(def name |::name|)") (h/file-uri "file:///user.cljc"))]
    (testing "on symbol without namespace"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///a.clj")
                                                :position (h/->position abar-start)
                                                :newName "foo"}))]
        (is (= {(h/file-uri "file:///a.clj") [{:new-text "foo" :range (h/->range abar-start abar-stop)}]
                (h/file-uri "file:///b.clj") [{:new-text "foo" :range (h/->range bbar-start bbar-stop)}]
                (h/file-uri "file:///c.clj") [{:new-text "foo" :range (h/->range cbar-start cbar-stop)}]}
               changes))))
    (testing "on symbol with metadata namespace"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///a.clj")
                                                :position (h/->position abaz-start)
                                                :newName "qux"}))]
        (is (= {(h/file-uri "file:///a.clj") [{:new-text "qux" :range (h/->range abaz-start abaz-stop)}]
                (h/file-uri "file:///c.clj") [{:new-text "qux" :range (h/->range cbaz-start cbaz-stop)}]}
               changes))))
    (testing "on symbol with namespace adds existing namespace"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///b.clj")
                                                :position (h/->position [(first bbar-start) (dec (second bbar-start))])
                                                :newName "foo"}))]
        (is (= {(h/file-uri "file:///a.clj") [{:new-text "foo" :range (h/->range abar-start abar-stop)}]
                (h/file-uri "file:///b.clj") [{:new-text "foo" :range (h/->range bbar-start bbar-stop)}]
                (h/file-uri "file:///c.clj") [{:new-text "foo" :range (h/->range cbar-start cbar-stop)}]}
               changes))))
    (testing "on symbol with namespace removes passed-in namespace"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///b.clj")
                                                :position (h/->position bbar-start)
                                                :newName "aa/foo"}))]
        (is (= {(h/file-uri "file:///a.clj") [{:new-text "foo" :range (h/->range abar-start abar-stop)}]
                (h/file-uri "file:///b.clj") [{:new-text "foo" :range (h/->range bbar-start bbar-stop)}]
                (h/file-uri "file:///c.clj") [{:new-text "foo" :range (h/->range cbar-start cbar-stop)}]}
               changes))))
    (testing "on ::keyword"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///a.clj")
                                                :position (h/->position akwbar-start)
                                                :newName "::foo"}))]
        (is (= {(h/file-uri "file:///a.clj") [{:new-text "::foo" :range (h/->range akw-start akwbar-stop)}]
                (h/file-uri "file:///b.clj") [{:new-text "::aa/foo" :range (h/->range ba2-kw-start ba2-kw-stop)}]}
               changes))))
    (testing "on single-name-namespace'd keyword"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///main.cljc")
                                                :position (h/->position main-uname-kw-start)
                                                :newName "::full-name"}))]
        (is (= {(h/file-uri "file:///main.cljc") [{:new-text "::u/full-name" :range (h/->range main-uname-kw-start main-uname-kw-end)}]
                (h/file-uri "file:///user.cljc") [{:new-text "::full-name" :range (h/->range uname-kw-start uname-kw-end)}]}
               changes))))
    (testing "on qualified keyword without alias"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///d.clj")
                                                :position (h/->position d-name-kw-start)
                                                :newName "::other-name"}))]
        (is (= {(h/file-uri "file:///d.clj") [{:new-text "::other-name" :range (h/->range d-name-kw-start d-name-kw-stop)}]
                (h/file-uri "file:///e.clj") [{:new-text "::dd/other-name" :range (h/->range kw-aliased-start kw-aliased-stop)}
                                              {:new-text ":d.dd/other-name" :range (h/->range kw-unaliased-start kw-unaliased-stop)}]}
               changes))))
    (testing "on alias changes namespaces inside file"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///b.clj")
                                                :position (h/->position balias-start)
                                                :newName "xx"}))]
        (is (= {(h/file-uri "file:///b.clj") [{:new-text "xx" :range (h/->range balias-start balias-stop)}
                                              {:new-text "xx/bar" :range (h/->range ba1-start bbar-stop)}
                                              {:new-text "::xx/bar" :range (h/->range ba2-kw-start ba2-kw-stop)}]}
               changes))))
    (testing "on a namespace"
      (reset! db/db {:project-root-uri (h/file-uri "file:///my-project")
                     :settings {:source-paths #{(h/file-path "/my-project/src") (h/file-path "/my-project/test")}}
                     :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
      (h/load-code-and-locs "(ns foo.bar-baz)" (h/file-uri "file:///my-project/src/foo/bar_baz.clj"))
      (is (= {:document-changes
              [{:text-document {:version 0
                                :uri (h/file-uri "file:///my-project/src/foo/bar_baz.clj")}
                :edits [{:range
                         {:start {:line 0 :character 4}
                          :end {:line 0 :character 15}}
                         :new-text "foo.baz-qux"}]}
               {:kind "rename"
                :old-uri (h/file-uri "file:///my-project/src/foo/bar_baz.clj")
                :new-uri (h/file-uri "file:///my-project/src/foo/baz_qux.clj")}]}
             (handlers/rename {:textDocument (h/file-uri "file:///my-project/src/foo/bar_baz.clj")
                               :position {:line 0 :character 4}
                               :newName "foo.baz-qux"}))))))

(deftest test-find-diagnostics
  (testing "wrong arity"
    (testing "for argument destructuring"
      (reset! db/db {})
      (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
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
        (h/load-code-and-locs code)
        (let [usages (h/diagnostics-or-timeout)]
          (is (= ["user/foo is called with 3 args but expects 1 or 2"
                  "user/baz is called with 1 arg but expects 3"
                  "user/bar is called with 0 args but expects 1 or more"
                  "user/foo is called with 3 args but expects 1 or 2"
                  "user/foo is called with 0 args but expects 1 or 2"
                  "user/foo is called with 4 args but expects 1 or 2"]
                 (map :message usages))))))
    (testing "for threading macros"
      (reset! db/db {})
      (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
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
        (h/load-code-and-locs code)
        (let [usages (h/diagnostics-or-timeout)]
          (is (= ["user/foo is called with 2 args but expects 1 or 3"
                  "user/bar is called with 1 arg but expects 0"
                  "user/bar is called with 1 arg but expects 0"
                  "user/bar is called with 3 args but expects 0"
                  "user/foo is called with 2 args but expects 1 or 3"
                  "user/bar is called with 1 arg but expects 0"]
                 (map :message usages))))))
    (testing "with annotations"
      (reset! db/db {})
      (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
      (let [code "(defn foo {:added \"1.0\"} [x] (inc x))
                  (defn ^:private bar ^String [^Class x & rest] (str x rest))
                  (foo foo)
                  (foo foo foo)
                  (bar :a)
                  (bar :a :b)"]
        (h/load-code-and-locs code)
        (let [usages (h/diagnostics-or-timeout)]
          (is (= ["user/foo is called with 2 args but expects 1"]
                 (map :message usages))))))
    (testing "for schema defs"
      (reset! db/db {})
      (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
      (let [code "(ns user (:require [schema.core :as s]))
                  (s/defn foo :- s/Str
                    [x :- Long y :- Long]
                    (str x y))
                  (foo)
                  (foo 1 2)
                  (foo 1)"]
        (h/load-code-and-locs code)
        (let [usages (h/diagnostics-or-timeout)]
          (is (= ["user/foo is called with 0 args but expects 2"
                  "user/foo is called with 1 arg but expects 2"]
                 (map :message usages)))))))
  (testing "custom unused namespace declaration"
    (reset! db/db {})
    (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
    (h/load-code-and-locs "(ns foo.bar)")
    (let [usages (h/diagnostics-or-timeout)]
      (is (empty?
            (map :message usages))))))

(deftest test-formatting
  (reset! db/db {:documents {(h/file-uri "file:///a.clj") {:text "(a  )\n(b c d)"}}})
  (is (= "(a)\n(b c d)"
         (:new-text (first (handlers/formatting {:textDocument (h/file-uri "file:///a.clj")}))))))

(deftest test-formatting-noop
  (reset! db/db {:documents {(h/file-uri "file:///a.clj") {:text "(a)\n(b c d)"}}})
  (let [r (handlers/formatting {:textDocument (h/file-uri "file:///a.clj")})]
    (is (empty? r))
    (is (vector? r))))

(deftest test-range-formatting
  (reset! db/db {:documents {(h/file-uri "file:///a.clj") {:text "(a  )\n(b c d)"}}})
  (is (= [{:range {:start {:line 0 :character 0}
                   :end {:line 0 :character 5}}
           :new-text "(a)"}]
         (handlers/range-formatting (h/file-uri "file:///a.clj") {:row 1 :col 1 :end-row 1 :end-col 4}))))

(deftest test-code-actions-handle
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        (h/file-uri "file:///a.clj"))
  (h/load-code-and-locs (str "(ns other-ns (:require [some-ns :as sns]))\n"
                             "(def bar 1)\n"
                             "(defn baz []\n"
                             "  bar)")
                        (h/file-uri "file:///b.clj"))
  (h/load-code-and-locs (str "(ns another-ns)\n"
                             "(def bar ons/bar)\n"
                             "(def foo sns/foo)\n"
                             "(deftest some-test)\n"
                             "MyClass.\n"
                             "Date.")
                        (h/file-uri "file:///c.clj"))
  (testing "when it has unresolved-namespace and can find namespace"
    (is (some #(= (:title %) "Add missing 'some-ns' require")
              (handlers/code-actions
                {:textDocument (h/file-uri "file:///c.clj")
                 :context {:diagnostics [{:code "unresolved-namespace"
                                          :range {:start {:line 2 :character 10}}}]}
                 :range {:start {:line 2 :character 10}}}))))
  (testing "without workspace edit client capability"
    (swap! db/db merge {:client-capabilities {:workspace {:workspace-edit false}}})
    (is (not-any? #(= (:title %) "Clean namespace")
                  (handlers/code-actions
                    {:textDocument (h/file-uri "file:///b.clj")
                     :context {:diagnostics []}
                     :range {:start {:line 1 :character 1}}}))))
  (testing "with workspace edit client capability"
    (swap! db/db merge {:client-capabilities {:workspace {:workspace-edit true}}})
    (is (some #(= (:title %) "Clean namespace")
              (handlers/code-actions
                {:textDocument (h/file-uri "file:///b.clj")
                 :context {:diagnostics []}
                 :range {:start {:line 1 :character 1}}})))))

(deftest test-code-lens
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo 1)\n"
                             "(defn- foo2 []\n"
                             " foo)\n"
                             "(defn bar [a b]\n"
                             "  (+ a b (foo2)))\n"
                             "(s/defn baz []\n"
                             "  (bar 2 3))\n"))
  (testing "references lens"
    (is (= (list {:range
                  {:start {:line 1 :character 5} :end {:line 1 :character 8}}
                  :data [(h/file-uri "file:///a.clj") 2 6]}
                 {:range
                  {:start {:line 2 :character 7} :end {:line 2 :character 11}}
                  :data [(h/file-uri "file:///a.clj") 3 8]}
                 {:range
                  {:start {:line 4 :character 6} :end {:line 4 :character 9}}
                  :data [(h/file-uri "file:///a.clj") 5 7]})
           (handlers/code-lens {:textDocument (h/file-uri "file:///a.clj")})))))

(deftest test-code-lens-resolve
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo 1)\n"
                             "(defn- foo2 []\n"
                             " foo)\n"
                             "(defn bar [a b]\n"
                             "  (+ a b (foo2)))\n"
                             "(s/defn baz []\n"
                             "  (bar 2 3))\n"))
  (testing "references"
    (testing "empty lens"
      (is (= {:range   {:start {:line      0
                                :character 5}
                        :end   {:line      0
                                :character 12}}
              :command {:title   "0 references"
                        :command "code-lens-references"
                        :arguments [(h/file-uri "file:///a.clj") 0 5]}}
             (handlers/code-lens-resolve {:data [(h/file-uri "file:///a.clj") 0 5]
                                          :range {:start {:line 0 :character 5} :end {:line 0 :character 12}}}))))
    (testing "some lens"
      (is (= {:range   {:start {:line      1
                                :character 5}
                        :end   {:line      1
                                :character 12}}
              :command {:title   "1 reference"
                        :command "code-lens-references"
                        :arguments [(h/file-uri "file:///a.clj") 2 6]}}
             (handlers/code-lens-resolve {:data [(h/file-uri "file:///a.clj") 2 6]
                                          :range {:start {:line 1 :character 5} :end {:line 1 :character 12}}})))
      (is (= {:range   {:start {:line      2
                                :character 7}
                        :end   {:line      2
                                :character 11}}
              :command {:title   "1 reference"
                        :command "code-lens-references"
                        :arguments [(h/file-uri "file:///a.clj") 3 8]}}
             (handlers/code-lens-resolve {:data [(h/file-uri "file:///a.clj") 3 8]
                                          :range {:start {:line 2 :character 7} :end {:line 2 :character 11}}}))))))
