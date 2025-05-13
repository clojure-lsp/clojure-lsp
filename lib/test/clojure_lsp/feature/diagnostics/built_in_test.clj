(ns clojure-lsp.feature.diagnostics.built-in-test
  (:require
   [clojure-lsp.feature.diagnostics.built-in :as f.diagnostics.built-in]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest testing]]))

(h/reset-components-before-test)

(defn ^:private lint! [uris kondo-config]
  (reduce
    (fn [acc [uri diags]]
      (concat acc (mapv #(assoc % :uri uri) diags)))
    []
    (f.diagnostics.built-in/analyze-uris!
      uris
      (assoc (h/db) :kondo-config kondo-config))))

(deftest lint-project-different-aliases
  (h/load-code-and-locs "(ns a (:require [clojure.string]))")
  (h/load-code-and-locs "(ns b (:require [clojure.string :as s]))"
                        (h/file-uri "file:///b.clj"))
  (testing "when there are two files but only one alias"
    (h/assert-submaps
      []
      (lint! [h/default-uri] {:linters {:clojure-lsp/different-aliases {:level :info}}})))
  (h/load-code-and-locs "(ns c (:require [clojure.string :as str]))"
                        (h/file-uri "file:///c.clj"))
  (h/load-code-and-locs "(ns d (:require [clojure.string :as string]))"
                        (h/file-uri "file:///d.clj"))
  (testing "when linter level is :off"
    (h/assert-submaps
      []
      (lint! [h/default-uri] {:linters {:clojure-lsp/different-aliases {:level :off}}})))
  (testing "when linter level is 3"
    (h/assert-submaps
      [{:uri (h/file-uri "file:///b.clj")
        :range {:start {:line 0 :character 36} :end {:line 0 :character 37}}
        :severity 3
        :source "clojure-lsp"
        :message "Different aliases #{s string str} found for clojure.string"
        :code "clojure-lsp/different-aliases"}
       {:uri (h/file-uri "file:///c.clj")
        :range {:start {:line 0 :character 36} :end {:line 0 :character 39}}
        :severity 3
        :source "clojure-lsp"
        :message "Different aliases #{s string str} found for clojure.string"
        :code "clojure-lsp/different-aliases"}
       {:uri (h/file-uri "file:///d.clj")
        :range {:start {:line 0 :character 36} :end {:line 0 :character 42}}
        :source "clojure-lsp"
        :severity 3
        :message "Different aliases #{s string str} found for clojure.string"
        :code "clojure-lsp/different-aliases"}]
      (lint! [h/default-uri] {:linters {:clojure-lsp/different-aliases {:level :info}}})))
  (testing "linter level by default is :off"
    (h/assert-submaps
      []
      (lint! [h/default-uri] {})))
  (h/load-code-and-locs "(ns e (:require [clojure.string :as sut]))"
                        (h/file-uri "file:///e.clj"))
  (testing "exclude-aliases"
    (h/assert-submaps
      [{:uri (h/file-uri "file:///b.clj")}
       {:uri (h/file-uri "file:///c.clj")}
       {:uri (h/file-uri "file:///d.clj")}]
      (lint! [h/default-uri] {:linters {:clojure-lsp/different-aliases {:level :error :exclude-aliases #{'sut}}}}))))

(deftest unused-public-var-declared-outside-test-namespaces
  (swap! (h/db*) merge {:project-root-uri (h/file-uri "file:///project")
                        :settings {:source-paths [(h/file-path "/project/src")
                                                  (h/file-path "/project/test")]}})
  (h/load-code-and-locs "(ns foo) (defn bar [a b] (+ a b))"
                        (h/file-uri "file:///project/src/foo.clj"))
  (h/load-code-and-locs "(ns foo-test (:require [foo])) (foo/bar 1 2)"
                        (h/file-uri "file:///project/test/foo_test.clj"))
  (testing "Given a public var foo/bar in a src namespace
            And being referenced in a test namespace foo-test
            When linter level is 3
            And the :ignore-test-references? flag is off
            Then it returns no findings because foo/bar is being used"
    (h/assert-submaps
      []
      (lint!
        [(h/file-uri "file:///project/src/foo.clj")
         (h/file-uri "file:///project/test/foo_test.clj")]
        {:linters
         {:clojure-lsp/unused-public-var
          {:level :info
           :ignore-test-references? false}}})))
  (testing "Given a public var foo/bar in a src namespace
            And being referenced in a test namespace foo-test
            When linter level is 3
            And the :ignore-test-references? flag is on
            Then it returns a finding because the test namespace is ignored"
    (h/assert-submaps
      [{:range {:start {:line 0 :character 15} :end {:line 0 :character 18}}
        :code "clojure-lsp/unused-public-var"
        :severity 3
        :uri (h/file-uri "file:///project/src/foo.clj")
        :message "Unused public var 'foo/bar'"
        :source "clojure-lsp"}]
      (lint!
        [(h/file-uri "file:///project/src/foo.clj")
         (h/file-uri "file:///project/test/foo_test.clj")]
        {:linters
         {:clojure-lsp/unused-public-var
          {:level :info
           :ignore-test-references? true}}}))))

(deftest unused-public-var-declared-inside-test-namespace
  (swap! (h/db*) merge {:project-root-uri (h/file-uri "file:///project")
                        :settings {:source-paths [(h/file-path "/project/test")]}})
  (h/load-code-and-locs "(ns foo-test) (defn helper [])"
                        (h/file-uri "file:///project/test/foo_test.clj"))
  (testing "Given a public var foo-test/helper in a test namespace
            When linter level is 3
            And the :ignore-test-references? flag is off
            Then it returns a finding because foo-test/helper is not being used"
    (h/assert-submaps
      [{:range {:start {:line 0 :character 20} :end {:line 0 :character 26}}
        :code "clojure-lsp/unused-public-var"
        :severity 3
        :uri (h/file-uri "file:///project/test/foo_test.clj")
        :message "Unused public var 'foo-test/helper'"}]
      (lint!
        [(h/file-uri "file:///project/test/foo_test.clj")]
        {:linters
         {:clojure-lsp/unused-public-var
          {:level :info
           :ignore-test-references? false}}})))
  (testing "Given a public var foo-test/helper in a test namespace
            When linter level is 3
            And the :ignore-test-references? flag is on
            Then it returns a finding because it is not being used although it is in a test namespace"
    (h/assert-submaps
      [{:range {:start {:line 0 :character 20} :end {:line 0 :character 26}}
        :code "clojure-lsp/unused-public-var"
        :severity 3
        :uri (h/file-uri "file:///project/test/foo_test.clj")
        :message "Unused public var 'foo-test/helper'"}]
      (lint!
        [(h/file-uri "file:///project/test/foo_test.clj")]
        {:linters
         {:clojure-lsp/unused-public-var
          {:level :info
           :ignore-test-references? true}}})))
  (testing "Given a public var bar-test/baz in a test namespace
            When linter level is 3
            And the :ignore-test-references? flag is on
            Then it returns NO findings because it is being used although it is in a test namespace"
    (h/assert-submaps
      []
      (lint!
        [(h/file-uri "file:///project/test/bar_test.clj")]
        {:linters {:clojure-lsp/unused-public-var
                   {:level :info
                    :ignore-test-references? true}}}))
    (h/load-code-and-locs "(ns bar-test) (defn baz []) (baz)"
                          (h/file-uri "file:///project/test/bar_test.clj"))))

(deftest lint-project-public-vars
  (swap! (h/db*) merge {:project-root-uri (h/file-uri "file:///project")
                        :settings {:source-paths [(h/file-path "/project/src") (h/file-path "/project/cool-test")]}})
  (h/load-code-and-locs "(ns some-ns) (defn foo [a b] (+ a b))" (h/file-uri "file:///project/src/a.clj"))
  (h/load-code-and-locs "(ns some-ns (:gen-class)) (defn -main [& _args] 1) (defn -foo [] 1)" (h/file-uri "file:///project/src/b.clj"))
  (h/load-code-and-locs (h/code "(ns some-ns (:require [re-frame.core :as r]))"
                                "(r/reg-event-fx :some/thing (fn []))"
                                "(r/reg-event-fx :otherthing (fn []))") (h/file-uri "file:///project/src/c.cljs"))
  (h/load-code-and-locs (h/code "(ns some-ns) (defn ^:export foobar (fn []))") (h/file-uri "file:///project/src/d.cljs"))
  (h/load-code-and-locs "(ns some-ns) (def foo 1) (comment (def bar 2))" (h/file-uri "file:///project/src/e.clj"))
  (h/load-code-and-locs (h/code "(ns f-ns)"
                                "(definterface Bar (bar [x]))"
                                "(definterface Foo (baz [x]))"
                                "(reify Foo (baz [_ x] x))") (h/file-uri "file:///project/src/f.clj"))
  (h/load-code-and-locs "(ns g-a) (defn foo [a b] (+ a b))" (h/file-uri "file:///project/src/g_a.clj"))
  (h/load-code-and-locs "(ns g-b (:require [g-a :as g-a])) (g-a/foo 1 2)" (h/file-uri "file:///project/cool-test/g_a_test.clj"))
  (h/load-code-and-locs "{:a h.b/foo}" (h/file-uri "file:///project/src/h_a.edn"))
  (h/load-code-and-locs "{:b h.b/bar}" (h/file-uri "file:///project/.lsp/config.edn"))
  (h/load-code-and-locs "(ns h.b) (defn foo [] 1) (defn bar [] 1)" (h/file-uri "file:///project/src/h_b.clj"))
  (testing "when linter level is 3"
    (h/assert-submaps
      [{:range {:start {:line 0 :character 19} :end {:line 0 :character 22}}
        :code "clojure-lsp/unused-public-var"
        :severity 3
        :uri (h/file-uri "file:///project/src/a.clj")
        :message "Unused public var 'some-ns/foo'"}]
      (lint!
        [(h/file-uri "file:///project/src/a.clj")]
        {:linters {:clojure-lsp/unused-public-var {:level :info}}})))
  (testing "when linter level is :warning"
    (h/assert-submaps
      [{:range {:start {:line 0 :character 19} :end {:line 0 :character 22}}
        :code "clojure-lsp/unused-public-var"
        :severity 2
        :uri (h/file-uri "file:///project/src/a.clj")
        :message "Unused public var 'some-ns/foo'"}]
      (lint!
        [(h/file-uri "file:///project/src/a.clj")]
        {:linters {:clojure-lsp/unused-public-var {:level :warning}}})))
  (testing "when linter level is :error"
    (h/assert-submaps
      [{:range {:start {:line 0 :character 19} :end {:line 0 :character 22}}
        :code "clojure-lsp/unused-public-var"
        :severity 1
        :uri (h/file-uri "file:///project/src/a.clj")
        :message "Unused public var 'some-ns/foo'"}]
      (lint!
        [(h/file-uri "file:///project/src/a.clj")]
        {:linters {:clojure-lsp/unused-public-var {:level :error}}})))
  (testing "when linter level is :off"
    (h/assert-submaps
      []
      (lint!
        [(h/file-uri "file:///project/src/a.clj")]
        {:linters {:clojure-lsp/unused-public-var {:level :off}}})))
  (testing "linter level by default is 3"
    (h/assert-submaps
      [{:range {:start {:line 0 :character 19} :end {:line 0 :character 22}}
        :code "clojure-lsp/unused-public-var"
        :severity 3
        :uri (h/file-uri "file:///project/src/a.clj")
        :message "Unused public var 'some-ns/foo'"}]
      (lint!
        [(h/file-uri "file:///project/src/a.clj")]
        {})))
  (testing "excluding the whole ns"
    (h/assert-submaps
      []
      (lint!
        [(h/file-uri "file:///project/src/a.clj")]
        {:linters {:clojure-lsp/unused-public-var {:exclude #{'some-ns}}}})))
  (testing "excluding the simple var from ns"
    (h/assert-submaps
      []
      (lint!
        [(h/file-uri "file:///project/src/a.clj")]
        {:linters {:clojure-lsp/unused-public-var {:exclude #{'foo}}}})))
  (testing "excluding the specific var"
    (h/assert-submaps
      []
      (lint!
        [(h/file-uri "file:///project/src/a.clj")]
        {:linters {:clojure-lsp/unused-public-var {:exclude #{'some-ns/foo}}}})))
  (testing "excluding specific syms"
    (h/assert-submaps
      []
      (lint!
        [(h/file-uri "file:///project/src/b.clj")]
        {})))
  (testing "excluding when inside comment block"
    (h/assert-submaps
      [{:code "clojure-lsp/unused-public-var"
        :uri (h/file-uri "file:///project/src/e.clj")
        :message "Unused public var 'some-ns/foo'"}]
      (lint!
        [(h/file-uri "file:///project/src/e.clj")]
        {})))
  (testing "unused keyword definitions"
    (h/assert-submaps
      [{:uri (h/file-uri "file:///project/src/c.cljs")
        :severity 3
        :code "clojure-lsp/unused-public-var"
        :message "Unused public keyword ':some/thing'"
        :range {:start {:line 1 :character 16} :end {:line 1 :character 27}}}
       {:uri (h/file-uri "file:///project/src/c.cljs")
        :severity 3
        :code "clojure-lsp/unused-public-var"
        :message "Unused public keyword ':otherthing'"
        :range {:start {:line 2 :character 16} :end {:line 2 :character 27}}}]
      (lint!
        [(h/file-uri "file:///project/src/c.cljs")]
        {})))
  (testing "var marked ^:export is excluded"
    (h/assert-submaps
      []
      (lint!
        [(h/file-uri "file:///project/src/d.cljs")]
        {})))
  (testing "var with dash and :gen-class on ns is excluded"
    (h/assert-submaps
      []
      (lint!
        [(h/file-uri "file:///project/src/b.clj")]
        {})))
  (testing "definterface methods are excluded when the interface is used."
    (h/assert-submaps
      [{:uri (h/file-uri "file:///project/src/f.clj")
        :severity 3
        :code "clojure-lsp/unused-public-var"
        :message "Unused public var 'f-ns/Bar'"
        :range {:start {:line 1 :character 14} :end {:line 1 :character 17}}}]
      (lint!
        [(h/file-uri "file:///project/src/f.clj")]
        {})))
  (testing "not ignoring tests references when flag is off"
    (h/assert-submaps
      []
      (lint!
        [(h/file-uri "file:///project/src/g_a.clj")
         (h/file-uri "file:///project/cool-test/g_a_test.clj")]
        {})))
  (testing "ignoring tests references when flag is on"
    (h/assert-submaps
      [{:uri (h/file-uri "file:///project/src/g_a.clj")
        :severity 3
        :code "clojure-lsp/unused-public-var"
        :message "Unused public var 'g-a/foo'"
        :range {:start {:line 0 :character 15} :end {:line 0 :character 18}}}]
      (lint!
        [(h/file-uri "file:///project/src/g_a.clj")
         (h/file-uri "file:///project/cool-test/g_a_test.clj")]
        {:linters {:clojure-lsp/unused-public-var {:ignore-test-references? true}}})))
  (testing "exclude when used in symbols"
    (h/assert-submaps
      []
      (lint!
        [(h/file-uri "file:///project/src/h_b.clj")]
        {:linters {:clojure-lsp/unused-public-var {:level :info}}}))))
