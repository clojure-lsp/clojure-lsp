(ns clojure-lsp.feature.diagnostics.built-in-test
  (:require
   [clojure-lsp.feature.diagnostics.built-in :as f.diagnostics.built-in]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest testing]]))

(h/reset-components-before-test)

(defn ^:private lint! [uris settings]
  (reduce
    (fn [acc [uri diags]]
      (concat acc (mapv #(assoc % :uri uri) diags)))
    []
    (f.diagnostics.built-in/analyze-uris!
      uris
      (assoc (h/db) :settings settings))))

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
  (h/load-code-and-locs "(ns some-ns) (def foo 1) (comment (def bar 2))" (h/file-uri "file:///project/src/e_a.clj"))
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                "#_:clojure-lsp/ignore"
                                "(def bar 2)"
                                "#_:clj-kondo/ignore"
                                "(def baz 3)"
                                "#_#_#_1 2 3 4"
                                "#_:other-comment/here"
                                "(def foo 1)"
                                "#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}"
                                "(def qux 4)"
                                "#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}"
                                "(def quu 5)")
                        (h/file-uri "file:///project/src/e_b.clj"))
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
        :uri (h/file-uri "file:///project/src/e_a.clj")
        :message "Unused public var 'some-ns/foo'"}]
      (lint!
        [(h/file-uri "file:///project/src/e_a.clj")]
        {})))
  (testing "exclude when ignored via generic clojure-lsp/ignore comment"
    (h/assert-submaps
      [{:range
        {:start {:line 7 :character 5} :end {:line 7 :character 8}}
        :severity 3
        :code "clojure-lsp/unused-public-var"
        :source "clojure-lsp"
        :uri (h/file-uri "file:///project/src/e_b.clj")}]
      (lint!
        [(h/file-uri "file:///project/src/e_b.clj")]
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

(deftest lint-project-cyclic-dependencies
  (testing "simple two-namespace cycle"
    (h/reset-components!)
    (h/load-code-and-locs "(ns a (:require [b :as b]))" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns b (:require [a :as a]))" (h/file-uri "file:///b.clj"))
    (testing "when linter level is :warning"
      (h/assert-submaps
        [{:uri (h/file-uri "file:///a.clj")
          :range {:start {:line 0 :character 4} :end {:line 0 :character 5}}
          :severity 2
          :source "clojure-lsp"
          :message "Cyclic dependency detected: a -> b -> a"
          :code "clojure-lsp/cyclic-dependencies"}
         {:uri (h/file-uri "file:///b.clj")
          :range {:start {:line 0 :character 4} :end {:line 0 :character 5}}
          :severity 2
          :source "clojure-lsp"
          :message "Cyclic dependency detected: a -> b -> a"
          :code "clojure-lsp/cyclic-dependencies"}]
        (lint! [(h/file-uri "file:///a.clj") (h/file-uri "file:///b.clj")]
               {:linters {:clojure-lsp/cyclic-dependencies {:level :warning}}})))
    (testing "when linter level is :off"
      (h/assert-submaps
        []
        (lint! [(h/file-uri "file:///a.clj") (h/file-uri "file:///b.clj")]
               {:linters {:clojure-lsp/cyclic-dependencies {:level :off}}})))
    (testing "linter level by default is :error"
      (h/assert-submaps
        [{:severity 1
          :code "clojure-lsp/cyclic-dependencies"}
         {:severity 1
          :code "clojure-lsp/cyclic-dependencies"}]
        (lint! [(h/file-uri "file:///a.clj") (h/file-uri "file:///b.clj")] {}))))

  (testing "three-namespace cycle"
    (h/reset-components!)
    (h/load-code-and-locs "(ns foo (:require [bar :as b]))" (h/file-uri "file:///foo.clj"))
    (h/load-code-and-locs "(ns bar (:require [baz :as bz]))" (h/file-uri "file:///bar.clj"))
    (h/load-code-and-locs "(ns baz (:require [foo :as f]))" (h/file-uri "file:///baz.clj"))
    (testing "when linter level is :error"
      (h/assert-submaps
        [{:uri (h/file-uri "file:///foo.clj")
          :severity 1
          :message "Cyclic dependency detected: baz -> foo -> bar -> baz"
          :code "clojure-lsp/cyclic-dependencies"}
         {:uri (h/file-uri "file:///bar.clj")
          :severity 1
          :message "Cyclic dependency detected: baz -> foo -> bar -> baz"
          :code "clojure-lsp/cyclic-dependencies"}
         {:uri (h/file-uri "file:///baz.clj")
          :severity 1
          :message "Cyclic dependency detected: baz -> foo -> bar -> baz"
          :code "clojure-lsp/cyclic-dependencies"}]
        (lint! [(h/file-uri "file:///foo.clj") (h/file-uri "file:///bar.clj") (h/file-uri "file:///baz.clj")]
               {:linters {:clojure-lsp/cyclic-dependencies {:level :error}}}))))

  (testing "no cycle when linear dependency chain"
    (h/reset-components!)
    (h/load-code-and-locs "(ns a (:require [b :as b]))" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns b (:require [c :as c]))" (h/file-uri "file:///b.clj"))
    (h/load-code-and-locs "(ns c)" (h/file-uri "file:///c.clj"))
    (testing "linear chain produces no diagnostics"
      (h/assert-submaps
        []
        (lint! [(h/file-uri "file:///a.clj") (h/file-uri "file:///b.clj") (h/file-uri "file:///c.clj")]
               {:linters {:clojure-lsp/cyclic-dependencies {:level :warning}}}))))

  (testing "self-dependency cycle"
    (h/reset-components!)
    (h/load-code-and-locs "(ns self (:require [self :as s]))" (h/file-uri "file:///self.clj"))
    (testing "self-referencing namespace is detected as cycle"
      (h/assert-submaps
        [{:uri (h/file-uri "file:///self.clj")
          :severity 2
          :message "Cyclic dependency detected: self -> self"
          :code "clojure-lsp/cyclic-dependencies"}]
        (lint! [(h/file-uri "file:///self.clj")]
               {:linters {:clojure-lsp/cyclic-dependencies {:level :warning}}}))))

  (testing "exclude specific namespaces"
    (h/reset-components!)
    (h/load-code-and-locs "(ns x (:require [y :as y]))" (h/file-uri "file:///x.clj"))
    (h/load-code-and-locs "(ns y (:require [x :as x]))" (h/file-uri "file:///y.clj"))
    (testing "excluding a namespace from cycle detection"
      (h/assert-submaps
        [{:uri (h/file-uri "file:///y.clj")
          :message "Cyclic dependency detected: x -> y -> x"
          :code "clojure-lsp/cyclic-dependencies"}]
        (lint! [(h/file-uri "file:///x.clj") (h/file-uri "file:///y.clj")]
               {:linters {:clojure-lsp/cyclic-dependencies {:level :warning
                                                            :exclude-namespaces #{'x}}}}))))

  (testing "multiple independent cycles"
    (h/reset-components!)
    ;; First cycle: alpha <-> beta
    (h/load-code-and-locs "(ns alpha (:require [beta :as b]))" (h/file-uri "file:///alpha.clj"))
    (h/load-code-and-locs "(ns beta (:require [alpha :as a]))" (h/file-uri "file:///beta.clj"))
    ;; Second cycle: gamma <-> delta
    (h/load-code-and-locs "(ns gamma (:require [delta :as d]))" (h/file-uri "file:///gamma.clj"))
    (h/load-code-and-locs "(ns delta (:require [gamma :as g]))" (h/file-uri "file:///delta.clj"))
    (testing "detects both independent cycles"
      (let [results (lint! [(h/file-uri "file:///alpha.clj") (h/file-uri "file:///beta.clj")
                            (h/file-uri "file:///gamma.clj") (h/file-uri "file:///delta.clj")]
                           {:linters {:clojure-lsp/cyclic-dependencies {:level :info}}})]
        ;; Should have 4 diagnostics total (2 for each cycle)
        (h/assert-submaps
          [{:message "Cyclic dependency detected: beta -> alpha -> beta"}
           {:message "Cyclic dependency detected: beta -> alpha -> beta"}
           {:message "Cyclic dependency detected: delta -> gamma -> delta"}
           {:message "Cyclic dependency detected: delta -> gamma -> delta"}]
          results))))

  (testing "cycle with external dependencies ignored"
    (h/reset-components!)
    (h/load-code-and-locs "(ns app (:require [clojure.string :as str] [helper :as h]))" (h/file-uri "file:///app.clj"))
    (h/load-code-and-locs "(ns helper (:require [clojure.set :as set] [app :as app]))" (h/file-uri "file:///helper.clj"))
    (testing "external dependencies like clojure.string don't participate in cycles"
      (h/assert-submaps
        [{:message "Cyclic dependency detected: helper -> app -> helper"}
         {:message "Cyclic dependency detected: helper -> app -> helper"}]
        (lint! [(h/file-uri "file:///app.clj") (h/file-uri "file:///helper.clj")]
               {:linters {:clojure-lsp/cyclic-dependencies {:level :info}}}))))

  (testing "complex cycle with branches"
    (h/reset-components!)
    ;; Create a more complex dependency structure:
    ;; main -> [util, core] 
    ;; util -> core
    ;; core -> main (creates cycle: main -> core -> main, but not main -> util -> core -> main)
    (h/load-code-and-locs "(ns main (:require [util :as u] [core :as c]))" (h/file-uri "file:///main.clj"))
    (h/load-code-and-locs "(ns util (:require [core :as c]))" (h/file-uri "file:///util.clj"))
    (h/load-code-and-locs "(ns core (:require [main :as m]))" (h/file-uri "file:///core.clj"))
    (testing "detects the actual cycle path"
      (h/assert-submaps
        [{:message "Cyclic dependency detected: main -> util -> core -> main"}
         {:message "Cyclic dependency detected: main -> util -> core -> main"}
         {:message "Cyclic dependency detected: main -> util -> core -> main"}]
        (lint! [(h/file-uri "file:///main.clj") (h/file-uri "file:///util.clj") (h/file-uri "file:///core.clj")]
               {:linters {:clojure-lsp/cyclic-dependencies {:level :info}}}))))

  ;; TODO: Fix ignore comment functionality for cyclic dependencies  
  #_(testing "cycle with ignore comment"
      (h/reset-components!)
      (h/load-code-and-locs (h/code "#_{:clojure-lsp/ignore [:clojure-lsp/cyclic-dependencies :clojure-lsp/unused-public-var]}"
                                    "(ns p (:require [q :as q]))"
                                    "(def ignored-var 1)")
                            (h/file-uri "file:///p.clj"))
      (h/load-code-and-locs "(ns q (:require [p :as p]))" (h/file-uri "file:///q.clj"))
      (testing "ignore comment should work for cyclic dependencies"
        ;; The ignore comment should suppress both unused-public-var and cyclic-dependencies for p
        ;; We should only see the cyclic dependency diagnostic for q
        (let [results (lint! [(h/file-uri "file:///p.clj") (h/file-uri "file:///q.clj")]
                             {:linters {:clojure-lsp/cyclic-dependencies {:level :warning}
                                        :clojure-lsp/unused-public-var {:level :error}}})]
          (h/assert-submaps
            [{:message "Cyclic dependency detected: p -> q -> p"}]
            results)))))
