(ns clojure-lsp.feature.diagnostics-test
  (:require
   [clj-depend.api :as clj-depend]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.core.async :as async]
   [clojure.test :refer [deftest is testing]]))

(def findings (atom []))

(h/reset-components-before-test)

(deftest lint-project-public-vars
  (h/load-code-and-locs "(ns some-ns) (defn foo [a b] (+ a b))")
  (h/load-code-and-locs "(ns some-ns (:gen-class)) (defn -main [& _args] 1) (defn -foo [] 1)" (h/file-uri "file:///b.clj"))
  (h/load-code-and-locs (h/code "(ns some-ns (:require [re-frame.core :as r]))"
                                "(r/reg-event-fx :some/thing (fn []))"
                                "(r/reg-event-fx :otherthing (fn []))") (h/file-uri "file:///c.cljs"))
  (h/load-code-and-locs (h/code "(ns some-ns) (defn ^:export foobar (fn []))") (h/file-uri "file:///d.cljs"))
  (testing "when linter level is :info"
    (reset! findings [])
    (f.diagnostic/custom-lint-uris!
      [h/default-uri]
      (h/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:level :info}}}})
    (h/assert-submaps
      [{:uri h/default-uri
        :level :info
        :type :clojure-lsp/unused-public-var
        :message "Unused public var 'some-ns/foo'"
        :row 1
        :col 20
        :end-row 1
        :end-col 23}]
      @findings))
  (testing "when linter level is :warning"
    (reset! findings [])
    (f.diagnostic/custom-lint-uris!
      [h/default-uri]
      (h/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:level :warning}}}})
    (h/assert-submaps
      [{:uri h/default-uri
        :level :warning
        :type :clojure-lsp/unused-public-var
        :message "Unused public var 'some-ns/foo'"
        :row 1
        :col 20
        :end-row 1
        :end-col 23}]
      @findings))
  (testing "when linter level is :error"
    (reset! findings [])
    (f.diagnostic/custom-lint-uris!
      [h/default-uri]
      (h/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:level :error}}}})
    (h/assert-submaps
      [{:uri h/default-uri
        :level :error
        :type :clojure-lsp/unused-public-var
        :message "Unused public var 'some-ns/foo'"
        :row 1
        :col 20
        :end-row 1
        :end-col 23}]
      @findings))
  (testing "when linter level is :off"
    (reset! findings [])
    (f.diagnostic/custom-lint-uris!
      [h/default-uri]
      (h/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:level :off}}}})
    (h/assert-submaps
      [{:uri h/default-uri
        :level :off
        :type :clojure-lsp/unused-public-var
        :message "Unused public var 'some-ns/foo'"
        :row 1
        :col 20
        :end-row 1
        :end-col 23}]
      @findings))
  (testing "linter level by default is :info"
    (reset! findings [])
    (f.diagnostic/custom-lint-uris!
      [h/default-uri]
      (h/db)
      {:reg-finding! #(swap! findings conj %)
       :config {}})
    (h/assert-submaps
      [{:uri h/default-uri
        :level :info
        :type :clojure-lsp/unused-public-var
        :message "Unused public var 'some-ns/foo'"
        :row 1
        :col 20
        :end-row 1
        :end-col 23}]
      @findings))
  (testing "excluding the whole ns"
    (reset! findings [])
    (f.diagnostic/custom-lint-uris!
      [h/default-uri]
      (h/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:exclude #{'some-ns}}}}})
    (h/assert-submaps
      []
      @findings))
  (testing "excluding the simple var from ns"
    (reset! findings [])
    (f.diagnostic/custom-lint-uris!
      [h/default-uri]
      (h/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:exclude #{'foo}}}}})
    (h/assert-submaps
      []
      @findings))
  (testing "excluding the specific var"
    (reset! findings [])
    (f.diagnostic/custom-lint-uris!
      [h/default-uri]
      (h/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:exclude #{'some-ns/foo}}}}})
    (h/assert-submaps
      []
      @findings))
  (testing "excluding specific syms"
    (reset! findings [])
    (f.diagnostic/custom-lint-uris!
      [(h/file-uri "file:///b.clj")]
      (h/db)
      {:reg-finding! #(swap! findings conj %)
       :config {}})
    (h/assert-submaps
      []
      @findings))
  (testing "unused keyword definitions"
    (reset! findings [])
    (f.diagnostic/custom-lint-uris!
      [(h/file-uri "file:///c.cljs")]
      (h/db)
      {:reg-finding! #(swap! findings conj %)
       :config {}})
    (h/assert-submaps
      [{:uri (h/file-uri "file:///c.cljs")
        :level :info
        :type :clojure-lsp/unused-public-var
        :message "Unused public keyword ':some/thing'"
        :row 2
        :col 17
        :end-row 2
        :end-col 28}
       {:uri (h/file-uri "file:///c.cljs")
        :level :info
        :type :clojure-lsp/unused-public-var
        :message "Unused public keyword ':otherthing'"
        :row 3
        :col 17
        :end-row 3
        :end-col 28}]
      @findings))
  (testing "var marked ^:export is excluded"
    (reset! findings [])
    (f.diagnostic/custom-lint-uris!
      [(h/file-uri "file:///d.cljs")]
      (h/db)
      {:reg-finding! #(swap! findings conj %)
       :config {}})
    (h/assert-submaps [] @findings))
  (testing "var with dash and :gen-class on ns is excluded"
    (reset! findings [])
    (f.diagnostic/custom-lint-uris!
      [(h/file-uri "file:///b.clj")]
      (h/db)
      {:reg-finding! #(swap! findings conj %)
       :config {}})
    (h/assert-submaps [] @findings)))

(deftest lint-clj-kondo-findings
  (h/load-code-and-locs "(ns some-ns) (defn ^:private foo [a b] (+ a b))" (h/file-uri "file:///some_ns.clj"))
  (h/load-code-and-locs "(ns other-ns) (assert )" (h/file-uri "file:///other_ns.clj"))
  (h/load-code-and-locs "(ns other-ns) foo" (h/file-uri "file:///another_ns.clj"))
  (h/load-code-and-locs "(ns cljc-ns) x (let #?(:clj [x 1] :cljs []))" (h/file-uri "file:///cljc_ns.cljc"))
  (testing "when linter level is :off"
    (swap! (h/db*) shared/deep-merge {:settings {:linters {:clj-kondo {:level :off}}}})
    (is (= []
           (f.diagnostic/find-diagnostics (h/file-uri "file:///some_ns.clj") (h/db)))))
  (testing "when linter level is not :off but has matching :ns-exclude-regex"
    (h/load-code-and-locs "(ns some-ns) (defn ^:private foo [a b] (+ a b))" (h/file-uri "file:///project/src/some_ns.clj"))
    (swap! (h/db*) merge {:project-root-uri (h/file-uri "file:///project")
                          :settings {:source-paths [(h/file-path "/project/src")]
                                     :linters {:clj-kondo {:ns-exclude-regex "some-ns.*"}}}})
    (is (= []
           (f.diagnostic/find-diagnostics (h/file-uri "file:///project/src/some_ns.clj") (h/db)))))
  (testing "when linter level is not :off"
    (swap! (h/db*) merge {:settings {:linters {:clj-kondo {:level :error}}}})
    (h/assert-submaps [{:range {:start {:line 0 :character 29} :end {:line 0 :character 32}}
                        :message "Unused private var some-ns/foo"
                        :code "unused-private-var"
                        :tags [1]
                        :severity 2
                        :source "clj-kondo"}]
                      (f.diagnostic/find-diagnostics (h/file-uri "file:///some_ns.clj") (h/db))))
  (testing "when linter is not specified"
    (swap! (h/db*) merge {:settings {}})
    (h/assert-submaps [{:range {:start {:line 0 :character 29} :end {:line 0 :character 32}}
                        :message "Unused private var some-ns/foo"
                        :code "unused-private-var"
                        :tags [1]
                        :severity 2
                        :source "clj-kondo"}]
                      (f.diagnostic/find-diagnostics (h/file-uri "file:///some_ns.clj") (h/db))))
  (testing "when inside a expression and range-type is not specified (:full)"
    (swap! (h/db*) merge {:settings {}})
    (h/assert-submaps [{:range {:start {:line 0 :character 14} :end {:line 0 :character 23}}
                        :message "clojure.core/assert is called with 0 args but expects 1 or 2"
                        :code "invalid-arity"
                        :tags []
                        :severity 1
                        :source "clj-kondo"}]
                      (f.diagnostic/find-diagnostics (h/file-uri "file:///other_ns.clj") (h/db))))
  (testing "when is a expression and range-type is simple"
    (swap! (h/db*) merge {:settings {:diagnostics {:range-type :simple}}})
    (h/assert-submaps [{:range {:start {:line 0 :character 14} :end {:line 0 :character 14}}
                        :message "clojure.core/assert is called with 0 args but expects 1 or 2"
                        :code "invalid-arity"
                        :tags []
                        :severity 1
                        :source "clj-kondo"}]
                      (f.diagnostic/find-diagnostics (h/file-uri "file:///other_ns.clj") (h/db))))
  (testing "when is not a expression, range-type is simple"
    (swap! (h/db*) merge {:settings {:diagnostics {:range-type :simple}}})
    (h/assert-submaps [{:range {:start {:line 0 :character 14} :end {:line 0 :character 17}}
                        :message "Unresolved symbol: foo"
                        :code "unresolved-symbol"
                        :tags []
                        :severity 1
                        :source "clj-kondo"}]
                      (f.diagnostic/find-diagnostics (h/file-uri "file:///another_ns.clj") (h/db))))
  (testing "when file is external"
    (h/load-code-and-locs "(ns some-ns) (defn ^:private foo [a b] (+ a b))" (h/file-uri "file:///some/place.jar:some/file.clj"))
    (swap! (h/db*) merge {:project-root-uri (h/file-uri "file:///project")
                          :settings {:source-paths ["/project/src"]}})
    (is (= []
           (f.diagnostic/find-diagnostics (h/file-uri "file:///some/place.jar:some/file.clj") (h/db)))))
  (testing "when file doesn't exist"
    (swap! (h/db*) merge {:project-root-uri (h/file-uri "file:///project")
                          :settings {:source-paths ["/project/src"]}})
    (is (= []
           (f.diagnostic/find-diagnostics (h/file-uri "file:///file.clj") (h/db)))))
  (testing "clj-kondo show langs in cljc file"
    (swap! (h/db*) merge {:kondo-config {:output {:langs true}
                                         :linters {:unused-binding {:level :warning}}}})
    (h/assert-submaps
      '({:range
         {:start {:line 0, :character 13}, :end {:line 0, :character 14}},
         :tags [],
         :message "Unresolved symbol: x [clj, cljs]",
         :code "unresolved-symbol",
         :langs (:clj :cljs),
         :severity 1,
         :source "clj-kondo"}
        {:range
         {:start {:line 0, :character 29}, :end {:line 0, :character 30}},
         :tags [1],
         :message "unused binding x [clj]",
         :code "unused-binding",
         :langs (:clj),
         :severity 2,
         :source "clj-kondo"})
      (f.diagnostic/find-diagnostics (h/file-uri "file:///cljc_ns.cljc") (h/db)))))

(deftest lint-clj-depend-findings
  (testing "when clj-depend is not configured even through the clojure-lsp config"
    (swap! (h/db*) shared/deep-merge {:project-root-uri (h/file-uri "file:///project")
                                      :settings {:source-paths ["/project/src"]}})
    (with-redefs [clj-depend/configured? (constantly false)
                  clj-depend/analyze (constantly {:violations [{:namespace 'bar :message "Foo issue"}]})]
      (h/load-code-and-locs "(ns foo) (def a 1)" (h/file-uri "file:///project/src/foo.clj"))
      (h/load-code-and-locs "(ns bar (:require [foo :as f])) f/a" (h/file-uri "file:///project/src/bar.clj")))

    (is (= []
           (f.diagnostic/find-diagnostics (h/file-uri "file:///project/src/bar.clj") (h/db)))))

  (testing "when clj-depend is configured even without clojure-lsp configuration"
    (swap! (h/db*) shared/deep-merge {:project-root-uri (h/file-uri "file:///project")
                                      :settings {:source-paths [(h/file-path "/project/src")]}})
    (with-redefs [clj-depend/configured? (constantly true)
                  clj-depend/analyze (constantly {:violations [{:namespace 'bar :message "Foo issue"}]})]
      (h/load-code-and-locs "(ns foo) (def a 1)" (h/file-uri "file:///project/src/foo.clj"))
      (h/load-code-and-locs "(ns bar (:require [foo :as f])) f/a" (h/file-uri "file:///project/src/bar.clj")))

    (is (= [{:range
             {:start {:line 0 :character 4} :end {:line 0 :character 7}}
             :tags []
             :message "Foo issue"
             :code "clj-depend"
             :severity 3
             :source "clj-depend"}]
           (f.diagnostic/find-diagnostics (h/file-uri "file:///project/src/bar.clj") (h/db)))))

  (testing "when clj-depend config is found but linter level is :off"
    (swap! (h/db*) shared/deep-merge {:settings {:source-paths ["/project/src"]
                                                 :clj-depend {:layers {:foo {:defined-by ".*foo.*"
                                                                             :accessed-by-layers #{}}
                                                                       :bar {:defined-by ".*bar.*"
                                                                             :accessed-by-layers #{:baz}}}}
                                                 :linters {:clj-depend {:level :off}}}})
    (with-redefs [clj-depend/configured? (constantly false)
                  clj-depend/analyze (constantly {:violations [{:namespace 'bar :message "Foo issue"}]})]
      (h/load-code-and-locs "(ns foo) (def a 1)" (h/file-uri "file:///project/src/foo.clj"))
      (h/load-code-and-locs "(ns bar (:require [foo :as f])) f/a" (h/file-uri "file:///project/src/bar.clj")))

    (is (= []
           (f.diagnostic/find-diagnostics (h/file-uri "file:///project/src/bar.clj") (h/db)))))

  (testing "when clj-depend config is found and a violation is present"
    (swap! (h/db*) shared/deep-merge {:project-root-uri (h/file-uri "file:///project")
                                      :settings {:linters {:clj-depend {:level :info}}
                                                 :source-paths [(h/file-path "/project/src")]
                                                 :clj-depend {:layers {:foo {:defined-by ".*foo.*"
                                                                             :accessed-by-layers #{}}
                                                                       :bar {:defined-by ".*bar.*"
                                                                             :accessed-by-layers #{:baz}}}}}})
    (with-redefs [clj-depend/configured? (constantly false)
                  clj-depend/analyze (constantly {:violations [{:namespace 'bar :message "Foo issue"}]})]
      (h/load-code-and-locs "(ns foo) (def a 1)" (h/file-uri "file:///project/src/foo.clj"))
      (h/load-code-and-locs "(ns bar (:require [foo :as f])) f/a" (h/file-uri "file:///project/src/bar.clj")))

    (is (= [{:range
             {:start {:line 0 :character 4} :end {:line 0 :character 7}}
             :tags []
             :message "Foo issue"
             :code "clj-depend"
             :severity 3
             :source "clj-depend"}]
           (f.diagnostic/find-diagnostics (h/file-uri "file:///project/src/bar.clj") (h/db))))))

(deftest test-find-diagnostics
  (testing "wrong arity"
    (testing "for argument destructuring"
      (h/reset-components!)
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
                  (foo 1 2 3 {:k 1 :v 2})"
            mock-diagnostics-chan (async/chan 1)]
        (h/load-code-and-locs code h/default-uri (assoc (h/components)
                                                        :diagnostics-chan mock-diagnostics-chan))
        (let [{:keys [uri diagnostics]} (h/take-or-timeout mock-diagnostics-chan 500)]
          (is (= (h/file-uri "file:///a.clj") uri))
          (is (= ["user/foo is called with 3 args but expects 1 or 2"
                  "user/baz is called with 1 arg but expects 3"
                  "user/bar is called with 0 args but expects 1 or more"
                  "user/foo is called with 3 args but expects 1 or 2"
                  "user/foo is called with 0 args but expects 1 or 2"
                  "user/foo is called with 4 args but expects 1 or 2"]
                 (map :message diagnostics))))))
    (testing "for threading macros"
      (h/reset-components!)
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
                    (bar))"
            mock-diagnostics-chan (async/chan 1)]
        (h/load-code-and-locs code h/default-uri (assoc (h/components)
                                                        :diagnostics-chan mock-diagnostics-chan))
        (let [{:keys [uri diagnostics]} (h/take-or-timeout mock-diagnostics-chan 500)]
          (is (= (h/file-uri "file:///a.clj") uri))
          (is (= ["user/foo is called with 2 args but expects 1 or 3"
                  "user/bar is called with 1 arg but expects 0"
                  "user/bar is called with 1 arg but expects 0"
                  "user/bar is called with 3 args but expects 0"
                  "user/foo is called with 2 args but expects 1 or 3"
                  "user/bar is called with 1 arg but expects 0"]
                 (map :message diagnostics))))))
    (testing "with annotations"
      (h/reset-components!)
      (let [code "(defn foo {:added \"1.0\"} [x] (inc x))
                  (defn ^:private bar ^String [^Class x & rest] (str x rest))
                  (foo foo)
                  (foo foo foo)
                  (bar :a)
                  (bar :a :b)"
            mock-diagnostics-chan (async/chan 1)]
        (h/load-code-and-locs code h/default-uri (assoc (h/components)
                                                        :diagnostics-chan mock-diagnostics-chan))
        (let [{:keys [uri diagnostics]} (h/take-or-timeout mock-diagnostics-chan 500)]
          (is (= h/default-uri uri))
          (is (= ["user/foo is called with 2 args but expects 1"]
                 (map :message diagnostics))))))
    (testing "for schema defs"
      (h/reset-components!)
      (let [code "(ns user (:require [schema.core :as s]))
                  (s/defn foo :- s/Str
                    [x :- Long y :- Long]
                    (str x y))
                  (foo)
                  (foo 1 2)
                  (foo 1)"
            mock-diagnostics-chan (async/chan 1)]
        (h/load-code-and-locs code h/default-uri (assoc (h/components)
                                                        :diagnostics-chan mock-diagnostics-chan))
        (let [{:keys [uri diagnostics]} (h/take-or-timeout mock-diagnostics-chan 500)]
          (is (= h/default-uri uri))
          (is (= ["user/foo is called with 0 args but expects 2"
                  "user/foo is called with 1 arg but expects 2"]
                 (map :message diagnostics)))))))
  (testing "custom unused namespace declaration"
    (h/reset-components!)
    (let [mock-diagnostics-chan (async/chan 1)]
      (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///foo/bar.clj") (assoc (h/components)
                                                                                     :diagnostics-chan mock-diagnostics-chan))
      (let [{:keys [uri diagnostics]} (h/take-or-timeout mock-diagnostics-chan 500)]
        (is (= (h/file-uri "file:///foo/bar.clj") uri))
        (is (= [] diagnostics))))))
