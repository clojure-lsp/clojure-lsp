(ns clojure-lsp.features.diagnostics-test
  (:require
   [clj-depend.api :as clj-depend]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.test-helper :as h]
   [clojure.core.async :as async]
   [clojure.test :refer [deftest is testing]]))

(deftest lint-project-public-vars
  (let [components (h/make-components)]
    (h/load-code "(ns some-ns) (defn foo [a b] (+ a b))" h/default-uri components)
    (h/load-code "(ns some-ns) (defn -main [& _args] 1)" (h/file-uri "file:///b.clj") components)
    (h/load-code (h/code "(ns some-ns (:require [re-frame.core :as r]))"
                         "(r/reg-event-fx :some/thing (fn []))"
                         "(r/reg-event-fx :otherthing (fn []))") (h/file-uri "file:///c.cljs") components)
    (h/load-code (h/code "(ns some-ns) (defn ^:export foobar (fn []))") (h/file-uri "file:///d.cljs") components)
    (testing "when linter level is :info"
      (let [findings (atom [])]
        (f.diagnostic/custom-lint-file!
          "/a.clj"
          (h/db components)
          {:reg-finding! #(swap! findings conj %)
           :config {:linters {:clojure-lsp/unused-public-var {:level :info}}}})
        (h/assert-submaps
          [{:filename "/a.clj"
            :level :info
            :type :clojure-lsp/unused-public-var
            :message "Unused public var 'some-ns/foo'"
            :row 1
            :col 20
            :end-row 1
            :end-col 23}]
          @findings)))
    (testing "when linter level is :warning"
      (let [findings (atom [])]
        (f.diagnostic/custom-lint-file!
          "/a.clj"
          (h/db components)
          {:reg-finding! #(swap! findings conj %)
           :config {:linters {:clojure-lsp/unused-public-var {:level :warning}}}})
        (h/assert-submaps
          [{:filename "/a.clj"
            :level :warning
            :type :clojure-lsp/unused-public-var
            :message "Unused public var 'some-ns/foo'"
            :row 1
            :col 20
            :end-row 1
            :end-col 23}]
          @findings)))
    (testing "when linter level is :error"
      (let [findings (atom [])]
        (f.diagnostic/custom-lint-file!
          "/a.clj"
          (h/db components)
          {:reg-finding! #(swap! findings conj %)
           :config {:linters {:clojure-lsp/unused-public-var {:level :error}}}})
        (h/assert-submaps
          [{:filename "/a.clj"
            :level :error
            :type :clojure-lsp/unused-public-var
            :message "Unused public var 'some-ns/foo'"
            :row 1
            :col 20
            :end-row 1
            :end-col 23}]
          @findings)))
    (testing "when linter level is :off"
      (let [findings (atom [])]
        (f.diagnostic/custom-lint-file!
          "/a.clj"
          (h/db components)
          {:reg-finding! #(swap! findings conj %)
           :config {:linters {:clojure-lsp/unused-public-var {:level :off}}}})
        (h/assert-submaps
          [{:filename "/a.clj"
            :level :off
            :type :clojure-lsp/unused-public-var
            :message "Unused public var 'some-ns/foo'"
            :row 1
            :col 20
            :end-row 1
            :end-col 23}]
          @findings)))
    (testing "linter level by default is :info"
      (let [findings (atom [])]
        (f.diagnostic/custom-lint-file!
          "/a.clj"
          (h/db components)
          {:reg-finding! #(swap! findings conj %)
           :config {}})
        (h/assert-submaps
          [{:filename "/a.clj"
            :level :info
            :type :clojure-lsp/unused-public-var
            :message "Unused public var 'some-ns/foo'"
            :row 1
            :col 20
            :end-row 1
            :end-col 23}]
          @findings)))
    (testing "excluding the whole ns"
      (let [findings (atom [])]
        (f.diagnostic/custom-lint-file!
          "/a.clj"
          (h/db components)
          {:reg-finding! #(swap! findings conj %)
           :config {:linters {:clojure-lsp/unused-public-var {:exclude #{'some-ns}}}}})
        (h/assert-submaps
          []
          @findings)))
    (testing "excluding the simple var from ns"
      (let [findings (atom [])]
        (f.diagnostic/custom-lint-file!
          "/a.clj"
          (h/db components)
          {:reg-finding! #(swap! findings conj %)
           :config {:linters {:clojure-lsp/unused-public-var {:exclude #{'foo}}}}})
        (h/assert-submaps
          []
          @findings)))
    (testing "excluding the specific var"
      (let [findings (atom [])]
        (f.diagnostic/custom-lint-file!
          "/a.clj"
          (h/db components)
          {:reg-finding! #(swap! findings conj %)
           :config {:linters {:clojure-lsp/unused-public-var {:exclude #{'some-ns/foo}}}}})
        (h/assert-submaps
          []
          @findings)))
    (testing "excluding specific syms"
      (let [findings (atom [])]
        (f.diagnostic/custom-lint-file!
          "/b.clj"
          (h/db components)
          {:reg-finding! #(swap! findings conj %)
           :config {}})
        (h/assert-submaps
          []
          @findings)))
    (testing "unused keyword definitions"
      (let [findings (atom [])]
        (f.diagnostic/custom-lint-file!
          "/c.cljs"
          (h/db components)
          {:reg-finding! #(swap! findings conj %)
           :config {}})
        (h/assert-submaps
          [{:filename "/c.cljs"
            :level :info
            :type :clojure-lsp/unused-public-var
            :message "Unused public keyword ':some/thing'"
            :row 2
            :col 17
            :end-row 2
            :end-col 28}
           {:filename "/c.cljs"
            :level :info
            :type :clojure-lsp/unused-public-var
            :message "Unused public keyword ':otherthing'"
            :row 3
            :col 17
            :end-row 3
            :end-col 28}]
          @findings)))
    (testing "var marked ^:export is excluded"
      (let [findings (atom [])]
        (f.diagnostic/custom-lint-file!
          "/d.cljs"
          (h/db components)
          {:reg-finding! #(swap! findings conj %)
           :config {}})
        (h/assert-submaps [] @findings)))))

(deftest lint-clj-kondo-findings
  (let [components (h/make-components)
        db-with-settings #(-> components :db* deref (merge %))]
    (h/load-code "(ns some-ns) (defn ^:private foo [a b] (+ a b))" (h/file-uri "file:///some_ns.clj") components)
    (h/load-code "(ns other-ns) (assert )" (h/file-uri "file:///other_ns.clj") components)
    (testing "when linter level is :off"
      (is (= []
             (f.diagnostic/find-diagnostics (h/file-uri "file:///some_ns.clj")
                                            (db-with-settings {:settings {:linters {:clj-kondo {:level :off}}}})))))
    (testing "when linter level is not :off but has matching :ns-exclude-regex"
      (h/load-code "(ns some-ns) (defn ^:private foo [a b] (+ a b))" (h/file-uri "file:///project/src/some_ns.clj") components)
      (is (= []
             (f.diagnostic/find-diagnostics (h/file-uri "file:///project/src/some_ns.clj")
                                            (db-with-settings {:project-root-uri (h/file-uri "file:///project")
                                                               :settings {:source-paths ["/project/src"]
                                                                          :linters {:clj-kondo {:ns-exclude-regex "some-ns.*"}}}})))))
    (testing "when linter level is not :off"
      (h/assert-submaps [{:range {:start {:line 0 :character 29} :end {:line 0 :character 32}}
                          :message "Unused private var some-ns/foo"
                          :code "unused-private-var"
                          :tags [1]
                          :severity 2
                          :source "clj-kondo"}]
                        (f.diagnostic/find-diagnostics (h/file-uri "file:///some_ns.clj")
                                                       (db-with-settings {:settings {:linters {:clj-kondo {:level :error}}}}))))
    (testing "when linter is not specified"
      (h/assert-submaps [{:range {:start {:line 0 :character 29} :end {:line 0 :character 32}}
                          :message "Unused private var some-ns/foo"
                          :code "unused-private-var"
                          :tags [1]
                          :severity 2
                          :source "clj-kondo"}]
                        (f.diagnostic/find-diagnostics (h/file-uri "file:///some_ns.clj")
                                                       (db-with-settings {:settings {}}))))
    (testing "when inside expression?"
      (h/assert-submaps [{:range {:start {:line 0 :character 14} :end {:line 0 :character 23}}
                          :message "clojure.core/assert is called with 0 args but expects 1 or 2"
                          :code "invalid-arity"
                          :tags []
                          :severity 1
                          :source "clj-kondo"}]
                        (f.diagnostic/find-diagnostics (h/file-uri "file:///other_ns.clj")
                                                       (db-with-settings {:settings {}}))))
    (testing "when file is external"
      (h/load-code "(ns some-ns) (defn ^:private foo [a b] (+ a b))" (h/file-uri "file:///some/place.jar:some/file.clj") components)
      (is (= []
             (f.diagnostic/find-diagnostics (h/file-uri "file:///some/place.jar:some/file.clj")
                                            (db-with-settings {:project-root-uri (h/file-uri "file:///project")
                                                               :settings {:source-paths ["/project/src"]}})))))))
(deftest lint-clj-depend-findings
  (testing "when no clj-depend config is found"
    (let [components (h/make-components {:project-root-uri (h/file-uri "file:///project")
                                         :settings {:source-paths ["/project/src"]}})]

      (with-redefs [clj-depend/analyze (constantly {:violations [{:namespace 'bar :violation "Foo issue"}]})]
        (h/load-code "(ns foo) (def a 1)" (h/file-uri "file:///project/src/foo.clj") components)
        (h/load-code "(ns bar (:require [foo :as f])) f/a" (h/file-uri "file:///project/src/bar.clj") components))
      (is (= []
             (f.diagnostic/find-diagnostics (h/file-uri "file:///project/src/bar.clj") (h/db components))))))
  (testing "when clj-depend config is found but linter level is :off"
    (let [components (h/make-components {:settings {:source-paths ["/project/src"]
                                                    :clj-depend {:layers {:foo {:defined-by ".*foo.*"
                                                                                :accessed-by-layers #{}}
                                                                          :bar {:defined-by ".*bar.*"
                                                                                :accessed-by-layers #{:baz}}}}
                                                    :linters {:clj-depend {:level :off}}}})]
      (with-redefs [clj-depend/analyze (constantly {:violations [{:namespace 'bar :violation "Foo issue"}]})]
        (h/load-code "(ns foo) (def a 1)" (h/file-uri "file:///project/src/foo.clj") components)
        (h/load-code "(ns bar (:require [foo :as f])) f/a" (h/file-uri "file:///project/src/bar.clj") components))
      (is (= []
             (f.diagnostic/find-diagnostics (h/file-uri "file:///project/src/bar.clj") (h/db components))))))
  (testing "when clj-depend config is found and a violation is present"
    (let [components (h/make-components {:project-root-uri (h/file-uri "file:///project")
                                         :settings {:linters {:clj-depend {:level :info}}
                                                    :source-paths ["/project/src"]
                                                    :clj-depend {:layers {:foo {:defined-by ".*foo.*"
                                                                                :accessed-by-layers #{}}
                                                                          :bar {:defined-by ".*bar.*"
                                                                                :accessed-by-layers #{:baz}}}}}})]
      (with-redefs [clj-depend/analyze (constantly {:violations [{:namespace 'bar :message "Foo issue"}]})]
        (h/load-code "(ns foo) (def a 1)" (h/file-uri "file:///project/src/foo.clj") components)
        (h/load-code "(ns bar (:require [foo :as f])) f/a" (h/file-uri "file:///project/src/bar.clj") components))
      (is (= [{:range
               {:start {:line 0 :character 4} :end {:line 0 :character 7}}
               :tags []
               :message "Foo issue"
               :code "clj-depend"
               :severity 3
               :source "clj-depend"}]
             (f.diagnostic/find-diagnostics (h/file-uri "file:///project/src/bar.clj") (h/db components)))))))

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
                  (foo 1 2 3 {:k 1 :v 2})"
            mock-diagnostics-chan (async/chan 1)]
        (h/load-code code h/default-uri (assoc (h/make-components)
                                               :diagnostics-chan mock-diagnostics-chan))
        (let [{:keys [uri diagnostics]} (h/take-or-timeout mock-diagnostics-chan 500)]
          (is (= "file:///a.clj" uri))
          (is (= ["user/foo is called with 3 args but expects 1 or 2"
                  "user/baz is called with 1 arg but expects 3"
                  "user/bar is called with 0 args but expects 1 or more"
                  "user/foo is called with 3 args but expects 1 or 2"
                  "user/foo is called with 0 args but expects 1 or 2"
                  "user/foo is called with 4 args but expects 1 or 2"]
                 (map :message diagnostics))))))
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
                    (bar))"
            mock-diagnostics-chan (async/chan 1)]
        (h/load-code code h/default-uri (assoc (h/make-components)
                                               :diagnostics-chan mock-diagnostics-chan))
        (let [{:keys [uri diagnostics]} (h/take-or-timeout mock-diagnostics-chan 500)]
          (is (= "file:///a.clj" uri))
          (is (= ["user/foo is called with 2 args but expects 1 or 3"
                  "user/bar is called with 1 arg but expects 0"
                  "user/bar is called with 1 arg but expects 0"
                  "user/bar is called with 3 args but expects 0"
                  "user/foo is called with 2 args but expects 1 or 3"
                  "user/bar is called with 1 arg but expects 0"]
                 (map :message diagnostics))))))
    (testing "with annotations"
      (let [code "(defn foo {:added \"1.0\"} [x] (inc x))
                  (defn ^:private bar ^String [^Class x & rest] (str x rest))
                  (foo foo)
                  (foo foo foo)
                  (bar :a)
                  (bar :a :b)"
            mock-diagnostics-chan (async/chan 1)]
        (h/load-code code h/default-uri (assoc (h/make-components)
                                               :diagnostics-chan mock-diagnostics-chan))
        (let [{:keys [uri diagnostics]} (h/take-or-timeout mock-diagnostics-chan 500)]
          (is (= "file:///a.clj" uri))
          (is (= ["user/foo is called with 2 args but expects 1"]
                 (map :message diagnostics))))))
    (testing "for schema defs"
      (let [code "(ns user (:require [schema.core :as s]))
                  (s/defn foo :- s/Str
                    [x :- Long y :- Long]
                    (str x y))
                  (foo)
                  (foo 1 2)
                  (foo 1)"
            mock-diagnostics-chan (async/chan 1)]
        (h/load-code code h/default-uri (assoc (h/make-components)
                                               :diagnostics-chan mock-diagnostics-chan))
        (let [{:keys [uri diagnostics]} (h/take-or-timeout mock-diagnostics-chan 500)]
          (is (= "file:///a.clj" uri))
          (is (= ["user/foo is called with 0 args but expects 2"
                  "user/foo is called with 1 arg but expects 2"]
                 (map :message diagnostics)))))))
  (testing "custom unused namespace declaration"
    (let [mock-diagnostics-chan (async/chan 1)]
      (h/load-code "(ns foo.bar)" (h/file-uri "file:///foo/bar.clj") (assoc (h/make-components)
                                                                            :diagnostics-chan mock-diagnostics-chan))
      (let [{:keys [uri diagnostics]} (h/take-or-timeout mock-diagnostics-chan 500)]
        (is (= "file:///foo/bar.clj" uri))
        (is (= [] diagnostics))))))
