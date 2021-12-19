(ns clojure-lsp.features.diagnostics-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(def findings (atom []))

(h/reset-db-after-test)

(deftest lint-project-public-vars
  (h/load-code-and-locs "(ns some-ns) (defn foo [a b] (+ a b))")
  (h/load-code-and-locs "(ns some-ns) (defn -main [& _args] 1)" (h/file-uri "file:///b.clj"))
  (testing "when linter level is :info"
    (reset! findings [])
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:level :info}}}}
      db/db)
    (h/assert-submaps
      [{:filename "/a.clj"
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
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:level :warning}}}}
      db/db)
    (h/assert-submaps
      [{:filename "/a.clj"
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
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:level :error}}}}
      db/db)
    (h/assert-submaps
      [{:filename "/a.clj"
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
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:level :off}}}}
      db/db)
    (h/assert-submaps
      [{:filename "/a.clj"
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
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
      {:reg-finding! #(swap! findings conj %)
       :config {}}
      db/db)
    (h/assert-submaps
      [{:filename "/a.clj"
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
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:exclude #{'some-ns}}}}}
      db/db)
    (h/assert-submaps
      []
      @findings))
  (testing "excluding the simple var from ns"
    (reset! findings [])
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:exclude #{'foo}}}}}
      db/db)
    (h/assert-submaps
      []
      @findings))
  (testing "excluding the specific var"
    (reset! findings [])
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:exclude #{'some-ns/foo}}}}}
      db/db)
    (h/assert-submaps
      []
      @findings))
  (testing "excluding specific syms"
    (reset! findings [])
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/b.clj"
      (:analysis @db/db)
      {:reg-finding! #(swap! findings conj %)
       :config {}}
      db/db)
    (h/assert-submaps
      []
      @findings)))

(deftest lint-clj-kondo-findings
  (h/load-code-and-locs "(ns some-ns) (defn ^:private foo [a b] (+ a b))")
  (h/load-code-and-locs "(ns other-ns) (assert )" (h/file-uri "file:///b.clj"))
  (testing "when linter level is :off"
    (swap! db/db shared/deep-merge {:settings {:linters {:clj-kondo {:level :off}}}})
    (is (= []
           (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") db/db))))
  (testing "when linter level is not :off but has matching :ns-exclude-regex"
    (h/load-code-and-locs "(ns some-ns) (defn ^:private foo [a b] (+ a b))" (h/file-uri "file:///project/src/a.clj"))
    (swap! db/db merge {:project-root-uri (h/file-uri "file:///project")
                        :settings {:source-paths ["/project/src"]
                                   :linters {:clj-kondo {:ns-exclude-regex "a.*"}}}})
    (is (= []
           (#'f.diagnostic/find-diagnostics (h/file-uri "file:///project/src/a.clj") db/db))))
  (testing "when linter level is not :off"
    (swap! db/db merge {:settings {:linters {:clj-kondo {:level :error}}}})
    (h/assert-submaps [{:range {:start {:line 0 :character 29} :end {:line 0 :character 32}}
                        :message "Unused private var some-ns/foo"
                        :code "unused-private-var"
                        :tags [1]
                        :severity 2
                        :source "clj-kondo"}]
                      (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") db/db)))
  (testing "when linter is not specified"
    (swap! db/db merge {:settings {}})
    (h/assert-submaps [{:range {:start {:line 0 :character 29} :end {:line 0 :character 32}}
                        :message "Unused private var some-ns/foo"
                        :code "unused-private-var"
                        :tags [1]
                        :severity 2
                        :source "clj-kondo"}]
                      (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") db/db)))
  (testing "when inside expression?"
    (swap! db/db merge {:settings {}})
    (h/assert-submaps [{:range {:start {:line 0 :character 14} :end {:line 0 :character 23}}
                        :message "clojure.core/assert is called with 0 args but expects 1 or 2"
                        :code "invalid-arity"
                        :tags []
                        :severity 1
                        :source "clj-kondo"}]
                      (#'f.diagnostic/find-diagnostics (h/file-uri "file:///b.clj") db/db))))

(deftest test-find-diagnostics
  (testing "wrong arity"
    (testing "for argument destructuring"
      (h/clean-db!)
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
        (h/with-mock-diagnostics
          (h/load-code-and-locs code)
          (is (= ["user/foo is called with 3 args but expects 1 or 2"
                  "user/baz is called with 1 arg but expects 3"
                  "user/bar is called with 0 args but expects 1 or more"
                  "user/foo is called with 3 args but expects 1 or 2"
                  "user/foo is called with 0 args but expects 1 or 2"
                  "user/foo is called with 4 args but expects 1 or 2"]
                 (map :message (get @h/mock-diagnostics "file:///a.clj")))))))
    (testing "for threading macros"
      (h/clean-db!)
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
        (h/with-mock-diagnostics
          (h/load-code-and-locs code)
          (is (= ["user/foo is called with 2 args but expects 1 or 3"
                  "user/bar is called with 1 arg but expects 0"
                  "user/bar is called with 1 arg but expects 0"
                  "user/bar is called with 3 args but expects 0"
                  "user/foo is called with 2 args but expects 1 or 3"
                  "user/bar is called with 1 arg but expects 0"]
                 (map :message (get @h/mock-diagnostics "file:///a.clj")))))))
    (testing "with annotations"
      (h/clean-db!)
      (let [code "(defn foo {:added \"1.0\"} [x] (inc x))
                  (defn ^:private bar ^String [^Class x & rest] (str x rest))
                  (foo foo)
                  (foo foo foo)
                  (bar :a)
                  (bar :a :b)"]
        (h/with-mock-diagnostics
          (h/load-code-and-locs code)
          (is (= ["user/foo is called with 2 args but expects 1"]
                 (map :message (get @h/mock-diagnostics "file:///a.clj")))))))
    (testing "for schema defs"
      (h/clean-db!)
      (let [code "(ns user (:require [schema.core :as s]))
                  (s/defn foo :- s/Str
                    [x :- Long y :- Long]
                    (str x y))
                  (foo)
                  (foo 1 2)
                  (foo 1)"]
        (h/with-mock-diagnostics
          (h/load-code-and-locs code)
          (is (= ["user/foo is called with 0 args but expects 2"
                  "user/foo is called with 1 arg but expects 2"]
                 (map :message (get @h/mock-diagnostics "file:///a.clj"))))))))
  (testing "custom unused namespace declaration"
    (h/clean-db!)
    (h/with-mock-diagnostics
      (h/load-code-and-locs "(ns foo.bar)")
      (is (empty?
            (map :message (get @h/mock-diagnostics "file:///a.clj")))))))
