(ns clojure-lsp.features.diagnostics-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
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
      @findings))
  (testing "when linter level is :warning"
    (reset! findings [])
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
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
      @findings))
  (testing "when linter level is :error"
    (reset! findings [])
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
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
      @findings))
  (testing "when linter level is :off"
    (reset! findings [])
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
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
      @findings))
  (testing "linter level by default is :info"
    (reset! findings [])
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
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
      @findings))
  (testing "excluding the whole ns"
    (reset! findings [])
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:exclude #{'some-ns}}}}})
    (h/assert-submaps
      []
      @findings))
  (testing "excluding the simple var from ns"
    (reset! findings [])
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:exclude #{'foo}}}}})
    (h/assert-submaps
      []
      @findings))
  (testing "excluding the specific var"
    (reset! findings [])
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/a.clj"
      (:analysis @db/db)
      {:reg-finding! #(swap! findings conj %)
       :config {:linters {:clojure-lsp/unused-public-var {:exclude #{'some-ns/foo}}}}})
    (h/assert-submaps
      []
      @findings))
  (testing "excluding specific syms"
    (reset! findings [])
    (f.diagnostic/unused-public-var-lint-for-single-file!
      "/b.clj"
      (:analysis @db/db)
      {:reg-finding! #(swap! findings conj %)
       :config {}})
    (h/assert-submaps
      []
      @findings)))

(deftest lint-clj-kondo-findings
  (h/load-code-and-locs "(ns some-ns) (defn ^:private foo [a b] (+ a b))")
  (h/load-code-and-locs "(ns other-ns) (assert )" (h/file-uri "file:///b.clj"))
  (testing "when linter level is :off"
    (swap! db/db merge {:settings {:linters {:clj-kondo {:level :off}}}})
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
