(ns clojure-lsp.features.diagnostics-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(deftest lint-project-public-vars
  (h/load-code-and-locs "(ns some-ns) (defn foo [a b] (+ a b))")
  (h/load-code-and-locs "(ns some-ns) (defn -main [& _args] 1)" (h/file-uri "file:///b.clj"))
  (testing "when linter level is :off"
    (swap! db/db merge {:settings {:linters {:unused-public-var {:level :off}}}})
    (is (= []
           (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") @db/db))))
  (testing "when linter level is :info"
    (swap! db/db merge {:settings {:linters {:unused-public-var {:level :info}}}})
    (h/assert-submaps
     [{:range {:start {:line 0 :character 19} :end {:line 0 :character 22}}
       :message "Unused public var 'some-ns/foo'"
       :code "unused-public-var"
       :tags [1]
       :severity 3
       :source "clojure-lsp"}]
     (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") @db/db)))
  (testing "when linter level is :warning"
    (swap! db/db merge {:settings {:linters {:unused-public-var {:level :warning}}}})
    (h/assert-submaps
     [{:range {:start {:line 0 :character 19} :end {:line 0 :character 22}}
       :message "Unused public var 'some-ns/foo'"
       :code "unused-public-var"
       :tags [1]
       :severity 2
       :source "clojure-lsp"}]
     (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") @db/db)))
  (testing "when linter level is :error"
    (swap! db/db merge {:settings {:linters {:unused-public-var {:level :error}}}})
    (h/assert-submaps
     [{:range {:start {:line 0 :character 19} :end {:line 0 :character 22}}
       :message "Unused public var 'some-ns/foo'"
       :code "unused-public-var"
       :tags [1]
       :severity 1
       :source "clojure-lsp"}]
     (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") @db/db)))
  (testing "linter level by default is :info"
    (swap! db/db merge {:settings {}})
    (h/assert-submaps
     [{:range {:start {:line 0 :character 19} :end {:line 0 :character 22}}
       :message "Unused public var 'some-ns/foo'"
       :code "unused-public-var"
       :tags [1]
       :severity 3
       :source "clojure-lsp"}]
     (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") @db/db)))
  (testing "excluding the whole ns"
    (swap! db/db merge {:settings {:linters {:unused-public-var {:exclude #{'some-ns}}}}})
    (h/assert-submaps
     []
     (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") @db/db)))
  (testing "excluding the simple var from ns"
    (swap! db/db merge {:settings {:linters {:unused-public-var {:exclude #{'foo}}}}})
    (h/assert-submaps
     []
     (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") @db/db)))
  (testing "excluding the specific function"
    (swap! db/db merge {:settings {:linters {:unused-public-var {:exclude #{'some-ns/foo}}}}})
    (h/assert-submaps
     []
     (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") @db/db)))
  (testing "excluding specific syms"
    (swap! db/db merge {})
    (h/assert-submaps
      []
     (#'f.diagnostic/find-diagnostics (h/file-uri "file:///b.clj") @db/db))))

(deftest lint-clj-kondo-findings
  (h/load-code-and-locs "(ns some-ns) (defn ^:private foo [a b] (+ a b))")
  (h/load-code-and-locs "(ns other-ns) (assert )" (h/file-uri "file:///b.clj"))
  (testing "when linter level is :off"
    (swap! db/db merge {:settings {:linters {:clj-kondo {:level :off}}}})
    (is (= []
           (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") @db/db))))
  (testing "when linter level is not :off"
    (swap! db/db merge {:settings {:linters {:clj-kondo {:level :error}}}})
    (h/assert-submaps [{:range {:start {:line 0 :character 29} :end {:line 0 :character 32}}
                        :message "Unused private var some-ns/foo"
                        :code "unused-private-var"
                        :tags [1]
                        :severity 2
                        :source "clj-kondo"}]
                      (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") @db/db)))
  (testing "when linter is not specified"
    (swap! db/db merge {:settings {}})
    (h/assert-submaps [{:range {:start {:line 0 :character 29} :end {:line 0 :character 32}}
                        :message "Unused private var some-ns/foo"
                        :code "unused-private-var"
                        :tags [1]
                        :severity 2
                        :source "clj-kondo"}]
                      (#'f.diagnostic/find-diagnostics (h/file-uri "file:///a.clj") @db/db)))
  (testing "when inside expression?"
    (swap! db/db merge {:settings {}})
    (h/assert-submaps [{:range {:start {:line 0 :character 14} :end {:line 0 :character 23}}
                        :message "clojure.core/assert is called with 0 args but expects 1 or 2"
                        :code "invalid-arity"
                        :tags []
                        :severity 1
                        :source "clj-kondo"}]
                      (#'f.diagnostic/find-diagnostics (h/file-uri "file:///b.clj") @db/db))))
