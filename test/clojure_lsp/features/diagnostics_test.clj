(ns clojure-lsp.features.diagnostics-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest testing is]]))

(deftest lint-public-vars
  (h/load-code-and-locs "(ns some-ns) (defn foo [a b] (+ a b))")
  (testing "when linter level is :off"
    (is (= nil
           (#'f.diagnostic/lint-public-vars "file:///a.clj" (:analysis @db/db) {:linters {:unused-public-var {:level :off}}}))))
  (testing "when linter level is :info"
    (h/assert-submaps
     [{:range {:start {:line 0 :character 19} :end {:line 0 :character 22}}
       :message "Unused public var 'some-ns/foo'"
       :code "unused-public-var"
       :severity 3
       :source "clojure-lsp"}]
     (#'f.diagnostic/lint-public-vars "file:///a.clj" (:analysis @db/db) {:linters {:unused-public-var {:level :info}}})))
  (testing "when linter level is :warning"
    (h/assert-submaps
     [{:range {:start {:line 0 :character 19} :end {:line 0 :character 22}}
       :message "Unused public var 'some-ns/foo'"
       :code "unused-public-var"
       :severity 2
       :source "clojure-lsp"}]
     (#'f.diagnostic/lint-public-vars "file:///a.clj" (:analysis @db/db) {:linters {:unused-public-var {:level :warning}}})))
  (testing "when linter level is :error"
    (h/assert-submaps
     [{:range {:start {:line 0 :character 19} :end {:line 0 :character 22}}
       :message "Unused public var 'some-ns/foo'"
       :code "unused-public-var"
       :severity 1
       :source "clojure-lsp"}]
     (#'f.diagnostic/lint-public-vars "file:///a.clj" (:analysis @db/db) {:linters {:unused-public-var {:level :error}}})))
  (testing "linter level by default is :info"
    (h/assert-submaps
     [{:range {:start {:line 0 :character 19} :end {:line 0 :character 22}}
       :message "Unused public var 'some-ns/foo'"
       :code "unused-public-var"
       :severity 3
       :source "clojure-lsp"}]
     (#'f.diagnostic/lint-public-vars "file:///a.clj" (:analysis @db/db) {})))
  (testing "excluding the whole ns"
    (h/assert-submaps
     []
     (#'f.diagnostic/lint-public-vars "file:///a.clj" (:analysis @db/db) {:linters {:unused-public-var {:exclude #{'some-ns}}}})))
  (testing "excluding the specific function"
    (h/assert-submaps
     []
     (#'f.diagnostic/lint-public-vars "file:///a.clj" (:analysis @db/db) {:linters {:unused-public-var {:exclude #{'some-ns/foo}}}}))))
