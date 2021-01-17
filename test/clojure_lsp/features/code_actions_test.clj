(ns clojure-lsp.features.code-actions-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.code-actions :as f.code-actions]
    [clojure-lsp.parser :as parser]
    [clojure.string :as string]
    [clojure.test :refer [deftest testing is]]
    [clojure-lsp.test-helper :as h]))

(defn zloc-at [file row col]
  (-> @db/db
      (get-in [:documents file])
      :text
      (parser/loc-at-pos row col)))

(deftest test-code-actions-without-resolve-support
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        "file://a.clj")
  (h/load-code-and-locs (str "(ns other-ns (:require [some-ns :as sns]))\n"
                             "(def bar 1)\n"
                             "(defn baz []\n"
                             "  bar)")
                        "file://b.clj")
  (h/load-code-and-locs (str "(ns another-ns)\n"
                             "(def bar ons/bar)\n"
                             "(def foo sns/foo)\n"
                             "(deftest some-test)\n"
                             "MyClass.\n"
                             "Date.\n"
                             "Date/parse")
                        "file://c.clj")
    (testing "Add missing namespace"
      (testing "when it has not unresolved-namespace diagnostic"
        (is (not-any? #(string/starts-with? (:title %) "Add missing")
                      (f.code-actions/all (zloc-at "file://c.clj" 2 10)
                                          "file://c.clj"
                                          2
                                          10
                                          [] {}))))

      (testing "when it has unresolved-namespace but cannot find namespace"
        (is (not-any? #(string/starts-with? (:title %) "Add missing")
                      (f.code-actions/all (zloc-at "file://c.clj" 2 11)
                                          "file://c.clj"
                                          2
                                          11
                                          [{:code "unresolved-namespace"
                                            :range {:start {:line 1 :character 10}}}] {}))))
      (testing "when it has unresolved-symbol and it's a known refer"
        (is (some #(= (:title %) "Add missing 'clojure.test' require")
                  (f.code-actions/all (zloc-at "file://c.clj" 4 2)
                                      "file://c.clj"
                                      4
                                      2
                                      [{:code "unresolved-namespace"
                                        :range {:start {:line 3 :character 1}}}] {}))))
      (testing "when it has unresolved-symbol but it's not a known refer"
        (is (not-any? #(string/starts-with? (:title %) "Add missing")
                      (f.code-actions/all (zloc-at "file://c.clj" 4 11)
                                          "file://c.clj"
                                          4
                                          11
                                          [{:code "unresolved-symbol"
                                            :range {:start {:line 3 :character 15}}}] {})))))
    (testing "Add common missing import"
      (testing "when it has no unknown-symbol diagnostic"
        (is (not-any? #(string/starts-with? (:title %) "Add missing")
                      (f.code-actions/all (zloc-at "file://c.clj" 5 2)
                                          "file://c.clj"
                                          5
                                          2
                                          [] {}))))

      (testing "when it has unknown-symbol but it's not a common import"
        (is (not-any? #(string/starts-with? (:title %) "Add missing")
                      (f.code-actions/all (zloc-at "file://c.clj" 5 2)
                                          "file://c.clj"
                                          5
                                          2
                                          [{:code "unresolved-symbol"
                                            :range {:start {:line 4 :character 2}}}] {}))))
      (testing "when it has unknown-symbol and it's a common import"
        (is (some #(= (:title %) "Add missing 'java.util.Date' import")
                  (f.code-actions/all (zloc-at "file://c.clj" 6 2)
                                      "file://c.clj"
                                      6
                                      2
                                      [{:code "unresolved-symbol"
                                        :range {:start {:line 5 :character 2}}}] {}))))
      (testing "when it has unresolved-namespace and it's a common import via method"
        (is (some #(= (:title %) "Add missing 'java.util.Date' import")
                  (f.code-actions/all (zloc-at "file://c.clj" 7 2)
                                      "file://c.clj"
                                      7
                                      2
                                      [{:code "unresolved-namespace"
                                        :range {:start {:line 6 :character 2}}}] {})))))
    (testing "Inline symbol"
      (testing "when in not a let/def symbol"
        (is (not-any? #(= (:title %) "Inline symbol")
                      (f.code-actions/all (zloc-at "file://b.clj" 4 8)
                                          "file://b.clj"
                                          4
                                          8
                                          [] {}))))
      (testing "when in let/def symbol"
        (is (some #(= (:title %) "Inline symbol")
                  (f.code-actions/all (zloc-at "file://b.clj" 4 5)
                                      "file://b.clj"
                                      4
                                      5
                                      [] {})))))
    (testing "Cycle privacy"
      (testing "when non function location"
        (is (not-any? #(= (:title %) "Cycle privacy")
                      (f.code-actions/all (zloc-at "file://a.clj" 1 5)
                                          "file://a.clj"
                                          1
                                          5
                                          [] {}))))
      (testing "when on function location"
        (is (some #(= (:title %) "Cycle privacy")
                  (f.code-actions/all (zloc-at "file://a.clj" 2 5)
                                      "file://a.clj"
                                      2
                                      5
                                      [] {})))))
    (testing "Extract function"
      (testing "when non function location"
        (is (not-any? #(= (:title %) "Extract function")
                      (f.code-actions/all (zloc-at "file://a.clj" 1 5)
                                          "file://a.clj"
                                          1
                                          5
                                          [] {}))))
      (testing "when on function location"
        (is (some #(= (:title %) "Extract function")
                  (f.code-actions/all (zloc-at "file://a.clj" 2 5)
                                      "file://a.clj"
                                      2
                                      5
                                      [] {})))))
    (testing "clean namespace"
      (testing "without workspace edit client capability"
        (is (not-any? #(= (:title %) "Clean namespace")
                      (f.code-actions/all (zloc-at "file://b.clj" 2 2)
                                          "file://b.clj"
                                          2
                                          2
                                          [] {}))))

      (testing "with workspace edit client capability"
        (swap! db/db assoc-in [:client-capabilities :workspace :workspace-edit] true)
        (is (some #(= (:title %) "Clean namespace")
                  (f.code-actions/all (zloc-at "file://b.clj" 2 2)
                                      "file://b.clj"
                                      2
                                      2
                                      [] {:workspace {:workspace-edit true}}))))))
