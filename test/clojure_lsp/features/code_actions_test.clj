(ns clojure-lsp.features.code-actions-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.code-actions :as f.code-actions]
    [clojure-lsp.parser :as parser]
    [clojure.string :as string]
    [clojure.test :refer [deftest testing is]]
    [clojure-lsp.test-helper :as h]))

(h/reset-db-after-test)

(defn zloc-at [file row col]
  (-> @db/db
      (get-in [:documents file])
      :text
      (parser/loc-at-pos row col)))

(deftest add-alias-suggestion-code-actions
  (h/load-code-and-locs "(ns clojure.set)" "file:///clojure.core.clj")
  (h/load-code-and-locs "(ns medley.core)" "file:///medley.core.clj")
  (h/load-code-and-locs "(ns clojure.data.json)" "file:///clojure.data.json.clj")
  (h/load-code-and-locs "(ns some (:require [chesire :as json]))" "file:///some.clj")
  (h/load-code-and-locs (h/code "(ns some)"
                                "(clojure.set/union #{} #{})"
                                "(medley.core/foo 1 2)"
                                "(clojure.data.json/bar 1 2)"))
  (testing "simple ns"
    (is (some #(= (:title %) "Add require 'clojure.set' as 'set'")
              (f.code-actions/all (zloc-at "file:///a.clj" 2 4)
                                  "file:///a.clj"
                                  2
                                  4
                                  [{:code "unresolved-namespace"
                                    :range {:start {:line 1 :character 3}}}] {}))))
  (testing "core ns"
    (is (some #(= (:title %) "Add require 'medley.core' as 'medley'")
              (f.code-actions/all (zloc-at "file:///a.clj" 3 4)
                                  "file:///a.clj"
                                  3
                                  4
                                  [{:code "unresolved-namespace"
                                    :range {:start {:line 2 :character 3}}}] {}))))
  (testing "already used alias, we add one more suggestion"
    (let [result (f.code-actions/all (zloc-at "file:///a.clj" 4 4)
                                       "file:///a.clj"
                                       4
                                       4
                                       [{:code "unresolved-namespace"
                                         :range {:start {:line 3 :character 3}}}] {})]
      (is (some #(= (:title %) "Add require 'clojure.data.json' as 'json'") result))
      (is (some #(= (:title %) "Add require 'clojure.data.json' as 'data.json'") result)))))

(deftest add-missing-namespace-code-actions
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
  (testing "when it has not unresolved-namespace diagnostic"
    (is (not-any? #(string/starts-with? (:title %) "Add missing")
                  (f.code-actions/all (zloc-at "file://c.clj" 2 10)
                                      "file://c.clj"
                                      2
                                      10
                                      [] {}))))
  (testing "when it has unresolved-namespace and can find namespace"
    (is (some #(= (:title %) "Add missing 'some-ns' require")
              (f.code-actions/all (zloc-at "file://c.clj" 3 11)
                                  "file://c.clj"
                                  3
                                  11
                                  [{:code "unresolved-namespace"
                                    :range {:start {:line 2 :character 10}}}] {}))))
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

(deftest add-common-missing-import-code-action
  (h/load-code-and-locs (str "(ns another-ns)\n"
                             "(def bar ons/bar)\n"
                             "(def foo sns/foo)\n"
                             "(deftest some-test)\n"
                             "MyClass.\n"
                             "Date.\n"
                             "Date/parse")
                        "file://c.clj")
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

(deftest inline-symbol-code-action
  (h/load-code-and-locs (str "(ns other-ns (:require [some-ns :as sns]))\n"
                             "(def bar 1)\n"
                             "(defn baz []\n"
                             "  bar)")
                        "file://b.clj")
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

(deftest cycle-privacy-code-action
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        "file://a.clj")
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

(deftest extract-function-code-action
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        "file://a.clj")
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

(deftest clean-ns-code-actions
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
                                  [] {:workspace {:workspace-edit true}})))))
