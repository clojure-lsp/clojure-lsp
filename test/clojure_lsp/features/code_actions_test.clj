(ns clojure-lsp.features.code-actions-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.code-actions :as f.code-actions]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.test-helper :as h]
    [clojure.string :as string]
    [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(defn zloc-at [file row col]
  (-> @db/db
      (get-in [:documents file])
      :text
      (parser/loc-at-pos row col)))

(deftest add-alias-suggestion-code-actions
  (h/load-code-and-locs "(ns clojure.set)" (h/file-uri "file:///clojure.core.clj"))
  (h/load-code-and-locs "(ns medley.core)" (h/file-uri "file:///medley.core.clj"))
  (h/load-code-and-locs "(ns clojure.data.json)" (h/file-uri "file:///clojure.data.json.clj"))
  (h/load-code-and-locs "(ns some (:require [chesire :as json]))" (h/file-uri "file:///some.clj"))
  (h/load-code-and-locs (h/code "(ns some)"
                                "(clojure.set/union #{} #{})"
                                "(medley.core/foo 1 2)"
                                "(clojure.data.json/bar 1 2)"))
  (testing "simple ns"
    (is (some #(= (:title %) "Add require 'clojure.set' as 'set'")
              (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 2 4)
                                  (h/file-uri "file:///a.clj")
                                  2
                                  4
                                  [{:code "unresolved-namespace"
                                    :range {:start {:line 1 :character 3}}}] {}))))
  (testing "core ns"
    (is (some #(= (:title %) "Add require 'medley.core' as 'medley'")
              (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 3 4)
                                  (h/file-uri "file:///a.clj")
                                  3
                                  4
                                  [{:code "unresolved-namespace"
                                    :range {:start {:line 2 :character 3}}}] {}))))
  (testing "already used alias, we add one more suggestion"
    (let [result (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 4 4)
                                     (h/file-uri "file:///a.clj")
                                     4
                                     4
                                     [{:code "unresolved-namespace"
                                       :range {:start {:line 3 :character 3}}}] {})]
      (is (some #(= (:title %) "Add require 'clojure.data.json' as 'json'") result))
      (is (some #(= (:title %) "Add require 'clojure.data.json' as 'data.json'") result)))))

(deftest add-missing-namespace-code-actions
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        (h/file-uri "file:///a.clj"))
  (h/load-code-and-locs (str "(ns other-ns (:require [some-ns :as sns]))\n"
                             "(def bar 1)\n"
                             "(defn baz []\n"
                             "  bar)")
                        (h/file-uri "file:///b.clj"))
  (h/load-code-and-locs (str "(ns another-ns)\n"
                             "(def bar ons/bar)\n"
                             "(def foo sns/foo)\n"
                             "(deftest some-test)\n"
                             "MyClass.\n"
                             "Date.\n"
                             "Date/parse")
                        (h/file-uri "file:///c.clj"))
  (testing "when it has not unresolved-namespace diagnostic"
    (is (not-any? #(string/starts-with? (:title %) "Add missing")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///c.clj") 2 10)
                                      (h/file-uri "file:///c.clj")
                                      2
                                      10
                                      [] {}))))
  (testing "when it has unresolved-namespace and can find namespace"
    (is (some #(= (:title %) "Add missing 'some-ns' require")
              (f.code-actions/all (zloc-at (h/file-uri "file:///c.clj") 3 11)
                                  (h/file-uri "file:///c.clj")
                                  3
                                  11
                                  [{:code "unresolved-namespace"
                                    :range {:start {:line 2 :character 10}}}] {}))))
  (testing "when it has unresolved-namespace but cannot find namespace"
    (is (not-any? #(string/starts-with? (:title %) "Add missing")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///c.clj") 2 11)
                                      (h/file-uri "file:///c.clj")
                                      2
                                      11
                                      [{:code "unresolved-namespace"
                                        :range {:start {:line 1 :character 10}}}] {}))))
  (testing "when it has unresolved-symbol and it's a known refer"
    (is (some #(= (:title %) "Add missing 'clojure.test' require")
              (f.code-actions/all (zloc-at (h/file-uri "file:///c.clj") 4 2)
                                  (h/file-uri "file:///c.clj")
                                  4
                                  2
                                  [{:code "unresolved-namespace"
                                    :range {:start {:line 3 :character 1}}}] {}))))
  (testing "when it has unresolved-symbol but it's not a known refer"
    (is (not-any? #(string/starts-with? (:title %) "Add missing")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///c.clj") 4 11)
                                      (h/file-uri "file:///c.clj")
                                      4
                                      11
                                      [{:code "unresolved-symbol"
                                        :message "Unresolved symbol: foo"
                                        :range {:start {:line 3 :character 15}}}] {})))))

(deftest add-common-missing-import-code-action
  (h/load-code-and-locs (str "(ns another-ns)\n"
                             "(def bar ons/bar)\n"
                             "(def foo sns/foo)\n"
                             "(deftest some-test)\n"
                             "MyClass.\n"
                             "Date.\n"
                             "Date/parse")
                        (h/file-uri "file:///c.clj"))
  (testing "when it has no unresolved-symbol diagnostic"
    (is (not-any? #(string/starts-with? (:title %) "Add missing")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///c.clj") 5 2)
                                      (h/file-uri "file:///c.clj")
                                      5
                                      2
                                      [] {}))))

  (testing "when it has unresolved-symbol but it's not a common import"
    (is (not-any? #(string/starts-with? (:title %) "Add missing")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///c.clj") 5 2)
                                      (h/file-uri "file:///c.clj")
                                      5
                                      2
                                      [{:code "unresolved-symbol"
                                        :message "Unresolved symbol: foo"
                                        :range {:start {:line 4 :character 2}}}] {}))))
  (testing "when it has unresolved-symbol and it's a common import"
    (is (some #(= (:title %) "Add missing 'java.util.Date' import")
              (f.code-actions/all (zloc-at (h/file-uri "file:///c.clj") 6 2)
                                  (h/file-uri "file:///c.clj")
                                  6
                                  2
                                  [{:code "unresolved-symbol"
                                    :message "Unresolved symbol: foo"
                                    :range {:start {:line 5 :character 2}}}] {}))))
  (testing "when it has unresolved-namespace and it's a common import via method"
    (is (some #(= (:title %) "Add missing 'java.util.Date' import")
              (f.code-actions/all (zloc-at (h/file-uri "file:///c.clj") 7 2)
                                  (h/file-uri "file:///c.clj")
                                  7
                                  2
                                  [{:code "unresolved-namespace"
                                    :range {:start {:line 6 :character 2}}}] {})))))

(deftest inline-symbol-code-action
  (h/load-code-and-locs (str "(ns other-ns (:require [some-ns :as sns]))\n"
                             "(def bar 1)\n"
                             "(defn baz []\n"
                             "  bar)")
                        (h/file-uri "file:///b.clj"))
  (testing "when in not a let/def symbol"
    (is (not-any? #(= (:title %) "Inline symbol")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///b.clj") 4 8)
                                      (h/file-uri "file:///b.clj")
                                      4
                                      8
                                      [] {}))))
  (testing "when in let/def symbol"
    (is (some #(= (:title %) "Inline symbol")
              (f.code-actions/all (zloc-at (h/file-uri "file:///b.clj") 4 5)
                                  (h/file-uri "file:///b.clj")
                                  4
                                  5
                                  [] {})))))

(deftest change-coll-code-action
  (h/load-code-and-locs (h/code "\"some string\""
                                "(some-function 1 2)"
                                "{:some :map}"
                                "[:some :vector]"
                                "#{:some :set}")
                        (h/file-uri "file:///b.clj"))
  (testing "when in not a coll"
    (is (not-any? #(= (:title %) "Change coll to")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///b.clj") 1 1)
                                      (h/file-uri "file:///b.clj")
                                      1
                                      1
                                      [] {}))))
  (testing "when in a list"
    (is (some #(= (:title %) "Change coll to map")
              (f.code-actions/all (zloc-at (h/file-uri "file:///b.clj") 2 1) (h/file-uri "file:///b.clj") 2 1 [] {}))))
  (testing "when in a map"
    (is (some #(= (:title %) "Change coll to vector")
              (f.code-actions/all (zloc-at (h/file-uri "file:///b.clj") 3 1) (h/file-uri "file:///b.clj") 3 1 [] {}))))
  (testing "when in a vector"
    (is (some #(= (:title %) "Change coll to set")
              (f.code-actions/all (zloc-at (h/file-uri "file:///b.clj") 4 1) (h/file-uri "file:///b.clj") 4 1 [] {}))))
  (testing "when in a set"
    (is (some #(= (:title %) "Change coll to list")
              (f.code-actions/all (zloc-at (h/file-uri "file:///b.clj") 5 1) (h/file-uri "file:///b.clj") 5 1 [] {})))))

(deftest move-to-let-code-action
  (h/load-code-and-locs (h/code "(let [a 1"
                                "      b 2]"
                                "  (+ 1 2))"
                                "(+ 1 2)")
                        (h/file-uri "file:///b.clj"))
  (testing "when not inside a let form"
    (is (not-any? #(= (:title %) "Move to let")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///b.clj") 4 1)
                                      (h/file-uri "file:///b.clj")
                                      4
                                      1
                                      [] {}))))
  (testing "when inside let form"
    (is (some #(= (:title %) "Move to let")
              (f.code-actions/all (zloc-at (h/file-uri "file:///b.clj") 3 3)
                                  (h/file-uri "file:///b.clj")
                                  3
                                  3
                                  [] {})))))

(deftest cycle-privacy-code-action
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        (h/file-uri "file:///a.clj"))
  (testing "when non function location"
    (is (not-any? #(= (:title %) "Cycle privacy")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 1 5)
                                      (h/file-uri "file:///a.clj")
                                      1
                                      5
                                      [] {}))))
  (testing "when on function location"
    (is (some #(= (:title %) "Cycle privacy")
              (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 2 5)
                                  (h/file-uri "file:///a.clj")
                                  2
                                  5
                                  [] {})))))

(deftest extract-function-code-action
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        (h/file-uri "file:///a.clj"))
  (testing "when non function location"
    (is (not-any? #(= (:title %) "Extract function")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 1 5)
                                      (h/file-uri "file:///a.clj")
                                      1
                                      5
                                      [] {}))))
  (testing "when on function location"
    (is (some #(= (:title %) "Extract function")
              (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 2 5)
                                  (h/file-uri "file:///a.clj")
                                  2
                                  5
                                  [] {})))))

(deftest create-private-function-code-action
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                "(def foo (+ 1 2))"
                                "(def bar (some-func 1 2))"))
  (testing "when not in a unresolved symbol"
    (is (not-any? #(= (:title %) "Create private function")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 1 10)
                                      (h/file-uri "file:///a.clj")
                                      1
                                      10
                                      [] {}))))
  (testing "when in a unresolved symbol"
    (is (some #(= (:title %) "Create private function 'some-func'")
              (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 2 10)
                                  (h/file-uri "file:///a.clj")
                                  2
                                  10
                                  [{:code "unresolved-symbol"
                                    :message "Unresolved symbol: some-func"
                                    :range {:start {:line 2 :character 11}}}] {})))))

(deftest thread-first-all-action
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                "(def foo)"
                                "(- (+ 1 1) 2)")
                        (h/file-uri "file:///a.clj"))
  (testing "when in a ns or :require"
    (is (not-any? #(= (:title %) "Thread first all")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 1 1) (h/file-uri "file:///a.clj") 1 1 [] {}))))
  (testing "when in a def similar location"
    (is (not-any? #(= (:title %) "Thread first all")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 2 1) (h/file-uri "file:///a.clj") 2 1 [] {}))))
  (testing "when on a valid function that can be threaded"
    (is (some #(= (:title %) "Thread first all")
              (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 3 1) (h/file-uri "file:///a.clj") 3 1 [] {})))))

(deftest thread-last-all-action
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                "(def foo)"
                                "(- (+ 1 1) 2)")
                        (h/file-uri "file:///a.clj"))
  (testing "when in a ns or :require"
    (is (not-any? #(= (:title %) "Thread last all")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 1 1) (h/file-uri "file:///a.clj") 1 1 [] {}))))
  (testing "when in a def similar location"
    (is (not-any? #(= (:title %) "Thread last all")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 2 1) (h/file-uri "file:///a.clj") 2 1 [] {}))))
  (testing "when on a valid function that can be threaded"
    (is (some #(= (:title %) "Thread last all")
              (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 3 1) (h/file-uri "file:///a.clj") 3 1 [] {})))))

(deftest clean-ns-code-actions
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        (h/file-uri "file:///a.clj"))
  (h/load-code-and-locs (str "(ns other-ns (:require [some-ns :as sns]))\n"
                             "(def bar 1)\n"
                             "(defn baz []\n"
                             "  bar)")
                        (h/file-uri "file:///b.clj"))
  (h/load-code-and-locs (str "(ns another-ns)\n"
                             "(def bar ons/bar)\n"
                             "(def foo sns/foo)\n"
                             "(deftest some-test)\n"
                             "MyClass.\n"
                             "Date.\n"
                             "Date/parse")
                        (h/file-uri "file:///c.clj"))
  (testing "without workspace edit client capability"
    (is (not-any? #(= (:title %) "Clean namespace")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///b.clj") 2 2)
                                      (h/file-uri "file:///b.clj")
                                      2
                                      2
                                      [] {}))))

  (testing "with workspace edit client capability"
    (swap! db/db assoc-in [:client-capabilities :workspace :workspace-edit] true)
    (is (some #(= (:title %) "Clean namespace")
              (f.code-actions/all (zloc-at (h/file-uri "file:///b.clj") 2 2)
                                  (h/file-uri "file:///b.clj")
                                  2
                                  2
                                  [] {:workspace {:workspace-edit true}})))))

(deftest resolve-macro-as-code-actions
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                "(defmacro foo [name & body] @body)"
                                "(foo my-fn)"
                                "(+ 1 2)"))
  (testing "when inside a macro usage"
    (is (some #(= (:title %) "Resolve macro 'some-ns/foo' as...")
              (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 3 7) (h/file-uri "file:///a.clj") 3 7 [] {}))))
  (testing "when not inside a macro usage"
    (is (not-any? #(= (:title %) "Resolve macro 'some-ns/foo' as...")
                  (f.code-actions/all (zloc-at (h/file-uri "file:///a.clj") 4 4) (h/file-uri "file:///a.clj") 4 4 [] {})))))
