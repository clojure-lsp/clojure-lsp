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
      ;; TODO kondo
      (testing "when it has unresolved-namespace and can find namespace"
        (is (some #(= (:title %) "Add missing 'some-ns' require")
                  (f.code-actions/all (zloc-at "file://c.clj" 3 11)
                                      "file://c.clj"
                                      3
                                      11
                                      [{:code "unresolved-namespace"
                                        :range {:start {:line 2 :character 10}}}] {})))))
