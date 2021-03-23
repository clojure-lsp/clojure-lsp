(ns clojure-lsp.features.workspace-symbols-test
  (:require
   [clojure.test :refer [deftest testing]]
   [clojure-lsp.test-helper :as h]
   [clojure-lsp.feature.workspace-symbols :as f.workspace-symbols]))

(h/reset-db-after-test)

(deftest workspace-symbols
  (h/load-code-and-locs (h/code "(ns foo.bar.ns (:require [clojure.string :as string]))"
                                "(defonce my-atom (atom {}))"
                                "(def bar 1)"
                                "(defn baz [a b] bar)"))
  (testing "querying all symbols"
    (h/assert-submaps
     [{:name "foo.bar.ns"
       :kind :namespace
       :location
       {:uri "file:///a.clj"
        :range {:start {:line 0 :character 0} :end {:line 0 :character 14}}}}
      {:name "my-atom"
       :kind :variable
       :location
       {:uri "file:///a.clj"
        :range {:start {:line 1 :character 0} :end {:line 1 :character 27}}}}
      {:name "bar"
       :kind :variable
       :location
       {:uri "file:///a.clj"
        :range {:start {:line 2 :character 0} :end {:line 2 :character 11}}}}
      {:name "baz"
       :kind :function
       :location
       {:uri "file:///a.clj"
        :range {:start {:line 3 :character 0} :end {:line 3 :character 20}}}}]
     (f.workspace-symbols/workspace-symbols "")))
  (testing "querying a specific function using fuzzy search"
    (h/assert-submaps
     [{:name "bar"
       :kind :variable
       :location
       {:uri "file:///a.clj"
        :range {:start {:line 2 :character 0} :end {:line 2 :character 11}}}}
      {:name "baz"
       :kind :function
       :location
       {:uri "file:///a.clj"
        :range {:start {:line 3 :character 0} :end {:line 3 :character 20}}}}
      {:name "foo.bar.ns"
       :kind :namespace
       :location
       {:uri "file:///a.clj"
        :range {:start {:line 0 :character 0} :end {:line 0 :character 14}}}}]
     (f.workspace-symbols/workspace-symbols "ba"))))
