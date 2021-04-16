(ns clojure-lsp.features.workspace-symbols-test
  (:require
    [clojure.test :refer [deftest testing is]]
    [clojure-lsp.test-helper :as h]
    [clojure-lsp.feature.workspace-symbols :as f.workspace-symbols]))

(h/reset-db-after-test)

(deftest workspace-symbols
  (h/load-code-and-locs (h/code "(ns foo.alpaca.ns (:require [clojure.string :as string]))"
                                "(defonce my-alpapapaca (atom {}))"
                                "(def alpac 1)"
                                "(defn alpacas [a b] alpac)"))
  (testing "querying all symbols"
    (is (= [{:name "foo.alpaca.ns"
             :kind :namespace
             :location
             {:uri "file:///a.clj"
              :range {:start {:line 0 :character 0} :end {:line 0 :character 17}}}}
            {:name "my-alpapapaca"
             :kind :variable
             :location
             {:uri "file:///a.clj"
              :range {:start {:line 1 :character 0} :end {:line 1 :character 33}}}}
            {:name "alpac"
             :kind :variable
             :location
             {:uri "file:///a.clj"
              :range {:start {:line 2 :character 0} :end {:line 2 :character 13}}}}
            {:name "alpacas"
             :kind :function
             :location
             {:uri "file:///a.clj"
              :range {:start {:line 3 :character 0} :end {:line 3 :character 26}}}}]
           (f.workspace-symbols/workspace-symbols ""))))
  (testing "querying a specific function using fuzzy search"
    (is (= [{:name "foo.alpaca.ns"
             :kind :namespace
             :location
             {:uri "file:///a.clj"
              :range {:start {:line 0 :character 0} :end {:line 0 :character 17}}}}
            {:name "alpacas"
             :kind :function
             :location
             {:uri "file:///a.clj"
              :range {:start {:line 3 :character 0} :end {:line 3 :character 26}}}}
            {:name "my-alpapapaca"
             :kind :variable
             :location
             {:uri "file:///a.clj"
              :range {:start {:line 1 :character 0} :end {:line 1 :character 33}}}}]
           (f.workspace-symbols/workspace-symbols "alpaca")))))
