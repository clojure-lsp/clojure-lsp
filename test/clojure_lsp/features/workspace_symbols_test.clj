(ns clojure-lsp.features.workspace-symbols-test
  (:require
    [clojure-lsp.feature.workspace-symbols :as f.workspace-symbols]
    [clojure-lsp.test-helper :as h]
    [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(deftest workspace-symbols
  (h/load-code-and-locs (h/code "(ns foo.alpaca.ns (:require [clojure.string :as string]))"
                                "(defonce my-alpapapaca (atom {}))"
                                "(def alpac 1)"
                                "(defn alpacas [a b] alpac)"))
  (h/load-code-and-locs (h/code "(ns foo.goat.ns (:require [foo.alpaca.ns :as a]))"
                                "(defn goats-from-alpacas [alpacas] (map inc alpacas))")
                        (h/file-uri "file:///b.clj"))
  (testing "querying all symbols"
    (is (= [{:name "foo.alpaca.ns"
             :kind :namespace
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 0 :character 0} :end {:line 0 :character 17}}}}
            {:name "my-alpapapaca"
             :kind :variable
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 1 :character 0} :end {:line 1 :character 33}}}}
            {:name "alpac"
             :kind :variable
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 2 :character 0} :end {:line 2 :character 13}}}}
            {:name "alpacas"
             :kind :function
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 3 :character 0} :end {:line 3 :character 26}}}}
            {:kind :namespace,
             :location {:range {:end {:character 15, :line 0}, :start {:character 0, :line 0}},
                        :uri (h/file-uri "file:///b.clj")},
             :name "foo.goat.ns"}
            {:kind :function,
             :location {:range {:end {:character 53, :line 1}, :start {:character 0, :line 1}},
                        :uri (h/file-uri "file:///b.clj")},
             :name "goats-from-alpacas"}]
           (f.workspace-symbols/workspace-symbols ""))))
  (testing "querying a specific function using fuzzy search"
    (is (= [{:name "foo.alpaca.ns"
             :kind :namespace
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 0 :character 0} :end {:line 0 :character 17}}}}
            {:name "alpacas"
             :kind :function
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 3 :character 0} :end {:line 3 :character 26}}}}
            {:name "my-alpapapaca"
             :kind :variable
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 1 :character 0} :end {:line 1 :character 33}}}}
            {:kind :function,
             :location {:range {:end {:character 53, :line 1}, :start {:character 0, :line 1}},
                        :uri (h/file-uri "file:///b.clj")},
             :name "goats-from-alpacas"}]
           (f.workspace-symbols/workspace-symbols "alpaca")))))
