(ns clojure-lsp.features.completion-test
  (:require
    [clojure-lsp.feature.completion :as f.completion]
    [clojure-lsp.test-helper :as h]
    [clojure.string :as string]
    [clojure.test :refer [deftest testing is]]
    [clojure-lsp.db :as db]))

(h/reset-db-after-test)

(defn code [& strings] (string/join "\n" strings))

(deftest test-completion
  (reset! db/db {})
  (h/load-code-and-locs (code "(ns alpaca.ns (:require [user :as alpaca]))"
                              "(def barr)"
                              "(def bazz)") "file:///a.cljc")
  (h/load-code-and-locs (code "(ns user)"
                              "(def alpha)"
                              "alp"
                              "ba") "file:///b.clj")
  (h/load-code-and-locs (code "(ns alpaca.ns)"
                              "(def baff)") "file:///c.cljs")
  (h/load-code-and-locs (code "(ns d (:require [alpaca.ns :as alpaca])) frequen"
                              "(def bar 123)"
                              "(defn barbaz [a b] 123)"
                              "(def some 123)")
                        "file:///d.clj")
  (h/load-code-and-locs (code "(ns e (:require [alpaca.ns :refer [ba]]"
                              "                [d :as d-alias]))"
                              "Syste"
                              "d-alias/b")
                        "file:///e.clj")
  (testing "complete-a"
    (h/assert-submaps
      [{:label "alpha" :kind :variable}
       {:label "alpaca" :kind :module}
       {:label "alpaca" :kind :module}
       {:label "alpaca.ns" :kind :module}
       {:label "alpaca.ns" :kind :module}]
      (f.completion/completion "file:///b.clj" 3 3)))
  (testing "complete-ba"
    (h/assert-submaps
      [{:label "bases" :detail "clojure.core"}]
      (f.completion/completion "file:///b.clj" 4 3)))
  (testing "complete-alpaca"
    (h/assert-submaps
      [{:label "alpha" :kind :variable}
       {:label "alpaca" :kind :module}
       {:label "alpaca" :kind :module}
       {:label "alpaca.ns" :kind :module}
       {:label "alpaca.ns" :kind :module}]
      (f.completion/completion "file:///b.clj" 3 3)))
  (testing "complete-core-stuff"
    (h/assert-submaps
      [{:label "frequencies", :detail "clojure.core"}]
      (f.completion/completion "file:///d.clj" 1 49))
    (h/assert-submaps
      [{:label "bar", :kind :variable}
       {:label "barbaz", :kind :function}]
      (f.completion/completion "file:///e.clj" 4 10)))
  (testing "complete-core-stuff"
    (h/assert-submaps
      [{:label "frequencies", :detail "clojure.core"}]
      (f.completion/completion "file:///d.clj" 1 49))
    (h/assert-submaps
      [{:label "System", :detail "java.lang"}]
      (f.completion/completion "file:///e.clj" 3 6))))

(deftest resolve-item-test
  (testing "When element does not contains data"
    (is (= {:label "Some"}
           (f.completion/resolve-item {:label "Some"}))))
  (testing "When element contains data"
    (is (= {:label "Some" :documentation ["some\n"]}
           (f.completion/resolve-item {:label "Some" :data {:name 'some}})))))
