(ns clojure-lsp.features.completion-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.completion :as f.completion]
    [clojure-lsp.test-helper :as h]
    [clojure.string :as string]
    [clojure.test :refer [deftest testing is]]))

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
                              "(def bar \"some good docs\"123)"
                              "(defn barbaz [a b] 123)"
                              "(def some 123)")
                        "file:///d.clj")
  (h/load-code-and-locs (code "(ns e (:require [alpaca.ns :refer [ba]]"
                              "                [d :as d-alias]))"
                              "Syste"
                              "d-alias/b"
                              "123")
                        "file:///e.clj")

  (swap! db/db merge {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
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
    (testing "complete symbols from alias"
      (h/assert-submaps
       [{:label "d-alias/bar"
         :kind :variable
         :documentation {:kind "markdown"
                         :value "```clojure\nd/bar\n```\n\n----\n```clojure\nsome good docs\n```\n----\n*/d.clj*"}}
        {:label "d-alias/barbaz", :kind :function}]
       (f.completion/completion "file:///e.clj" 4 10))))
  (testing "complete-core-stuff"
    (h/assert-submaps
     [{:label "frequencies", :detail "clojure.core"}]
     (f.completion/completion "file:///d.clj" 1 49))
    (h/assert-submaps
     [{:label "System", :detail "java.lang"}]
     (f.completion/completion "file:///e.clj" 3 6)))
  (testing "complete non symbols doesn't blow up"
    (is (= [] (f.completion/completion "file:///e.clj" 5 3)))))
