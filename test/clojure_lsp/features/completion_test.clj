(ns clojure-lsp.features.completion-test
  (:require
    [clojure-lsp.feature.completion :as f.completion]
    [clojure-lsp.test-helper :as h]
    [clojure.test :refer [deftest testing is]]))

(h/reset-db-after-test)

(deftest test-completion
  (h/load-code-and-locs (str "(ns alpaca.ns (:require [user :as alpaca]))\n"
                             "(def barr)\n"
                             "(def bazz)") "file:///a.cljc")
  (h/load-code-and-locs (str "(ns user)\n"
                             "(def alpha)\n"
                             "alp\n"
                             "ba") "file:///b.clj")
  (h/load-code-and-locs (str "(ns alpaca.ns)\n"
                             "(def baff)\n") "file:///c.cljs")
  (h/load-code-and-locs (str "(ns d (:require [alpaca.ns :as alpaca])) frequen")
                        "file:///d.clj")
  (h/load-code-and-locs (str "(ns e (:require [alpaca.ns :refer [ba]])) Syste")
                        "file:///e.clj")
  (testing "complete-a"
    (h/assert-submaps
      [{:label "alpha" :kind :function}
       {:label "alpaca" :kind :module}
       {:label "alpaca.ns" :kind :module}
       {:label "alpaca.ns" :kind :module}
       {:label "alpaca.ns" :kind :module}]
      (f.completion/completion "file:///b.clj" 3 3)))
  (testing "complete-ba"
    (h/assert-submaps
      [{:label "bases" :detail "clojure.core"}]
      (f.completion/completion "file:///b.clj" 4 3)))
  (testing "complete-alpaca"
    (h/assert-submaps
      [{:label "alpha" :kind :function}
       {:label "alpaca" :kind :module}
       {:label "alpaca.ns" :kind :module}
       {:label "alpaca.ns" :kind :module}
       {:label "alpaca.ns" :kind :module}]
      (f.completion/completion "file:///b.clj" 3 3)))
  (testing "complete-core-stuff"
    (h/assert-submaps
      [{:label "frequencies", :detail "clojure.core"}]
      (f.completion/completion "file:///d.clj" 1 49))
    (h/assert-submaps
      [{:label "System", :detail "java.lang"}]
      (f.completion/completion "file:///e.clj" 1 48))))

(deftest resolve-item-test
  (testing "When element does not contains data"
    (is (= {:label "Some"}
           (f.completion/resolve-item {:label "Some"}))))
  (testing "When element contains data"
    (is (= {:label "Some" :documentation ["some\n"]}
           (f.completion/resolve-item {:label "Some" :data {:name 'some}})))))
