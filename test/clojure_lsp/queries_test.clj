(ns clojure-lsp.queries-test
  (:require
    [clojure.test :refer [deftest is]]
    [clojure-lsp.queries :as q]))

(deftest find-element-under-cursor []
  (is (= [] (q/find-element-under-cursor [{:name-row 1 :name-col 1 :name-end-row 1 :name-end-col 10}] "filename" 1 10))))
