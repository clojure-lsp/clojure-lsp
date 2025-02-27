(ns clojure-lsp.test-helper-test
  (:require
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is]]))

(deftest changes->code-test
  (is (= (h/code "(foo)" "(something)")
         (#'h/results->doc
          (h/code "(foo)" "(bar)")
          [{:loc "something"
            :range {:row 2 :col 2 :end-row 2 :end-col 5}}]))))
