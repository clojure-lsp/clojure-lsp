(ns clojure-lsp.test-helper-test
  (:require
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is]]))

(deftest changes->code-test
  (is (= (h/code "(foo)" "(something)")
         (#'h/results->doc
          (h/code "(foo)" "(bar)")
          [{:loc "something"
            :range {:row 2 :col 2 :end-row 2 :end-col 5}}])))
  ;; The test below is going to fail due to a bug on rewrite-clj
  #_(is (= (h/code  "(get {} :a)")
           (#'h/results->doc
            (h/code "(get {}) :a")
            [{:loc  "(get {} :a)",
              :range {:row 1, :col 1, :end-row 1, :end-col 11}}]))))
