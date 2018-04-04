(ns clojure-lsp.handlers-test
  (:require
   [clojure.test :refer :all]
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]))

(deftest test-rename
  (reset! db/db {:file-envs
                 {"a.clj" {:usages [{:sym 'a/bar :sexpr 'bar :row 1 :col 1 :end-row 1 :end-col 4}]}
                  "b.clj" {:usages [{:sym 'a/bar :sexpr 'a/bar :row 1 :col 1 :end-row 1 :end-col 6}]}}})
  (is (= nil (handlers/rename "a.clj" 1 1 "foo")))
  (is (= nil (handlers/rename "b.clj" 1 1 "foo")))
  (is (= nil (handlers/rename "b.clj" 1 1 "a/foo"))))

