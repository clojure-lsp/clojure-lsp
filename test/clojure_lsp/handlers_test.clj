(ns clojure-lsp.handlers-test
  (:require
   [clojure.test :refer :all]
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]))

(deftest test-rename
  (reset! db/db {:file-envs
                 {"file://a.clj" {:usages [{:sym 'a/bar :sexpr 'bar :row 1 :col 1 :end-row 1 :end-col 4}]}
                  "file://b.clj" {:usages [{:sym 'a/bar :sexpr 'a/bar :row 1 :col 1 :end-row 1 :end-col 6}]}}})
  (testing "rename on symbol without namespace"
    (is (= "foo" (get-in (handlers/rename "file://a.clj" 1 1 "foo") [:changes "file://a.clj" 0 :new-text])))
    (is (= "a/foo" (get-in (handlers/rename "file://a.clj" 1 1 "foo") [:changes "file://b.clj" 0 :new-text]))))
  (testing "rename on symbol with namespace adds existing namespace"
    (is (= "foo" (get-in (handlers/rename "file://b.clj" 1 1 "foo") [:changes "file://a.clj" 0 :new-text])))
    (is (= "a/foo" (get-in (handlers/rename "file://b.clj" 1 1 "foo") [:changes "file://b.clj" 0 :new-text]))))
  (testing "rename on symbol with namespace removes passed-in namespace"
    (is (= "foo" (get-in (handlers/rename "file://b.clj" 1 1 "a/foo") [:changes "file://a.clj" 0 :new-text])))
    (is (= "a/foo" (get-in (handlers/rename "file://b.clj" 1 1 "a/foo") [:changes "file://b.clj" 0 :new-text])))) )

