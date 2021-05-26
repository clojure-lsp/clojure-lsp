(ns clojure-lsp.shared-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.shared :as shared]
   [clojure.test :refer [deftest is testing]]))

(deftest uri->project-related-path
  (is (= "/src/my-project/some/ns.clj"
         (shared/uri->project-related-path "file:///home/foo/bar/my-project/src/my-project/some/ns.clj" "file:///home/foo/bar/my-project"))))

(deftest filename->uri
  (testing "when it is not a jar and contains slash"
    (reset! db/db {})
    (is (= "file:///some-project/foo/bar_baz.clj"
           (shared/filename->uri "/some-project/foo/bar_baz.clj"))))
  (testing "when it is not a jar and do not contains slash"
    (reset! db/db {})
    (is (= "file:///some-project/foo/bar_baz.clj"
           (shared/filename->uri "some-project/foo/bar_baz.clj"))))
  (testing "when it is a jar via zipfile"
    (reset! db/db {})
    (is (= "zipfile:///home/some/.m2/some-jar.jar::clojure/core.clj"
           (shared/filename->uri "/home/some/.m2/some-jar.jar:clojure/core.clj"))))
  (testing "when it is a jar via jarfile"
    (reset! db/db {:settings {:dependency-scheme "jar"}})
    (is (= "jar:file:////home/some/.m2/some-jar.jar!/clojure/core.clj"
           (shared/filename->uri "/home/some/.m2/some-jar.jar:clojure/core.clj")))))

(deftest uri->filename
  (testing "should decode special characters in file URI"
    (is (= "/path+/encoded characters!"
           (shared/uri->filename "file:///path%2B/encoded%20characters%21")))))

(deftest ->range-test
  (testing "should subtract 1 from row and col values"
    (is (= {:start {:line      1
                    :character 1}
            :end   {:line      1
                    :character 1}}
           (shared/->range {:row 2 :end-row 2 :col 2 :end-col 2}))))
  (testing "should not return negative line and character values"
    (is (= {:start {:line      0
                    :character 0}
            :end   {:line      0
                    :character 0}}
           (shared/->range {:row 0 :end-row 0 :col 0 :end-col 0})))))
