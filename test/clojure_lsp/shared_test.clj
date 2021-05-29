(ns clojure-lsp.shared-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.shared :as shared]
    [clojure-lsp.test-helper :as h]
    [clojure.string :as string]
    [clojure.test :refer [deftest is testing]]))

(deftest filename->uri
  (testing "when it is not a jar"
    (reset! db/db {})
    (is (= (if h/windows? "file:///C:/some%20project/foo/bar_baz.clj" "file:///some%20project/foo/bar_baz.clj")
           (shared/filename->uri "/some project/foo/bar_baz.clj"))))
  (testing "when it is a jar via zipfile"
    (reset! db/db {})
    (is (= (if h/windows? "zipfile:///C:/home/some/.m2/some-jar.jar::clojure/core.clj" "zipfile:///home/some/.m2/some-jar.jar::clojure/core.clj")
           (shared/filename->uri "/home/some/.m2/some-jar.jar:clojure/core.clj"))))
  (testing "when it is a jar via jarfile"
    (reset! db/db {:settings {:dependency-scheme "jar"}})
    (is (= (if h/windows? "jar:file:///C:/home/some/.m2/some-jar.jar!/clojure/core.clj" "jar:file:///home/some/.m2/some-jar.jar!/clojure/core.clj")
           (shared/filename->uri "/home/some/.m2/some-jar.jar:clojure/core.clj"))))
  (testing "Windows URIs"
    (is (= (when h/windows? "file:///c:/c.clj")
           (when h/windows? (shared/filename->uri "c:\\c.clj"))))))

(deftest uri->filename
  (testing "should decode special characters in file URI"
    (is (= (h/file-path "/path+/encoded characters!")
           (shared/uri->filename "file:///path%2B/encoded%20characters%21"))))
  (testing "when it is a jar via zipfile"
    (is (= (h/file-path "/something.jar:something/file.cljc")
           (shared/uri->filename "zipfile:///something.jar::something/file.cljc"))))
  (testing "when it is a jar via zipfile with encoding"
    (is (= (h/file-path "/something.jar:something/file.cljc")
           (shared/uri->filename "zipfile:///something.jar%3A%3Asomething/file.cljc"))))
  (testing "when it is a jar via jarfile"
    (is (= (str (h/file-path "/Users/clojure-1.9.0.jar") ":clojure/string.clj")
           (shared/uri->filename "jar:file:///Users/clojure-1.9.0.jar!/clojure/string.clj"))))
  (testing "Windows URIs"
    (is (= (when h/windows? "c:\\c.clj")
           (when h/windows? (shared/uri->filename "file:/c:/c.clj"))))
    (is (= (when h/windows? "c:\\c.clj")
           (when h/windows? (shared/uri->filename "file:///c:/c.clj"))))))

(deftest relativize-filepath
  (is (= (h/file-path "some/path.clj")
         (shared/relativize-filepath
           (h/file-path "/User/rich/some/path.clj")
           (h/file-path "/User/rich")))))

(deftest uri->relative-filepath
  (is (= (h/file-path "some foo/path.clj")
         (shared/uri->relative-filepath "file:///User/ricky%20bar/some%20foo/path.clj" "file:///User/ricky%20bar"))))

(deftest join-filepaths
  (is (= (h/file-path "/users/melon/toasty/onion")
         (if h/windows?
           (shared/join-filepaths "C:\\users" "melon\\toasty" "onion")
           (shared/join-filepaths "/users" "melon/toasty" "onion")))))

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
