(ns clojure-lsp.shared-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.shared :as shared]
    [clojure-lsp.test-helper :as h]
    [clojure.test :refer [deftest is testing]]))

(deftest uri->filename
  (testing "should decode special characters in file URI"
    (is (= (h/file-path "/path+/encoded characters!")
           (shared/uri->filename (h/file-uri "file:///path%2B/encoded%20characters%21")))))
  (testing "when it is a jar via zipfile"
    (is (= (h/file-path "/something.jar:something/file.cljc")
           (shared/uri->filename (h/file-uri "zipfile:///something.jar::something/file.cljc")))))
  (testing "when it is a jar via zipfile with encoding"
    (is (= (h/file-path "/something.jar:something/file.cljc")
           (shared/uri->filename (h/file-uri "zipfile:///something.jar%3A%3Asomething/file.cljc")))))
  (testing "when it is a jar via jarfile"
    (is (= (str (h/file-path "/Users/clojure-1.9.0.jar") ":clojure/string.clj")
           (shared/uri->filename (h/file-uri "jar:file:///Users/clojure-1.9.0.jar!/clojure/string.clj")))))
  (testing "Windows URIs"
    (is (= (when h/windows? "C:\\c.clj")
           (when h/windows? (shared/uri->filename "file:/c:/c.clj"))))
    (is (= (when h/windows? "C:\\c.clj")
           (when h/windows? (shared/uri->filename "file:///c:/c.clj"))))))

(deftest filename->uri
  (testing "when it is not a jar"
    (reset! db/db {})
    (is (= (if h/windows? "file:///c:/some%20project/foo/bar_baz.clj" "file:///some%20project/foo/bar_baz.clj")
           (shared/filename->uri (h/file-path "/some project/foo/bar_baz.clj")))))
  (testing "when it is a jar via zipfile"
    (reset! db/db {})
    (is (= (if h/windows? "zipfile:///c:/home/some/.m2/some-jar.jar::clojure/core.clj" "zipfile:///home/some/.m2/some-jar.jar::clojure/core.clj")
           (shared/filename->uri (h/file-path "/home/some/.m2/some-jar.jar:clojure/core.clj")))))
  (testing "when it is a jar via jarfile"
    (reset! db/db {:settings {:dependency-scheme "jar"}})
    (is (= (if h/windows? "jar:file:///c:/home/some/.m2/some-jar.jar!/clojure/core.clj" "jar:file:///home/some/.m2/some-jar.jar!/clojure/core.clj")
           (shared/filename->uri (h/file-path "/home/some/.m2/some-jar.jar:clojure/core.clj")))))
  (testing "Windows URIs"
    (reset! db/db {})
    (is (= (when h/windows? "file:///c:/c.clj")
           (when h/windows? (shared/filename->uri "c:\\c.clj"))))))

(deftest conform-uri
  (testing "lower case drive letter and encode colons"
    (reset! db/db {:settings {:uri-format {:encode-colons-in-path?   true
                                           :upper-case-drive-letter? false}}})
    (is (= "file:///c%3A/path"
           (shared/conform-uri "file:///C:/path"))))
  (testing "upper case drive letter and do not encode colons"
    (reset! db/db {:settings {:uri-format {:encode-colons-in-path?   false
                                           :upper-case-drive-letter? true}}})
    (is (= "file:///C:/path"
           (shared/conform-uri "file:///c:/path")))))

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
           (shared/join-filepaths (h/file-path "/users") "melon\\toasty" "onion")
           (shared/join-filepaths (h/file-path "/users") "melon/toasty" "onion")))))

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
