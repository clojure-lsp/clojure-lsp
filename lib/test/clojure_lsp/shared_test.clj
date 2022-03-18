(ns clojure-lsp.shared-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [medley.core :as medley]))

(h/reset-db-after-test)

(deftest deep-merge
  (testing "simple deep merge"
    (is (= {:a {:b 2 :c 3}} (shared/deep-merge {:a {:b 2}} {:a {:c 3}}))))
  (testing "concating colls"
    (is (= {:a {:b [1 2 3 4] :c 3}} (shared/deep-merge {:a {:b [1 2]}} {:a {:b [3 4] :c 3}})))
    (is (= {:a {:b [1 2 3 4] :c 3}} (shared/deep-merge {:a {:b #{1 2}}} {:a {:b [3 4] :c 3}})))
    (is (= {:a {:b [1 2 4 3] :c 3}} (shared/deep-merge {:a {:b [1 2]}} {:a {:b #{3 4} :c 3}})))))

(deftest external-filename?
  (is (not (shared/external-filename? "/some/project/src/a.clj" #{"/some/project/src"})))
  (is (not (shared/external-filename? "/some/project/src/a.clj" #{})))
  (is (shared/external-filename? "/some/project/src/a.clj" #{"/some/project/src/b.clj"}))
  (is (shared/external-filename? "/some/place/file.jar:some/path/to/file.clj" #{"/some/project/src/a.clj"}))
  (is (shared/external-filename? "/some/place/file.jar:some/path/to/file.clj" #{}))
  (is (shared/external-filename? "/some/place/file.jar:some/path/to/file.clj" #{"/some/place/file.jar:some/path"}))
  (is (shared/external-filename? "/some/user/.emacs.d/.local/etc/workspace/.cache/something.cljc" #{"/some/place/file.clj"})))

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
    (h/clean-db!)
    (is (= (if h/windows? "file:///c:/some%20project/foo/bar_baz.clj" "file:///some%20project/foo/bar_baz.clj")
           (shared/filename->uri (h/file-path "/some project/foo/bar_baz.clj") db/db))))
  (testing "when it is a jar via zipfile"
    (h/clean-db!)
    (is (= (if h/windows? "zipfile:///c:/home/some/.m2/some-jar.jar::clojure/core.clj" "zipfile:///home/some/.m2/some-jar.jar::clojure/core.clj")
           (shared/filename->uri (h/file-path "/home/some/.m2/some-jar.jar:clojure/core.clj") db/db))))
  (testing "when it is a jar via jarfile"
    (swap! db/db shared/deep-merge {:settings {:dependency-scheme "jar"}})
    (is (= (if h/windows? "jar:file:///c:/home/some/.m2/some-jar.jar!/clojure/core.clj" "jar:file:///home/some/.m2/some-jar.jar!/clojure/core.clj")
           (shared/filename->uri (h/file-path "/home/some/.m2/some-jar.jar:clojure/core.clj") db/db))))
  (testing "Windows URIs"
    (h/clean-db!)
    (is (= (when h/windows? "file:///c:/c.clj")
           (when h/windows? (shared/filename->uri "c:\\c.clj" db/db))))))

(deftest uri->namespace
  (testing "when don't have a project root"
    (h/clean-db!)
    (is (nil? (shared/uri->namespace (h/file-uri "file:///user/project/src/foo/bar.clj") db/db))))
  (testing "when it has a project root and not a source-path"
    (swap! db/db shared/deep-merge {:settings {:auto-add-ns-to-new-files? true
                                               :source-paths #{(h/file-uri "file:///user/project/bla")}}
                                    :project-root-uri (h/file-uri "file:///user/project")})
    (is (nil? (shared/uri->namespace (h/file-uri "file:///user/project/src/foo/bar.clj") db/db))))
  (testing "when it has a project root and a source-path"
    (swap! db/db shared/deep-merge {:settings {:auto-add-ns-to-new-files? true
                                               :source-paths #{(h/file-path "/user/project/src")}}
                                    :project-root-uri (h/file-uri "file:///user/project")})
    (is (= "foo.bar"
           (shared/uri->namespace (h/file-uri "file:///user/project/src/foo/bar.clj") db/db))))
  (testing "when it has a project root a source-path on mono repos"
    (swap! db/db medley/deep-merge {:settings {:auto-add-ns-to-new-files? true
                                               :source-paths #{(h/file-path "/user/project/src/clj")
                                                               (h/file-path "/user/project/src/cljs")}}
                                    :project-root-uri (h/file-uri "file:///user/project")})
    (is (= "foo.bar"
           (shared/uri->namespace (h/file-uri "file:///user/project/src/clj/foo/bar.clj") db/db))))
  (testing "when it has a project root and nested source-paths"
    (swap! db/db shared/deep-merge {:settings {:auto-add-ns-to-new-files? true
                                               :source-paths #{(h/file-path "/user/project/src")
                                                               (h/file-path "/user/project/src/some")}}
                                    :project-root-uri (h/file-uri "file:///user/project")})
    (is (= "foo.bar"
           (shared/uri->namespace (h/file-uri "file:///user/project/src/some/foo/bar.clj") db/db))))
  (testing "when an invalid source-path with a valid source-path prefixing it"
    (swap! db/db medley/deep-merge {:settings {:source-paths #{(h/file-path "/user/project/src/clj")}}
                                    :project-root-uri (h/file-uri "file:///user/project")})
    (with-redefs [shared/directory? (constantly true)]
      (is (= nil
             (shared/uri->namespace (h/file-uri "file:///user/project/src/cljs/foo/bar.clj") db/db))))))

(deftest conform-uri
  (testing "lower case drive letter and encode colons"
    (is (= "file:///c%3A/path"
           (#'shared/conform-uri "file:///C:/path" {:encode-colons-in-path?   true
                                                    :upper-case-drive-letter? false}
                                 (:logger h/components)))))
  (testing "upper case drive letter and do not encode colons"
    (is (= "file:///C:/path"
           (#'shared/conform-uri "file:///c:/path" {:encode-colons-in-path?   false
                                                    :upper-case-drive-letter? true}
                                 (:logger h/components))))))

(deftest relativize-filepath
  (is (= (h/file-path "some/path.clj")
         (shared/relativize-filepath
           (h/file-path "/User/rich/some/path.clj")
           (h/file-path "/User/rich")))))

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

(def unescape-uri #'shared/unescape-uri)

(deftest unescape-uri-test
  (testing "URI should unescape."
    (is (= "jar:file:///home/foo/bar.jar!baz.clj"
           (unescape-uri "jar:file%3A///home/foo/bar.jar%21baz.clj"))))
  (testing "URI should remain the same."
    (is (= "file:///home/foo/bar.jar"
           (unescape-uri "file:///home/foo/bar.jar"))))
  (testing "URI should remain the same as IllegalArgumentException is thrown."
    (is (= "file:///home/foo/bar.jar%%"
           (unescape-uri "file:///home/foo/bar.jar%%")))))

(deftest inside?
  (testing "when b has end scope"
    (testing "when a outside before b"
      (is (= false (shared/inside?
                     {:name-row 1 :name-col 1}
                     {:name-row 1 :name-col 2 :scope-end-row 1 :scope-end-col 4}))))
    (testing "when a outside after b"
      (is (= false (shared/inside?
                     {:name-row 2 :name-col 2}
                     {:name-row 1 :name-col 2 :scope-end-row 1 :scope-end-col 4}))))
    (testing "when a inside b"
      (is (= true (shared/inside?
                    {:name-row 1 :name-col 3}
                    {:name-row 1 :name-col 2 :scope-end-row 1 :scope-end-col 4})))))
  (testing "when b doesn't have end scope"
    (testing "when a outside before b"
      (is (= false (shared/inside?
                     {:name-row 1 :name-col 1}
                     {:name-row 1 :name-col 2 :name-end-row 1 :name-end-col 4}))))
    (testing "when a outside after b"
      (is (= false (shared/inside?
                     {:name-row 2 :name-col 2}
                     {:name-row 1 :name-col 2 :name-end-row 1 :name-end-col 4}))))
    (testing "when a inside b"
      (is (= true (shared/inside?
                    {:name-row 1 :name-col 3}
                    {:name-row 1 :name-col 2 :name-end-row 1 :name-end-col 4}))))))

(deftest namespace+source-path->filename
  (is (= "/project/test/some/cool_ns.clj"
         (shared/namespace+source-path->filename "some.cool-ns" "/project/test" :clj)))
  (is (= "/project/test/some/cool_ns.clj"
         (shared/namespace+source-path->filename "some.cool-ns" "/project/test/" :clj))))
