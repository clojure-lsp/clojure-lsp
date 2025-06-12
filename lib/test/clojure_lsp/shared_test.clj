(ns clojure-lsp.shared-test
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [are deftest is testing]]
   [medley.core :as medley]))

(h/reset-components-before-test)

(deftest deep-merge
  (testing "simple deep merge"
    (is (= {:a {:b 2 :c 3}} (shared/deep-merge {:a {:b 2}} {:a {:c 3}}))))
  (testing "concating colls"
    (is (= {:a {:b [1 2 3 4] :c 3}} (shared/deep-merge {:a {:b [1 2]}} {:a {:b [3 4] :c 3}})))
    (is (= {:a {:b [1 2 3 4] :c 3}} (shared/deep-merge {:a {:b #{1 2}}} {:a {:b [3 4] :c 3}})))
    (is (= {:a {:b [1 2 4 3] :c 3}} (shared/deep-merge {:a {:b [1 2]}} {:a {:b #{3 4} :c 3}})))))

(deftest external-filename?
  (is (not (shared/external-filename? (h/file-path "/some/project/src/a.clj") #{(h/file-path "/some/project/src")})))
  (is (not (shared/external-filename? (h/file-path "/some/project/src/a.clj") #{})))
  (is (shared/external-filename? (h/file-path "/some/project/src/a.clj") #{(h/file-path "/some/project/src/b.clj")}))
  (is (shared/external-filename? (h/file-path "/some/place/file.jar:some/path/to/file.clj") #{(h/file-path "/some/project/src/a.clj")}))
  (is (shared/external-filename? (h/file-path "/some/place/file.jar:some/path/to/file.clj") #{}))
  (is (shared/external-filename? (h/file-path "/some/place/file.jar:some/path/to/file.clj") #{(h/file-path "/some/place/file.jar:some/path")}))
  (is (shared/external-filename? (h/file-path "/some/user/.emacs.d/.local/etc/workspace/.cache/something.cljc") #{(h/file-path "/some/place/file.clj")}))
  (is (not (shared/external-filename? (h/file-path "/some/project/.lsp/config.edn") #{(h/file-path "/some/project/src")}))))

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
    (is (= (when h/windows? "C:\\Users\\FirstName LastName\\c.clj")
           (when h/windows? (shared/uri->filename "file:/c:/Users/FirstName%20LastName/c.clj"))))
    (is (= (when h/windows? "C:\\c.clj")
           (when h/windows? (shared/uri->filename "file:///c:/c.clj"))))))

(deftest filename->uri
  (testing "when it is not a jar"
    (h/reset-components!)
    (is (= (if h/windows?
             "file:///C:/some%20project/foo/bar_baz.clj"
             "file:///some%20project/foo/bar_baz.clj")
           (shared/filename->uri (h/file-path "/some project/foo/bar_baz.clj") (h/db)))))
  (testing "when it is a jar via zipfile"
    (h/reset-components!)
    (is (= (if h/windows?
             "zipfile:///C:/home/some/.m2/some-jar.jar::clojure/core.clj"
             "zipfile:///home/some/.m2/some-jar.jar::clojure/core.clj")
           (shared/filename->uri (h/file-path "/home/some/.m2/some-jar.jar:clojure/core.clj") (h/db)))))
  (testing "when it is a jar via jarfile"
    (swap! (h/db*) shared/deep-merge {:settings {:dependency-scheme "jar"}})
    (is (= (if h/windows?
             "jar:file:///C:/home/some/.m2/some-jar.jar!/clojure/core.clj"
             "jar:file:///home/some/.m2/some-jar.jar!/clojure/core.clj")
           (shared/filename->uri (h/file-path "/home/some/.m2/some-jar.jar:clojure/core.clj") (h/db)))))
  (testing "Windows URIs"
    (h/reset-components!)
    (is (= (when h/windows? "file:///C:/c.clj")
           (when h/windows? (shared/filename->uri "C:\\c.clj" (h/db)))))))

(deftest uri->namespace
  (testing "when don't have a project root"
    (h/reset-components!)
    (is (nil? (shared/uri->namespace (h/file-uri "file:///user/project/src/foo/bar.clj") (h/db)))))
  (testing "when it has a project root and not a source-path"
    (swap! (h/db*) shared/deep-merge {:settings {:auto-add-ns-to-new-files? true
                                                 :source-paths #{(h/file-uri "file:///user/project/bla")}}
                                      :project-root-uri (h/file-uri "file:///user/project")})
    (is (nil? (shared/uri->namespace (h/file-uri "file:///user/project/src/foo/bar.clj") (h/db)))))
  (testing "when it has a project root and a source-path"
    (swap! (h/db*) shared/deep-merge {:settings {:auto-add-ns-to-new-files? true
                                                 :source-paths #{(h/file-path "/user/project/src")}}
                                      :project-root-uri (h/file-uri "file:///user/project")})
    (is (= "foo.bar"
           (shared/uri->namespace (h/file-uri "file:///user/project/src/foo/bar.clj") (h/db)))))
  (testing "when it has a project root a source-path on mono repos"
    (swap! (h/db*) medley/deep-merge {:settings {:auto-add-ns-to-new-files? true
                                                 :source-paths #{(h/file-path "/user/project/src/clj")
                                                                 (h/file-path "/user/project/src/cljs")}}
                                      :project-root-uri (h/file-uri "file:///user/project")})
    (is (= "foo.bar"
           (shared/uri->namespace (h/file-uri "file:///user/project/src/clj/foo/bar.clj") (h/db)))))
  (testing "when it has a project root and nested source-paths"
    (swap! (h/db*) shared/deep-merge {:settings {:auto-add-ns-to-new-files? true
                                                 :source-paths #{(h/file-path "/user/project/src")
                                                                 (h/file-path "/user/project/src/some")}}
                                      :project-root-uri (h/file-uri "file:///user/project")})
    (is (= "foo.bar"
           (shared/uri->namespace (h/file-uri "file:///user/project/src/some/foo/bar.clj") (h/db)))))
  (testing "when an invalid source-path with a valid source-path prefixing it"
    (swap! (h/db*) medley/deep-merge {:settings {:source-paths #{(h/file-path "/user/project/src/clj")}}
                                      :project-root-uri (h/file-uri "file:///user/project")})
    (with-redefs [shared/directory? (constantly true)]
      (is (= nil
             (shared/uri->namespace (h/file-uri "file:///user/project/src/cljs/foo/bar.clj") (h/db)))))))

(deftest conform-uri
  (testing "lower case drive letter and encode colons"
    (is (= "file:///c%3A/path"
           (#'shared/conform-uri "file:///C:/path" {:encode-colons-in-path?   true
                                                    :upper-case-drive-letter? false}))))
  (testing "upper case drive letter and do not encode colons"
    (is (= "file:///C:/path"
           (#'shared/conform-uri "file:///c:/path" {:encode-colons-in-path?   false
                                                    :upper-case-drive-letter? true})))))

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
  (is (= (h/file-path "/project/test/some/cool_ns.clj")
         (shared/namespace+source-path->filename "some.cool-ns" (h/file-path "/project/test") :clj)))
  (is (= (h/file-path "/project/test/some/cool_ns.clj")
         (shared/namespace+source-path->filename "some.cool-ns" (h/file-path "/project/test/") :clj))))

(deftest uri->source-paths
  (is (= [(h/file-path "/dir/project/src")]
         (shared/uri->source-paths (h/file-uri "file:///dir/project/src/clj/a/b.clj")
                                   [(h/file-path "/dir/project/test") (h/file-path "/dir/project/src")])))
  (testing "one source-path is a prefix of another"
    (is (= [(h/file-path "/dir/project/src/cljs")]
           (shared/uri->source-paths (h/file-uri "file:///dir/project/src/cljs/a/b.clj")
                                     [(h/file-path "/dir/project/src/clj") (h/file-path "/dir/project/src/cljs")])))))

(deftest jar-file?-test
  (is (= false (shared/jar-file? "")))
  (is (= false (shared/jar-file? "/foo")))
  (is (= false (shared/jar-file? "/foo")))
  (is (= false (shared/jar-file? "/foo/bar")))
  (is (= false (shared/jar-file? "/foo/bar.clj")))
  (is (= false (shared/jar-file? "/jar/bar.clj")))
  (is (= true (shared/jar-file? "/foo/bar.jar")))
  (is (= true (shared/jar-file? "/foo/bar.jar!/some/file.clj")))
  (is (= true (shared/jar-file? "/foo/bar.jar!/some/file.jar")))
  (is (= false (shared/jar-file? "file:///foo")))
  (is (= false (shared/jar-file? "file:///foo")))
  (is (= false (shared/jar-file? "file:///foo/bar")))
  (is (= false (shared/jar-file? "file:///foo/bar.clj")))
  (is (= false (shared/jar-file? "file:///jar/bar.clj")))
  (is (= true (shared/jar-file? "file:///foo/bar.jar")))
  (is (= true (shared/jar-file? "jar:file:///foo/bar.jar!/some/file.clj")))
  (is (= true (shared/jar-file? "jar:file:///foo/bar.jar!/some/file.jar"))))

(deftest class-file?-test
  (is (= false (shared/class-file? "")))
  (is (= false (shared/class-file? "/foo")))
  (is (= false (shared/class-file? "/foo/bar")))
  (is (= false (shared/class-file? "/foo/bar.clj")))
  (is (= false (shared/class-file? "/foo/bar.jar")))
  (is (= true (shared/class-file? "/foo/bar.class")))
  (is (= false (shared/class-file? "/foo/bar.jar!/some/file.clj")))
  (is (= true (shared/class-file? "/foo/bar.jar!/some/file.class")))
  (is (= false (shared/class-file? "file:///foo")))
  (is (= false (shared/class-file? "file:///foo/bar")))
  (is (= false (shared/class-file? "file:///foo/bar.clj")))
  (is (= false (shared/class-file? "file:///foo/bar.jar")))
  (is (= true (shared/class-file? "file:///foo/bar.class")))
  (is (= false (shared/class-file? "jar:file:///foo/bar.jar!/some/file.clj")))
  (is (= true (shared/class-file? "jar:file:///foo/bar.jar!/some/file.class"))))

(deftest normalize-uri-from-client
  (testing "jar files"
    ;; standard
    (is (= (h/file-uri "jar:file:///some/path/some.jar!/some/file.clj")
           (shared/normalize-uri-from-client (h/file-uri "jar:file:///some/path/some.jar!/some/file.clj"))))
    ;; Calva
    ;; Calva escapes aggressively, meaning h/file-uri doesn't work
    (if h/windows? ;; TODO: is this how URIs look on Windows in Calva
      (is (= "jar:file:///C:/some/path/some.jar!/some/file.clj"
             (shared/normalize-uri-from-client "jar:file%3A///C%3A/some/path/some.jar%21/some/file.clj")))
      (is (= "jar:file:///some/path/some.jar!/some/file.clj"
             (shared/normalize-uri-from-client "jar:file%3A///some/path/some.jar%21/some/file.clj"))))
    ;; with spaces
    ;; TODO: this fails because `(unescape-uri uri)` converts %20 to a space
    ;; character, which we don't want. But, we can't remove `(unescape-uri uri)`,
    ;; or else the Calva jar file test above fails. I think it's rare for jar file
    ;; paths to contain spaces, so I'm leaving this test commented out. Would be
    ;; nice to fix someday.
    #_(is (= (h/file-uri "jar:file:///some%20spaces/path/some.jar!/some%20spaces/file.clj")
             (shared/normalize-uri-from-client (h/file-uri "jar:file:///some%20spaces/path/some.jar!/some%20spaces/file.clj")))))
  (testing "zipfiles"
    ;; standard
    (is (= (h/file-uri "zipfile:///some/path/some.jar::some/file.clj")
           (shared/normalize-uri-from-client (h/file-uri "zipfile:///some/path/some.jar::some/file.clj"))))
    ;; coc.nvim
    ;; coc.nvim doesn't include // authority, meaning h/file-uri doesn't work
    (if h/windows?
      (is (= "zipfile:///C:/some/path/some.jar::some/file.clj"
             (shared/normalize-uri-from-client "zipfile:/C:/some/path/some.jar%3a%3asome/file.clj")))
      (is (= "zipfile:///some/path/some.jar::some/file.clj"
             (shared/normalize-uri-from-client "zipfile:/some/path/some.jar%3a%3asome/file.clj")))))
  (testing "standard files"
    ;; standard
    (is (= (h/file-uri "file:///some/file.clj")
           (shared/normalize-uri-from-client (h/file-uri "file:///some/file.clj"))))
    ;; with spaces
    (is (= (h/file-uri "file:///some%20spaces/file%20spaces.clj")
           (shared/normalize-uri-from-client (h/file-uri "file:///some%20spaces/file%20spaces.clj"))))
    ;; Windows
    (when h/windows?
      (are [uri] (= "file:///c:/c.clj"
                    (shared/normalize-uri-from-client uri))
        "file:/c:/c.clj"
        "file:///c:/c.clj"))))

(deftest dir-uris->file-uris-test
  (testing "when the dir-uri is a dir inside source-path"
    (with-redefs [fs/glob (constantly [(h/file-path "/user/project/src/foo/bar.clj")
                                       (h/file-uri "/user/project/src/foo/baz.clj")])
                  fs/canonicalize identity]
      (is (= [(h/file-uri "file:///user/project/src/foo/bar.clj")
              (h/file-uri "file:///user/project/src/foo/baz.clj")]
             (shared/dir-uris->file-uris [(h/file-uri "file:///user/project/src")] (h/db))))))
  (testing "when the dir-uri is absolute file URI"
    (is (= [(h/file-uri "file:///user/project/src/foo/bar.clj")]
           (shared/dir-uris->file-uris [(h/file-uri "file:///user/project/src/foo/bar.clj")] (h/db))))))
