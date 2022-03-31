(ns clojure-lsp.feature.java-interop-test
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.java-interop :as f.java-interop]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(deftest uri->translated-uri-test
  (is (= "" (f.java-interop/uri->translated-uri "" h/components)))
  (is (= "/foo/bar.clj" (f.java-interop/uri->translated-uri "/foo/bar.clj" h/components)))
  (is (= "file:///foo/bar.clj" (f.java-interop/uri->translated-uri "file:///foo/bar.clj" h/components)))
  (is (= "jar:file:///foo/bar.clj" (f.java-interop/uri->translated-uri "jar:file:///foo/bar.clj" h/components)))
  (is (= "jar:file:///foo.jar!/bar.clj" (f.java-interop/uri->translated-uri "jar:file:///foo.jar!/bar.clj" h/components)))
  (is (= "jar:file:///foo.jar!/Bar.java" (f.java-interop/uri->translated-uri "jar:file:///foo.jar!/Bar.java" h/components)))
  (is (= "zipfile:///foo.jar::/bar.clj" (f.java-interop/uri->translated-uri "zipfile:///foo.jar::/bar.clj" h/components)))
  (is (= "zipfile:///foo.jar::/bar.java" (f.java-interop/uri->translated-uri "zipfile:///foo.jar::/bar.java" h/components))))

(deftest retrieve-jdk-source-and-analyze!-test
  (swap! db/db shared/deep-merge {:project-root-uri "file:///project"})
  (with-redefs [config/global-lsp-cache-dir (constantly (io/file "global-cache"))]
    (testing "when user has a JDK source already installed on global cache"
      (with-redefs [shared/file-exists? (constantly true)
                    slurp (constantly "local-jdk")]
        (is (= true
               (f.java-interop/retrieve-jdk-source-and-analyze! h/components)))))
    (testing "when user has not a JDK source installed"
      (testing ""
        (with-redefs [shared/file-exists? (constantly true)
                      slurp (constantly "local-jdk")]
          (is (= true
                 (f.java-interop/retrieve-jdk-source-and-analyze! h/components))))))))
