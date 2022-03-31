(ns clojure-lsp.feature.java-interop-test
  (:require
   [clojure.test :refer [deftest is]]
   [clojure-lsp.feature.java-interop :as f.java-interop]
   [clojure-lsp.test-helper :as h]))

(deftest uri->translated-uri-test
  (is (= "" (f.java-interop/uri->translated-uri "" h/components)))
  (is (= "/foo/bar.clj" (f.java-interop/uri->translated-uri "/foo/bar.clj" h/components)))
  (is (= "file:///foo/bar.clj" (f.java-interop/uri->translated-uri "file:///foo/bar.clj" h/components)))
  (is (= "jar:file:///foo/bar.clj" (f.java-interop/uri->translated-uri "jar:file:///foo/bar.clj" h/components)))
  (is (= "jar:file:///foo.jar!/bar.clj" (f.java-interop/uri->translated-uri "jar:file:///foo.jar!/bar.clj" h/components)))
  (is (= "jar:file:///foo.jar!/Bar.java" (f.java-interop/uri->translated-uri "jar:file:///foo.jar!/Bar.java" h/components)))
  (is (= "zipfile:///foo.jar::/bar.clj" (f.java-interop/uri->translated-uri "zipfile:///foo.jar::/bar.clj" h/components)))
  (is (= "zipfile:///foo.jar::/bar.java" (f.java-interop/uri->translated-uri "zipfile:///foo.jar::/bar.java" h/components))))
