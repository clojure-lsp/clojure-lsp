(ns clojure-lsp.interop-test
  (:require
    [clojure-lsp.interop :as interop]
    [clojure.test :refer [deftest is]])
  (:import
    (org.eclipse.lsp4j TextDocumentItem)))

(deftest document->decoded-uri
    (is (= (interop/document->decoded-uri (new TextDocumentItem "" "clojure" 1 ""))  ""))
    (is (= (interop/document->decoded-uri (new TextDocumentItem "http%3A%2F%2Ffoo%20bar%2F" "clojure" 1 ""))  "http://foo bar/"))
    (is (= (interop/document->decoded-uri (new TextDocumentItem "http://foo bar/" "clojure" 1 ""))  "http://foo bar/"))
    (is (= (interop/document->decoded-uri (new TextDocumentItem "jar:file:///Users/clojure-1.9.0.jar!/clojure/string.clj" "clojure" 1 ""))  "jar:file:///Users/clojure-1.9.0.jar!/clojure/string.clj"))
    (is (= (interop/document->decoded-uri (new TextDocumentItem "zipfile:///something.jar::something/file.cljc" "clojure" 1 ""))  "zipfile:///something.jar::something/file.cljc"))
    (is (= (interop/document->decoded-uri (new TextDocumentItem "zipfile:///something.jar%3A%3Asomething/file.cljc" "clojure" 1 ""))  "zipfile:///something.jar::something/file.cljc")))
