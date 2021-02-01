(ns clojure-lsp.interop-test
  (:require
    [clojure-lsp.interop :as interop]
    [clojure.test :refer [deftest is]])
  (:import
    (org.eclipse.lsp4j TextDocumentIdentifier)))

(deftest document->decoded-uri
    (is (= (interop/document->decoded-uri (TextDocumentIdentifier. ""))  ""))
    (is (= (interop/document->decoded-uri (TextDocumentIdentifier. "http%3A%2F%2Ffoo%20bar%2F"))  "http://foo bar/"))
    (is (= (interop/document->decoded-uri (TextDocumentIdentifier. "http://foo bar/"))  "http://foo bar/"))
    (is (= (interop/document->decoded-uri (TextDocumentIdentifier. "jar:file:///Users/clojure-1.9.0.jar!/clojure/string.clj"))  "jar:file:///Users/clojure-1.9.0.jar!/clojure/string.clj"))
    (is (= (interop/document->decoded-uri (TextDocumentIdentifier. "zipfile:///something.jar::something/file.cljc"))  "zipfile:///something.jar::something/file.cljc"))
    (is (= (interop/document->decoded-uri (TextDocumentIdentifier. "zipfile:///something.jar%3A%3Asomething/file.cljc"))  "zipfile:///something.jar::something/file.cljc")))
