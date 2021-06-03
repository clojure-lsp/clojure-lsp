(ns clojure-lsp.interop-test
  (:require
    [clojure-lsp.interop :as interop]
    [clojure.test :refer [deftest is]])
  (:import
    (org.eclipse.lsp4j TextDocumentIdentifier)))

(deftest document->uri
  (is (= ""
         (interop/document->uri (TextDocumentIdentifier. ""))))
  (is (= "http://example.com/foo"
         (interop/document->uri (TextDocumentIdentifier. "http://example.com/foo"))))
  (is (= "file:///foo/bar/c.clj"
         (interop/document->uri (TextDocumentIdentifier. "file:///foo/bar/c.clj")))))
