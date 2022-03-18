(ns clojure-lsp.coercer-test
  (:require
   [lsp4clj.coercer :as coercer]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is]])
  (:import
   (org.eclipse.lsp4j TextDocumentIdentifier)))

(h/reset-db-after-test)

(deftest document->uri
  (is (= ""
         (coercer/document->uri (TextDocumentIdentifier. ""))))
  (is (= "http://example.com/foo"
         (coercer/document->uri (TextDocumentIdentifier. "http://example.com/foo"))))
  (is (= "file:///foo/bar/c.clj"
         (coercer/document->uri (TextDocumentIdentifier. "file:///foo/bar/c.clj")))))
