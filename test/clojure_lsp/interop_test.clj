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

(deftest kwd-string-test
  (is (= :foo (interop/kwd-string :foo)))
  (is (= :foo (interop/kwd-string "foo")))
  (is (= :foo (interop/kwd-string ":foo")))
  (is (nil? (interop/kwd-string 'foo)))
  (is (nil? (interop/kwd-string 1)))
  (is (nil? (interop/kwd-string [])))
  (is (nil? (interop/kwd-string nil))))

(deftest clean-keyword-strings-test
  (is (nil? (interop/clean-keyword-strings [])))
  (doseq [f [:foo "foo" ":foo"]
          b [:bar "bar" ":bar"]]
    (is (= #{:foo :bar} (interop/clean-keyword-strings [f b]))))
  (doseq [b ['bar 1 [] nil]]
    (is (= #{:foo} (interop/clean-keyword-strings [:foo b])))))
