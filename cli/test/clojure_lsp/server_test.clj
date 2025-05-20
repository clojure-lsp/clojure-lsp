(ns clojure-lsp.server-test
  (:require
   [clojure-lsp.server :as server]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(deftest test-client-settings
  (testing "initializationOptions are null"
    (let [params {:initialization-options nil}]
      (is (= "zipfile"
             (get (server/client-settings params) :dependency-scheme)))))
  (testing "document-formatting? is set to true if not provided"
    (let [params {}]
      (is (= true (:document-formatting? (server/client-settings params))))))
  (testing "document-range-formatting? is set to true if not provided"
    (let [params {}]
      (is (= true (:document-range-formatting? (server/client-settings params))))))
  (testing "text-document-sync-kind converts strings"
    (doseq [kind [:incremental "incremental" ":incremental"]]
      (let [params {:initialization-options {:text-document-sync-kind kind}}]
        (is (= :incremental (:text-document-sync-kind (server/client-settings params)))))))
  (testing "source-paths accepts strings"
    (doseq [path ["foo" ":foo"]]
      (let [params {:initialization-options {:source-paths [path]}}]
        (is (= #{"foo"} (:source-paths (server/client-settings params)))))))
  (testing "source-paths converts strings"
    (let [params {:initialization-options {:source-paths ["foo"]}}]
      (is (= #{"foo"} (:source-paths (server/client-settings params))))))
  (testing "source-aliases converts strings"
    (doseq [path [:foo "foo" ":foo"]]
      (let [params {:initialization-options {:source-aliases [path]}}]
        (is (= #{:foo} (:source-aliases (server/client-settings params)))))))
  (testing "vectors and keywords are converted properly"
    (let [params {:initialization-options {:cljfmt {:indents {:something [["block" 1]]}}}}]
      (is (= {:indents {'something [[:block 1]]}} (:cljfmt (server/client-settings params)))))))
