(ns clojure-lsp.main-test
  (:require
   [clojure-lsp.main :as main]
   [clojure.test :refer :all])
  (:import
   (org.eclipse.lsp4j
     InitializeParams)))

(deftest test-client-settings
  (testing "initializationOptions are null"
    (let [params (InitializeParams.)]
      (.setInitializationOptions params nil)
      (is (= #{"src" "test"}
             (get (main/client-settings params) :source-paths)))))
  (testing "document-formatting? is set to true if not provided"
    (let [params (InitializeParams.)]
      (is (= true (:document-formatting? (main/client-settings params))))))
  (testing "document-range-formatting? is set to true if not provided"
    (let [params (InitializeParams.)]
      (is (= true (:document-range-formatting? (main/client-settings params))))))
  (testing "semantic-tokens? is set to false if not provided"
    (let [params (InitializeParams.)]
      (is (= false (:semantic-tokens? (main/client-settings params)))))))
