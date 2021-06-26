(ns clojure-lsp.server-test
  (:require
   [clojure-lsp.server :as server]
   [clojure.test :refer [deftest is testing]])
  (:import
   (org.eclipse.lsp4j
     InitializeParams)))

(deftest test-client-settings
  (testing "initializationOptions are null"
    (let [params (InitializeParams.)]
      (.setInitializationOptions params nil)
      (is (= "zipfile"
             (get (#'server/client-settings params) :dependency-scheme)))))
  (testing "document-formatting? is set to true if not provided"
    (let [params (InitializeParams.)]
      (is (= true (:document-formatting? (#'server/client-settings params))))))
  (testing "document-range-formatting? is set to true if not provided"
    (let [params (InitializeParams.)]
      (is (= true (:document-range-formatting? (#'server/client-settings params)))))))
