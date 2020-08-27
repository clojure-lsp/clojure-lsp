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
             (get (main/client-settings params) :source-paths))))))
