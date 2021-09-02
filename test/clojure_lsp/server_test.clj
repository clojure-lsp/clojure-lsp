(ns clojure-lsp.server-test
  (:require
   [clojure-lsp.interop :as interop]
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
      (is (= true (:document-range-formatting? (#'server/client-settings params))))))
  (testing "text-document-sync-kind converts strings"
    (doseq [kind [:incremental "incremental" ":incremental"]]
      (let [params (doto (InitializeParams.)
                     (.setInitializationOptions
                       (interop/clj->java
                         {:text-document-sync-kind kind})))]
        (is (= :incremental (:text-document-sync-kind (#'server/client-settings params)))))))
  (testing "source-paths accepts strings"
    (doseq [path [["foo"] [":foo"]]]
      (let [params (doto (InitializeParams.)
                     (.setInitializationOptions
                       (interop/clj->java
                         {:source-paths path})))]
        (is (= #{"foo"} (:source-paths (#'server/client-settings params)))))))
  (testing "source-paths rejects non-strings"
    (let [params (doto (InitializeParams.)
                   (.setInitializationOptions
                     (interop/clj->java
                       {:source-paths [:foo]})))]
      (is (nil? (:source-paths (#'server/client-settings params))))))
  (testing "source-aliases converts strings"
    (doseq [path [[:foo] ["foo"] [":foo"]]]
      (let [params (doto (InitializeParams.)
                     (.setInitializationOptions
                       (interop/clj->java
                         {:source-aliases path})))]
        (is (= #{:foo} (:source-aliases (#'server/client-settings params))))))))
