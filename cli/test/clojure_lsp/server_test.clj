(ns clojure-lsp.server-test
  (:require
   [lsp4clj.coercer :as coercer]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [clojure-lsp.server :as server])
  (:import
   (org.eclipse.lsp4j
     InitializeParams)))

(h/reset-db-after-test)

(deftest test-client-settings
  (testing "initializationOptions are null"
    (let [params (InitializeParams.)]
      (.setInitializationOptions params nil)
      (is (= "zipfile"
             (get (#'server/client-settings (coercer/java->clj params)) :dependency-scheme)))))
  (testing "document-formatting? is set to true if not provided"
    (let [params (InitializeParams.)]
      (is (= true (:document-formatting? (#'server/client-settings (coercer/java->clj params)))))))
  (testing "document-range-formatting? is set to true if not provided"
    (let [params (InitializeParams.)]
      (is (= true (:document-range-formatting? (#'server/client-settings (coercer/java->clj params)))))))
  (testing "text-document-sync-kind converts strings"
    (doseq [kind [:incremental "incremental" ":incremental"]]
      (let [params (doto (InitializeParams.)
                     (.setInitializationOptions
                       (coercer/clj->java
                         {:text-document-sync-kind kind})))]
        (is (= :incremental (:text-document-sync-kind (#'server/client-settings (coercer/java->clj params))))))))
  (testing "source-paths accepts strings"
    (doseq [path [["foo"] [":foo"]]]
      (let [params (doto (InitializeParams.)
                     (.setInitializationOptions
                       (coercer/clj->java
                         {:source-paths path})))]
        (is (= #{"foo"} (:source-paths (#'server/client-settings (coercer/java->clj params))))))))
  (testing "source-paths converts strings"
    (let [params (doto (InitializeParams.)
                   (.setInitializationOptions
                     (coercer/clj->java
                       {:source-paths [:foo]})))]
      (is (= #{"foo"} (:source-paths (#'server/client-settings (coercer/java->clj params)))))))
  (testing "source-aliases converts strings"
    (doseq [path [[:foo] ["foo"] [":foo"]]]
      (let [params (doto (InitializeParams.)
                     (.setInitializationOptions
                       (coercer/clj->java
                         {:source-aliases path})))]
        (is (= #{:foo} (:source-aliases (#'server/client-settings (coercer/java->clj params))))))))
  (testing "vectors and keywords are converted properly"
    (let [params (doto (InitializeParams.)
                   (.setInitializationOptions
                     (coercer/clj->java
                       {:cljfmt {:indents {'something [[:block 1]]}}})))]
      (is (= {:indents {'something [[:block 1]]}} (:cljfmt (#'server/client-settings (coercer/java->clj params))))))))

(deftest clj->java
  (testing "converting map with keyword and vectors"
    (is (= {"cljfmt" {"indents" {"something" [["block" 1]]}}}
           (coercer/clj->java {:cljfmt {:indents {'something [[:block 1]]}}})))))
