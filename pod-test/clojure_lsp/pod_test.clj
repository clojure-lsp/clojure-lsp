(ns clojure-lsp.pod-test
  (:require
   [babashka.pods :as pods]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(def pod-spec (if (= "native" (System/getenv "CLOJURE_LSP_TEST_ENV"))
                ["./clojure-lsp"]
                ["clojure" "-M:run"]))

(pods/load-pod pod-spec)
(require '[clojure-lsp.api :as clojure-lsp])

#_{:clj-kondo/ignore [:unresolved-var]}
(deftest pod-test
  (testing "analyze-project!"
    (let [result (clojure-lsp/analyze-project!
                   {:project-root (io/file "integration-test/sample-test")})]
      (is result)))
  (testing "clean-ns!"
    (let [result (clojure-lsp/clean-ns!
                   {:project-root (io/file "integration-test/sample-test")
                    :namespace '[sample-test.api.format.a]
                    :dry? true})]
      (is (= 1 (:result-code result)))
      (is (seq (:edits result)))))
  (testing "diagnostics"
    (let [result (clojure-lsp/diagnostics
                   {:project-root (io/file "integration-test/sample-test")
                    :dry? true})]
      (is (not= 0 (:result-code result)))
      (is (seq (:diagnostics result)))))
  (testing "format!"
    (let [result (clojure-lsp/format!
                   {:project-root (io/file "integration-test/sample-test")
                    :namespace '[sample-test.api.format.a]
                    :dry? true})]
      (is (= 1 (:result-code result)))
      (is (seq (:edits result))))))
