(ns performance.did-open-test
  "run performance tests for didOpen"
  (:require
   [clojure.test :refer [deftest is testing]]
   [integration.fixture :as fixture]
   [integration.lsp :as lsp]
   [performance.helper :as h]))

;; test will fail if average is higher than this
(def ^:private p50-max-runtime-ms 750)
(def ^:private p90-max-runtime-ms 1500)
(def ^:private p99-max-runtime-ms 2000)

(lsp/clean-after-test)

(deftest view-and-execute-did-open
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request
                  {:initializationOptions fixture/default-init-options
                   :capabilities          {:window    {:showDocument {:support true}}
                                           :workspace {:workspaceEdit {:documentChanges true}}}}))
  (lsp/notify! (fixture/initialized-notification))

  (testing "measure open time in simple sample file"
    (let [execution-times (h/measure-n-times (fn [file-name _]
                                               (lsp/notify! (fixture/did-open-source-path-notification file-name))
                                               (lsp/client-awaits-server-diagnostics file-name))
                                             (fn [file-name _]
                                               (lsp/notify! (fixture/did-close file-name)))
                                             h/sample-file-name
                                             (range 1 11))
          p50 (h/compute-percentile 0.5 execution-times)
          p90 (h/compute-percentile 0.9 execution-times)
          p99 (h/compute-percentile 0.99 execution-times)]
      (h/report-percentiles "many locations" p50 p90 p99)
      (is (< p50 p50-max-runtime-ms))
      (is (< p90 p90-max-runtime-ms))
      (is (< p99 p99-max-runtime-ms))))
  (testing "measure open time in sample file with many symbols"
    (let [execution-times (h/measure-n-times (fn [file-name _]
                                               (lsp/notify! (fixture/did-open-source-path-notification file-name))
                                               (lsp/client-awaits-server-diagnostics file-name))
                                             (fn [file-name _]
                                               (lsp/notify! (fixture/did-close file-name)))
                                             h/sample-file-with-many-symbols
                                             (range 1 11))
          p50 (h/compute-percentile 0.5 execution-times)
          p90 (h/compute-percentile 0.9 execution-times)
          p99 (h/compute-percentile 0.99 execution-times)]
      (h/report-percentiles "many symbols" p50 p90 p99)
      (is (< p50 p50-max-runtime-ms))
      (is (< p90 p90-max-runtime-ms))
      (is (< p99 p99-max-runtime-ms))))
  (testing "measure open time in sample file with high fanout"
    (let [execution-times (h/measure-n-times (fn [file-name _]
                                               (lsp/notify! (fixture/did-open-source-path-notification file-name))
                                               (lsp/client-awaits-server-diagnostics file-name))
                                             (fn [file-name _]
                                               (lsp/notify! (fixture/did-close file-name)))
                                             h/sample-file-with-high-fanout
                                             (range 1 11))
          p50 (h/compute-percentile 0.5 execution-times)
          p90 (h/compute-percentile 0.9 execution-times)
          p99 (h/compute-percentile 0.99 execution-times)]
      (h/report-percentiles "high fanout" p50 p90 p99)
      (is (< p50 p50-max-runtime-ms))
      (is (< p90 p90-max-runtime-ms))
      (is (< p99 p99-max-runtime-ms))))
  (testing "measure open time in sample file with large structures"
    (let [execution-times (h/measure-n-times (fn [file-name _]
                                               (lsp/notify! (fixture/did-open-source-path-notification file-name))
                                               (lsp/client-awaits-server-diagnostics file-name))
                                             (fn [file-name _]
                                               (lsp/notify! (fixture/did-close file-name)))
                                             h/sample-file-with-large-structures
                                             (range 1 11))
          p50 (h/compute-percentile 0.5 execution-times)
          p90 (h/compute-percentile 0.9 execution-times)
          p99 (h/compute-percentile 0.99 execution-times)]
      (h/report-percentiles "large structures" p50 p90 p99)
      (is (< p50 p50-max-runtime-ms))
      (is (< p90 p90-max-runtime-ms))
      (is (< p99 p99-max-runtime-ms)))))

