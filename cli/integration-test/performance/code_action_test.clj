(ns performance.code-action-test
  "run performance tests for Code Actions"
  (:require
   [clojure.test :refer [deftest is testing]]
   [integration.fixture :as fixture]
   [integration.lsp :as lsp]
   [performance.helper :as h]))

;; test will fail if average is higher than this
(def ^:private p50-max-runtime-ms 500)
(def ^:private p90-max-runtime-ms 1000)
(def ^:private p99-max-runtime-ms 2000)

(lsp/clean-after-test)

(deftest view-and-execute-code-action
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request
                  {:initializationOptions fixture/default-init-options
                   :capabilities          {:window    {:showDocument {:support true}}
                                           :workspace {:workspaceEdit {:documentChanges true}}}}))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification h/sample-file-name))

  (testing "Verify code action requests aren't over max specification"
    (testing "measure only one location near top of file"
      (let [execution-times (sort (h/execute-multiple #(lsp/request! (fixture/code-action-request h/sample-file-name 5 4)) nil))
            p50 (h/compute-percentile 0.5 execution-times)
            p90 (h/compute-percentile 0.9 execution-times)
            p99 (h/compute-percentile 0.99 execution-times)]
        (h/report-percentiles "one location" p50 p90 p99)
        (is (< p50 p50-max-runtime-ms))
        (is (< p90 p90-max-runtime-ms))
        (is (< p99 p99-max-runtime-ms))))

    (testing "measure many locations in simple sample file"
      (let [execution-times (h/measure-n-times (fn [file-name line] (lsp/request! (fixture/code-action-request file-name line 1)))
                                               nil
                                               h/sample-file-name
                                               (range 1 301))
            p50 (h/compute-percentile 0.5 execution-times)
            p90 (h/compute-percentile 0.9 execution-times)
            p99 (h/compute-percentile 0.99 execution-times)]
        (h/report-percentiles "many locations" p50 p90 p99)
        (is (< p50 p50-max-runtime-ms))
        (is (< p90 p90-max-runtime-ms))
        (is (< p99 p99-max-runtime-ms))))

    (testing "measure many locations in sample file with many lines"
      (let [execution-times (h/measure-n-times (fn [file-name line] (lsp/request! (fixture/code-action-request file-name line 1)))
                                               nil
                                               h/sample-file-with-many-lines
                                               (range 1 301))
            p50 (h/compute-percentile 0.5 execution-times)
            p90 (h/compute-percentile 0.9 execution-times)
            p99 (h/compute-percentile 0.99 execution-times)]
        (h/report-percentiles "many locations with many lines" p50 p90 p99)
        (is (< p50 p50-max-runtime-ms))
        (is (< p90 p90-max-runtime-ms))
        (is (< p99 p99-max-runtime-ms))))

    (testing "measure many locations in sample file with many symbols"
      (let [execution-times (h/measure-n-times (fn [file-name line] (lsp/request! (fixture/code-action-request file-name line 1)))
                                               nil
                                               h/sample-file-with-many-symbols
                                               (range 1 301))
            p50 (h/compute-percentile 0.5 execution-times)
            p90 (h/compute-percentile 0.9 execution-times)
            p99 (h/compute-percentile 0.99 execution-times)]
        (h/report-percentiles "many locations with many symbols" p50 p90 p99)
        (is (< p50 p50-max-runtime-ms))
        (is (< p90 p90-max-runtime-ms))
        (is (< p99 p99-max-runtime-ms))))

    (testing "measure many locations in sample file with high fanout of imports"
      (let [execution-times (h/measure-n-times (fn [file-name line] (lsp/request! (fixture/code-action-request file-name line 1)))
                                               nil
                                               h/sample-file-with-high-fanout
                                               (range 1 301))
            p50 (h/compute-percentile 0.5 execution-times)
            p90 (h/compute-percentile 0.9 execution-times)
            p99 (h/compute-percentile 0.99 execution-times)]
        (h/report-percentiles "many locations with high fanout" p50 p90 p99)
        (is (< p50 p50-max-runtime-ms))
        (is (< p90 p90-max-runtime-ms))
        (is (< p99 p99-max-runtime-ms))))

    (testing "measure many locations in sample file with large structures"
      (let [execution-times (h/measure-n-times (fn [file-name line] (lsp/request! (fixture/code-action-request file-name line 1)))
                                               nil
                                               h/sample-file-with-large-structures
                                               (range 1 301))
            p50 (h/compute-percentile 0.5 execution-times)
            p90 (h/compute-percentile 0.9 execution-times)
            p99 (h/compute-percentile 0.99 execution-times)]
        (h/report-percentiles "many locations with large structures" p50 p90 p99)
        (is (< p50 p50-max-runtime-ms))
        (is (< p90 p90-max-runtime-ms))
        (is (< p99 p99-max-runtime-ms))))))

