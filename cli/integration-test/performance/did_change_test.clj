(ns performance.did-change-test
  "run performance tests for Did Change"
  (:require
   [clojure.test :refer [deftest is testing]]
   [integration.fixture :as fixture]
   [integration.lsp :as lsp]
   [performance.helper :as h]))

;; waiting tests will fail if average is higher than this time
(def ^:private p50-sync-max-runtime-ms 750)
(def ^:private p90-sync-max-runtime-ms 1000)
(def ^:private p99-sync-max-runtime-ms 2000)

;; async tests will fail if average is higher than this time
(def ^:private p50-async-max-runtime-ms 250)
(def ^:private p90-async-max-runtime-ms 500)
(def ^:private p99-async-max-runtime-ms 750)

(lsp/clean-after-test)

;; measure how long didChange takes to execute
(deftest view-and-execute-did-change
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request
                  {:initializationOptions fixture/default-init-options
                   :capabilities          {:window    {:showDocument {:support true}}
                                           :workspace {:workspaceEdit {:documentChanges true}}}
                   :textDocumentSync {:openClose true
                                      :change 1
                                      :save {:includeText true}}}))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification h/sample-file-name))

  ;; this test is only testing the responsiveness to change requests; since
  ;; the didChange API is asynchronous, it should return very quickly
  (testing "measure time to change in simple sample file but not wait for the change"
    (let [execution-times (h/measure-n-times (fn [file-name version]
                                               (lsp/notify! (fixture/did-change-notification file-name version [["changed text\n" version 0 version 0]])))
                                             (fn [file-name version]
                                               (lsp/notify! (fixture/did-change-notification file-name (inc version) [["" version 0 (inc version) 0]])))
                                             h/sample-file-name
                                             (range 1 50 2))
          p50 (h/compute-percentile 0.5 execution-times)
          p90 (h/compute-percentile 0.9 execution-times)
          p99 (h/compute-percentile 0.99 execution-times)]
      (h/report-percentiles "sample file async" p50 p90 p99)
      (is (< p50 p50-async-max-runtime-ms))
      (is (< p90 p90-async-max-runtime-ms))
      (is (< p99 p99-async-max-runtime-ms))))

  ;; in this test, wait for the server to do something
  (testing "measure time to change in simple sample file and verify that the change happened"
    (let [execution-times (h/measure-n-times (fn [file-name version]
                                               (lsp/notify! (fixture/did-change-notification file-name version [["changed text\n" version 0 version 0]]))
                                               (lsp/client-awaits-server-diagnostics file-name))
                                             (fn [file-name version]
                                               (lsp/notify! (fixture/did-change-notification file-name (inc version) [["" version 0 (inc version) 0]]))
                                               (lsp/client-awaits-server-diagnostics file-name))
                                             h/sample-file-name
                                             (range 1 50 2))
          p50 (h/compute-percentile 0.5 execution-times)
          p90 (h/compute-percentile 0.9 execution-times)
          p99 (h/compute-percentile 0.99 execution-times)]
      (h/report-percentiles "sample file sync" p50 p90 p99)
      (is (< p50 p50-sync-max-runtime-ms))
      (is (< p90 p90-sync-max-runtime-ms))
      (is (< p99 p99-sync-max-runtime-ms)))))

