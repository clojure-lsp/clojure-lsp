(ns performance.code-action-test
  "run performance tests for Code Actions"
  (:require
   [clojure.math :as math]
   [clojure.test :refer [deftest is testing]]
   [integration.fixture :as fixture]
   [integration.lsp :as lsp]))

;; read files from the actual clojure-lsp codebase.  Note that this relies on the directory structure
(def ^:private sample-file-name "../../../../../lib/src/clojure_lsp/feature/code_actions.clj")
(def ^:private sample-file-with-many-lines "../../../../../lib/src/clojure_lsp/refactor/transform.clj")
(def ^:private sample-file-with-many-symbols "../../../../../lib/src/clojure_lsp/queries.clj")
(def ^:private sample-file-with-high-fanout "../../../../../lib/src/clojure_lsp/handlers.clj")
(def ^:private sample-file-with-large-structures "../../../../../lib/src/clojure_lsp/common_symbols.clj")

;; test will fail if average is higher than this
(def ^:private median-max-runtime-ms 500)
(def ^:private p90-max-runtime-ms 1000)

(def ^:private execution-count 15)

(defn ^:private nano->ms [end-time start-time]
  (int (/ (- end-time start-time) 1000000)))

(defn ^:private execute-multiple
  "find the mean runtime "
  [body-fn]
  (doall (for [_ (range execution-count)
               :let [start-time (System/nanoTime)]]
           (do
             (body-fn)
             (let [end-time (System/nanoTime)]
               (nano->ms end-time start-time))))))

(defn ^:private compute-mean [execution-times]
  (int (/ (apply + execution-times) (count execution-times))))

;; from Clojure Cookbook
(defn ^:private compute-median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (compute-mean [bottom-val top-val])))))

(defn ^:private compute-percentile [p coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        index (int (math/ceil (* p (dec cnt))))]
    (nth sorted index)))

(lsp/clean-after-test)

(deftest view-and-execute-code-action
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request
                  {:initializationOptions fixture/default-init-options
                   :capabilities          {:window    {:showDocument {:support true}}
                                           :workspace {:workspaceEdit {:documentChanges true}}}}))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification sample-file-name))

  (testing "Verify code action requests aren't over max average runtime"
    (testing "measure only one location near top of file"
      (let [execution-times (execute-multiple #(lsp/request! (fixture/code-action-request sample-file-name 5 4)))]
        (is (< (compute-mean execution-times) median-max-runtime-ms))
        (is (< (compute-median execution-times) median-max-runtime-ms))
        (is (< (compute-percentile 0.9 execution-times) p90-max-runtime-ms))))

    (testing "measure many locations in simple sample file"
      (doseq [line-num (range 1 301)]
        (let [execution-times (execute-multiple #(lsp/request! (fixture/code-action-request sample-file-name line-num 1)))]
          (is (< (compute-median execution-times) median-max-runtime-ms))
          (is (< (compute-percentile 0.9 execution-times) p90-max-runtime-ms)))))

    (testing "measure many locations in sample file with many lines"
      (doseq [line-num (range 1 301)]
        (let [execution-times (execute-multiple #(lsp/request! (fixture/code-action-request sample-file-with-many-lines line-num 1)))]
          (is (< (compute-median execution-times) median-max-runtime-ms))
          (is (< (compute-percentile 0.9 execution-times) p90-max-runtime-ms)))))

    (testing "measure many locations in sample file with many symbols"
      (doseq [line-num (range 1 301)]
        (let [execution-times (execute-multiple #(lsp/request! (fixture/code-action-request sample-file-with-many-symbols line-num 1)))]
          (is (< (compute-median execution-times) median-max-runtime-ms))
          (is (< (compute-percentile 0.9 execution-times) p90-max-runtime-ms)))))

    (testing "measure many locations in sample file with high fanout of imports"
      (doseq [line-num (range 1 301)]
        (let [execution-times (execute-multiple #(lsp/request! (fixture/code-action-request sample-file-with-high-fanout line-num 1)))]
          (is (< (compute-median execution-times) median-max-runtime-ms))
          (is (< (compute-percentile 0.9 execution-times) p90-max-runtime-ms)))))

    (testing "measure many locations in sample file with large structures"
      (doseq [line-num (range 1 301)]
        (let [execution-times (execute-multiple #(lsp/request! (fixture/code-action-request sample-file-with-large-structures line-num 1)))]
          (is (< (compute-median execution-times) median-max-runtime-ms))
          (is (< (compute-percentile 0.9 execution-times) p90-max-runtime-ms)))))))

