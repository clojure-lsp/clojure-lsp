(ns performance.initialization-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [integration.fixture :as fixture]
   [integration.lsp :as lsp]
   [performance.helper :as h]))

(def ^:private p50-cold-max-ms 15000)
(def ^:private p50-warm-max-ms 2000)

(defn- delete-cache! 
  []
  "safely removes cache files"
  (let [root-path (.getPath (java.net.URI. (get-in (fixture/initialize-request) [1 :rootUri])))
        cache-dir (io/file root-path ".lsp" ".cache")]
    (when (.exists cache-dir)
      (run! io/delete-file (reverse (file-seq cache-dir))))))

(defn- run-init-test 
  [label max-p50 setup-fn]
  (testing (str "measure " label)
    (let [times (sort
                  (h/execute-multiple
                    (fn []
                      (setup-fn)
                      (lsp/start-process!)
                      (lsp/request! (fixture/initialize-request
                                      {:initializationOptions fixture/default-init-options}))
                      (lsp/notify! (fixture/initialized-notification)))
                    (fn []
                      (lsp/request! (fixture/shutdown-request))
                      (lsp/notify! [:exit {}]))))
          p50 (h/compute-percentile 0.5 times)
          p90 (h/compute-percentile 0.9 times)
          p99 (h/compute-percentile 0.99 times)]
      (h/report-percentiles label p50 p90 p99)
      (is (< p50 max-p50)))))

(deftest initialization-performance
  (lsp/clean-after-test)

  (run-init-test "Initialization (Cold Start)" p50-cold-max-ms delete-cache!)

  (run-init-test "Initialization (Warm Start)" p50-warm-max-ms
                 (fn []
                   (when-not (.exists (io/file "integration-test/sample-test/.lsp/.cache"))
                     (delete-cache!)))))