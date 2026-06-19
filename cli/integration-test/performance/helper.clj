(ns performance.helper
  (:require
   [clojure.math :as math]))

;; read files from the actual clojure-lsp codebase.  Note that this relies on the directory structure
(def sample-file-name "../../../../../lib/src/clojure_lsp/feature/code_actions.clj")
(def sample-file-with-many-lines "../../../../../lib/src/clojure_lsp/refactor/transform.clj")
(def sample-file-with-many-symbols "../../../../../lib/src/clojure_lsp/queries.clj")
(def sample-file-with-high-fanout "../../../../../lib/src/clojure_lsp/handlers.clj")
(def sample-file-with-large-structures "../../../../../lib/src/clojure_lsp/common_symbols.clj")

(defn report-percentiles [message p50 p90 p99]
  (println message)
  (println "  p50: " p50)
  (println "  p90: " p90)
  (println "  p99: " p99))

(def ^:private execution-count 15)

(defn ^:private nano->ms [end-time start-time]
  (int (/ (- end-time start-time) 1000000)))

(defn execute-multiple
  "Execute body multiple times, returning a collection of the runtimes.
   Optional finalize-fn can be used for cleanup without effecting body runtime"
  [body-fn finalize-fn]
  (doall (for [_ (range execution-count)
               :let [start-time (System/nanoTime)]]
           (do
             (body-fn)
             (let [end-time (System/nanoTime)]
               (when finalize-fn
                 (finalize-fn))
               (nano->ms end-time start-time))))))

(defn measure-n-times [body finalize file-name input-range]
  (sort (reduce (fn [acc line] (into acc
                                     (execute-multiple
                                       (partial body file-name line)
                                       (when finalize (partial finalize file-name line)))))
                [] input-range)))

(defn compute-percentile
  "assumes coll is already sorted"
  [p coll]
  (when (seq coll)
    (let [cnt (count coll)
          index (int (math/ceil (* p (dec cnt))))]
      (nth coll index))))