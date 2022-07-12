(ns clojure-lsp.debug
  "This namespace is for debugging clojure-lsp during development.

  It provides a few examples of how to conduct performance analysis.

  It's on the classpath when building with `make` (or `make debug-cli`) so don't
  leave any clutter that would break the build."
  (:require
   [clj-async-profiler.core :as profiler]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.completion :as f.completion]
   [clojure-lsp.shared :as shared]
   [criterium.core :as bench]))

(defn uri-in-project [filepath]
  (let [db @db/db*]
    (shared/filename->uri (shared/absolute-path filepath db) db)))

(defn open-file
  "Opens a `file` with its default application."
  [file]
  (.open (java.awt.Desktop/getDesktop) file))

(defmacro profile-times
  "Profiles the `body` by running it in a loop `n` times, returning the
  flamegraph file."
  [n body]
  `(let [n# ~n]
     (println "profiling" n# "iterations")
     (let [time-start# (System/nanoTime)
           result# (profiler/profile {:min-width 5
                                      :return-file true}
                                     (dotimes [_n# n#] ~body))
           elapsed-s# (/ (- (System/nanoTime) time-start#) 1e9)]
       (println "Profiled for" elapsed-s# "seconds. (Should be 5-10 seconds to have enough samples.)")
       result#)))

(defmacro profile-by-runtime
  "Generate a flamegraph of `body`. Uses the `:est-runtime-in-ms` in the `opts`
  to calculate how many times it needs to execute `body` for the profiling to
  run for about 10 seconds. This should get a reasonable number of samples. See
  http://clojure-goes-fast.com/blog/clj-async-profiler-tips/#make-sure-you-have-enough-samples

  If opts includes `:open? true`, will [open][open-file] the generated flamegraph."
  [opts body]
  `(let [opts# ~opts
         n# (int (/ 10e3 (:est-runtime-in-ms opts#)))
         flamegraph-file# (profile-times n# ~body)]
     (when (:open? opts#) (open-file flamegraph-file#))
     flamegraph-file#))

(defn estimate-runtime-ms-naive* [f]
  (let [time-start (System/nanoTime)]
    (f)
    (let [est-ms (/ (- (System/nanoTime) time-start) 1e6)]
      (println "mean runtime" est-ms "ms")
      est-ms)))

(defmacro estimate-runtime-ms-naive [body]
  `(estimate-runtime-ms-naive* (fn [] ~body)))

(defn estimate-runtime-ms-quick-bench* [f]
  (let [results (bench/quick-benchmark* f {})]
    (bench/report-point-estimate "mean runtime" (:mean results))
    (bench/report-point-estimate-sqrt "std-deviation" (:variance results))
    (* 1e3 (first (:mean results)))))

(defmacro estimate-runtime-ms-quick-bench [body]
  `(estimate-runtime-ms-quick-bench* (fn [] ~body)))

(defmacro auto-profile
  "Generate a flamegraph of `body`. Estimates the runtime of the body, then
  delegates to [[profile-by-runtime]] to get a reasonable number of samples.

  To estimate the runtime of the body by running it once, choose
  `:estimate-strategy :naive` (default).

  For a more rigorous estimate, delegate to criterium by choosing
  `:estimate-strategy :quick-bench`.

  To [[open-file]] the generated flamegraph, add `:open? true`.

  If you already know the mean runtime of the `body`, use [[profile-by-runtime]]
  or [[profile-times]] to avoid extraneous runtime estimates.

  One way you might know the runtime of the `body` is by inspecting the *stdout*
  of this command. It will report its estimate and how many iterations it
  intends to do."
  [opts body]
  `(let [opts# ~opts
         estimate-strategy# (:estimate-strategy opts# :naive)]
     (println "estimating runtime via" estimate-strategy# "strategy")
     (let [est-ms# (case estimate-strategy#
                     :naive       (estimate-runtime-ms-naive ~body)
                     :quick-bench (estimate-runtime-ms-quick-bench ~body))
           opts# (assoc opts# :est-runtime-in-ms est-ms#)]
       (profile-by-runtime opts# ~body))))

(comment

  (do
    (println "\nProfiling completion in queries.clj")
    (auto-profile
      {:open? true
       ;; :estimate-strategy :quick-bench
       }
      (f.completion/completion (uri-in-project "lib/src/clojure_lsp/queries.clj") 24 8 @db/db*)))


  #_())
