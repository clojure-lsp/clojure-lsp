(ns performance.entrypoint
  (:require
   [babashka.fs :as fs]
   [babashka.process :as p]
   [clojure.java.io :as io]
   [performance.code-actions :as perf.code-actions]))

(defn ^:private setup-project [git-url branch project-name]
  (let [path (.getCanonicalPath (io/file "performance-test" project-name))]
    (fs/delete-tree path)
    (p/shell "git" "clone" "--depth=1" "--branch" branch
             git-url path)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn run-all [& args]
  (when-not (first args)
    (println "First arg must be path to clojure-lsp binary")
    (System/exit 1))

  (setup-project "https://github.com/ericdallo/clojure-sample.git" "clojure-lsp-performance-test" "small-project")

  (perf.code-actions/run "small-project" 10000)

  (System/exit 0))
