(ns make
  (:refer-clojure :exclude [test])
  (:require
   [babashka.deps :as deps]
   [babashka.fs :as fs]
   [babashka.process :as p]
   [babashka.tasks :refer [clojure]]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def windows? (#'fs/windows?))

(defn lsp-bin-filename
  "Returns the `clojure-lsp` executable filename for this system based
  on the intented USAGE.

  On MS-Windows, the extensions of the script and native executable
  are different (`clojure-lsp.bat` vs `clojure-lsp.exe`) and serve
  different needs. We should be able to differentiate
  betweent the two.

  USAGE can be one of

  :native The filename of the native executable.

  :script The filename of the executable script wrapper."
  [usage]
  (cond-> "clojure-lsp"
    windows? (str (case usage
                    :native ".exe"
                    :script ".bat"))))

(defn clean
  "Clean all artifacts produced by the various tasks."
  []
  (let [lsp-bin-native (lsp-bin-filename :native)
        lsp-bin-script (lsp-bin-filename :script)
        files (into ["cli/target"
                     (fs/path "cli" lsp-bin-native)
                     (fs/path "cli" lsp-bin-script)
                     "cli/clojure-lsp-cli-standalone.jar"
                     "lib/clojure-lsp.jar"
                     lsp-bin-native
                     lsp-bin-script
                     "docs/README.md"
                     "docs/CHANGELOG.md"]
                    (fs/match "." "regex:clojure-lsp.*\\.jar"))]
    (doseq [f files]
      (fs/delete-tree f))))

(defn ^:private clj! [dir cmd]
  (-> (deps/clojure cmd {:dir dir, :inherit true})
      (p/check)))

(defn ^:private build [dir tool] (clj! dir ["-T:build" tool]))

(defn ^:private unit-test [dir]
  (println :running-unit-tests... dir)
  (clj! dir ["-M:test"])
  (println))

(defn ^:private mv-here [file]
  (fs/move file "." {:replace-existing true}))

(defn lib-pom [] (build "lib" "pom"))

(defn cli-pom [] (build "cli" "pom"))

(defn test-helper-pom [] (build "test-helper" "pom"))

(defn lib-jar []
  (build "lib" "jar")
  (mv-here "lib/target/clojure-lsp.jar"))

(defn server-jar
  "Build `cli` server jar."
  []
  (build "cli" "server-jar")
  (mv-here "cli/target/clojure-lsp-server.jar"))

(defn test-helper-jar []
  (build "test-helper" "jar"))

(defn server-install
  "Build `cli` server jar and install."
  []
  (build "cli" "server-install"))

(defn standalone-install
  "Build `cli` standalone jar and install."
  []
  (build "cli" "standalone-install"))

(defn cli-debug-jar []
  (build "cli" "debug-jar")
  (mv-here "cli/target/clojure-lsp-standalone.jar"))

(defn cli-prod-jar []
  (build "cli" "prod-jar")
  (mv-here "cli/target/clojure-lsp-standalone.jar"))

(defn debug-cli
  "Build the `clojure-lsp[.bat]` cli exec script (suppots `cider-nrepl`/`clj-async-profile`)."
  []
  (build "cli" "debug-cli")
  (mv-here (fs/path "cli" (lsp-bin-filename :script))))

(defn prod-cli
  "Build the `clojure-lsp[.bat]` cli exec script."
  []
  (build "cli" "prod-cli")
  (mv-here (fs/path "cli" (lsp-bin-filename :script))))

(defn native-cli
  "Build the native `clojure-lsp[.exe]` cli executable with `graalvm`."
  []
  (build "cli" "native-cli")
  (mv-here (fs/path "cli" (lsp-bin-filename :native))))

(defn native-cli-pgo-instrument
  "For pgo profiles instumentation for improve native-image performance."
  []
  (build "cli" "native-cli-pgo-instrument")
  (mv-here (fs/path "cli" (lsp-bin-filename :native))))

(defn test-lib "Run all unit tests in lib/." [] (unit-test "lib"))

(defn test-cli "Run all unit tests in cli/." [] (unit-test "cli"))

(defn pod-test [] (clj! "cli" ["-M:pod-test"]))

(defn integration-test
  "Run the integration tests in 'test/integration-test/' using `./clojure-lsp[.bat|.exe]`.

  There should only be one clojure-lsp executable found, throws error
  otherwise."
  []
  (let [lsp-bins (->> [:native :script] (map lsp-bin-filename) distinct)
        lsp-bins-found (->> lsp-bins (filter fs/exists?) (into #{}))
        bb (str \" (.get (.command (.info (java.lang.ProcessHandle/current)))) \")]
    (case (count lsp-bins-found)
      0 (throw (ex-info "No clojure-lsp executables found." {:searched-for lsp-bins}))
      1 (p/shell {:dir "cli"} bb "integration-test" (str (fs/path ".." (first lsp-bins-found))))
      (throw (ex-info "More than one clojure-lsp executables found. Can only work with one."
                      {:bin-found lsp-bins-found})))))

(defn ^:private lint
  ([linter] (lint linter {}))
  ([linter {:keys [dry?]}]
   (clj! "cli" (cond-> ["-M:run" linter "--ns-exclude-regex" ".*sample-test.*" "--project-root" "../"]
                 dry? (conj "--dry")))))

(defn lint-clean [] (lint "clean-ns" {:dry? true}))

(defn lint-format [] (lint "format" {:dry? true}))

(defn lint-diagnostics [] (lint "diagnostics" {:dry? true}))

(defn lint-fix [] (run! lint ["clean-ns" "format"]))

(defn release []
  (p/shell "./release"))

(defn deploy-clojars [] (build "lib" "deploy-clojars"))

(defn deploy-clojars-standalone [] (build "cli" "deploy-clojars"))

(defn deploy-clojars-test-helper [] (build "test-helper" "deploy-clojars"))

(defn local-webpage []
  (let [files ["CHANGELOG.md" "README.md"]]
    (doseq [f files]
      (fs/copy f "docs" {:replace-existing true}))
    (fs/copy-tree "images" "docs" {:replace-existing true})
    (p/shell "docker login docker.pkg.github.com")
    (p/shell (str "docker run --rm -it -p 8000:8000 -v " (fs/cwd) ":/docs docker.pkg.github.com/clojure-lsp/docs-image/docs-image"))))

(defn test-reflection []
  (let [err (:err (clojure
                    {:dir "cli" :continue true :err :string}
                    "-M:dev -e" "(require '[clojure-lsp.main])"))]
    (when-not (str/blank? err)
      (println err))
    (assert (not (str/includes? err "Reflection warning")))))
