(ns make
  (:refer-clojure :exclude [test])
  (:require
   [babashka.deps :as deps]
   [babashka.fs :as fs]
   [babashka.process :as p]))

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

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn clean
  "Clean all artifacts produced by the various tasks."
  []
  (let [lsp-bin-native (lsp-bin-filename :native)
        lsp-bin-script  (lsp-bin-filename :script)
        files (into ["cli/target"
                     (fs/path "cli" lsp-bin-native)
                     (fs/path "cli" lsp-bin-script)
                     "cli/clojure-lsp-standalone.jar"
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

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn lib-pom [] (build "lib" "pom"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn cli-pom [] (build "cli" "pom"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn lib-jar []
  (build "lib" "jar")
  (mv-here "lib/target/clojure-lsp.jar"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn cli-jar
  "Build `cli` jar."
  []
  (build "cli" "prod-jar")
  (mv-here "cli/target/clojure-lsp-standalone.jar"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn cli-debug-jar []
  (build "cli" "debug-jar")
  (mv-here "cli/target/clojure-lsp-standalone.jar"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn cli-jar-for-native []
  (build "cli" "prod-jar-for-native")
  (mv-here "cli/target/clojure-lsp-standalone.jar"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn debug-cli
  "Build the `clojure-lsp[.bat]` cli exec script (suppots `cider-nrepl`/`clj-async-profile`)."
  []
  (build "cli" "debug-cli")
  (mv-here (fs/path "cli" (lsp-bin-filename :script))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn prod-cli
  "Build the `clojure-lsp[.bat]` cli exec script."
  []
  (build "cli" "prod-cli")
  (mv-here (fs/path "cli" (lsp-bin-filename :script))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn native-cli
  "Build the native `clojure-lsp[.exe]` cli executable with `graalvm`."
  []
  (build "cli" "native-cli")
  (mv-here (fs/path "cli" (lsp-bin-filename :native))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn test-lib "Run all unit tests in lib/." [] (unit-test "lib"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn test-cli "Run all unit tests in cli/." [] (unit-test "cli"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn pod-test [] (clj! "cli" ["-M:pod-test"]))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
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
   (clj! "cli" (cond-> ["-M:run" linter "--ns-exclude-regex" "sample-test.*" "--project-root" "../"]
                 dry? (conj "--dry")))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn lint-clean [] (lint "clean-ns" {:dry? true}))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn lint-format [] (lint "format" {:dry? true}))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn lint-diagnostics [] (lint "diagnostics" {:dry? true}))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn lint-fix [] (run! lint ["clean-ns" "format"]))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn release []
  (p/shell "./release"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn deploy-clojars [] (build "lib" "deploy-clojars"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn deploy-clojars-standalone [] (build "cli" "deploy-clojars"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn local-webpage []
  (let [files ["CHANGELOG.md" "README.md"]]
    (doseq [f files]
      (fs/copy f "docs" {:replace-existing true}))
    (fs/copy-tree "images" "docs" {:replace-existing true})
    (p/shell "docker login docker.pkg.github.com")
    (p/shell (str "docker run --rm -it -p 8000:8000 -v " (fs/cwd) ":/docs docker.pkg.github.com/clojure-lsp/docs-image/docs-image"))))
