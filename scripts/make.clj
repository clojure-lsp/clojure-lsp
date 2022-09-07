(ns make
  (:require [babashka.fs :as fs]
            [babashka.deps :as deps]
            [babashka.process :as p]))

(def lsp-bin (if (fs/windows?)
               "clojure-lsp.exe"
               "clojure-lsp"))

(defn clean []
  (let [files (into ["cli/target"
                     (fs/path "cli" lsp-bin)
                     "cli/clojure-lsp-standalone.jar"
                     "lib/clojure-lsp.jar"
                     lsp-bin
                     "docs/README.md"
                     "docs/CHANGELOG.md"]
                    (fs/match "." "regex:clojure-lsp.*\\.jar"))]
    (doseq [f files]
      (fs/delete-tree f))))

(defn lib-pom []
  (-> (deps/clojure ["-T:build" "pom"] {:dir "lib" :inherit true})
      (p/check)))

(defn cli-pom []
  (-> (deps/clojure ["-T:build" "pom"] {:dir "cli" :inherit true})
      (p/check)))

(defn lib-jar []
  (-> (deps/clojure ["-T:build" "jar"] {:dir "lib" :inherit true})
      (p/check))
  (fs/move "lib/target/clojure-lsp.jar" "." {:replace-existing true}))

(defn cli-jar []
  (-> (deps/clojure ["-T:build" "prod-jar"] {:dir "cli" :inherit true})
      (p/check))
  (fs/move "cli/target/clojure-lsp-standalone.jar" "." {:replace-existing true}))

(defn cli-debug-jar []
  (-> (deps/clojure ["-T:build" "debug-jar"] {:dir "cli" :inherit true})
      (p/check))
  (fs/move "cli/target/clojure-lsp-standalone.jar" "." {:replace-existing true}))

(defn cli-jar-for-native []
  (-> (deps/clojure ["-T:build" "prod-jar-for-native"]
                    {:dir "cli"})
      (p/check))
  (fs/move "cli/target/clojure-lsp-standalone.jar" "." {:replace-existing true}))

(defn debug-cli []
  (-> (deps/clojure ["-T:build" "debug-cli"] {:dir "cli" :inherit true})
      p/check)
  (fs/move (fs/path "cli" lsp-bin) "." {:replace-existing true}))

(defn prod-cli []
  (-> (deps/clojure ["-T:build" "prod-cli"] {:dir "cli" :inherit true})
      p/check)
  (fs/move (fs/path "cli" lsp-bin) "." {:replace-existing true}))

(defn native-cli []
  (-> (deps/clojure ["-T:build" "native-cli"] {:dir "cli" :inherit true})
      (p/check))
  (fs/move (fs/path "cli" lsp-bin) "." {:replace-existing true}))

(defn test []
  (doseq [dir ["lib" "cli"]]
    (-> (deps/clojure ["-M:test"] {:dir dir :inherit true})
        (p/check))))

(defn pod-test []
  (-> (deps/clojure ["-M:pod-test"] {:dir "cli" :inherit true})
      (p/check)))

(defn integration-test []
  (let [bb (str \" (.get (.command (.info (java.lang.ProcessHandle/current)))) \")]
    (p/shell {:dir "cli"} (str bb " integration-test ../" lsp-bin))))

(defn lint-clean []
  (-> (deps/clojure ["-M:run" "clean-ns" "--dry" "--ns-exclude-regex" "sample-test.*" "--project-root" "../"]
                    {:dir "cli" :inherit true})
      (p/check)))

(defn lint-format []
  (-> (deps/clojure ["-M:run" "format" "--dry" "--ns-exclude-regex" "sample-test.*" "--project-root" "../"]
                    {:dir "cli" :inherit true})
      (p/check)))

(defn lint-diagnostics []
  (-> (deps/clojure ["-M:run" "diagnostics" "--dry" "--ns-exclude-regex" "sample-test.*" "--project-root" "../"]
                    {:dir "cli" :inherit true})
      (p/check)))

(defn lint-fix []
  (doseq [linter ["clean-ns" "format"]]
    (-> (deps/clojure ["-M:run" linter "--ns-exclude-regex" "sample-test.*" "--project-root" "../"]
                      {:dir "cli" :inherit true})
        (p/check))))

(defn release []
  (p/shell "./release"))

(defn deploy-clojars []
  (-> (deps/clojure ["-T:build" "deploy-clojars"]
                    {:dir "lib" :inherit true})
      (p/check)))

(defn deploy-clojars-standalone []
  (-> (deps/clojure ["-T:build" "deploy-clojars"]
                    {:dir "cli" :inherit true})
      (p/check)))

(defn local-webpage []
  (let [files ["CHANGELOG.md" "README.md"]]
    (doseq [f files]
      (fs/copy f "docs" {:replace-existing true}))
    (fs/copy-tree "images" "docs" {:replace-existing true})
    (p/shell "docker login docker.pkg.github.com")
    (p/shell (str "docker run --rm -it -p 8000:8000 -v " (fs/cwd) ":/docs docker.pkg.github.com/clojure-lsp/docs-image/docs-image"))))
