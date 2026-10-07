(ns clojure-lsp.feature.kondo-repro-test
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.config :as config]
   [clojure-lsp.feature.kondo-repro :as f.kondo-repro]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing use-fixtures]]))

(use-fixtures :each (fn [f]
                      ;; ignore the global clojure-lsp config of the machine
                      (with-redefs [config/resolve-for-root (constantly {})]
                        (f))))

(defn ^:private temp-project []
  (let [root (fs/canonicalize (fs/create-temp-dir))
        file (fn [& paths] (str (apply fs/path root paths)))]
    (fs/create-dirs (fs/path root "src"))
    (fs/create-dirs (fs/path root "deps"))
    (fs/create-dirs (fs/path root ".clj-kondo"))
    (spit (file "src" "a.clj") "(ns a)")
    (spit (file "src" "b.clj") "(ns b)")
    (spit (file "deps" "dep.jar") "")
    {:root (str root)
     :src (file "src")
     :a-file (file "src" "a.clj")
     :b-file (file "src" "b.clj")
     :jar (file "deps" "dep.jar")}))

(defn ^:private project-db
  ([project]
   (project-db project {}))
  ([{:keys [root src jar]} settings]
   {:env :unit-test
    :project-root-uri (shared/filename->uri root {})
    :classpath [src jar]
    :settings (merge {:source-paths #{src}} settings)}))

(defn ^:private lint-paths [args]
  (rest (drop-while #(not= "--lint" %) args)))

(defn ^:private arg-value [args flag]
  (second (drop-while #(not= flag %) args)))

(defn ^:private flag? [args flag]
  (boolean (some #{flag} args)))

(deftest repro-test
  (let [{:keys [root src a-file b-file jar] :as project} (temp-project)
        a-uri (shared/filename->uri a-file {})
        b-uri (shared/filename->uri b-file {})]
    (testing "startup with full dependencies analysis and the lint of an opened file"
      (let [{:keys [steps project-root analysis-type clj-kondo-coordinate]}
            (f.kondo-repro/repro (project-db project) {:uris [a-uri]})
            [copy-configs external internal single-file] steps]
        (is (= root project-root))
        (is (= :project-and-full-dependencies analysis-type))
        (is (= (lsp.kondo/clj-kondo-coordinate) clj-kondo-coordinate))
        (is (= [:copy-configs :external-paths :internal-paths :single-file] (map :id steps)))
        (is (every? (comp nil? :unsupported-options) steps))
        (testing "copies configs from the whole classpath"
          (is (= [src jar] (lint-paths (:args copy-configs))))
          (is (flag? (:args copy-configs) "--skip-lint"))
          (is (flag? (:args copy-configs) "--copy-configs")))
        (testing "analyzes without linting the classpath minus the project paths"
          (is (= [jar] (lint-paths (:args external))))
          (is (flag? (:args external) "--skip-lint"))
          (is (flag? (:args external) "--parallel"))
          (is (string/includes? (arg-value (:args external) "--config") ":shallow true")))
        (testing "analyzes and lints the project files"
          (is (= #{a-file b-file} (set (lint-paths (:args internal)))))
          (is (not (flag? (:args internal) "--skip-lint")))
          (is (= (str (fs/path root ".clj-kondo")) (arg-value (:args internal) "--config-dir"))))
        (testing "lints the opened file via stdin"
          (is (= ["-"] (lint-paths (:args single-file))))
          (is (= a-file (:stdin single-file)))
          (is (= a-file (arg-value (:args single-file) "--filename")))
          (is (= "clj" (arg-value (:args single-file) "--lang")))
          (is (not (flag? (:args single-file) "--parallel"))))))
    (testing "project only analysis doesn't analyze the external classpath"
      (let [{:keys [steps]} (f.kondo-repro/repro (project-db project) {:analysis-type :project-only})]
        (is (= [:copy-configs :internal-paths] (map :id steps)))
        (is (string/includes? (arg-value (:args (last steps)) "--config") ":locals false"))))
    (testing "shallow analysis analyzes the external classpath with less data"
      (let [{:keys [steps]} (f.kondo-repro/repro (project-db project) {:analysis-type :project-and-shallow-analysis})
            external (first (filter #(= :external-paths (:id %)) steps))]
        (is (string/includes? (arg-value (:args external) "--config") ":arglists false"))))
    (testing "configs are not copied when disabled"
      (let [{:keys [steps]} (f.kondo-repro/repro (project-db project {:copy-kondo-configs? false}) {})]
        (is (= [:external-paths :internal-paths] (map :id steps)))
        (is (not-any? #(flag? (:args %) "--copy-configs") steps))))
    (testing "files matching :paths-ignore-regex are not linted"
      (let [{:keys [steps notes]} (f.kondo-repro/repro (project-db project {:paths-ignore-regex [".*b\\.clj"]})
                                                       {:uris [a-uri b-uri]})]
        (is (= [a-file] (lint-paths (:args (first (filter #(= :internal-paths (:id %)) steps))))))
        (is (= [a-file] (keep :stdin steps)))
        (is (some #(string/includes? % b-file) notes))))
    (testing "the external classpath is analyzed in batches"
      (let [jars (mapv #(str (fs/path root "deps" (str "dep-" % ".jar"))) (range 121))
            _ (run! #(spit % "") jars)
            {:keys [steps]} (f.kondo-repro/repro (assoc (project-db project) :classpath (cons src jars)) {})]
        (is (= ["Analyze the external classpath, batch 1/2"
                "Analyze the external classpath, batch 2/2"]
               (keep #(when (= :external-paths (:id %)) (:description %)) steps)))))
    (testing "no classpath"
      (let [{:keys [steps notes]} (f.kondo-repro/repro (assoc (project-db project) :classpath nil) {})]
        (is (= [:internal-paths] (map :id steps)))
        (is (some #(string/includes? % "No classpath found") notes))))))

(deftest ->script-test
  (let [repro {:clojure-lsp-version "1.0.0"
               :clj-kondo-version "2026.01.01"
               :clj-kondo-coordinate {:mvn/version "2026.01.01"}
               :project-root "/my project"
               :analysis-type :project-and-full-dependencies
               :notes ["Some note."]
               :steps [{:id :internal-paths
                        :description "Analyze and lint the project files"
                        :args ["--parallel" "--config" "{:a 'b}" "--lint" "/my project/src/a.clj" "/src/b.clj"]}
                       {:id :single-file
                        :description "Lint a.clj"
                        :args ["--lang" "clj" "--lint" "-"]
                        :stdin "/my project/src/a.clj"}]}
        script (f.kondo-repro/->script repro)
        lines (string/split-lines script)]
    (testing "header"
      (is (= "#!/bin/sh" (first lines)))
      (is (some #{"# Note: Some note."} lines))
      (is (some #{"  clojure -Sdeps '{:deps {clj-kondo/clj-kondo {:mvn/version \"2026.01.01\"}}}' -M -m clj-kondo.main \"$@\""} lines))
      (is (some #{"CACHE_DIR=\"${CACHE_DIR:-$(mktemp -d)}\""} lines))
      (is (some #{"cd '/my project' || exit 1"} lines)))
    (testing "setup steps output is silenced when there are opened files"
      (is (string/includes? script (string/join "\n" ["# 1/2 Analyze and lint the project files"
                                                      "clj_kondo --cache-dir \"$CACHE_DIR\" --parallel --config '{:a '\"'\"'b}' \\"
                                                      "  --lint \\"
                                                      "  '/my project/src/a.clj' \\"
                                                      "  /src/b.clj \\"
                                                      "  > /dev/null"]))))
    (testing "opened files are linted via stdin"
      (is (string/includes? script (string/join "\n" ["# 2/2 Lint a.clj"
                                                      "clj_kondo --cache-dir \"$CACHE_DIR\" --lang clj \\"
                                                      "  --lint - \\"
                                                      "  < '/my project/src/a.clj'"]))))
    (testing "project files lint output is shown when there are no opened files"
      (let [script (f.kondo-repro/->script (update repro :steps (partial take 1)))]
        (is (string/includes? script "  /src/b.clj\n"))
        (is (not (string/includes? script "/dev/null")))))
    (testing "uses the clj-kondo binary when the bundled version is unknown"
      (is (string/includes? (f.kondo-repro/->script (assoc repro :clj-kondo-coordinate nil))
                            "  clj-kondo \"$@\"")))
    (testing "doesn't use the cache dir when the cache is disabled"
      (let [step {:id :internal-paths
                  :description "Lint"
                  :args ["--cache" "false" "--lint" "a.clj"]}]
        (is (string/includes? (f.kondo-repro/->script (assoc repro :steps [step]))
                              "clj_kondo --cache false \\"))))))
