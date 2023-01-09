(ns build
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.build.api :as b]))

(def standalone-lib 'com.github.clojure-lsp/clojure-lsp-standalone)
(def server-lib 'com.github.clojure-lsp/clojure-lsp-server)
(def current-version (string/trim (slurp (io/resource "CLOJURE_LSP_VERSION"))))
(def class-dir "target/classes")
(def basis {:project "deps.edn"
            :extra "../lib/deps.edn"})
(def server-file "target/clojure-lsp-server.jar")
(def standalone-file "target/clojure-lsp-standalone.jar")

(defn clean [_]
  (b/delete {:path "target"}))

(defn pom [opts]
  (let [lib (or (:lib opts) server-lib)]
    (b/write-pom {:class-dir class-dir
                  :lib lib
                  :src-pom "./pom.xml"
                  :version current-version
                  :basis (b/create-basis (update basis :aliases concat (:extra-aliases opts)))
                  :src-dirs ["src" "../lib/src"]
                  :resource-dirs ["resources" "../lib/resources"]
                  :scm {:tag current-version}})))

(defn ^:private standalone-jar [opts]
  (clean opts)
  (pom (assoc opts :lib standalone-lib))
  (b/copy-dir {:src-dirs ["src" "../lib/src" "resources" "../lib/resources"]
               :target-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file standalone-file
           :main 'clojure-lsp.main
           :basis (b/create-basis (update basis :aliases concat (:extra-aliases opts)))}))

(defn ^:private standalone-aot-jar [opts]
  (clean opts)
  (println "Building uberjar...")
  (let [basis (b/create-basis (update basis :aliases concat (:extra-aliases opts)))
        src-dirs (into ["src" "resources"] (:extra-dirs opts))]
    (b/copy-dir {:src-dirs src-dirs
                 :target-dir class-dir})
    (b/compile-clj {:basis basis
                    :src-dirs src-dirs
                    :java-opts ["-Xmx2g" "-server"]
                    :class-dir class-dir})
    (b/uber {:class-dir class-dir
             :uber-file standalone-file
             :main 'clojure-lsp.main
             :basis basis})))

(defn ^:private bin
  "Create an executable `clojure-lsp` script out of UBER-FILE jar with
  the given OPTS.

  OPTS can be a map of
  :jvm-opts A vector of options ot pass to the JVM."
  [opts]
  (println "Generating bin...")

  (let [jvm-opts (concat (:jvm-opts opts []) ["-Xmx2g" "-server"])]
    ((requiring-resolve 'deps-bin.impl.bin/build-bin)
     {:jar standalone-file
      :name "clojure-lsp"
      :jvm-opts jvm-opts
      :skip-realign true})))

(defn server-jar [opts]
  (clean opts)
  (pom (assoc opts :lib server-lib))
  (b/copy-file {:src (str class-dir "/META-INF/maven/" server-lib "/pom.xml") :target "pom.xml"})
  (b/copy-file {:src (str class-dir "/META-INF/maven/" server-lib "/pom.properties") :target "pom.properties"})
  (println "Building jar...")
  (b/copy-dir {:src-dirs ["../lib/src" "../lib/resources" "src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file server-file}))

(defn server-install [opts]
  (server-jar opts)
  (println "Installing to local mvn repo...")
  (b/install {:basis (b/create-basis (update basis :aliases concat [:debug :test]))
              :lib server-lib
              :version current-version
              :jar-file server-file
              :class-dir class-dir}))

(defn debug-jar [opts]
  (standalone-aot-jar (merge opts {:extra-aliases [:debug :test]
                                   :extra-dirs ["dev"]})))

(defn debug-cli [opts]
  (standalone-aot-jar (merge opts {:extra-aliases [:debug :test]
                                   :extra-dirs ["dev"]}))
  (bin {:jvm-opts ["-XX:-OmitStackTraceInFastThrow"
                   "-Djdk.attach.allowAttachSelf=true"
                   "-Dclojure.core.async.go-checking=true"]}))

(defn prod-jar [opts]
  (standalone-aot-jar (merge opts {:extra-aliases [:native]})))

(defn prod-cli [opts]
  (standalone-aot-jar opts)
  (bin {}))

(defn native-cli [opts]
  (println "Building native image...")
  (if-let [graal-home (System/getenv "GRAALVM_HOME")]
    (let [jar (or (System/getenv "CLOJURE_LSP_JAR")
                  (do (prod-jar opts)
                      standalone-file))
          native-image (if (fs/windows?) "native-image.cmd" "native-image")
          command (->> [(str (io/file graal-home "bin" native-image))
                        "-jar" jar
                        "clojure-lsp"
                        "-H:+ReportExceptionStackTraces"
                        "--verbose"
                        "--no-fallback"
                        "--native-image-info"
                        (or (System/getenv "CLOJURE_LSP_XMX")
                            "-J-Xmx8g")
                        (when (= "true" (System/getenv "CLOJURE_LSP_STATIC"))
                          ["--static"
                           (if (= "true" (System/getenv "CLOJURE_LSP_MUSL"))
                             ["--libc=musl" "-H:CCompilerOption=-Wl,-z,stack-size=2097152"]
                             ["-H:+StaticExecutableWithDynamicLibC"])])]
                       (flatten)
                       (remove nil?))
          {:keys [exit]} (b/process {:command-args command})]
      (when-not (= 0 exit)
        (System/exit exit)))
    (println "Set GRAALVM_HOME env")))

(defn deploy-clojars [opts]
  (server-jar opts)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
   (merge {:installer :remote
           :artifact server-file
           :pom-file "pom.xml"}
          opts))
  (standalone-jar opts)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
   (merge {:installer :remote
           :artifact standalone-file
           :pom-file "pom.xml"}
          opts))
  opts)
