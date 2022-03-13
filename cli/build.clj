(ns build
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.build.api :as b]))

(def lib 'com.github.clojure-lsp/clojure-lsp)
(def clojars-lib 'com.github.clojure-lsp/clojure-lsp-standalone)
(def current-version (string/trim (slurp (io/resource "CLOJURE_LSP_VERSION"))))
(def lsp-class-dir "target/lsp-classes")
(def class-dir "target/classes")
(def basis {:project "deps.edn"})
(def uber-file (format "target/%s-standalone.jar" (name lib)))

(defn clean [_]
  (b/delete {:path "target"}))

(defn javac [opts]
  (clean opts)
  (println "Compiling java classes...")
  (b/javac {:src-dirs ["src-java"]
            :class-dir lsp-class-dir
            :basis (b/create-basis basis)})
  (b/copy-dir {:src-dirs [lsp-class-dir]
               :target-dir class-dir}))

(defn pom [opts]
  (b/write-pom {:target ""
                :lib clojars-lib
                :version current-version
                :basis (b/create-basis (update basis :aliases concat (:extra-aliases opts)))
                :src-dirs ["src" "../lib/src" "../protocols/src" "../lsp4clj/src"]
                :resource-dirs ["resources"]
                :scm {:tag current-version}}))

(defn ^:private uber [opts]
  (clean opts)
  (javac opts)
  (pom opts)
  (b/copy-dir {:src-dirs ["src" "../lib/src" "../protocols/src" "../lsp4clj/src" "resources" "../lib/resources"]
               :target-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :main 'clojure-lsp.main
           :basis (b/create-basis (update basis :aliases concat (:extra-aliases opts)))}))

(defn ^:private uber-aot [opts]
  (clean opts)
  (javac opts)
  (println "Building uberjar...")
  (let [basis (b/create-basis (update basis :aliases concat (:extra-aliases opts)))]
    (b/copy-dir {:src-dirs ["src" "resources"]
                 :target-dir class-dir})
    (b/compile-clj {:basis basis
                    :src-dirs ["src" "resources"]
                    :java-opts ["-Xmx2g" "-server"]
                    :class-dir class-dir})
    (b/uber {:class-dir class-dir
             :uber-file uber-file
             :main 'clojure-lsp.main
             :basis basis})))

(defn ^:private bin [opts]
  (println "Generating bin...")
  ((requiring-resolve 'deps-bin.impl.bin/build-bin)
   {:jar uber-file
    :name "clojure-lsp"
    :jvm-opts (concat (:jvm-opts opts []) ["-Xmx2g" "-server"])
    :skip-realign true}))

(def prod-jar uber-aot)

(defn prod-jar-for-native [opts]
  (uber-aot (merge opts {:extra-aliases [:native]})))

(defn debug-cli [opts]
  (uber-aot (merge opts {:extra-aliases [:debug]}))
  (bin {}))

(defn debug-perf-cli [opts]
  (uber-aot (merge opts {:extra-aliases [:debug :performance]}))
  (bin {:jvm-opts ["-Djdk.attach.allowAttachSelf=true"]}))

(defn prod-cli [opts]
  (prod-jar opts)
  (bin {}))

(defn native-cli [opts]
  (println "Building native image...")
  (if-let [graal-home (System/getenv "GRAALVM_HOME")]
    (let [jar (or (System/getenv "CLOJURE_LSP_JAR")
                  (do (prod-jar-for-native opts)
                      uber-file))
          command (->> [(str (io/file graal-home "bin" "native-image"))
                        "-jar" jar
                        "-H:+ReportExceptionStackTraces"
                        "--verbose"
                        "--no-fallback"
                        "--native-image-info"
                        (or (System/getenv "CLOJURE_LSP_XMX")
                            "-J-Xmx8g")
                        (when (= "true" (System/getenv "CLOJURE_LSP_STATIC"))
                          ["--static" "-H:+StaticExecutableWithDynamicLibC"])
                        "clojure-lsp"]
                       (flatten)
                       (remove nil?))
          {:keys [exit]} (b/process {:command-args command})]
      (when-not (= 0 exit)
        (System/exit exit)))
    (println "Set GRAALVM_HOME env")))

(defn deploy-clojars [opts]
  (uber opts)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
   (merge {:installer :remote
           :artifact uber-file
           :pom-file "pom.xml"}
          opts))
  opts)
