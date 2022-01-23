(ns build
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.build.api :as b]
   [deps-bin.impl.bin :as deps-bin]))

(def lib 'com.github.clojure-lsp/clojure-lsp)
(def current-version (string/trim (slurp (io/resource "CLOJURE_LSP_VERSION"))))
(def class-dir "target/classes")
(def basis {:project "deps.edn"})
(def uber-file (format "target/%s-standalone.jar" (name lib)))

(defn clean [_]
  (b/delete {:path "target"}))

(defn javac [opts]
  (clean opts)
  (println "Compiing java classes...")
  (b/javac {:src-dirs ["src-java"]
            :class-dir class-dir
            :basis (b/create-basis basis)}))

(defn ^:private uber [opts]
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

(defn ^:private bin []
  (println "Generating bin...")
  (deps-bin/build-bin {:jar uber-file
                       :name "clojure-lsp"
                       :jvm-opts ["-Xmx2g" "-server"]
                       :skip-realign true}))

(def prod-jar uber)

(defn prod-jar-for-native [opts]
  (uber (merge opts {:extra-aliases [:native]})))

(defn debug-cli [opts]
  (uber (merge opts {:extra-aliases [:debug]}))
  (bin))

(defn prod-cli [opts]
  (prod-jar opts)
  (bin))

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
