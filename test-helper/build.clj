(ns build
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.build.api :as b]))

(def lib 'com.github.clojure-lsp/clojure-lsp-test-helper)
(def current-version (string/trim (slurp (io/resource "CLOJURE_LSP_VERSION"))))
(def class-dir "target/classes")
(def basis {:project "deps.edn"})
(def file (format "target/%s.jar" (name lib)))

(defn clean [_]
  (b/delete {:path "target"}))

(defn pom [_opts]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :src-pom "./pom.xml"
                :version current-version
                :basis (b/create-basis basis)
                :src-dirs ["src"]
                :scm {:tag current-version}})
  (b/copy-file {:src (str class-dir "/META-INF/maven/" lib "/pom.xml") :target "pom.xml"})
  (b/copy-file {:src (str class-dir "/META-INF/maven/" lib "/pom.properties") :target "pom.properties"}))

(defn jar [opts]
  (clean opts)
  (pom opts)
  (b/copy-dir {:src-dirs ["src"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file file}))

(defn deploy-clojars [opts]
  (jar opts)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
   (merge {:installer :remote
           :artifact file
           :pom-file "pom.xml"}
          opts))
  opts)
