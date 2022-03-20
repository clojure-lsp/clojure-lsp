(ns build
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.build.api :as b]))

(def lib 'com.github.clojure-lsp/lsp4clj)
(def current-version "0.0.1")
(def class-dir "target/classes")
(def basis {:project "deps.edn"})
(def jar-file (format "target/%s.jar" (name lib)))

(defn clean [_]
  (b/delete {:path "target"}))

(defn pom [opts]
  (b/write-pom {:target ""
                :lib lib
                :version current-version
                :basis (b/create-basis (update basis :aliases concat (:extra-aliases opts)))
                :src-dirs ["src" "../protocols/src"]
                :scm {:tag current-version}}))

(defn jar [opts]
  (clean nil)
  (pom opts)
  (b/copy-dir {:src-dirs ["src" "../protocols/src"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn deploy-clojars [opts]
  (jar opts)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
   (merge {:installer :remote
           :artifact jar-file
           :pom-file "pom.xml"}
          opts))
  opts)
