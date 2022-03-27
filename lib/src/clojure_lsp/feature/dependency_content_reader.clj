(ns clojure-lsp.feature.dependency-content-reader
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [lsp4clj.protocols.logger :as logger])
  (:import
   (java.io File)
   (java.net
     URL
     JarURLConnection)
   (org.benf.cfr.reader.api
     CfrDriver
     CfrDriver$Builder)))

(set! *warn-on-reflection* true)

(defn ^:private copy-class-file [uri entry stream db]
  (let [cache-path (config/cache-file db)
        dest-file (io/file cache-path "java" "classes" (str entry))]
    (logger/info (format "Copying class URI %s to %s" uri dest-file))
    (io/make-parents dest-file)
    (io/copy stream dest-file)
    dest-file))

(defn ^:private decompile! [^File class-file dest-path db]
  (let [cache-path (config/cache-file db)
        decompiled-file (io/file cache-path "java" "decompiled")
        class-path (.getCanonicalPath class-file)
        _ (logger/info (format "Decompiling java class %s" class-path))
        driver ^CfrDriver (.. (CfrDriver$Builder.)
                              ;; CFR stout is not reliable, prefer output to file so we can read later
                              (withOptions {"outputdir" (.getCanonicalPath decompiled-file)})
                              (build))
        err-sym (java.io.StringWriter.)]
    (binding [*err* err-sym]
      (with-out-str
        (.analyse driver [class-path]))
      (when-not (string/blank? (str err-sym))
        (logger/warn "Non-fatal error from CFR:" (str err-sym))))
    (io/file decompiled-file dest-path)))

(defn ^:private uri->translated-file [uri {:keys [db]}]
  (if (shared/jar-file? uri)
    (let [jar-uri (shared/ensure-jarfile uri)]
      (if (shared/class-file? jar-uri)
        (let [url (URL. jar-uri)
              connection ^JarURLConnection (.openConnection url)
              jar (.getJarFile connection)
              entry (.getJarEntry connection)]
          (with-open [stream (.getInputStream jar entry)]
            (let [file (copy-class-file jar-uri entry stream db)
                  dest-file (string/replace (str entry) #".class$" ".java")]
              (shared/filename->uri (.getCanonicalPath ^File (decompile! file dest-file db)) db))))
        jar-uri))
    uri))

(defn uri->translated-uri [uri components]
  (uri->translated-file uri components))

(defn read-content! [uri components]
  (slurp (uri->translated-file uri components)))
