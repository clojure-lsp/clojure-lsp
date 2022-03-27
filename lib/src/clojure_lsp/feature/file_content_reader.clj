(ns clojure-lsp.feature.file-content-reader
  (:require
   [clojure-lsp.settings :as settings]
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

(defn ^:private cache-file [db]
  (let [project-root (shared/uri->path (:project-root-uri @db))
        overwritten-path (some-> (settings/get db [:cache-path])
                                 io/file)
        default (io/file (str project-root) ".lsp" ".cache")]
    ^java.io.File (or overwritten-path default)))

(defn ^:private copy-class-file [uri entry stream db]
  (let [cache-path (cache-file db)
        dest-file (io/file cache-path "java" "classes" (str entry))]
    (logger/info (format "Copying class URI %s to %s" uri dest-file))
    (io/make-parents dest-file)
    (io/copy stream dest-file)
    dest-file))

(defn ^:private decompile! [^File class-file dest-path db]
  (let [cache-path (cache-file db)
        decompiled-file (io/file cache-path "java" "decompiled")
        class-path (.getCanonicalPath class-file)
        _ (logger/info (format "Decompiling java class %s" class-path))
        driver ^CfrDriver (.. (CfrDriver$Builder.)
                              (withOptions {"outputdir" (.getCanonicalPath decompiled-file)})
                              (build))
        err-sym (java.io.StringWriter.)]
    (binding [*err* err-sym]
      (with-out-str
        (.analyse driver [class-path]))
      (when-not (string/blank? (str err-sym))
        (logger/warn "Non-fatal error from cfr:" (str err-sym))))
    (slurp (io/file decompiled-file dest-path))))

(defn read-content [uri {:keys [db]}]
  (let [url (URL. uri)
        connection ^JarURLConnection (.openConnection url)
        jar (.getJarFile connection)
        entry (.getJarEntry connection)]
    (if (shared/class-file? uri)
      (let [file (with-open [stream (.getInputStream jar entry)]
                   (copy-class-file uri entry stream db))]
        (decompile! file (string/replace (str entry) #".class$" ".java") db))
      (with-open [stream (.getInputStream jar entry)]
        (slurp stream)))))
