(ns clojure-lsp.feature.file-content-reader
  (:require
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
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
        dest-file (io/file cache-path "java-decompiled" (str entry))]
    (logger/info (format "Copying class URI %s to %s" uri dest-file))
    (io/make-parents dest-file)
    (io/copy stream dest-file)
    dest-file))

(defn ^:private decompile! [^File class-file]
  (let [class-path (.getCanonicalPath class-file)
        driver ^CfrDriver (.. (CfrDriver$Builder.)
                              (build))]
    (logger/info (format "Decompiling java class %s" class-path))
    (with-out-str
      (.analyse driver [class-path]))))

(defn read-content [uri {:keys [db]}]
  (let [url (URL. uri)
        connection ^JarURLConnection (.openConnection url)
        jar (.getJarFile connection)
        entry (.getJarEntry connection)]
    (with-open [stream (.getInputStream jar entry)]
      (if (shared/class-file? uri)
        (-> uri
            (copy-class-file entry stream db)
            decompile!)
        (slurp stream)))))
