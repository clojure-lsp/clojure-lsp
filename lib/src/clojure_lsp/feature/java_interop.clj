(ns clojure-lsp.feature.java-interop
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.db :as db]
   [clojure-lsp.http :as http]
   [clojure-lsp.kondo :as lsp.kondo]
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
   (java.util.zip ZipInputStream)
   (org.benf.cfr.reader.api
     CfrDriver
     CfrDriver$Builder)))

(set! *warn-on-reflection* true)

(defn ^:private decompile! [^File class-file dest-path db]
  (let [cache-path (config/cache-file db)
        decompiled-file (io/file cache-path "java" "decompiled")
        class-path (.getCanonicalPath class-file)
        _ (logger/info (format "[JDK] Decompiling java class %s" class-path))
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

(defn ^:private copy-class-file [uri entry stream db]
  (let [cache-path (config/cache-file db)
        dest-file (io/file cache-path "java" "classes" (str entry))]
    (logger/info (format "[JDK] Copying class URI %s to %s" uri dest-file))
    (io/make-parents dest-file)
    (io/copy stream dest-file)
    dest-file))

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
                  dest-file (string/replace (str entry) #".class$" ".java")
                  decompiled-file ^File (decompile! file dest-file db)]
              (shared/filename->uri (.getCanonicalPath decompiled-file) db))))
        jar-uri))
    uri))

(defn uri->translated-uri [uri components]
  (uri->translated-file uri components))

(defn read-content! [uri components]
  (slurp (uri->translated-file uri components)))

(def ^:private default-jdk-source-uri
  "https://raw.githubusercontent.com/clojure-lsp/jdk-source/main/openjdk-19/reduced/source.zip")

(defn ^:private download-jdk!
  [^String jdk-download-url
   ^File dest-jdk-file]
  (io/make-parents dest-jdk-file)
  (logger/info "[JDK] Downloading JDK source...")
  (shared/logging-time
    "[JDK] Downloading JDK source took %s secs."
    (try
      (let [{:keys [body content-type status error]} (http/request! jdk-download-url)]
        (if (or error
                (not= 200 status))
          (logger/error "[JDK] Could not download JDK source." error)
          ;; TODO handle mode common downloadable content-types
          (case content-type
            "application/zip"
            (with-open [stream (ZipInputStream. body)]
              (loop [entry (.getNextEntry stream)]
                (when entry
                  (let [savePath (str dest-jdk-file File/separatorChar (.getName entry))
                        saveFile (File. savePath)]
                    (if (.isDirectory entry)
                      (when-not (.exists saveFile)
                        (.mkdirs saveFile))
                      (let [parentDir (File. (.substring savePath 0 (.lastIndexOf savePath (int File/separatorChar))))]
                        (when-not (.exists parentDir)
                          (.mkdirs parentDir))
                        (clojure.java.io/copy stream saveFile)))
                    (recur (.getNextEntry stream))))))
            (logger/error "[JDK] Could not download JDK source, unknown content-type" content-type))))
      (catch Exception e
        (logger/error "[JDK] Error Downloading JDK source." e)))))

(defn ^:private analyze-jdk-source! [path db]
  (let [result (shared/logging-time
                 "[JDK] Analyzing JDK source with clj-kondo took %s secs."
                 (lsp.kondo/run-kondo-on-jdk-source! path))
        kondo-analysis (select-keys (:analysis result) [:java-class-definitions])
        analysis (->> kondo-analysis
                      lsp.kondo/normalize-analysis
                      (group-by :filename))]
    (loop [state-db @db]
      (when-not (compare-and-set! db state-db (update state-db :analysis merge analysis))
        (logger/warn "Analyzis outdated from java analysis, trying again...")
        (recur @db)))
    (-> (shared/uri->path (:project-root-uri @db))
        (db/read-cache db)
        (update :analysis merge analysis)
        (db/upsert-cache! db))))

(defn ^:private download-jdk-source
  [jdk-source-uri db]
  (let [global-cache-dir (config/global-lsp-cache-dir)
        dest-jdk-file (io/file global-cache-dir "jdk")
        dest-jdk-result-file (io/file dest-jdk-file "successfully-downloaded")]
    (if (shared/file-exists? dest-jdk-result-file)
      (logger/info "[JDK] Skipping downloading JDK source, already found at" dest-jdk-file)
      (do
        (download-jdk! jdk-source-uri dest-jdk-file)
        (spit dest-jdk-result-file "1")))
    (.getCanonicalPath dest-jdk-file) db))

(defn retrieve-and-analyze! [{:keys [db]}]
  (let [jdk-source-uri (settings/get db [:java :jdk-source-uri] default-jdk-source-uri)
        download-jdk-source? (settings/get db [:java :download-jdk-source?] false)]
    (when-let [jdk-source-uri' (or (and (shared/plain-uri? jdk-source-uri)
                                        (shared/filename->uri jdk-source-uri db))
                                   (when download-jdk-source? (download-jdk-source jdk-source-uri db)))]
      (logger/info (str "[JDK] Analyzing JDK source found at " jdk-source-uri "..."))
      (analyze-jdk-source! jdk-source-uri' db)
      (logger/info "[JDK] Source analyzed successfully."))))
