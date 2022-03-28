(ns clojure-lsp.feature.java-interop
  (:require
   [babashka.fs :as fs]
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
   (java.nio.file Path)
   (java.util.zip ZipInputStream)
   (org.benf.cfr.reader.api
     CfrDriver
     CfrDriver$Builder)))

(set! *warn-on-reflection* true)

(def ^:private java-logger-tag (shared/colorize "[Java]" :bright-yellow))

(defn ^:private decompile! [^File class-file dest-path db]
  (let [cache-path (config/cache-file db)
        decompiled-file (io/file cache-path "java" "decompiled")
        class-path (.getCanonicalPath class-file)
        _ (logger/info java-logger-tag (format "Decompiling java class %s" class-path))
        driver ^CfrDriver (.. (CfrDriver$Builder.)
                              ;; CFR stout is not reliable, prefer output to file so we can read later
                              (withOptions {"outputdir" (.getCanonicalPath decompiled-file)})
                              (build))
        err-sym (java.io.StringWriter.)]
    (binding [*err* err-sym]
      (with-out-str
        (.analyse driver [class-path]))
      (when-not (string/blank? (str err-sym))
        (logger/warn java-logger-tag "Non-fatal error from CFR:" (str err-sym))))
    (io/file decompiled-file dest-path)))

(defn ^:private copy-class-file [uri entry stream db]
  (let [cache-path (config/cache-file db)
        dest-file (io/file cache-path "java" "classes" (str entry))]
    (logger/info java-logger-tag (format "Copying class URI %s to %s" uri dest-file))
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

(def ^:private jdk-source-zip-filename "src.zip")

(defn ^:private jdk-path->jdk-source-paths [^File root-path]
  (when root-path
    (let [parent (.getParentFile root-path)]
      [(io/file root-path jdk-source-zip-filename)
       (io/file root-path "lib" jdk-source-zip-filename)
       (io/file parent jdk-source-zip-filename)
       (io/file parent "lib" jdk-source-zip-filename)])))

(defn ^:private find-local-jdk-source []
  (let [java-home (some-> (or (config/get-property "java.home")
                              (config/get-env "JAVA_HOME")) io/file)
        paths (concat []
                      (jdk-path->jdk-source-paths java-home)
                      (->> (fs/which-all "java")
                           (map (comp
                                  jdk-path->jdk-source-paths
                                  #(some-> ^File % .getParentFile .getParentFile)
                                  #(.toFile ^Path %)
                                  fs/real-path))
                           flatten))]
    (some->> paths
             (filter #(.canRead ^File %))
             first)))

(def ^:private default-jdk-source-uri
  "https://raw.githubusercontent.com/clojure-lsp/jdk-source/main/openjdk-19/reduced/source.zip")

(defn ^:private download-jdk!
  [^String jdk-download-url
   ^File dest-jdk-file]
  (io/make-parents dest-jdk-file)
  (shared/logging-time
    (str java-logger-tag " Downloading JDK source took %s secs.")
    (try
      (let [{:keys [body content-type status error]} (http/request! jdk-download-url)]
        (if (or error
                (not= 200 status))
          (logger/error java-logger-tag "Could not download JDK source." error)
          ;; TODO handle more common downloadable content-types
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
            (logger/error java-logger-tag "Could not download JDK source, unknown content-type" content-type))))
      (catch Exception e
        (logger/error java-logger-tag "Error Downloading JDK source." e)))))

(defn ^:private analyze-jdk-source! [path db]
  (let [result (shared/logging-time
                 (str java-logger-tag " Analyzing JDK source with clj-kondo took %s secs.")
                 (lsp.kondo/run-kondo-on-jdk-source! path))
        kondo-analysis (select-keys (:analysis result) [:java-class-definitions])
        analysis (->> kondo-analysis
                      lsp.kondo/normalize-analysis
                      (group-by :filename))]
    (loop [state-db @db]
      (when-not (compare-and-set! db state-db (update state-db :analysis merge analysis))
        (logger/warn java-logger-tag "Analyzis outdated from java analysis, trying again...")
        (recur @db)))
    (-> (shared/uri->path (:project-root-uri @db))
        (db/read-cache db)
        (update :analysis merge analysis)
        (db/upsert-cache! db))))

(defn retrieve-jdk-source-and-analyze!
  "Find JDK source and analyze it with clj-kondo for java class definitions.

  If user has custom :jdk-source-uri, we don't try to find JDK source local
  or download it, we make sure zip is extracted otherwize extract it.

  If no custom URI set, we try to find a local JDK source on Java
  instalation which is present most of the time and extract to global
  cache dir.

  Otherwise, we download default OpenJDK source if setting is enabled."
  [{:keys [db]}]
  (let [global-cache-dir (config/global-lsp-cache-dir)
        jdk-dir-file (io/file global-cache-dir "jdk")
        jdk-result-file (io/file jdk-dir-file "result")
        installed-jdk-source-uri (and (shared/file-exists? jdk-result-file)
                                      (slurp jdk-result-file))
        user-jdk-source-uri (settings/get db [:java :jdk-source-uri])
        jdk-source-uri (or user-jdk-source-uri default-jdk-source-uri)]

    (if (or (not installed-jdk-source-uri)
            (and user-jdk-source-uri
                 (= installed-jdk-source-uri user-jdk-source-uri)))
      (if-let [local-jdk-source-zip (and (not user-jdk-source-uri)
                                         (find-local-jdk-source))]
        (do
          (logger/info java-logger-tag "Found local JDK source zip, extracting to global LSP cache dir...")
          (fs/unzip local-jdk-source-zip jdk-dir-file {:replace-existing true})
          (spit jdk-result-file (.getCanonicalPath ^File local-jdk-source-zip)))
        (do
          (logger/info java-logger-tag "Local JDK source not found.")
          (if-let [local-jdk-uri (or (and (shared/plain-uri? jdk-source-uri)
                                          jdk-source-uri)
                                     (shared/filename->uri jdk-source-uri db))]
            (do
              (logger/info java-logger-tag "Found local JDK source URI, extracting to global LSP cache dir...")
              (fs/unzip (io/file (shared/uri->filename local-jdk-uri)) jdk-dir-file {:replace-existing true})
              (spit jdk-result-file local-jdk-uri))
            (if (settings/get db [:java :download-jdk-source?] false)
              (do
                (logger/info java-logger-tag "Downloading JDK source to global LSP cache dir...")
                (download-jdk! jdk-source-uri jdk-dir-file)
                (spit jdk-result-file jdk-source-uri))
              (logger/warn java-logger-tag "Skipping download JDK source, setting `:java :download-jdk-source?` is disabled.")))))
      (logger/info java-logger-tag "JDK source already present on global LSP cache dir."))

    (if (or installed-jdk-source-uri
            (and (shared/file-exists? jdk-result-file)
                 (slurp jdk-result-file)))
      (do
        (analyze-jdk-source! (.getCanonicalPath jdk-dir-file) db)
        (logger/info java-logger-tag "JDK Source analyzed successfully."))
      (logger/warn java-logger-tag "JDK Source not found, skipping java analysis."))))
