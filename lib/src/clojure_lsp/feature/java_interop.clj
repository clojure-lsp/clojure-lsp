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
   (java.net JarURLConnection URL)
   (java.nio.file Path)
   (java.util.zip ZipInputStream)
   (org.benf.cfr.reader.api CfrDriver CfrDriver$Builder)))

(set! *warn-on-reflection* true)

(def ^:private java-logger-tag "[Java]")

(defn ^:private decompile! [^File class-file dest-path db]
  (let [cache-path (config/local-cache-dir db)
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
  (let [cache-path (config/local-cache-dir db)
        dest-file (io/file cache-path "java" "classes" (str entry))]
    (logger/info java-logger-tag (format "Copying class URI %s to %s" uri dest-file))
    (io/make-parents dest-file)
    (io/copy stream dest-file)
    dest-file))

(defn ^:private uri->translated-file [uri db]
  ;; TODO consider local class files not from jar
  (if (shared/jar-file? uri)
    (let [jar-uri (shared/ensure-jarfile uri db)]
      (if (shared/class-file? jar-uri)
        ;; TODO zipfile doesn't work with URL
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

(defn uri->translated-uri [uri db]
  (uri->translated-file uri db))

(defn read-content! [uri db]
  (slurp (uri->translated-file uri db)))

(def ^:private jdk-source-zip-filename "src.zip")

(defn ^:private jdk-path->jdk-source-paths [^File root-path]
  (when root-path
    (let [parent (.getParentFile root-path)]
      [(io/file root-path jdk-source-zip-filename)
       (io/file root-path "lib" jdk-source-zip-filename)
       (io/file parent jdk-source-zip-filename)
       (io/file parent "lib" jdk-source-zip-filename)])))

(defn ^:private find-local-jdk-source [java-home]
  (let [java-home (some-> (or java-home
                              (config/get-property "java.home")
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

(defn ^:private download-jdk!
  [^String jdk-download-url
   ^File dest-jdk-file]
  (shared/logging-time
    (str java-logger-tag " Downloading JDK source took %s")
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
                    (recur (.getNextEntry stream)))))
              true)
            (logger/error java-logger-tag "Could not download JDK source, unknown content-type" content-type))))
      (catch Exception e
        (logger/error java-logger-tag "Error Downloading JDK source." e)))))

(defn ^:private analyze-and-cache-jdk-source! [paths new-checksums db*]
  (let [results (shared/logging-time
                  (str java-logger-tag " Analyzing JDK source with clj-kondo took %s")
                  (lsp.kondo/run-kondo-on-jdk-source! paths @db*))]
    (swap! db* #(-> %
                    (lsp.kondo/db-with-results results)
                    (update :analysis-checksums merge new-checksums)))
    (db/read-and-update-global-cache!
      (fn [db]
        (-> db
            (update :analysis-checksums merge new-checksums)
            (update :analysis merge (:analysis results))
            (assoc :version db/version))))))

(def ^:private default-jdk-source-uri
  "https://raw.githubusercontent.com/clojure-lsp/jdk-source/main/openjdk-19/reduced/source.zip")

(defn ^:private jdk-analysis-decision
  [installed-jdk-source-uri
   custom-jdk-source-uri
   local-jdk-source-file*
   download-jdk-source?]
  (cond
    (and installed-jdk-source-uri
         (or (not custom-jdk-source-uri)
             (= installed-jdk-source-uri custom-jdk-source-uri)))
    {:result :jdk-already-installed}

    (and (not custom-jdk-source-uri)
         @local-jdk-source-file*)
    {:result :automatic-local-jdk
     :jdk-zip-file @local-jdk-source-file*}

    ;; Local custom zip as uri
    (and custom-jdk-source-uri
         (shared/plain-uri? custom-jdk-source-uri))
    {:result :manual-local-jdk
     :jdk-zip-file (io/file (shared/uri->filename custom-jdk-source-uri))}

    ;; Local custom zip as file path
    (and custom-jdk-source-uri
         (not (shared/valid-url? custom-jdk-source-uri)))
    {:result :manual-local-jdk
     :jdk-zip-file (io/file custom-jdk-source-uri)}

    download-jdk-source?
    {:result :download-jdk
     :download-uri (or custom-jdk-source-uri default-jdk-source-uri)}

    :else
    {:result :no-source-found}))

(defn ^:private jdk-dir->java-filenames [^File jdk-dir]
  (keep (fn [^File file]
          (let [path (.getCanonicalPath file)]
            (when (and (.isFile file)
                       (string/ends-with? path ".java"))
              path)))
        (file-seq jdk-dir)))

(defn ^:private read-installed-jdk-source-uri [jdk-result-file]
  (and (shared/file-exists? jdk-result-file)
       (slurp jdk-result-file)))

(defn retrieve-jdk-source-and-analyze!
  "Find JDK source and analyze it with clj-kondo for java class definitions.

  If user has custom :jdk-source-uri, we don't try to find JDK source local
  or download it, we make sure zip is extracted otherwize extract it.

  If no custom URI set, we try to find a local JDK source on Java
  instalation which is present most of the time and extract to global
  cache dir.

  Otherwise, we download default OpenJDK source if setting is enabled."
  [db*]
  (let [db @db*
        jdk-dir-file (io/file (config/global-cache-dir) "jdk")
        jdk-result-file (io/file jdk-dir-file "result")
        installed-jdk-source-uri (read-installed-jdk-source-uri jdk-result-file)
        {custom-jdk-source-uri :jdk-source-uri java-home :home-path} (settings/get db [:java])
        local-jdk-source-file* (delay (find-local-jdk-source java-home))
        download-jdk-source? (settings/get db [:java :download-jdk-source?] false)
        {:keys [result jdk-zip-file download-uri]} (jdk-analysis-decision
                                                     installed-jdk-source-uri
                                                     custom-jdk-source-uri
                                                     local-jdk-source-file*
                                                     download-jdk-source?)]
    (io/make-parents jdk-result-file)
    (case result
      :jdk-already-installed
      (logger/info java-logger-tag "JDK source already present on global LSP cache dir.")

      :automatic-local-jdk
      (do
        (logger/info java-logger-tag (format "Automatically found local JDK source zip at %s, extracting to global LSP cache dir..." jdk-zip-file))
        (fs/unzip jdk-zip-file jdk-dir-file {:replace-existing true})
        (spit jdk-result-file (shared/filename->uri (.getCanonicalPath ^File jdk-zip-file) db)))

      :manual-local-jdk
      (do
        (logger/info java-logger-tag (format "Using provided local JDK source URI %s, extracting to global LSP cache dir..." jdk-zip-file))
        (fs/unzip jdk-zip-file jdk-dir-file {:replace-existing true})
        (spit jdk-result-file (shared/filename->uri (.getCanonicalPath ^File jdk-zip-file) db)))

      :download-jdk
      (do
        (logger/info java-logger-tag "Downloading JDK source to global LSP cache dir...")
        (when (download-jdk! download-uri jdk-dir-file)
          (spit jdk-result-file download-uri)))

      ;; else
      (logger/warn java-logger-tag "Skipping download JDK source, setting `:java :download-jdk-source?` is disabled."))

    (cond
      ;; recheck uri is still valid and present
      (not (read-installed-jdk-source-uri jdk-result-file))
      (logger/warn java-logger-tag "JDK source not found, skipping java analysis.")

      (= :jdk-already-installed result)
      (let [global-db (db/read-global-cache)]
        (swap! db* #(-> %
                        (lsp.kondo/db-with-analysis {:analysis (:analysis global-db)
                                                     :external? true})
                        (update :analysis-checksums merge (:analysis-checksums global-db))))
        (logger/info java-logger-tag "JDK source cached loaded successfully."))

      :else
      (let [java-filenames (jdk-dir->java-filenames jdk-dir-file)
            global-db (db/read-global-cache)
            {:keys [new-checksums paths-not-on-checksum]} (shared/generate-and-update-analysis-checksums java-filenames global-db @db*)]
        (if (seq paths-not-on-checksum)
          (do
            (analyze-and-cache-jdk-source! paths-not-on-checksum new-checksums db*)
            (logger/info java-logger-tag "JDK source analyzed and cached successfully."))
          (logger/info java-logger-tag "JDK source cached loaded successfully."))))))
