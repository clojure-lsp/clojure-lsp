(ns clojure-lsp.feature.java-interop
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.config :as config]
   [clojure-lsp.db :as db]
   [clojure-lsp.http :as http]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.string :as string])
  (:import
   (java.io File)
   (java.net JarURLConnection URL)
   (java.nio.file Path)
   (java.util.jar JarFile JarFile$JarFileEntry)
   (java.util.zip ZipInputStream)
   (org.benf.cfr.reader.api CfrDriver CfrDriver$Builder)))

(set! *warn-on-reflection* true)

(def ^:private java-logger-tag "[Java]")

(defn ^:private decompile! [^String input-path ^String output-path]
  (let [driver ^CfrDriver (.. (CfrDriver$Builder.)
                              ;; CFR stout is not reliable, prefer output to file so we can read later
                              (withOptions {"outputdir" output-path})
                              (build))
        err-sym (java.io.StringWriter.)]
    (binding [*err* err-sym]
      (with-out-str
        (.analyse driver [input-path]))
      (when-not (string/blank? (str err-sym))
        (logger/warn java-logger-tag "Non-fatal error from CFR:" (str err-sym))))))

(defn ^:private jar->java-project-info [^JarFile jar]
  (->> (enumeration-seq (.entries jar))
       (keep
         (fn [^JarFile$JarFileEntry entry]
           (let [name (.getName entry)]
             (when-not (.isDirectory entry)
               (when (and (string/starts-with? name "META-INF")
                          (string/ends-with? name "pom.xml"))
                 (let [pom-stream (.getInputStream jar entry)
                       pom-content (slurp pom-stream)
                       [_ group-id] (re-find #"<groupId>(.+)</groupId>" pom-content)
                       [_ artifact-id] (re-find #"<artifactId>(.+)</artifactId>" pom-content)
                       [_ version] (re-find #"<version>(.+)</version>" pom-content)
                       [_ source-path] (or (re-find #"<sourceDirectory>(.+)</sourceDirectory>" pom-content)
                                           [nil "src"])]
                   {:jar-path (.getName jar)
                    :group-id group-id
                    :artifact-id artifact-id
                    :version version
                    :source-path source-path
                    :pom-content pom-content}))))))
       first))

(defn ^:private decompile-file [^JarFile jar entry db]
  (let [cache-path (config/local-cache-dir db)
        class-file (io/file cache-path "java" "classes" (str entry))
        decompile-folder-file (io/file cache-path "java" "decompiled")
        java-file (io/file decompile-folder-file (string/replace (str entry) #".class$" ".java"))]
    (io/make-parents class-file)
    (with-open [stream (.getInputStream jar entry)]
      (io/copy stream class-file))
    (logger/info java-logger-tag (format "Decompiling java class %s" class-file))
    (decompile! (.getCanonicalPath class-file) (.getCanonicalPath decompile-folder-file))
    (shared/filename->uri (.getCanonicalPath java-file) db)))

(defn ^:private decompile-jar-as-java-project
  [{:keys [group-id artifact-id version jar-path source-path pom-content]}
   ^JarFile$JarFileEntry entry
   db
   producer]
  (let [prefix-path (io/file group-id artifact-id (str artifact-id "-" version))
        cache-path (config/global-cache-dir)
        project-folder (io/file cache-path "java" "decompiled" prefix-path)
        project-source-folder (io/file project-folder source-path)
        projectile-file (io/file project-folder ".projectile")
        pom-file (io/file project-folder "pom.xml")
        dest-path (io/file project-source-folder (string/replace (.getName entry) #".class$" ".java"))
        dest-uri (shared/filename->uri (.getCanonicalPath dest-path) db)]
    (if (shared/file-exists? pom-file)
      (logger/info java-logger-tag (format "Already decompiled jar %s" jar-path))
      (do
        (producer/show-message producer "Decompiling jar for the first time, please wait..." :info nil)
        (logger/info java-logger-tag (format "Decompiling whole jar %s" jar-path))
        (io/make-parents pom-file)
        (spit pom-file pom-content)
        (spit projectile-file "")
        (shared/logging-task :decompile-jar
                             (decompile! jar-path (.getCanonicalPath project-source-folder)))
        (logger/info java-logger-tag (format "Decompiled jar %s" jar-path))))
    dest-uri))

(defn ^:private uri->translated-file! [uri db producer]
  ;; TODO consider decompiling local class files not necessarly
  ;; from inside a jar
  (if (and (shared/jar-file? uri)
           (shared/class-file? uri))
    (let [jar-uri (shared/ensure-jarfile uri db)
          url (URL. jar-uri)
          connection ^JarURLConnection (.openConnection url)
          jar (.getJarFile connection)
          entry (.getJarEntry connection)]
      (if-let [java-project-info (and (settings/get db [:java :decompile-jar-as-project?] true)
                                      (jar->java-project-info jar))]
        (decompile-jar-as-java-project java-project-info entry db producer)
        (decompile-file jar entry db)))
    uri))

(defn uri->translated-uri [uri db producer]
  (uri->translated-file! uri db producer))

(defn read-content! [uri db producer]
  (slurp (uri->translated-file! uri db producer)))

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
        global-db (db/read-global-cache)
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

    ;; keep only java packages
    (doseq [file-dir (fs/list-dir jdk-dir-file)]
      (when-not (or (string/starts-with? (str (fs/relativize jdk-dir-file file-dir)) "java")
                    (= (str file-dir) (str jdk-result-file)))
        (fs/delete-tree file-dir)))

    (cond
      ;; recheck uri is still valid and present
      (not (read-installed-jdk-source-uri jdk-result-file))
      (logger/warn java-logger-tag "JDK source not found, skipping java analysis.")

      (and (= :jdk-already-installed result)
           global-db)
      (do
        (swap! db* #(-> %
                        (lsp.kondo/db-with-analysis {:analysis (:analysis global-db)
                                                     :external? true})
                        (update :analysis-checksums merge (:analysis-checksums global-db))))
        (logger/info java-logger-tag "JDK source analysis cache loaded successfully."))

      :else
      (let [java-filenames (jdk-dir->java-filenames jdk-dir-file)
            {:keys [new-checksums paths-not-on-checksum]} (shared/generate-and-update-analysis-checksums java-filenames global-db @db*)]
        (if (seq paths-not-on-checksum)
          (do
            (logger/info java-logger-tag "Analyzing JDK source via clj-kondo...")
            (analyze-and-cache-jdk-source! paths-not-on-checksum new-checksums db*)
            (logger/info java-logger-tag "JDK source analyzed and cached successfully."))
          (logger/info java-logger-tag "JDK source cache loaded successfully."))))))
