(ns clojure-lsp.crawler
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.clojuredocs :as f.clojuredocs]
   [clojure-lsp.feature.stubs :as stubs]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.logging :as logging]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.data :as j]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as string]
   [medley.core :as medley]
   [taoensso.timbre :as log])
  (:import
   (com.google.common.io ByteStreams)
   (java.net URI)
   (java.security MessageDigest
                  DigestInputStream)))

(set! *warn-on-reflection* true)

(defn ^:private lookup-classpath [root-path {:keys [classpath-cmd env]} db]
  (let [command (string/join " " classpath-cmd)]
    (log/info (format "Finding classpath via `%s`" command))
    (try
      (let [sep (re-pattern (System/getProperty "path.separator"))
            {:keys [exit out err]} (apply shell/sh (into classpath-cmd
                                                         (cond-> [:dir (str root-path)]
                                                           env (conj :env (merge {} (System/getenv) env)))))]
        (if (= 0 exit)
          (let [paths (-> out
                          string/split-lines
                          last
                          string/trim-newline
                          (string/split sep))]
            (log/debug "Classpath found, paths: " paths)
            paths)
          (do
            (log/error (format "Error while looking up classpath info in %s. Exit status %s. Error: %s" (str root-path) exit err))
            (producer/window-show-message (format "Classpath lookup failed when running `%s`. Some features may not work properly. Error: %s" command err) :error err db)
            [])))
      (catch clojure.lang.ExceptionInfo e
        (throw e))
      (catch Exception e
        (log/error e (format "Error while looking up classpath info in %s" (str root-path)) (.getMessage e))
        (producer/window-show-message (format "Classpath lookup failed when running `%s`. Some features may not work properly. Error: %s" command (.getMessage e)) :error (.getMessage e) db)
        []))))

(defn ^:private md5 [^java.io.File file]
  (let [digest (MessageDigest/getInstance "MD5")]
    (with-open [input-stream (io/input-stream file)
                digest-stream (DigestInputStream. input-stream digest)
                output-stream (ByteStreams/nullOutputStream)]
      (io/copy digest-stream output-stream))
    (format "%032x" (BigInteger. 1 (.digest digest)))))

(defn ^:private valid-project-specs-with-hash [root-path project-specs]
  (keep
    (fn [{:keys [project-path] :as project-spec}]
      (let [project-file (shared/to-file root-path project-path)]
        (when (shared/file-exists? project-file)
          (assoc project-spec :hash (md5 project-file)))))
    project-specs))

(defn ^:private get-cp-entry-type [^java.io.File e]
  (cond (.isFile e) :file
        (.isDirectory e) :directory
        :else :unkown))

(defn ^:private analyze-source-paths! [paths db]
  (let [result (shared/logging-time
                 "Project only paths analyzed, took %s secs"
                 (lsp.kondo/run-kondo-on-paths! paths false db))
        analysis (->> (:analysis result)
                      lsp.kondo/normalize-analysis
                      (group-by :filename))]
    (swap! db update :analysis merge analysis)
    (swap! db assoc :kondo-config (:config result))
    (swap! db update :findings merge (group-by :filename (:findings result)))
    analysis))

(defn ^:private report-batch-analysis-percentage
  [start-progress-percentage fulfill-progress-percentage report-callback db index count]
  (let [real-percentage (- fulfill-progress-percentage start-progress-percentage)]
    (report-callback
      (+ start-progress-percentage (/ (* index real-percentage) count))
      "Analyzing external classpath"
      db)))

(defn ^:private analyze-external-classpath!
  [paths start-progress-percentage fulfill-progress-percentage report-callback db]
  (let [batch-update-callback (partial report-batch-analysis-percentage start-progress-percentage fulfill-progress-percentage report-callback db)
        result (shared/logging-time
                 "External classpath paths analyzed, took %s secs. Caching for next startups..."
                 (lsp.kondo/run-kondo-on-paths-batch! paths true batch-update-callback db))
        kondo-analysis (-> (:analysis result)
                           (dissoc :namespace-usages :var-usages)
                           (update :var-definitions (fn [usages] (remove :private usages))))
        analysis (->> kondo-analysis
                      lsp.kondo/normalize-analysis
                      (group-by :filename))]
    (swap! db update :analysis merge analysis)
    (swap! db assoc :kondo-config (:config result))
    analysis))

(defn analyze-reference-filenames! [filenames db]
  (let [result (shared/logging-time
                 "Files analyzed, took %s secs"
                 (lsp.kondo/run-kondo-on-reference-filenames! filenames db))
        analysis (->> (:analysis result)
                      lsp.kondo/normalize-analysis
                      (group-by :filename))
        empty-findings (reduce (fn [map filename] (assoc map filename [])) {} filenames)
        new-findings (merge empty-findings (group-by :filename (:findings result)))]
    (swap! db update :analysis merge analysis)
    (swap! db assoc :kondo-config (:config result))
    (swap! db update :findings merge new-findings)
    analysis))

(defn ^:private analyze-classpath! [root-path source-paths settings report-callback db]
  (report-callback 8 "Finding project specs" db)
  (let [project-specs (->> (:project-specs settings)
                           (valid-project-specs-with-hash root-path))
        ignore-directories? (get settings :ignore-classpath-directories)
        stubs-namespaces (->> settings :stubs :generation :namespaces (map str) set)
        project-hash (reduce str (map :hash project-specs))
        kondo-config-hash (lsp.kondo/config-hash (str root-path))
        _ (report-callback 10 "Finding cache" db)
        db-cache (db/read-cache root-path db)
        use-db-analysis? (and (= (:project-hash db-cache) project-hash)
                              (= (:kondo-config-hash db-cache) kondo-config-hash))]
    (report-callback 15 "Discovering classpath" db)
    (if use-db-analysis?
      (do
        (log/info "Using cached classpath for project root" root-path)
        (swap! db (fn [state-db]
                    (-> state-db
                        (update :analysis merge (:analysis db-cache))
                        (assoc :classpath (:classpath db-cache))
                        (assoc :stubs-generation-namespaces (:stubs-generation-namespaces db-cache))))))
      (do
        (log/info "Analyzing classpath for project root" root-path)
        (when-let [classpath (->> project-specs
                                  (mapcat #(lookup-classpath root-path % db))
                                  vec
                                  seq)]
          (swap! db assoc :classpath classpath)
          (let [source-paths-abs (set (map #(shared/relativize-filepath % (str root-path)) source-paths))
                external-classpath (cond->> (->> classpath
                                                 (remove (set source-paths-abs))
                                                 (remove (set source-paths)))
                                     ignore-directories? (remove #(let [f (io/file %)] (= :directory (get-cp-entry-type f)))))
                analysis (analyze-external-classpath! external-classpath 15 80 report-callback db)
                new-db {:version db/version
                        :project-root (str root-path)
                        :project-hash project-hash
                        :kondo-config-hash kondo-config-hash
                        :classpath classpath
                        :stubs-generation-namespaces stubs-namespaces
                        :analysis analysis}]
            (shared/logging-time
              "Manual GC after classpath scan took %s secs"
              (System/gc))
            (if (:api? @db)
              (db/upsert-cache! new-db db)
              (async/go
                (db/upsert-cache! new-db db)))))
        (swap! db assoc :full-scan-analysis-startup true)))))

(defn ^:private create-kondo-folder! [^java.io.File clj-kondo-folder]
  (try
    (log/info (format "Folder %s not found, creating for necessary clj-kondo analysis..." (.getCanonicalPath clj-kondo-folder)))
    (.mkdir clj-kondo-folder)
    (catch Exception e
      (log/error "Error when creating '.clj-kondo' dir on project-root" e))))

(defn ^:private ensure-kondo-config-dir-exists!
  [project-root-uri db]
  (let [project-root-filename (shared/uri->filename project-root-uri)
        project-root-path (shared/uri->path project-root-uri)
        clj-kondo-folder (io/file project-root-filename ".clj-kondo")]
    (when-not (shared/file-exists? clj-kondo-folder)
      (create-kondo-folder! clj-kondo-folder)
      (when (db/db-exists? project-root-path db)
        (log/info "Removing outdated cached lsp db...")
        (db/remove-db! project-root-path db)))))

(defn initialize-project [project-root-uri client-capabilities client-settings force-settings report-callback db]
  (report-callback 0 "clojure-lsp" db)
  (let [project-settings (config/resolve-for-root project-root-uri)
        root-path (shared/uri->path project-root-uri)
        env (:env @db)
        encoding-settings {:uri-format {:upper-case-drive-letter? (->> project-root-uri URI. .getPath
                                                                       (re-find #"^/[A-Z]:/")
                                                                       boolean)
                                        :encode-colons-in-path? (string/includes? project-root-uri "%3A")}}
        raw-settings (shared/deep-merge encoding-settings
                                        client-settings
                                        project-settings
                                        force-settings)
        _ (when-let [log-path (:log-path raw-settings)]
            (logging/update-log-path log-path db))
        settings (settings/udpate-with-default-settings nil raw-settings project-root-uri env)]
    (swap! db assoc
           :project-root-uri project-root-uri
           :client-settings client-settings
           :project-settings project-settings
           :force-settings force-settings
           :settings settings
           :client-capabilities (medley/update-existing client-capabilities :experimental j/from-java))
    (report-callback 5 "Finding kondo config" db)
    (ensure-kondo-config-dir-exists! project-root-uri db)
    (analyze-classpath! root-path (:source-paths settings) settings report-callback db)
    (report-callback 90 "Resolving config paths" db)
    (when-let [classpath-settings (config/resolve-from-classpath-config-paths (:classpath @db) settings)]
      (swap! db assoc
             :settings (shared/deep-merge settings
                                          classpath-settings
                                          project-settings
                                          force-settings)
             :classpath-settings classpath-settings))
    (async/go
      (f.clojuredocs/refresh-cache! db))
    (log/info "Analyzing source paths for project root" root-path)
    (let [settings (:settings @db)]
      (when (stubs/check-stubs? settings)
        (report-callback 92 "Analyzing stubs" db)
        (stubs/generate-and-analyze-stubs! settings db))
      (report-callback 95 "Analyzing project files" db)
      (analyze-source-paths! (:source-paths settings) db))
    (report-callback 100 "Project analyzed" db)))
