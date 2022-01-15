(ns clojure-lsp.crawler
  (:require
   [clojure-lsp.classpath :as classpath]
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
   [clojure.string :as string]
   [medley.core :as medley]
   [taoensso.timbre :as log])
  (:import
   (com.google.common.io ByteStreams)
   (java.net URI)
   (java.security MessageDigest
                  DigestInputStream)))

(set! *warn-on-reflection* true)

(defn ^:private md5 [^java.io.File file]
  (let [digest (MessageDigest/getInstance "MD5")]
    (with-open [input-stream (io/input-stream file)
                digest-stream (DigestInputStream. input-stream digest)
                output-stream (ByteStreams/nullOutputStream)]
      (io/copy digest-stream output-stream))
    (format "%032x" (BigInteger. 1 (.digest digest)))))

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
  [start-progress-percentage fulfill-progress-percentage progress-token db index count]
  (let [real-percentage (- fulfill-progress-percentage start-progress-percentage)]
    (producer/publish-progress
      (:producer @db)
      (+ start-progress-percentage (/ (* index real-percentage) count))
      "Analyzing external classpath"
      progress-token)))

(defn ^:private analyze-external-classpath!
  [paths start-progress-percentage fulfill-progress-percentage progress-token db]
  (let [batch-update-callback (partial report-batch-analysis-percentage start-progress-percentage fulfill-progress-percentage progress-token db)
        result (shared/logging-time
                 "External classpath paths analyzed, took %s secs. Caching for next startups..."
                 (lsp.kondo/run-kondo-on-paths-batch! paths true batch-update-callback db))
        kondo-analysis (-> (:analysis result)
                           (dissoc :namespace-usages :var-usages)
                           (update :var-definitions (fn [usages] (remove :private usages))))
        analysis (->> kondo-analysis
                      lsp.kondo/normalize-analysis
                      (group-by :filename))]
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

(defn ^:private project-specs->hash [project-specs root-path]
  (->> project-specs
       (map (fn [{:keys [project-path]}]
              (md5 (shared/to-file root-path project-path))))
       (reduce str)))

(defn ^:private analyze-classpath! [root-path source-paths settings progress-token db]
  (let [ignore-directories? (get settings :ignore-classpath-directories)]
    (log/info "Analyzing classpath for project root" root-path)
    (when-let [classpath (:classpath @db)]
      (let [source-paths-abs (set (map #(shared/relativize-filepath % (str root-path)) source-paths))
            external-classpath (cond->> (->> classpath
                                             (remove (set source-paths-abs))
                                             (remove (set source-paths)))
                                 ignore-directories? (remove #(let [f (io/file %)] (= :directory (get-cp-entry-type f)))))
            analysis (analyze-external-classpath! external-classpath 15 80 progress-token db)]
        (swap! db update :analysis merge analysis)
        (shared/logging-time
          "Manual GC after classpath scan took %s secs"
          (System/gc))
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

(defn ^:private load-db-cache! [root-path db]
  (when-let [db-cache (db/read-cache root-path db)]
    (when-not (and (= :project-and-deps (:project-analysis-type @db))
                   (= :project-only (:project-analysis-type db-cache)))
      (swap! db (fn [state-db]
                  (-> state-db
                      (update :analysis merge (:analysis db-cache))
                      (assoc :classpath (:classpath db-cache)
                             :project-hash (:project-hash db-cache)
                             :kondo-config-hash (:kondo-config-hash db-cache)
                             :stubs-generation-namespaces (:stubs-generation-namespaces db-cache))))))))

(defn ^:private build-db-cache [db]
  (-> @db
      (select-keys [:project-hash :kondo-config-hash :project-analysis-type :classpath :analysis])
      (merge {:stubs-generation-namespaces (->> @db :settings :stubs :generation :namespaces (map str) set)
              :version db/version
              :project-root (str (shared/uri->path (:project-root-uri @db)))})))

(defn ^:private upsert-db-cache! [db]
  (if (:api? @db)
    (db/upsert-cache! (build-db-cache db) db)
    (async/go
      (db/upsert-cache! (build-db-cache db) db))))

(defn initialize-project [project-root-uri client-capabilities client-settings force-settings progress-token db]
  (producer/publish-progress (:producer @db) 0 "clojure-lsp" progress-token)
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
    (producer/publish-progress (:producer @db) 5 "Finding kondo config" progress-token)
    (ensure-kondo-config-dir-exists! project-root-uri db)
    (producer/publish-progress (:producer @db) 10 "Finding cache" progress-token)
    (load-db-cache! root-path db)
    (when-not (:classpath @db)
      (producer/publish-progress (:producer @db) 15 "Discovering classpath" progress-token)
      (swap! db assoc :classpath (classpath/scan-classpath! db)))
    (let [project-specs (->> (:project-specs settings)
                             (filter (partial classpath/valid-project-spec? root-path)))
          project-hash (project-specs->hash project-specs root-path)
          kondo-config-hash (lsp.kondo/config-hash (str root-path))
          use-db-analysis? (and (= (:project-hash @db) project-hash)
                                (= (:kondo-config-hash @db) kondo-config-hash))]
      (if use-db-analysis?
        (log/info "Using cached db for project root" root-path)
        (do
          (swap! db assoc
                 :project-hash project-hash
                 :kondo-config-hash kondo-config-hash)
          (when (= :project-and-deps (:project-analysis-type @db))
            (analyze-classpath! root-path (:source-paths settings) settings progress-token db))
          (upsert-db-cache! db))))
    (producer/publish-progress (:producer @db) 90 "Resolving config paths" progress-token)
    (when-let [classpath-settings (and (config/classpath-config-paths? settings)
                                       (:classpath @db)
                                       (config/resolve-from-classpath-config-paths (:classpath @db) settings))]
      (swap! db assoc
             :settings (shared/deep-merge settings
                                          classpath-settings
                                          project-settings
                                          force-settings)
             :classpath-settings classpath-settings))
    (when-not (:api? @db)
      (async/go
        (f.clojuredocs/refresh-cache! db)))
    (let [settings (:settings @db)]
      (when (stubs/check-stubs? settings)
        (producer/publish-progress (:producer @db) 92 "Analyzing stubs" progress-token)
        (stubs/generate-and-analyze-stubs! settings db))
      (producer/publish-progress (:producer @db) 95 "Analyzing project files" progress-token)
      (log/info "Analyzing source paths for project root" root-path)
      (analyze-source-paths! (:source-paths settings) db))
    (producer/publish-progress (:producer @db) 100 "Project analyzed" progress-token)))
