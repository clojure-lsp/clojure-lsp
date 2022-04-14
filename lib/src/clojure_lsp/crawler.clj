(ns clojure-lsp.crawler
  (:require
   [clojure-lsp.classpath :as classpath]
   [clojure-lsp.config :as config]
   [clojure-lsp.db :as db]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.source-paths :as source-paths]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [lsp4clj.protocols.logger :as logger]
   [lsp4clj.protocols.producer :as producer])
  (:import
   (java.net URI)))

(set! *warn-on-reflection* true)

(def startup-logger-tag "[Startup]")

(defn ^:private get-cp-entry-type [^java.io.File e]
  (cond (.isFile e) :file
        (.isDirectory e) :directory
        :else :unkown))

(defn ^:private analyze-source-paths! [paths db*]
  (let [result (shared/logging-time
                 (str startup-logger-tag " Project only paths analyzed, took %s")
                 (lsp.kondo/run-kondo-on-paths! paths false db*))
        analysis (->> (:analysis result)
                      (lsp.kondo/normalize-analysis false)
                      (group-by :filename))]
    (swap! db* (fn [state-db]
                 (-> state-db
                     (update :analysis merge analysis)
                     (assoc :kondo-config (:config result))
                     (update :findings merge (group-by :filename (:findings result))))))
    analysis))

(defn ^:private report-batch-analysis-percentage
  [start-progress-percentage fulfill-progress-percentage progress-token producer index count]
  (let [real-percentage (- fulfill-progress-percentage start-progress-percentage)]
    (producer/publish-progress
      producer
      (+ start-progress-percentage (/ (* index real-percentage) count))
      "Analyzing external classpath"
      progress-token)))

(defn ^:private analyze-external-classpath!
  [paths start-progress-percentage fulfill-progress-percentage progress-token {:keys [db* producer]}]
  (let [batch-update-callback (partial report-batch-analysis-percentage start-progress-percentage fulfill-progress-percentage progress-token producer)
        result (shared/logging-time
                 "External classpath paths analyzed, took %s. Caching for next startups..."
                 (lsp.kondo/run-kondo-on-paths-batch! paths true batch-update-callback db*))
        kondo-analysis (-> (:analysis result)
                           (dissoc :namespace-usages :var-usages)
                           (update :var-definitions (fn [usages] (remove :private usages))))
        analysis (->> kondo-analysis
                      (lsp.kondo/normalize-analysis true)
                      (group-by :filename))]
    (swap! db* assoc :kondo-config (:config result))
    analysis))

(defn analyze-reference-filenames! [filenames db*]
  (let [result (lsp.kondo/run-kondo-on-reference-filenames! filenames db*)
        analysis (->> (:analysis result)
                      (lsp.kondo/normalize-analysis false)
                      (group-by :filename))
        empty-findings (reduce (fn [map filename] (assoc map filename [])) {} filenames)
        new-findings (merge empty-findings (group-by :filename (:findings result)))]
    (swap! db* (fn [db]
                 (-> db
                     (update :analysis merge analysis)
                     (assoc :kondo-config (:config result))
                     (update :findings merge new-findings))))
    analysis))

(defn ^:private analyze-classpath! [root-path source-paths classpath settings progress-token {:keys [db*] :as components}]
  (let [ignore-directories? (get settings :ignore-classpath-directories)]
    (logger/info "Analyzing classpath for project root" root-path)
    (when classpath
      (let [source-paths-abs (set (map #(shared/relativize-filepath % (str root-path)) source-paths))
            external-paths (cond->> (->> classpath
                                         (remove (set source-paths-abs))
                                         (remove (set source-paths)))
                             ignore-directories? (remove #(let [f (io/file %)] (= :directory (get-cp-entry-type f)))))
            {:keys [new-checksums paths-not-on-checksum]} (shared/generate-and-update-analysis-checksums external-paths nil @db*)
            analysis (analyze-external-classpath! paths-not-on-checksum 25 80 progress-token components)]
        (swap! db* update :analysis-checksums merge new-checksums)
        (swap! db* update :analysis merge analysis)
        (shared/logging-time
          "Manual GC after classpath scan took %s"
          (System/gc))
        (swap! db* assoc :full-scan-analysis-startup true)))))

(defn ^:private copy-configs-from-classpath! [classpath settings db]
  (when (get settings :copy-kondo-configs? true)
    (logger/info "Copying kondo configs from classpath to project if any...")
    (when classpath
      (shared/logging-time
        "Copied kondo configs, took %s secs."
        (lsp.kondo/run-kondo-copy-configs! classpath db)))))

(defn ^:private create-kondo-folder! [^java.io.File clj-kondo-folder]
  (try
    (logger/info (format "Folder %s not found, creating for necessary clj-kondo analysis..." (.getCanonicalPath clj-kondo-folder)))
    (.mkdir clj-kondo-folder)
    (catch Exception e
      (logger/error "Error when creating '.clj-kondo' dir on project-root" e))))

(defn ^:private ensure-kondo-config-dir-exists!
  [project-root-uri db]
  (let [project-root-filename (shared/uri->filename project-root-uri)
        clj-kondo-folder (io/file project-root-filename ".clj-kondo")]
    (when-not (shared/file-exists? clj-kondo-folder)
      (create-kondo-folder! clj-kondo-folder)
      (when (db/db-exists? db)
        (logger/info startup-logger-tag "Removing outdated cached lsp db...")
        (db/remove-db! db)))))

(defn ^:private load-db-cache! [root-path db*]
  (let [db @db*]
    (when-let [db-cache (db/read-local-cache root-path db)]
      (when-not (and (= :project-and-deps (:project-analysis-type db))
                     (= :project-only (:project-analysis-type db-cache)))
        (swap! db* (fn [state-db]
                     (-> state-db
                         (merge (select-keys db-cache [:classpath :analysis-checksums :project-hash
                                                       :kondo-config-hash :stubs-generation-namespaces]))
                         (update :analysis merge (:analysis db-cache)))))))))

(defn ^:private build-db-cache [db]
  (-> db
      (select-keys [:project-hash :kondo-config-hash :project-analysis-type :classpath :analysis :analysis-checksums])
      (merge {:stubs-generation-namespaces (->> db :settings :stubs :generation :namespaces (map str) set)
              :version db/version
              :project-root (str (shared/uri->path (:project-root-uri db)))})))

(defn ^:private upsert-db-cache! [db]
  (if (:api? db)
    (db/upsert-local-cache! (build-db-cache db) db)
    (async/go
      (db/upsert-local-cache! (build-db-cache db) db))))

(defn initialize-project
  [project-root-uri
   client-capabilities
   client-settings
   force-settings
   progress-token
   {:keys [db* logger producer] :as components}]
  (producer/publish-progress producer 0 "clojure-lsp" progress-token)
  (let [project-settings (config/resolve-for-root project-root-uri)
        root-path (shared/uri->path project-root-uri)
        encoding-settings {:uri-format {:upper-case-drive-letter? (->> project-root-uri URI. .getPath
                                                                       (re-find #"^/[A-Z]:/")
                                                                       boolean)
                                        :encode-colons-in-path? (string/includes? project-root-uri "%3A")}}
        settings (shared/deep-merge encoding-settings
                                    client-settings
                                    project-settings
                                    force-settings)
        _ (when-let [log-path (:log-path settings)]
            (logger/set-log-path logger log-path)
            (swap! db* assoc :log-path log-path))
        settings (update settings :source-aliases #(or % source-paths/default-source-aliases))
        settings (update settings :project-specs #(or % (classpath/default-project-specs (:source-aliases settings))))]
    (swap! db* assoc
           :project-root-uri project-root-uri
           :client-settings client-settings
           :project-settings project-settings
           :force-settings force-settings
           :settings settings
           :client-capabilities client-capabilities)
    (producer/publish-progress producer 5 "Finding kondo config" progress-token)
    (ensure-kondo-config-dir-exists! project-root-uri @db*)
    (producer/publish-progress producer 10 "Finding cache" progress-token)
    (load-db-cache! root-path db*)
    (let [project-hash (classpath/project-specs->hash root-path settings)
          kondo-config-hash (lsp.kondo/config-hash (str root-path))
          use-db-analysis? (and (= (:project-hash @db*) project-hash)
                                (= (:kondo-config-hash @db*) kondo-config-hash))]
      (if use-db-analysis?
        (do
          (logger/info startup-logger-tag "Using cached db for project root" root-path)
          (swap! db* assoc
                 :settings (update settings :source-paths (partial source-paths/process-source-paths root-path (:classpath @db*) settings))))
        (do
          (producer/publish-progress producer 15 "Discovering classpath" progress-token)
          (let [classpath (classpath/scan-classpath! components)]
            (swap! db* assoc
                   :project-hash project-hash
                   :kondo-config-hash kondo-config-hash
                   :classpath classpath
                   :settings (update settings :source-paths (partial source-paths/process-source-paths root-path classpath settings)))
            (producer/publish-progress producer 20 "Copying kondo configs" progress-token)
            (copy-configs-from-classpath! classpath settings @db*)
            (when (= :project-and-deps (:project-analysis-type @db*))
              (producer/publish-progress producer 25 "Analyzing external classpath" progress-token)
              (analyze-classpath! root-path (-> @db* :settings :source-paths) classpath settings progress-token components)))
          (upsert-db-cache! @db*))))
    (producer/publish-progress producer 90 "Resolving config paths" progress-token)
    (when-let [classpath-settings (and (config/classpath-config-paths? settings)
                                       (:classpath @db*)
                                       (config/resolve-from-classpath-config-paths (:classpath @db*) settings))]
      (swap! db* assoc
             :settings (shared/deep-merge (:settings @db*)
                                          classpath-settings
                                          project-settings
                                          force-settings)
             :classpath-settings classpath-settings))
    (producer/publish-progress producer 95 "Analyzing project files" progress-token)
    (logger/info startup-logger-tag "Analyzing source paths for project root" root-path)
    (analyze-source-paths! (-> @db* :settings :source-paths) db*)
    (swap! db* assoc :settings-auto-refresh? true)
    (producer/publish-progress producer 100 "Project analyzed" progress-token)))
