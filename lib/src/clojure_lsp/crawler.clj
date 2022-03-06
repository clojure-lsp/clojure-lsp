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
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.source-paths :as source-paths]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [taoensso.timbre :as log])
  (:import
   (java.net URI)))

(set! *warn-on-reflection* true)

(defn ^:private analyze-test-paths! [db]
  (let [project-files (-> (:analysis @db/db)
                          (q/filter-project-analysis db/db)
                          keys)]
    (->> project-files
         (map #(shared/filename->uri % db))
         (producer/refresh-test-tree (:producer @db)))))

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
    (swap! db (fn [state-db]
                (-> state-db
                    (update :analysis merge analysis)
                    (assoc :kondo-config (:config result))
                    (update :findings merge (group-by :filename (:findings result))))))
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
        encoding-settings {:uri-format {:upper-case-drive-letter? (->> project-root-uri URI. .getPath
                                                                       (re-find #"^/[A-Z]:/")
                                                                       boolean)
                                        :encode-colons-in-path? (string/includes? project-root-uri "%3A")}}
        settings (shared/deep-merge encoding-settings
                                    client-settings
                                    project-settings
                                    force-settings)
        _ (when-let [log-path (:log-path settings)]
            (logging/update-log-path log-path db))
        settings (update settings :source-aliases #(or % source-paths/default-source-aliases))
        settings (update settings :project-specs #(or % (classpath/default-project-specs (:source-aliases settings))))]
    (swap! db assoc
           :project-root-uri project-root-uri
           :client-settings client-settings
           :project-settings project-settings
           :force-settings force-settings
           :settings settings
           :client-capabilities client-capabilities)
    (producer/publish-progress (:producer @db) 5 "Finding kondo config" progress-token)
    (ensure-kondo-config-dir-exists! project-root-uri db)
    (producer/publish-progress (:producer @db) 10 "Finding cache" progress-token)
    (load-db-cache! root-path db)
    (let [project-hash (classpath/project-specs->hash root-path settings)
          kondo-config-hash (lsp.kondo/config-hash (str root-path))
          use-db-analysis? (and (= (:project-hash @db) project-hash)
                                (= (:kondo-config-hash @db) kondo-config-hash))]
      (if use-db-analysis?
        (do
          (log/info "Using cached db for project root" root-path)
          (swap! db assoc
                 :settings (update settings :source-paths (partial source-paths/process-source-paths root-path (:classpath @db) settings))))
        (do
          (producer/publish-progress (:producer @db) 15 "Discovering classpath" progress-token)
          (let [classpath (classpath/scan-classpath! db)]
            (swap! db assoc
                   :project-hash project-hash
                   :kondo-config-hash kondo-config-hash
                   :classpath classpath
                   :settings (update settings :source-paths (partial source-paths/process-source-paths root-path classpath settings))))
          (when (= :project-and-deps (:project-analysis-type @db))
            (analyze-classpath! root-path (-> @db :settings :source-paths) settings progress-token db))
          (upsert-db-cache! db))))
    (producer/publish-progress (:producer @db) 90 "Resolving config paths" progress-token)
    (when-let [classpath-settings (and (config/classpath-config-paths? settings)
                                       (:classpath @db)
                                       (config/resolve-from-classpath-config-paths (:classpath @db) settings))]
      (swap! db assoc
             :settings (shared/deep-merge (:settings @db)
                                          classpath-settings
                                          project-settings
                                          force-settings)
             :classpath-settings classpath-settings))
    (producer/publish-progress (:producer @db) 95 "Analyzing project files" progress-token)
    (log/info "Analyzing source paths for project root" root-path)
    (analyze-source-paths! (-> @db :settings :source-paths) db)
    (swap! db assoc :settings-auto-refresh? true)
    (when-not (:api? @db)
      (async/go
        (f.clojuredocs/refresh-cache! db))
      (async/go
        (let [settings (:settings @db)]
          (when (stubs/check-stubs? settings)
            (stubs/generate-and-analyze-stubs! settings db))))
      (async/go
        (log/info "Analyzing test paths for project root" root-path)
        (analyze-test-paths! db)))
    (producer/publish-progress (:producer @db) 100 "Project analyzed" progress-token)))
