(ns clojure-lsp.startup
  (:require
   [clojure-lsp.classpath :as classpath]
   [clojure-lsp.clj-depend :as lsp.depend]
   [clojure-lsp.config :as config]
   [clojure-lsp.db :as db]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.source-paths :as source-paths]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [medley.core :as medley])
  (:import
   (java.net URI)))

(set! *warn-on-reflection* true)

(def startup-logger-tag "[Startup]")

(defn lerp "Linear interpolation" [a b t] (+ a (* (- b a) t)))

(defn init-tasks [tasks]
  (->> tasks
       (partition-all 2 1)
       (map (fn [[task next-task]]
              (assoc task :task/end-percent (if next-task
                                              (dec (:task/start-percent next-task))
                                              100))))
       (medley/index-by :task/id)))

(def fast-tasks
  (init-tasks
    [{:task/start-percent   0, :task/title "clojure-lsp",             :task/id :start}
     {:task/start-percent   5, :task/title "Finding kondo config",    :task/id :finding-kondo}
     {:task/start-percent  10, :task/title "Finding cache",           :task/id :finding-cache}
     {:task/start-percent  15, :task/title "Copying kondo configs",   :task/id :copying-kondo}
     {:task/start-percent  15, :task/title "Resolving config paths",  :task/id :resolving-config}
     {:task/start-percent  20, :task/title "Analyzing project files", :task/id :analyzing-project}
     {:task/start-percent 100, :task/title "Project analyzed",        :task/id :done}]))

(def slow-tasks
  (init-tasks
    [{:task/start-percent   0, :task/title "clojure-lsp",                  :task/id :start}
     {:task/start-percent   5, :task/title "Finding kondo config",         :task/id :finding-kondo}
     {:task/start-percent  10, :task/title "Finding cache",                :task/id :finding-cache}
     {:task/start-percent  15, :task/title "Discovering classpath",        :task/id :discovering-classpath}
     {:task/start-percent  20, :task/title "Copying kondo configs",        :task/id :copying-kondo}
     {:task/start-percent  25, :task/title "Analyzing external classpath", :task/id :analyzing-deps}
     {:task/start-percent  45, :task/title "Resolving config paths",       :task/id :resolving-config}
     {:task/start-percent  50, :task/title "Analyzing project files",      :task/id :analyzing-project}
     {:task/start-percent 100, :task/title "Project analyzed",             :task/id :done}]))

(defn batched-task [{:keys [:task/start-percent :task/end-percent] :as task} batch-idx batch-count]
  (assoc task
         :task/start-percent (lerp start-percent end-percent (/ (dec batch-idx) batch-count))
         :task/end-percent (lerp start-percent end-percent (/ batch-idx batch-count))))

(defn partial-task [{:keys [:task/start-percent :task/end-percent] :as task} subtask-idx subtask-count]
  (assoc task
         :task/current-percent (lerp start-percent end-percent (/ subtask-idx subtask-count))))

(defn publish-task-progress [producer {:keys [:task/title :task/current-percent :task/start-percent]} progress-token]
  (when progress-token
    (producer/publish-progress producer (or current-percent start-percent) title progress-token)))

(defn ^:private analyze-source-paths! [paths db* file-analyzed-fn]
  (let [kondo-result* (future
                        (shared/logging-time
                          (str startup-logger-tag " Project only paths analyzed by clj-kondo, took %s")
                          (lsp.kondo/run-kondo-on-paths! paths db* {:external? false} file-analyzed-fn)))
        depend-result* (future
                         (shared/logging-time
                           (str startup-logger-tag " Project only paths analyzed by clj-depend, took %s")
                           (lsp.depend/analyze-paths! paths @db*)))
        kondo-result @kondo-result*
        depend-result @depend-result*]
    (swap! db* (fn [state-db]
                 (-> state-db
                     (lsp.kondo/db-with-results kondo-result)
                     (lsp.depend/db-with-results depend-result))))))

(defn ^:private analyze-external-classpath! [root-path source-paths classpath progress-token {:keys [db* producer]}]
  (logger/info "Analyzing classpath for project root" (str root-path))
  (when classpath
    (let [source-paths-abs (set (map #(shared/relativize-filepath % (str root-path)) source-paths))
          external-paths (->> classpath
                              (remove (set source-paths-abs))
                              (remove (set source-paths)))
          {:keys [new-checksums paths-not-on-checksum]} (shared/generate-and-update-analysis-checksums external-paths nil @db*)
          batch-update-callback (fn [batch-index batch-count {:keys [total-files files-done]}]
                                  (let [task (-> (:analyzing-deps slow-tasks)
                                                 (batched-task batch-index batch-count)
                                                 (partial-task files-done total-files))]
                                    (publish-task-progress producer task progress-token)))
          normalization-config {:external? true
                                :filter-analysis (fn [analysis]
                                                   (update analysis :var-definitions #(remove :private %)))}
          kondo-result (shared/logging-time
                         "External classpath paths analyzed, took %s"
                         (lsp.kondo/run-kondo-on-paths-batch! paths-not-on-checksum normalization-config batch-update-callback db*))]
      (swap! db* #(-> %
                      (update :analysis-checksums merge new-checksums)
                      (lsp.kondo/db-with-results kondo-result)))
      (shared/logging-time
        "Manual GC after classpath scan took %s"
        (System/gc))
      (swap! db* assoc :full-scan-analysis-startup true))))

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

(defn ^:private consider-local-db-cache? [db db-cache]
  (or (= :project-and-full-dependencies (:project-analysis-type db-cache))
      (and (= :project-and-clojure-only-dependencies (:project-analysis-type db-cache))
           (or (= :project-and-clojure-only-dependencies (:project-analysis-type db))
               (= :project-only (:project-analysis-type db))))
      (= :project-only (:project-analysis-type db))))

(defn ^:private load-db-cache! [root-path db*]
  (let [db @db*]
    (when-let [db-cache (db/read-local-cache root-path db)]
      (when (consider-local-db-cache? db db-cache)
        (swap! db* (fn [state-db]
                     (-> state-db
                         (merge (select-keys db-cache [:classpath
                                                       :analysis-checksums
                                                       :project-hash
                                                       :kondo-config-hash
                                                       :dependency-scheme
                                                       :stubs-generation-namespaces]))
                         (lsp.kondo/db-with-analysis {:analysis (:analysis db-cache)
                                                      :external? true}))))))))

(defn ^:private build-db-cache [db]
  (-> db
      (select-keys [:project-hash
                    :kondo-config-hash
                    :dependency-scheme
                    :project-analysis-type
                    :classpath
                    :analysis
                    :analysis-checksums])
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
  (publish-task-progress producer (:start fast-tasks) progress-token)
  (let [task-list fast-tasks
        project-settings (config/resolve-for-root project-root-uri)
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
    (publish-task-progress producer (:finding-kondo task-list) progress-token)
    (ensure-kondo-config-dir-exists! project-root-uri @db*)
    (publish-task-progress producer (:finding-cache task-list) progress-token)
    (load-db-cache! root-path db*)
    (let [project-hash (classpath/project-specs->hash root-path settings)
          kondo-config-hash (lsp.kondo/config-hash (str root-path))
          dependency-scheme (:dependency-scheme settings)
          dependency-scheme-changed? (not= dependency-scheme (:dependency-scheme @db*))
          use-db-analysis? (and (= (:project-hash @db*) project-hash)
                                (= (:kondo-config-hash @db*) kondo-config-hash)
                                (not dependency-scheme-changed?))
          fast-startup? (or use-db-analysis?
                            (= :project-only (:project-analysis-type @db*)))
          task-list (if fast-startup? fast-tasks slow-tasks)]
      (if use-db-analysis?
        (let [classpath (:classpath @db*)]
          (logger/info startup-logger-tag (format "Using cached classpath %s" classpath))
          (swap! db* assoc
                 :settings (update settings :source-paths (partial source-paths/process-source-paths settings root-path classpath)))
          (publish-task-progress producer (:copying-kondo fast-tasks) progress-token)
          (copy-configs-from-classpath! classpath settings @db*))
        (do
          (publish-task-progress producer (:discovering-classpath slow-tasks) progress-token)
          (when-let [classpath (classpath/scan-classpath! components)]
            (swap! db* assoc
                   :project-hash project-hash
                   :kondo-config-hash kondo-config-hash
                   :analysis-checksums (if dependency-scheme-changed? nil (:analysis-checksums @db*))
                   :analysis (if dependency-scheme-changed? nil (:analysis @db*))
                   :dependency-scheme dependency-scheme
                   :classpath classpath
                   :settings (update settings :source-paths (partial source-paths/process-source-paths settings root-path classpath)))

            (publish-task-progress producer (:copying-kondo slow-tasks) progress-token)
            (copy-configs-from-classpath! classpath settings @db*)
            (when (contains? #{:project-and-full-dependencies
                               :project-and-clojure-only-dependencies} (:project-analysis-type @db*))
              (publish-task-progress producer (:analyzing-deps slow-tasks) progress-token)
              (analyze-external-classpath! root-path (-> @db* :settings :source-paths) classpath progress-token components))
            (logger/info "Caching db for next startup...")
            (upsert-db-cache! @db*))))
      (publish-task-progress producer (:resolving-config task-list) progress-token)
      (when-let [classpath-settings (and (config/classpath-config-paths? settings)
                                         (:classpath @db*)
                                         (config/resolve-from-classpath-config-paths (:classpath @db*) settings))]
        (swap! db* assoc
               :settings (shared/deep-merge (:settings @db*)
                                            classpath-settings
                                            project-settings
                                            force-settings)
               :classpath-settings classpath-settings))
      (publish-task-progress producer (:analyzing-project task-list) progress-token)
      (logger/info startup-logger-tag "Analyzing source paths for project root" (str root-path))
      (analyze-source-paths! (-> @db* :settings :source-paths)
                             db*
                             (fn [{:keys [total-files files-done]}]
                               (let [task (-> (:analyzing-project task-list)
                                              (partial-task files-done total-files))]
                                 (publish-task-progress producer task progress-token))))
      (swap! db* assoc :settings-auto-refresh? true)
      (publish-task-progress producer (:done task-list) progress-token))))
