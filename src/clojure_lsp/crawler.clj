(ns clojure-lsp.crawler
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.db :as db]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.logging :as logging]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.source-paths :as source-paths]
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
            (producer/window-show-message (format "Classpath lookup failed when running `%s`. Some features may not work correctly." command) :warning db)
            [])))
      (catch Exception e
        (log/error e (format "Error while looking up classpath info in %s" (str root-path)) (.getMessage e))
        (producer/window-show-message (format "Classpath lookup failed when running `%s`. Some features may not work correctly." command) :warning db)
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

(defn ^:private classpath-cmd->windows-safe-classpath-cmd
  [classpath]
  (if shared/windows-os?
    (into ["powershell.exe" "-NoProfile"] classpath)
    classpath))

(defn ^:private default-project-specs []
  (->> [{:project-path "project.clj"
         :classpath-cmd ["lein" "classpath"]}
        {:project-path "deps.edn"
         :classpath-cmd ["clojure" "-Spath"]}
        {:project-path "build.boot"
         :classpath-cmd ["boot" "show" "--fake-classpath"]}
        {:project-path "shadow-cljs.edn"
         :classpath-cmd ["npx" "shadow-cljs" "classpath"]}
        {:project-path "bb.edn"
         :classpath-cmd ["bb" "print-deps" "--format" "classpath"]}]
       (map #(update % :classpath-cmd classpath-cmd->windows-safe-classpath-cmd))))

(defn ^:private get-cp-entry-type [^java.io.File e]
  (cond (.isFile e) :file
        (.isDirectory e) :directory
        :else :unkown))

(defn ^:private report-startup-progress
  ([percentage db]
   (report-startup-progress percentage nil db))
  ([percentage message db]
   (let [progress (case percentage
                    0 {:kind :begin
                       :title message
                       :percentage percentage}
                    100 {:kind :end}
                    {:kind :report
                     :message message
                     :percentage percentage})]
     (producer/notify-progress {:token "clojure-lsp"
                                :value progress} db))))

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
  [start-progress-percentage fulfill-progress-percentage db index count]
  (let [real-percentage (- fulfill-progress-percentage start-progress-percentage)]
    (report-startup-progress
      (+ start-progress-percentage (/ (* index real-percentage) count))
      "Analyzing external classpath"
      db)))

(defn ^:private analyze-external-classpath!
  [paths start-progress-percentage fulfill-progress-percentage db]
  (let [batch-update-callback (partial report-batch-analysis-percentage start-progress-percentage fulfill-progress-percentage db)
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

(defn ^:private analyze-classpath! [root-path source-paths settings db]
  (let [project-specs (->> (or (get settings :project-specs) (default-project-specs))
                           (valid-project-specs-with-hash root-path))
        ignore-directories? (get settings :ignore-classpath-directories)
        project-hash (reduce str (map :hash project-specs))
        kondo-config-hash (lsp.kondo/config-hash (str root-path))
        db-cache (db/read-deps root-path db)
        use-db-analysis? (and (= (:project-hash db-cache) project-hash)
                              (= (:kondo-config-hash db-cache) kondo-config-hash))]
    (report-startup-progress 15 "Discovering classpath" db)
    (if use-db-analysis?
      (do
        (log/info "Using cached classpath for project root" root-path)
        (swap! db (fn [state-db]
                    (-> state-db
                        (update :analysis merge (:analysis db-cache))
                        (assoc :classpath (:classpath db-cache))))))
      (do
        (log/info "Analyzing classpath for project root" root-path)
        (when-let [classpath (->> project-specs
                                  (mapcat #(lookup-classpath root-path % db))
                                  vec
                                  seq)]
          (swap! db assoc :classpath classpath)
          (report-startup-progress 20 "External classpath" db)
          (let [external-classpath (cond->> classpath
                                     ignore-directories? (remove #(let [f (io/file %)] (= :directory (get-cp-entry-type f))))
                                     :always (remove (set source-paths)))
                analysis (analyze-external-classpath! external-classpath 20 80 db)]
            (shared/logging-time
              "Manual GC after classpath scan took %s secs"
              (System/gc))
            (db/save-deps! root-path project-hash kondo-config-hash classpath analysis db)))))))

(defn ^:private create-kondo-folder! [clj-kondo-folder]
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

(defn initialize-project [project-root-uri client-capabilities client-settings force-settings db]
  (report-startup-progress 0 "Resolving project" db)
  (let [project-settings (config/resolve-for-root project-root-uri)
        root-path (shared/uri->path project-root-uri)
        encoding-settings {:uri-format {:upper-case-drive-letter? (->> project-root-uri URI. .getPath
                                                                       (re-find #"^/[A-Z]:/")
                                                                       boolean)
                                        :encode-colons-in-path? (string/includes? project-root-uri "%3A")}}
        raw-settings (medley/deep-merge encoding-settings
                                        client-settings
                                        project-settings
                                        force-settings)
        _ (when-let [log-path (:log-path raw-settings)]
            (logging/update-log-path log-path db))
        settings (-> raw-settings
                     (update :project-specs #(or % (default-project-specs)))
                     (update :source-aliases #(or % source-paths/default-source-aliases))
                     (update :source-paths (partial source-paths/process-source-paths root-path raw-settings)))]
    (swap! db assoc
           :project-root-uri project-root-uri
           :project-settings project-settings
           :client-settings client-settings
           :settings settings
           :client-capabilities client-capabilities)
    (report-startup-progress 5 db)
    (ensure-kondo-config-dir-exists! project-root-uri db)
    (report-startup-progress 10 db)
    (analyze-classpath! root-path (:source-paths settings) settings db)
    (report-startup-progress 90 "Resolving config paths" db)
    (when-let [classpath-settings (config/resolve-from-classpath-config-paths (:classpath @db) settings)]
      (swap! db assoc
             :settings (medley/deep-merge settings
                                          classpath-settings
                                          project-settings
                                          force-settings)
             :classpath-configs classpath-settings))
    (log/info "Analyzing source paths for project root" root-path)
    (report-startup-progress 95 "Analyzing project files" db)
    (analyze-source-paths! (:source-paths settings) db)
    (report-startup-progress 100 db)))
