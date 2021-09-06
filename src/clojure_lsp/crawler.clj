(ns clojure-lsp.crawler
  (:require
   [cljfmt.main :as cljfmt.main]
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
   [digest :as digest]
   [medley.core :as medley]
   [taoensso.timbre :as log])
  (:import
   (java.net URI)))

(defn ^:private lookup-classpath [root-path {:keys [classpath-cmd env]} db]
  (log/info (format "Finding classpath via `%s`" (string/join " " classpath-cmd)))
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
          (producer/window-show-message "Classpath lookup failed in clojure-lsp. Some features may not work correctly." :warning db)
          [])))
    (catch Exception e
      (log/error e (format "Error while looking up classpath info in %s" (str root-path)) (.getMessage e))
      (producer/window-show-message "Classpath lookup failed in clojure-lsp. Some features may not work correctly." :warning db)
      [])))

(defn ^:private valid-project-specs-with-hash [root-path project-specs]
  (keep
    (fn [{:keys [project-path] :as project-spec}]
      (let [project-file (shared/to-file root-path project-path)]
        (when (shared/file-exists? project-file)
          (assoc project-spec :hash (digest/md5 project-file)))))
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

(defn ^:private analyze-external-classpath! [paths db]
  (let [result (shared/logging-time
                 "External classpath paths analyzed, took %s secs. Caching for next startups..."
                 (lsp.kondo/run-kondo-on-paths-batch! paths true db))
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
        loaded (db/read-deps root-path db)
        use-db-analysis? (= (:project-hash loaded) project-hash)]
    (if use-db-analysis?
      (do
        (log/info "Using cached classpath for project root" root-path)
        (swap! db update :analysis merge (:analysis loaded)))
      (do
        (log/info "Analyzing classpath for project root" root-path)
        (when-let [classpath (->> project-specs
                                  (mapcat #(lookup-classpath root-path % db))
                                  vec
                                  seq)]
          (let [external-classpath (cond->> classpath
                                     ignore-directories? (remove #(let [f (io/file %)] (= :directory (get-cp-entry-type f))))
                                     :always (remove (set source-paths)))
                analysis (analyze-external-classpath! external-classpath db)]
            (shared/logging-time
              "Manual GC after classpath scan took %s secs"
              (System/gc))
            (db/save-deps! root-path project-hash classpath analysis db)))))))

(defn ^:private create-kondo-folder! [clj-kondo-folder]
  (try
    (log/info (format "Folder %s not found, creating for necessary clj-kondo analysis..." (.getCanonicalPath clj-kondo-folder)))
    (.mkdir clj-kondo-folder)
    (catch Exception e
      (log/error "Error when creating '.clj-kondo' dir on project-root" e))))

(defn ^:private setup-pre-analysis!
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
  (let [project-settings (config/resolve-config project-root-uri)
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
                     (update :source-paths (partial source-paths/process-source-paths root-path raw-settings))
                     (update :cljfmt cljfmt.main/merge-default-options))]
    (swap! db assoc
           :project-root-uri project-root-uri
           :project-settings project-settings
           :client-settings client-settings
           :settings settings
           :client-capabilities client-capabilities)
    (setup-pre-analysis! project-root-uri db)
    (analyze-classpath! root-path (:source-paths settings) settings db)
    (log/info "Analyzing source paths for project root" root-path)
    (analyze-source-paths! (:source-paths settings) db)))
