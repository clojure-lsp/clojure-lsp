(ns clojure-lsp.crawler
  (:require
    [clj-kondo.core :as kondo]
    [cljfmt.main :as cljfmt.main]
    [clojure-lsp.config :as config]
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.diagnostics :as f.diagnostic]
    [clojure-lsp.logging :as logging]
    [clojure-lsp.producer :as producer]
    [clojure-lsp.shared :as shared]
    [clojure.core.async :as async]
    [clojure.java.io :as io]
    [clojure.java.shell :as shell]
    [clojure.set :as set]
    [clojure.string :as string]
    [digest :as digest]
    [taoensso.timbre :as log])
  (:import
   (java.net URI)))

(defn ^:private to-file ^java.io.File
  [^java.nio.file.Path path
   ^String child]
  (.toFile (.resolve path child)))

(defn ^:private lookup-classpath [root-path {:keys [classpath-cmd env]}]
  (log/info "Finding classpath via `" (string/join " " classpath-cmd) "`")
  (try
    (let [sep (re-pattern (System/getProperty "path.separator"))
          response (apply shell/sh (into classpath-cmd
                                         (cond-> [:dir (str root-path)]
                                           env (conj :env (merge {} (System/getenv) env)))))]
      (-> response
          (:out)
          (string/trim-newline)
          (string/split sep)))
    (catch Exception e
      (log/error e "Error while looking up classpath info in" (str root-path) (.getMessage e))
      (producer/window-show-message "Classpath lookup failed in clojure-lsp. Some features may not work correctly."  :warning)
      [])))

(defn ^:private valid-project-specs-with-hash [root-path project-specs]
  (keep
    (fn [{:keys [project-path] :as project-spec}]
      (let [project-file (to-file root-path project-path)]
        (when (.exists project-file)
          (assoc project-spec :hash (digest/md5 project-file)))))
    project-specs))

(defn ^:private classpath-cmd->windows-safe-classpath-cmd
  [classpath]
  (if shared/windows-os?
    (into ["powershell.exe"] classpath)
    classpath))

(def ^:private default-project-specs
  (->> [{:project-path "project.clj"
         :classpath-cmd ["lein" "classpath"]}
        {:project-path "deps.edn"
         :classpath-cmd ["clojure" "-Spath"]}
        {:project-path "build.boot"
         :classpath-cmd ["boot" "show" "--fake-classpath"]}
        {:project-path "shadow-cljs.edn"
         :classpath-cmd ["npx" "shadow-cljs" "classpath"]}]
       (map #(update % :classpath-cmd classpath-cmd->windows-safe-classpath-cmd))))

(defn ^:private get-cp-entry-type [^java.io.File e]
  (cond (.isFile e) :file
        (.isDirectory e) :directory
        :else :unkown))

(def clj-kondo-analysis-batch-size 50)

(defn ^:private run-kondo-on-paths! [paths settings]
  (let [err (java.io.StringWriter.)]
    (binding [*err* err]
      (let [result (kondo/run! (config/kondo-for-paths paths settings))]
        (when-not (string/blank? (str err))
          (log/error (str err)))
        result))))

(defn ^:private run-kondo-on-paths-batch!
  "Run kondo on paths by partition the paths, with this we should call
  kondo more times but we fewer paths to analyze, improving memory."
  [paths settings]
  (let [total (count paths)
        batch-count (int (Math/ceil (float (/ total clj-kondo-analysis-batch-size))))]
    (log/info "Analyzing" total "paths with clj-kondo with batch size of" batch-count "...")
    (if (<= total clj-kondo-analysis-batch-size)
      (run-kondo-on-paths! paths settings)
      (->> paths
           (partition-all clj-kondo-analysis-batch-size)
           (map-indexed (fn [index batch-paths]
                          (log/info "Analyzing" (str (inc index) "/" batch-count) "batch paths with clj-kondo...")
                          (run-kondo-on-paths! batch-paths settings)))
           (reduce shared/deep-merge)))))

(defn run-kondo-on-text! [text uri settings]
  (let [err (java.io.StringWriter.)]
    (binding [*err* err]
      (let [result (with-in-str
                     text
                     (kondo/run! (config/kondo-for-single-file uri settings)))]
        (when-not (string/blank? (str err))
          (log/error (str err)))
        result))))

(defn entry->normalized-entries [{:keys [bucket] :as element}]
  (cond
    ;; We create two entries here (and maybe more for refer)
    (= :namespace-usages bucket)
    (cond-> [(set/rename-keys element {:to :name})]
      (:alias element)
      (conj (set/rename-keys (assoc element :bucket :namespace-alias) {:alias-row :name-row :alias-col :name-col :alias-end-row :name-end-row :alias-end-col :name-end-col})))

    (contains? #{:locals :local-usages :keywords} bucket)
    [(-> element
         (assoc :name-row (or (:name-row element) (:row element))
                :name-col (or (:name-col element) (:col element))
                :name-end-row (or (:name-end-row element) (:end-row element))
                :name-end-col (or (:name-end-col element) (:end-col element))))]

    :else
    [element]))

(defn ^:private macro-expanded-element? [{:keys [name to row col] :as _element}]
  (and (not row)
       (not col)
       (or (= '-> name)
           (= '->> name)
           (= 'fn* name)
           (= 'let* name)
           (= 'let name)
           (= 'if name)
           (= 'new name))
       (#{'clojure.core 'cljs.core} to)))

(defn ^:private valid-element? [{:keys [name-row name-col name-end-row name-end-col] :as _element}]
  (and name-row
       name-col
       name-end-row
       name-end-col))

(defn normalize-analysis [analysis]
  (for [[bucket vs] analysis
        v vs
        element (entry->normalized-entries (assoc v :bucket bucket))
        :let [valid? (valid-element? element)
              _ (when (and (not valid?)
                           (not (macro-expanded-element? element)))
                  (log/debug "Invalid clj-kondo analysis. Cannot find position data for" (:name element) (pr-str element) (some-> (:name element) meta)))]
        :when valid?]
    element))

(defn update-analysis [db uri new-analysis]
  (assoc-in db [:analysis (shared/uri->filename uri)] (normalize-analysis new-analysis)))

(defn update-findings [db uri new-findings]
  (assoc-in db [:findings (shared/uri->filename uri)] new-findings))

(defn ^:private lint-project-files [paths]
  (async/go
    (doseq [path paths]
      (doseq [file (file-seq (io/file path))]
        (let [filename (.getAbsolutePath ^java.io.File file)
              uri (shared/filename->uri filename)]
          (when (not= :unknown (shared/uri->file-type uri))
            (f.diagnostic/sync-lint-file uri @db/db)))))))

(defn ^:private analyze-paths! [paths public-only?]
  (let [settings (:settings @db/db)
        start-time (System/nanoTime)
        result (if public-only?
                 (run-kondo-on-paths-batch! paths settings)
                 (run-kondo-on-paths! paths settings))
        end-time (float (/ (- (System/nanoTime) start-time) 1000000000))
        _ (log/info "Paths analyzed, took" end-time "secs. Caching for next startups...")
        kondo-analysis (cond-> (:analysis result)
                         public-only? (dissoc :namespace-usages :var-usages)
                         public-only? (update :var-definitions (fn [usages] (remove :private usages))))
        analysis (->> kondo-analysis
                      (normalize-analysis)
                      (group-by :filename))]
    (swap! db/db update :analysis merge analysis)
    (when-not public-only?
      (swap! db/db update :findings merge (group-by :filename (:findings result)))
      (when (get settings :lint-project-files-after-startup? true)
        (lint-project-files paths)))
    analysis))

(defn ^:private analyze-classpath! [root-path source-paths settings]
  (let [project-specs (->> (or (get settings :project-specs) default-project-specs)
                           (valid-project-specs-with-hash root-path))
        ignore-directories? (get settings :ignore-classpath-directories)
        project-hash (reduce str (map :hash project-specs))
        loaded (db/read-deps root-path)
        use-db-analysis? (= (:project-hash loaded) project-hash)]
    (if use-db-analysis?
      (swap! db/db update :analysis merge (:analysis loaded))
      (when-let [classpath (->> project-specs
                                (mapcat #(lookup-classpath root-path %))
                                vec
                                seq)]
        (log/info "Analyzing classpath for project root" root-path)
        (let [adjusted-cp (cond->> classpath
                            ignore-directories? (remove #(let [f (io/file %)] (= :directory (get-cp-entry-type f))))
                            :always (remove (set source-paths)))
              analysis (analyze-paths! adjusted-cp true)
              start-time (System/nanoTime)]
          (System/gc)
          (log/info "Manual GC after classpath scan took" (float (/ (- (System/nanoTime) start-time) 1000000000)) "seconds")
          (db/save-deps root-path project-hash classpath analysis))))))

(defn ^:private analyze-project! [project-root-uri]
  (let [root-path (shared/uri->path project-root-uri)
        settings (:settings @db/db)
        source-paths (get settings :source-paths)]
    (analyze-classpath! root-path source-paths settings)
    (log/info "Analyzing source paths for project root" root-path)
    (analyze-paths! source-paths false)))

(def default-source-paths #{"src" "test"})

(defn ^:private resolve-source-paths [root-path settings given-source-paths]
  (let [deps-file (to-file root-path "deps.edn")]
    (cond
      given-source-paths
      (do
        (log/info "Using given source-paths:" given-source-paths)
        given-source-paths)

      (.exists deps-file)
      (let [deps-source-paths (config/resolve-deps-source-paths (config/read-edn-file deps-file) settings)]
        (if (seq deps-source-paths)
          (do
            (log/info "Automatically resolved source-paths from deps.edn:" deps-source-paths)
            deps-source-paths)
          (do
            (log/info "Empty deps.edn source-paths, using default source-paths:" default-source-paths)
            default-source-paths)))

      :else
      (do
        (log/info "Using default source-paths:" default-source-paths)
        default-source-paths))))

(defn ^:private process-source-paths [root-path settings given-source-paths]
  (let [source-paths (resolve-source-paths root-path settings given-source-paths)]
    (mapv #(->> % (to-file root-path) .getAbsolutePath str) source-paths)))

(defn initialize-project [project-root-uri client-capabilities client-settings force-settings]
  (let [project-settings (config/resolve-config project-root-uri)
        root-path (shared/uri->path project-root-uri)
        encoding-settings {:uri-format {:upper-case-drive-letter? (->> project-root-uri URI. .getPath
                                                                       (re-find #"^/[A-Z]:/")
                                                                       boolean)
                                        :encode-colons-in-path? (string/includes? project-root-uri "%3A")}}
        raw-settings (merge encoding-settings
                            client-settings
                            project-settings
                            force-settings)
        _ (when-let [log-path (:log-path raw-settings)]
            (logging/update-log-path log-path))
        settings (-> raw-settings
                     (update :project-specs #(or % default-project-specs))
                     (update :source-aliases #(or % config/default-source-aliases))
                     (update :source-paths (partial process-source-paths root-path raw-settings))
                     (update :cljfmt cljfmt.main/merge-default-options))]
    (swap! db/db assoc
           :project-root-uri project-root-uri
           :project-settings project-settings
           :client-settings client-settings
           :settings settings
           :client-capabilities client-capabilities)
    (analyze-project! project-root-uri)))
