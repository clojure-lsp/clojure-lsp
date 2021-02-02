(ns clojure-lsp.crawler
  (:require
   [clj-kondo.core :as kondo]
   [cljfmt.main :as cljfmt.main]
   [clojure-lsp.db :as db]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.shared :as shared]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.set :as set]
   [clojure.string :as string]
   [taoensso.timbre :as log]
   [digest :as digest])
  (:import
   [java.nio.file Paths]))

(defn ^:private to-file ^java.io.File
  [^java.nio.file.Path path
   ^String child]
  (.toFile (.resolve path child)))

(defn ^:private lookup-classpath [root-path {:keys [classpath-cmd env]}]
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
      (producer/window-show-message "Classpath lookup failed in clojure-lsp. Some features may not work correctly." :warning)
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
         :classpath-cmd ["clj" "-Spath"]}
        {:project-path "build.boot"
         :classpath-cmd ["boot" "show" "--fake-classpath"]}
        {:project-path "shadow-cljs.edn"
         :classpath-cmd ["npx" "shadow-cljs" "classpath"]}]
       (map #(update % :classpath-cmd classpath-cmd->windows-safe-classpath-cmd))))

(defn ^:private get-cp-entry-type [^java.io.File e]
  (cond (.isFile e) :file
        (.isDirectory e) :directory
        :else :unkown))

(defn ^:private kondo-args [extra locals]
  (let [user-config (get-in @db/db [:settings :clj-kondo])
        kondo-dir (some-> (:project-root @db/db)
                          shared/uri->path
                          (.resolve ".clj-kondo"))]
    (cond-> {:cache true
             :cache-dir ".clj-kondo/.cache"}
      (some-> kondo-dir (.toFile) (.exists))
      (assoc :cache-dir (str (.resolve kondo-dir ".cache"))
             :config-dir (str kondo-dir))

      :always
      (merge extra)

      user-config
      (update-in [:config] merge user-config)

      :always
      (assoc-in [:config :output] {:analysis {:arglists true}
                                   :canonical-paths true})

      locals
      (assoc-in [:config :output :analysis :locals] true))))

(def clj-kondo-analysis-batch-size 50)

(defn ^:private run-kondo-on-paths! [paths]
  (kondo/run! (kondo-args {:parallel true
                           :lint [(string/join (System/getProperty "path.separator") paths)]} false)))

(defn ^:private deep-merge [a b]
  (merge-with (fn [x y]
                (cond (map? y) (deep-merge x y)
                      (vector? y) (concat x y)
                      :else y))
                 a b))

(defn ^:private run-kondo-on-paths-batch!
  "Run kondo on paths by partition the paths, with this we should call
  kondo more times but we fewer paths to analyze, improving memory."
  [paths]
  (let [total (count paths)
        batch-count (int (Math/ceil (float (/ total clj-kondo-analysis-batch-size))))]
    (log/info "Analyzing" total "paths with clj-kondo with batch size of" batch-count "...")
    (if (<= total clj-kondo-analysis-batch-size)
      (run-kondo-on-paths! paths)
      (->> paths
           (partition-all clj-kondo-analysis-batch-size)
           (map-indexed (fn [index batch-paths]
                          (log/info "Analyzing" (str (inc index) "/" batch-count) "batch paths with clj-kondo...")
                          (run-kondo-on-paths! batch-paths)))
           (reduce deep-merge)))))

(defn run-kondo-on-text! [text uri]
  (with-in-str
    text
    (kondo/run! (kondo-args {:lint ["-"]
                             :lang (shared/uri->file-type uri)
                             :filename (shared/uri->filename uri)}
                            true))))

(defn entry->normalized-entries [{:keys [bucket] :as element}]
  (cond
    ;; We create two entries here (and maybe more for refer)
    (= :namespace-usages bucket)
    (cond-> [(set/rename-keys element {:to :name})]
      (:alias element)
      (conj (set/rename-keys (assoc element :bucket :namespace-alias) {:alias-row :name-row :alias-col :name-col :alias-end-row :name-end-row :alias-end-col :name-end-col})))

    (contains? #{:locals :local-usages} bucket)
    [(set/rename-keys element {:row :name-row :col :name-col :end-row :name-end-row :end-col :name-end-col})]

    :else
    [element]))

(defn normalize-analysis [analysis]
  (for [[bucket vs] analysis
        v vs
        element (entry->normalized-entries (assoc v :bucket bucket))
        :let [{:keys [name-row name-col name-end-row name-end-col] :as element} element
              valid? (and name-row name-col name-end-row name-end-col)
              _ (when-not valid? (log/error "Cannot find position for:" (:name element) (pr-str element) (some-> (:name element) meta)))]
        :when valid?]
    element))

(defn update-analysis [db uri new-analysis]
  (assoc-in db [:analysis (shared/uri->filename uri)] (normalize-analysis new-analysis)))

(defn update-findings [db uri new-findings]
  (assoc-in db [:findings (shared/uri->filename uri)] new-findings))

(defn ^:private analyze-paths [paths public-only?]
  (let [start-time (System/nanoTime)
        result (if public-only?
                 (run-kondo-on-paths-batch! paths)
                 (run-kondo-on-paths! paths))
        end-time (float (/ (- (System/nanoTime) start-time) 1000000000))
        _ (log/info "Paths analyzed, took" end-time "secs. Caching for next startups...")
        kondo-analysis (cond-> (:analysis result)
                           public-only? (dissoc :namespace-usages :var-usages)
                           public-only? (update :var-definitions (fn [usages] (remove :private usages))))
        analysis (->> kondo-analysis
                      (normalize-analysis)
                      (group-by :filename))]
    (swap! db/db update :analysis merge analysis)
    analysis))

(defn ^:private analyze-classpath [root-path source-paths settings]
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
        (let [adjusted-cp (cond->> classpath
                            ignore-directories? (remove #(let [f (io/file %)] (= :directory (get-cp-entry-type f))))
                            :always (remove (set source-paths)))
              analysis (analyze-paths adjusted-cp true)
              start-time (System/nanoTime)]
            (System/gc)
            (log/info "Manual GC after classpath scan took" (float (/ (- (System/nanoTime) start-time) 1000000000)) "seconds")
            (db/save-deps root-path project-hash classpath analysis))))))

(defn ^:private analyze-project [project-root]
  (let [root-path (shared/uri->path project-root)
        settings (:settings @db/db)
        source-paths (get settings :source-paths)]
    (analyze-classpath root-path source-paths settings)
    (analyze-paths source-paths false)
    nil))

(defn ^:private find-raw-project-settings [project-root]
  (let [config-path (.toString (Paths/get ".lsp" (into-array ["config.edn"])))]
    (loop [dir (shared/uri->path project-root)]
      (let [full-config-path (.resolve dir config-path)
            file (.toFile full-config-path)
            parent-dir (.getParent ^java.nio.file.Path dir)]
        (cond
          (.exists file)
          (slurp file)

          parent-dir
          (recur parent-dir)

          :else
          "{}")))))

(defn ^:private find-project-settings [project-root]
  (->> (find-raw-project-settings project-root)
       (edn/read-string {:readers {'re re-pattern}})
       shared/keywordize-first-depth))

(defn initialize-project [project-root client-capabilities client-settings]
   (let [project-settings (find-project-settings project-root)
         root-path (shared/uri->path project-root)]
      (swap! db/db assoc
             :project-root project-root
             :project-settings project-settings
             :client-settings client-settings
             :settings (-> (merge client-settings project-settings)
                           (update :source-paths (fn [source-paths] (mapv #(str (.getAbsolutePath (to-file root-path %))) source-paths)))
                           (update :cljfmt cljfmt.main/merge-default-options))
             :client-capabilities client-capabilities)
      (analyze-project project-root)))
