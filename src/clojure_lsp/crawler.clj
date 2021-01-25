(ns clojure-lsp.crawler
  (:require
   [cljfmt.main :as cljfmt.main]
   [clj-kondo.core :as kondo]
   [clojure-lsp.db :as db]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.producer :as producer]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [digest :as digest])
  (:import
   [java.nio.file Paths]))

(defn ^:private to-file [path child]
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

(defn ^:private command-exists?
  [command]
  (let [check-command (if shared/windows-os? "where.exe" "which")]
    (try
      (= (:exit (apply shell/sh check-command [command])) 0)
      (catch Exception _
        false))))

(defn ^:private with-shadow-cljs-project-spec
  [specs]
  (cond
    (command-exists? "npx")
    (conj specs {:project-path "shadow-cljs.edn"
                 :classpath-cmd ["npx" "shadow-cljs" "classpath"]})

    :else
    (conj specs {:project-path "shadow-cljs.edn"
                 :classpath-cmd ["shadow-cljs" "classpath"]})))

(defn ^:private default-project-specs []
  (->> [{:project-path "project.clj"
         :classpath-cmd ["lein" "classpath"]}
        {:project-path "deps.edn"
         :classpath-cmd ["clj" "-Spath"]}
        {:project-path "build.boot"
         :classpath-cmd ["boot" "show" "--fake-classpath"]}]
       with-shadow-cljs-project-spec
       (map #(update % :classpath-cmd classpath-cmd->windows-safe-classpath-cmd))))

(default-project-specs)

(defn ^:private get-cp-entry-type [e]
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

(defn ^:private run-kondo-on-paths! [paths]
  (kondo/run! (kondo-args {:parallel true
                           :lint [(string/join (System/getProperty "path.separator") paths)]} false)))

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
  (log/info "Analyzing" (count paths) "paths with clj-kondo...")
  (let [result (run-kondo-on-paths! paths)
        _ (log/info "Paths analyzed, took" (-> result :summary :duration (/ 1000) float) "secs. Caching for next startups...")
        kondo-analysis (cond-> (:analysis result)
                           public-only? (dissoc :namespace-usages :var-usages)
                           public-only? (update :var-definitions (fn [usages] (remove :private usages))))
        analysis (->> kondo-analysis
                      (normalize-analysis)
                      (group-by :filename))]
    (swap! db/db update :analysis merge analysis)
    analysis))

(defn ^:private analyze-classpath [root-path source-paths settings]
  (let [project-specs (->> (or (get settings :project-specs) (default-project-specs))
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
              analysis (analyze-paths adjusted-cp true)]
          (db/save-deps root-path project-hash classpath analysis))))))

(defn ^:private analyze-project [project-root]
  (let [root-path (shared/uri->path project-root)
        settings (:settings @db/db)
        source-paths (get settings :source-paths)]
    (analyze-classpath root-path source-paths settings)
    (analyze-paths source-paths false)
    nil))

(defn ^:private find-raw-project-settings [project-root]
  (let [config-path (Paths/get ".lsp" (into-array ["config.edn"]))]
    (loop [dir (shared/uri->path project-root)]
      (let [full-config-path (.resolve dir config-path)
            file (.toFile full-config-path)
            parent-dir (.getParent dir)]
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
