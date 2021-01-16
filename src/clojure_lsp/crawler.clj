(ns clojure-lsp.crawler
  (:require
   [clj-kondo.core :as kondo]
   [clojure-lsp.db :as db]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.window :as window]
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
      (window/show-message "Error looking up classpath info" :error)
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
  (if (shared/windows-os?)
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
         :classpath-cmd ["shadow-cljs" "classpath"]}]
       (map #(update % :classpath-cmd classpath-cmd->windows-safe-classpath-cmd))))

(defn ^:private get-cp-entry-type [e]
  (cond (.isFile e) :file
        (.isDirectory e) :directory
        :else :unkown))

(defn ^:private kondo-args [extra locals]
  (let [root-path (shared/uri->path (:project-root @db/db))
        user-config (get-in @db/db [:settings :clj-kondo])
        kondo-dir (.resolve root-path ".clj-kondo")]
    (cond-> {:cache true
             :cache-dir (str (.resolve kondo-dir ".cache"))}
      (.exists (.toFile kondo-dir))
      (assoc :cache-dir (str (.resolve kondo-dir ".cache"))
             :config-dir (str kondo-dir))

      :always
      (merge extra)

      user-config
      (update-in [:config] merge user-config)

      :always
      (assoc-in [:config :output] {:analysis {:arglists true} :canonical-paths true})

      locals
      (assoc-in [:config :output :analysis :locals] true))))

(defn run-kondo-on-paths! [paths]
  (kondo/run! (kondo-args {:lint [(string/join (System/getProperty "path.separator") paths)]} false)))

(defn run-kondo-on-text! [text uri]
  (with-in-str
    text
    (kondo/run! (kondo-args {:lint ["-"]
                             :lang (shared/uri->file-type uri)
                             :filename (shared/uri->filename uri)}
                            true))))

(defn normalize-analysis [analysis]
  (reduce
    (fn [accum [k vs]]
      (->> vs
           (keep
             (fn [v]
               (when (or (:col v) (:name-col v))
                 (let [result (cond-> v
                                (= :namespace-usages k)
                                (assoc :end-row (:row v)
                                       :end-col (some-> (:alias v) name count (+ (:col v))))

                                (contains? #{:namespace-usages :locals :local-usages} k)
                                (set/rename-keys {:row :name-row :col :name-col :end-row :name-end-row :end-col :name-end-col})

                                :always
                                (assoc :bucket k))
                       {:keys [name-row name-col name-end-row name-end-col]} result
                       valid? (and name-row name-col name-end-row name-end-col)]
                   (if valid?
                     result
                     (do
                       (log/error "Cannot find position for:" (:name result ) (pr-str result) (some-> (:name result) meta))
                       nil))))))
           (into accum)))
    []
    analysis))

(defn update-analysis [db uri new-analysis]
  (assoc-in db [:analysis (shared/uri->filename uri)] (normalize-analysis new-analysis)))

(defn analyze-paths [paths]
  (let [result (run-kondo-on-paths! paths)
        analysis (->> (:analysis result)
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
                            :always (log/spy)
                            :always (remove (set source-paths))
                            :always (log/spy))
              analysis (analyze-paths adjusted-cp)]
          (db/save-deps root-path project-hash classpath analysis))))))

(defn determine-dependencies [project-root]
  (let [root-path (shared/uri->path project-root)
        settings (:settings @db/db)
        source-paths (mapv #(str (.getAbsolutePath (to-file root-path %))) (get settings :source-paths))]
    (analyze-classpath root-path source-paths settings)
    (analyze-paths source-paths)
    nil))

(defn find-raw-project-settings [project-root]
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

(defn find-project-settings [project-root]
  (->> (find-raw-project-settings project-root)
       (edn/read-string {:readers {'re re-pattern}})
       shared/keywordize-first-depth))
