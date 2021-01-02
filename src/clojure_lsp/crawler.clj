(ns clojure-lsp.crawler
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.diagnostics :as f.diagnostic]
    [clojure-lsp.feature.references :as f.references]
    [clojure-lsp.shared :as shared]
    [clojure-lsp.window :as window]
    [clojure.core.async :as async]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.java.shell :as shell]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [digest :as digest])
  (:import
    [java.util.jar JarFile]
    [java.nio.file Paths]))

(defn ^:private file->uri [file]
  (str (.toUri (.toPath file))))

(defn ^:private to-file [path child]
  (.toFile (.resolve path child)))

(defn ^:private process-unused
  [usages declarations]
  (let [ensure-sym (fn [s] (when-not (string? s) s))]
    (->> usages
         (remove (comp #(or (contains? % :declare)
                            (contains? % :refer)) :tags))
         (map #(some-> % :sym ensure-sym namespace symbol))
         set
         (set/difference (set (map :ns declarations))))))

(defn find-unused-aliases [uri]
  (let [usages (f.references/safe-find-references uri (slurp uri) false false)
        declarations (f.diagnostic/usages->declarations usages)
        excludes (-> (get-in @db/db [:settings :linters :unused-namespace :exclude] #{}) set)
        declared-aliases (->> declarations
                              (filter (comp #(contains? % :alias) :tags))
                              (remove (comp excludes :ns)))]
    (process-unused usages declared-aliases)))

(defn find-unused-refers [uri]
  (let [usages (f.references/safe-find-references uri (slurp uri) false false)
        declarations (f.diagnostic/usages->declarations usages)
        excludes (-> (get-in @db/db [:settings :linters :unused-namespace :exclude] #{}) set)
        declared-refers (->> declarations
                             (filter (comp #(contains? % :refer) :tags))
                             (remove (comp excludes :ns)))]
    (process-unused usages declared-refers)))

(defn crawl-jars [jars dependency-scheme]
  (log/info "Crawing" (count jars) "jars...")
  (let [jar-dependency-scheme? (= "jar" dependency-scheme)
        xf (comp
             (mapcat (fn [jar-file]
                       (log/info "Crawling jar" (.getPath jar-file))
                       (let [jar (JarFile. jar-file)]
                         (->> jar
                              (.entries)
                              (enumeration-seq)
                              (remove #(.isDirectory %))
                              (map (fn [entry]
                                     [(if jar-dependency-scheme?
                                        (str "jar:file:///" jar-file "!/" (.getName entry))
                                        (str "zipfile://" jar-file "::" (.getName entry)))
                                      entry
                                      jar]))))))
             (filter (fn [[uri _ _]]
                       (or (string/ends-with? uri ".clj")
                           (string/ends-with? uri ".cljc")
                           (string/ends-with? uri ".cljs"))))
             (map (fn [[uri entry jar]]
                    (let [text (with-open [stream (.getInputStream jar entry)]
                                 (slurp stream))]
                      [uri (f.references/safe-find-references uri text false true)])))
             (remove (comp nil? second)))
        output-chan (async/chan 5)]
    (async/pipeline-blocking 5 output-chan xf (async/to-chan! jars) true (fn [e] (log/error "Error crawling jars" e)))
    (async/<!! (async/into {} output-chan))))

(defn crawl-jars-async! [jars dependency-scheme root-path project-hash classpath]
  (async/go
    (let [jar-envs (crawl-jars jars dependency-scheme)]
      (db/save-deps root-path project-hash classpath jar-envs)
      (swap! db/db update :file-envs merge jar-envs)
      (window/show-message "Jars scanned async successfully" :info))))

(defn crawl-source-dirs [dirs]
  (log/info "Crawing" (count dirs) "source dirs...")
  (let [xf (comp
             (mapcat file-seq)
             (filter #(.isFile %))
             (map file->uri)
             (filter (fn [uri]
                       (or (string/ends-with? uri ".clj")
                           (string/ends-with? uri ".cljc")
                           (string/ends-with? uri ".cljs"))))
             (map (juxt identity (fn [uri]
                                   (f.references/safe-find-references uri (slurp uri) false false))))
             (remove (comp nil? second)))
        output-chan (async/chan)]
    (async/pipeline-blocking 5 output-chan xf (async/to-chan! dirs) true (fn [e] (log/warn "Could not crawl source dirs, exception: " e)))
    (async/<!! (async/into {} output-chan))))

(defn lookup-classpath [root-path command-args env]
  (try
    (let [sep (re-pattern (System/getProperty "path.separator"))
          response (apply shell/sh (into command-args
                                         (cond-> [:dir (str root-path)]
                                           env (conj :env (merge {} (System/getenv) env)))))]
      (-> response
          (:out)
          (string/trim-newline)
          (string/split sep)))
    (catch Exception e
      (log/warn e "Error while looking up classpath info in" (str root-path) (.getMessage e))
      [])))

(defn try-project [root-path project-path command-args env]
  (let [project-file (to-file root-path project-path)]
    (when (.exists project-file)
      (let [file-hash (digest/md5 project-file)
            classpath (lookup-classpath root-path command-args env)]
        {:project-hash file-hash :classpath classpath}))))

(def ^:private default-project-specs
  [{:project-path "project.clj"
    :classpath-cmd ["lein" "classpath"]}
   {:project-path "deps.edn"
    :classpath-cmd ["clj" "-Spath"]}
   {:project-path "build.boot"
    :classpath-cmd ["boot" "show" "--fake-classpath"]}
   {:project-path "shadow-cljs.edn"
    :classpath-cmd ["shadow-cljs" "classpath"]}])

(defn get-project-from [root-path project-specs]
  (reduce
    (fn [project {:keys [project-path classpath-cmd env]}]
      (if-let [subproject (try-project root-path project-path classpath-cmd env)]
        (-> project
            (update :project-hash (fnil str "") (:project-hash subproject))
            (update :classpath (fnil into []) (:classpath subproject)))
        project))
    {}
    project-specs))

(defn get-cp-entry-type [e]
  (cond (.isFile e) :file
        (.isDirectory e) :directory
        :else :unkown))

(defn start-clj-kondo-scan! [use-cp-cache classpath]
  (if use-cp-cache
    (log/info "skipping clj-kondo classpath scan due to project hash match")
    (do
      (log/info "starting clj-kondo project classpath scan (this takes awhile)")
      (let [results (f.diagnostic/run-kondo-on-paths! classpath)]
        (log/info "clj-kondo scanned project classpath in" (str (get-in results [:summary :duration]) "ms"))))))

(defn ^:private determine-project-dependencies!
  [root-path
   {:keys [project-hash classpath]}
   {:keys [source-paths dependency-scheme
           ignore-classpath-directories async-scan-jars-on-startup?]}]
  (let [loaded (db/read-deps root-path)
        use-cp-cache (= (:project-hash loaded) project-hash)
        classpath (if use-cp-cache
                    (:classpath loaded)
                    classpath)
        classpath-entries-by-type (->> classpath
                                       reverse
                                       (map io/file)
                                       (map #(vector (get-cp-entry-type %) %))
                                       (group-by first)
                                       (reduce-kv (fn [m k v]
                                                    (assoc m k (map second v))) {}))
        jars (:file classpath-entries-by-type)
        source-paths (mapv #(to-file root-path %) source-paths)
        source-envs-chan (async/go (crawl-source-dirs source-paths))
        file-envs-chan (async/go (when-not ignore-classpath-directories
                                   (crawl-source-dirs (:directory classpath-entries-by-type))))
        jar-envs-chan (async/go
                        (cond
                          use-cp-cache
                          (:jar-envs loaded)

                          async-scan-jars-on-startup?
                          (do (crawl-jars-async! jars dependency-scheme root-path project-hash classpath)
                              {})

                          :else
                          (crawl-jars jars dependency-scheme)))
        jar-envs (async/<!! jar-envs-chan)]
    (db/save-deps root-path project-hash classpath jar-envs)
    (start-clj-kondo-scan! use-cp-cache classpath)
    (merge (async/<!! source-envs-chan)
           (async/<!! file-envs-chan)
           jar-envs)))

(defn ^:private determine-non-project-dependencies! [root-path settings]
  (let [source-paths (mapv #(to-file root-path %) (get settings :source-paths))
        kondo-source-chan (async/go (f.diagnostic/run-kondo-on-paths! source-paths))
        crawler-output-chan (async/go (crawl-source-dirs source-paths))]
    (log/info "clj-kondo scanned source paths in"
              (str (get-in (async/<!! kondo-source-chan) [:summary :duration]) "ms"))
    (async/<!! crawler-output-chan)))

(defn determine-dependencies [project-root]
  (let [root-path (shared/uri->path project-root)
        settings (:settings @db/db)
        project-specs (or (get settings :project-specs) default-project-specs)
        project (get-project-from root-path project-specs)]
    (if (some? project)
      (determine-project-dependencies! root-path project settings)
      (determine-non-project-dependencies! root-path settings))))

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
