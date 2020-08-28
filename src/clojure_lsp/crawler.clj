(ns clojure-lsp.crawler
  (:require
    [clj-kondo.core :as kondo]
    [clojure-lsp.db :as db]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.shared :as shared]
    [clojure.core.async :as async]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.java.shell :as shell]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [digest :as digest])
  (:import
    [java.net URI]
    [java.util.jar JarFile]
    [java.nio.file Paths]))

(defn- file->uri [file]
  (str (.toUri (.toPath file))))

(defn- to-file [path child]
  (.toFile (.resolve path child)))

(defn- uri->path [uri]
  (Paths/get (URI. uri)))

(defn ^:private process-unused-aliases
  [usages declared-aliases]
  (let [ensure-sym (fn [s] (when-not (string? s) s))]
    (->> usages
         (remove (comp #(contains? % :declare) :tags))
         (map #(some-> % :sym ensure-sym namespace symbol))
         set
         (set/difference (set (map :ns declared-aliases))))))

(defn ^:private usages->declarations [usages]
  (->> usages
       (filter (comp #(and (contains? % :declare)
                           (not (contains? % :factory))
                           (not (contains? % :unused))) :tags))
       (remove (comp #(string/starts-with? % "_") name :sym))))

(defn- kondo-finding->diagnostic [{:keys [type message level row col] :as finding}]
  (let [expression? (not= row (:end-row finding))
        finding (cond-> (merge {:end-row row :end-col col} finding)
                  expression? (assoc :end-row row :end-col col))]
    {:range (shared/->range finding)
     :message message
     :code (name type)
     :severity (case level
                 :error   1
                 :warning 2
                 :info    3)
     :source "clj-kondo"}))

(defn index-by
  [f coll]
  (persistent! (reduce #(assoc! %1 (f %2) %2) (transient {}) coll)))

(defn kondo-ignore? [api-namespaces {:keys [:ns :export :defined-by :test :private :name]}]
  (or
   test
   export
   (when (contains? api-namespaces ns)
     (not private))
   (.startsWith (str name) "-")
   (= 'clojure.core/deftype defined-by)
   (= 'clojure.core/defrecord defined-by)
   (= 'clojure.core/defprotocol defined-by)
   (= 'clojure.core/definterface defined-by)))

(defn ^:private kondo-unused-diagnostics
  [uri {:keys [var-definitions var-usages]}]
  (let [other-analysis (dissoc (:uri->analysis @db/db) uri)
        var-usages (into (mapcat (comp :var-usages val) other-analysis) var-usages)
        api-namespaces #{}
        definitions-by-ns+name (index-by (juxt :ns :name) var-definitions)
        defined-vars (set (map (juxt :ns :name) var-definitions))
        used-vars (set (map (juxt :to :name) var-usages))
        unused-vars (set/difference (set defined-vars) used-vars)
        unused-vars-data (map definitions-by-ns+name unused-vars)
        unused-vars-data (remove #(kondo-ignore? api-namespaces %) unused-vars-data)
        results (into [] unused-vars-data)]
    (map
      (fn [{:keys [row col ns name] :as result}]
        (let [expression? (not= row (:end-row result))
              result-range (shared/->range (cond-> (merge {:end-row row :end-col col} result)
                                             expression? (assoc :end-row row :end-col col)))]
          {:range result-range
           :message "Unused"
           :source "clj-kondo"
           :severity 2}))
      results)))

(defn- kondo-args [extra]
  (let [root-path (uri->path (:project-root @db/db))
        user-config (get-in @db/db [:settings :clj-kondo])
        kondo-dir (.resolve root-path ".clj-kondo")]
    (cond-> {:cache true
             :cache-dir ".clj-kondo/.cache"}
      (.exists (.toFile kondo-dir))
      (assoc :cache-dir (str (.resolve kondo-dir ".cache")) :config-dir (str kondo-dir))

      :always
      (merge extra)

      user-config
      (update-in [:config] merge user-config))))

(comment
  (let [ana (get (get-in @db/db [:uri->analysis]) "file:///Users/case/dev/carve/src/bar.clj")]
    (count (:var-usages ana))))

(defn- run-kondo-on-text! [uri text]
  (with-in-str text (kondo/run! (kondo-args {:lint ["-"] :lang (shared/uri->file-type uri) :config {:output {:analysis true}}}))))

(defn- kondo-find-diagnostics [{:keys [findings]}]
  (->> findings
       (filter #(= "<stdin>" (:filename %)))
       (map kondo-finding->diagnostic)))

(defn find-diagnostics [uri kondo-results]
  (let [kondo-diagnostics (kondo-find-diagnostics kondo-results)
        unused (kondo-unused-diagnostics uri (:analysis kondo-results))
        result (concat unused kondo-diagnostics)]
    result))

(defn safe-find-references
  ([uri text]
    (safe-find-references uri text true false))
  ([uri text diagnose? remove-private?]
    (try
      (let [result (run-kondo-on-text! uri text)]
        (when diagnose?
          (async/put! db/diagnostics-chan
                      {:uri uri
                       :diagnostics (find-diagnostics uri result)}))
        (cond->> (:analysis result)
          remove-private? (remove :private)))
      (catch Throwable e
        (log/warn e "Cannot parse: " uri (.getMessage e))
       ;; On purpose
        nil))))

(defn crawl-jars [jars dependency-scheme]
  (let [xf (comp
             (mapcat (fn [jar-file]
                       (let [jar (JarFile. jar-file)]
                         (->> jar
                              (.entries)
                              (enumeration-seq)
                              (remove #(.isDirectory %))
                              (map (fn [entry]
                                     [(if (= "jar" dependency-scheme)
                                        (str "jar:file://" jar-file "!/" (.getName entry))
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
                      [uri (safe-find-references uri text false true)])))
             (remove (comp nil? second)))
        output-chan (async/chan 5)]
    (async/pipeline-blocking 5 output-chan xf (async/to-chan jars) true (fn [e] (log/warn e "hello")))
    (async/<!! (async/into {} output-chan))))

(defn crawl-source-dirs [dirs]
  (let [xf (comp
            (mapcat file-seq)
            (filter #(.isFile %))
            (map file->uri)
            (filter (fn [uri]
                      (or (string/ends-with? uri ".clj")
                          (string/ends-with? uri ".cljc")
                          (string/ends-with? uri ".cljs"))))
            (map (juxt identity (fn [uri]
                                  (safe-find-references uri (slurp uri) false false))))
            (remove (comp nil? second)))
        output-chan (async/chan)]
    (async/pipeline-blocking 5 output-chan xf (async/to-chan dirs) true (fn [e] (log/warn e "hello")))
    (async/<!! (async/into {} output-chan))))

(defn find-unused-aliases [uri]
  (let [usages (safe-find-references uri (slurp uri) false false)
        declarations (usages->declarations usages)
        excludes (-> (get-in @db/db [:settings :linters :unused-namespace :exclude] #{}) set)
        declared-aliases (->> declarations
                              (filter (comp #(contains? % :alias) :tags))
                              (remove (comp excludes :ns)))]
    (process-unused-aliases usages declared-aliases)))

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

(defn determine-dependencies [project-root]
  (let [root-path (uri->path project-root)
        settings (:settings @db/db)
        source-paths (mapv #(to-file root-path %) (get settings :source-paths))
        dependency-scheme (get settings :dependency-scheme)
        ignore-directories? (get settings :ignore-classpath-directories)
        project-specs (or (get settings :project-specs) default-project-specs)
        project (get-project-from root-path project-specs)]
    (if (some? project)
      (let [project-hash (:project-hash project)
            loaded (db/read-deps root-path)
            use-cp-cache (= (:project-hash loaded) project-hash)
            classpath (if use-cp-cache
                        (:classpath loaded)
                        (:classpath project))
            classpath-entries-by-type (->> classpath
                                           reverse
                                           (map io/file)
                                           (map #(vector (get-cp-entry-type %) %))
                                           (group-by first)
                                           (reduce-kv (fn [m k v]
                                                        (assoc m k (map second v))) {}))
            jars (:file classpath-entries-by-type)
            jar-envs (if use-cp-cache
                       (:jar-envs loaded)
                       (crawl-jars jars dependency-scheme))
            source-envs (crawl-source-dirs source-paths)
            file-envs (when-not ignore-directories? (crawl-source-dirs (:directory classpath-entries-by-type)))]
        (db/save-deps root-path project-hash classpath jar-envs)
        (merge source-envs file-envs jar-envs))
      (crawl-source-dirs source-paths))))

(defn find-raw-project-settings [project-root]
  (let [config-path (Paths/get ".lsp" (into-array ["config.edn"]))]
    (loop [dir (uri->path project-root)]
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
