(ns clojure-lsp.crawler
  (:require
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

(defn ^:private diagnose-unknown [project-aliases usages]
  (let [unknown-usages (seq (filter (fn [usage] (contains? (:tags usage) :unknown))
                                    usages))
        aliases (set/map-invert project-aliases)]
    (for [usage unknown-usages
          :let [known-alias? (some-> (:unkown-ns usage)
                                     aliases)
                problem (cond
                          known-alias?
                          :require

                          (:unknown-ns usage)
                          :unknown-ns

                          :else
                          :unknown)]]
      {:range (shared/->range usage)
       :code problem
       :message (case problem
                  :unknown (str "Unknown symbol: " (:str usage))
                  :unknown-ns (str "Unknown namespace: " (:unknown-ns usage))
                  :require "Needs Require")
       :severity 1})))

(defn ^:private diagnose-unknown-forward-declarations [usages]
  (let [forward-usages (seq (filter (fn [usage] (contains? (:tags usage) :forward))
                                    usages))
        forward-syms (set (map :sym forward-usages))
        found-declarations (set (keep (fn [usage]
                                        (when (and (contains? forward-syms (:sym usage))
                                                   (contains? (:tags usage) :declare))
                                          (:sym usage)))
                                      usages))
        unknown-forwards (filter (fn [usage]
                                   (not (contains? found-declarations (:sym usage))))
                                 forward-usages)]
    (for [usage unknown-forwards]
      {:range (shared/->range usage)
       :code :unknown
       :message (str "Unknown forward declaration: " (:str usage))
       :severity 1})))

(defn ^:private supports-argc [signature argc]
  (let [min-argc (count (take-while #(not= '& %) signature))
        has-rest (not= min-argc (count signature))]
    (if has-rest
      (>= argc min-argc)
      (= argc min-argc))))

(defn ^:private diagnose-wrong-arity [uri usages]
  (let [all-envs (assoc (:file-envs @db/db) uri usages)
        function-references (filter :signatures usages)
        call-sites (->> all-envs
                        (mapcat (comp val))
                        (filter :argc))]
    (for [call-site call-sites
          :let [argc (:argc call-site)
                function-sym (:sym call-site)
                relevant-functions (filter #(->> % (:sym) (= function-sym)) function-references)]
          :when (when-let [relevant-function (last relevant-functions)]
                  (not-any? #(supports-argc % argc) (:signatures relevant-function)))]
      {:range (shared/->range call-site)
       :code :wrong-arity
       :message (let [plural (not= argc 1)]
                  (format "No overload %s for %d argument%s"
                          (:str call-site) argc (if plural "s" "")))
       :severity 1})))

(defn ^:private diagnose-unused-references [uri declared-references all-envs]
  (let [references (->> all-envs
                        (mapcat (comp val))
                        (remove (comp #(contains? % :declare) :tags))
                        (remove (comp #(contains? % :forward) :tags))
                        (map :sym)
                        set)
        unused-syms (set/difference (set (map :sym declared-references)) references)]
    (for [usage (filter (comp unused-syms :sym) declared-references)
          :let [code (condp set/subset? (:tags usage)
                       #{:param} :unused-param
                       #{:ns} :unused-ns
                       #{:public} :unused-public
                       :unused)]
          :when (or (not= :unused-ns code)
                    (not (string/index-of uri "test/")))]
      {:range (shared/->range usage)
       :code code
       :message (case code
                  :unused-ns (str "Unused namespace: " (:str usage))
                  :unused-param (str "Unused parameter: " (:str usage))
                  (str "Unused declaration: " (:str usage)))
       :severity 2})))

(defn ^:private diagnose-unused-aliases [_uri declared-aliases usages]
  (let [references (->> usages
                        (remove (comp #(contains? % :declare) :tags))
                        (map #(some-> % :sym namespace symbol))
                        set)
        unused-aliases (set/difference (set (map :ns declared-aliases)) references)]
    (for [usage (filter (comp unused-aliases :ns) declared-aliases)]
      {:range (shared/->range usage)
       :code :unused-alias
       :message (str "Unused alias: " (:str usage))
       :severity 2})))

(defn ^:private diagnose-unused [uri usages]
  (let [all-envs (assoc (:file-envs @db/db) uri usages)
        declarations (->> usages
                          (filter (comp #(and (contains? % :declare)
                                              (not (contains? % :factory))
                                              (not (contains? % :unused))) :tags))
                          (remove (comp #(string/starts-with? % "_") name :sym)))
        declared-references (remove (comp #(contains? % :alias) :tags) declarations)
        declared-aliases (filter (comp #(contains? % :alias) :tags) declarations)]
    (concat (diagnose-unused-aliases uri declared-aliases usages)
            (diagnose-unused-references uri declared-references all-envs))))

(defn find-diagnostics [project-aliases uri usages]
  (let [unknown (diagnose-unknown project-aliases usages)
        unused (diagnose-unused uri usages)
        unknown-forwards (diagnose-unknown-forward-declarations usages)
        wrong-arity (diagnose-wrong-arity uri usages)
        result (concat unknown unused unknown-forwards wrong-arity)]
    result))

(defn safe-find-references
  ([uri text]
   (safe-find-references uri text true false))
  ([uri text diagnose? remove-private?]
   (try
     #_(log/warn "trying" uri (get-in @db/db [:documents uri :v]))
     (let [file-type (shared/uri->file-type uri)
           macro-defs (get-in @db/db [:settings "macro-defs"])
           references (cond->> (parser/find-usages uri text file-type macro-defs)
                        remove-private? (filter (fn [{:keys [tags]}] (and (:public tags) (:declare tags)))))]
       (when diagnose?
         (async/put! db/diagnostics-chan
                     {:uri uri
                      :diagnostics (find-diagnostics (:project-aliases @db/db) uri references)}))
       references)
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
    :classpath-cmd ["boot" "show" "--fake-classpath"]}])

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
        source-paths (mapv #(to-file root-path %) (get settings "source-paths"))
        dependency-scheme (get settings "dependency-scheme")
        ignore-directories? (get settings "ignore-classpath-directories")
        project-specs (or (get settings "project-specs") default-project-specs)
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

(defn find-project-settings [project-root]
  (let [config-path (Paths/get ".lsp" (into-array ["config.edn"]))]
    (loop [dir (uri->path project-root)]
      (let [full-config-path (.resolve dir config-path)
            file (.toFile full-config-path)
            parent-dir (.getParent dir)]
        (cond
          (.exists file)
          (edn/read-string {:readers {'re re-pattern}} (slurp file))

          parent-dir
          (recur parent-dir)

          :else
          {})))))
