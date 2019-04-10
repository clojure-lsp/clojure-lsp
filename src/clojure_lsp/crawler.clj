(ns clojure-lsp.crawler
  (:require
    [clojure.string :as string]
    [clojure.core.async :as async]
    [clojure.tools.logging :as log]
    [clojure.java.shell :as shell]
    [digest :as digest]
    [clojure-lsp.db :as db]
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.shared :as shared])
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
                problem (if known-alias?
                          :require
                          :unknown)]]
      {:range (shared/->range usage)
       :code problem
       :message (case problem
                  :unknown "Unknown symbol"
                  :require "Needs Require")
       :severity 1})))

(defn ^:private diagnose-unused-references [declared-references all-envs]
  (let [references (->> all-envs
                        (mapcat (comp val))
                        (remove (comp #(contains? % :declare) :tags))
                        (map :sym)
                        set)
        unused-syms (set/difference (set (map :sym declared-references)) references)]
    (for [usage (filter (comp unused-syms :sym) declared-references)]
      {:range (shared/->range usage)
       :code :unused
       :message (str "Unused declaration: " (:str usage))
       :severity 1})))

(defn ^:private diagnose-unused-aliases [declared-aliases usages]
  (let [references (->> usages
                        (remove (comp #(contains? % :declare) :tags))
                        (map #(some-> % :sym namespace symbol))
                        set)
        unused-aliases (set/difference (set (map :ns declared-aliases)) references)]
    (for [usage (filter (comp unused-aliases :ns) declared-aliases)]
      {:range (shared/->range usage)
       :code :unused
       :message (str "Unused alias: " (:str usage))
       :severity 1})))

(defn ^:private diagnose-unused [uri usages]
  (let [all-envs (assoc (:file-envs @db/db) uri usages)
        declarations (->> usages
                          (filter (comp #(and (contains? % :declare) (not (contains? % :unused))) :tags))
                          (remove (comp #(string/starts-with? % "_") name :sym)))
        declared-references (remove (comp #(contains? % :alias) :tags) declarations)
        declared-aliases (filter (comp #(contains? % :alias) :tags) declarations)]
    (concat (diagnose-unused-aliases declared-aliases usages)
            (diagnose-unused-references declared-references all-envs))))

(defn find-diagnostics [project-aliases uri usages]
  (let [unknown (diagnose-unknown project-aliases usages)
        unused (diagnose-unused uri usages)
        result (concat unknown unused)]
    result))

(defn safe-find-references
  ([uri text]
   (safe-find-references uri text true false))
  ([uri text diagnose? remove-private?]
   (try
     #_(log/warn "trying" uri (get-in @db/db [:documents uri :v]))
     (let [file-type (shared/uri->file-type uri)
           macro-defs (get-in @db/db [:client-settings "macro-defs"])
           references (cond->> (parser/find-usages text file-type macro-defs)
                        remove-private? (filter (fn [{:keys [tags]}] (and (:public tags) (:declare tags)))))]
       (when diagnose?
         (async/put! db/diagnostics-chan
                     {:uri uri
                      :diagnostics (find-diagnostics (:project-aliases @db/db) uri references)}))
       references)
     (catch Throwable e
       (log/warn "Cannot parse: " uri (.getMessage e))
       ;; On purpose
       nil))))

(defn crawl-jars [jars]
  (let [xf (comp
             (mapcat (fn [jar-file]
                       (let [jar (JarFile. jar-file)]
                         (->> jar
                              (.entries)
                              (enumeration-seq)
                              (remove #(.isDirectory %))
                              (map (fn [entry]
                                     [(str "zipfile://" jar-file "::" (.getName entry))
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

(defn lookup-classpath [root-path command-args]
  (try
    (let [sep (re-pattern (System/getProperty "path.separator"))]
      (-> (apply shell/sh (into command-args [:dir (str root-path)]))
          (:out)
          (string/trim-newline)
          (string/split sep)))
    (catch Exception e
      (log/warn e "Error while looking up classpath info in" (str root-path) (.getMessage e))
      [])))

(defn try-project [root-path project-path command-args]
  (let [project-file (to-file root-path project-path)]
    (when (.exists project-file)
      (let [file-hash (digest/md5 project-file)
            classpath (lookup-classpath root-path command-args)]
        {:project-hash file-hash :classpath classpath}))))

(def project-specs
  [{:project-path "project.clj"
    :classpath-cmd ["lein" "classpath"]}
   {:project-path "build.boot"
    :classpath-cmd ["boot" "show" "--fake-classpath"]}])

(defn get-project-from [root-path]
  (reduce
    (fn [project {:keys [project-path classpath-cmd]}]
      (if-let [subproject (try-project root-path project-path classpath-cmd)]
        (-> project
            (update :project-hash (fnil str "") (:project-hash subproject))
            (update :classpath (fnil into []) (:classpath subproject)))
        project))
    {}
    project-specs))

(defn determine-dependencies [project-root]
  (let [root-path (uri->path project-root)
        source-paths (mapv #(to-file root-path %)
                           (get-in @db/db [:client-settings "source-paths"]))
        project (get-project-from root-path)]
    (if (some? project)
      (let [project-hash (:project-hash project)
            loaded (db/read-deps root-path)
            use-cp-cache (= (:project-hash loaded) project-hash)
            classpath (if use-cp-cache
                        (:classpath loaded)
                        (:classpath project))
            jars (filter #(.isFile %) (map io/file (reverse classpath)))
            jar-envs (if use-cp-cache
                       (:jar-envs loaded)
                       (crawl-jars jars))
            file-envs (crawl-source-dirs source-paths)]
        (db/save-deps root-path project-hash classpath jar-envs)
        (merge file-envs jar-envs))
      (crawl-source-dirs source-paths))))

