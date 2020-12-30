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

(defn ^:private diagnose-unused-references [uri declared-references all-envs excluded-unused-ns-declarations]
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
          :when (and (not= code :unused-param)
                     (or (not= :unused-ns code)
                         (not-any? #(string/index-of uri %)
                                   (set/union #{"test/"} excluded-unused-ns-declarations))))]
      {:range (shared/->range usage)
       :code code
       :message (case code
                  :unused-ns (str "Unused namespace: " (:str usage))
                  (str "Unused declaration: " (:str usage)))
       :severity 2})))

(defn ^:private process-unused
  [usages declarations]
  (let [ensure-sym (fn [s] (when-not (string? s) s))]
    (->> usages
         (remove (comp #(or (contains? % :declare)
                            (contains? % :refer)) :tags))
         (map #(some-> % :sym ensure-sym namespace symbol))
         set
         (set/difference (set (map :ns declarations))))))

(defn ^:private usages->declarations [usages]
  (->> usages
       (filter (comp #(and (or (contains? % :declare)
                               (contains? % :refer))
                           (not (contains? % :factory))
                           (not (contains? % :unused))) :tags))
       (remove (comp #(string/starts-with? % "_") name :sym))))

(defn ^:private diagnose-unused [uri usages excluded-unused-ns-declarations]
  (let [all-envs (assoc (:file-envs @db/db) uri usages)
        declarations (usages->declarations usages)
        declared-references (remove (comp #(contains? % :alias) :tags) declarations)]
    (concat (diagnose-unused-references uri declared-references all-envs excluded-unused-ns-declarations))))

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

(defn- kondo-args [extra]
  (let [root-path (uri->path (:project-root @db/db))
        user-config (get-in @db/db [:settings :clj-kondo])
        kondo-dir (.resolve root-path ".clj-kondo")]
    (cond-> {:cache true
             :cache-dir ".clj-kondo/cache"}
      (.exists (.toFile kondo-dir))
      (assoc :cache-dir (str (.resolve kondo-dir ".cache")) :config-dir (str kondo-dir))

      :always
      (merge extra)

      user-config
      (update-in [:config] merge user-config))))

(defn- run-kondo-on-paths! [paths]
  (kondo/run! (kondo-args {:lint [(string/join (System/getProperty "path.separator") paths)]})))

(defn- run-kondo-on-text! [text lang]
  (with-in-str text (kondo/run! (kondo-args {:lint ["-"] :lang lang}))))

(defn- kondo-find-diagnostics [uri text]
  (let [file-type (shared/uri->file-type uri)
        {:keys [findings]} (run-kondo-on-text! text file-type)]
    (->> findings
         (filter #(= "<stdin>" (:filename %)))
         (map kondo-finding->diagnostic))))

(defn find-diagnostics [uri text usages excluded-unused-ns-declarations]
  (let [kondo-diagnostics (kondo-find-diagnostics uri text)
        unused (diagnose-unused uri usages excluded-unused-ns-declarations)
        unknown-forwards (diagnose-unknown-forward-declarations usages)
        result (concat unused unknown-forwards kondo-diagnostics)]
    result))

(defn safe-find-references
  ([uri text]
   (safe-find-references uri text true false))
  ([uri text diagnose? remove-private?]
   (try
     (let [file-type (shared/uri->file-type uri)
           macro-defs (get-in @db/db [:settings :macro-defs])
           excluded-unused-ns-declarations (get-in @db/db [:settings :linters :unused-namespace-declarations] #{})
           references (cond->> (parser/find-usages uri text file-type macro-defs)
                        remove-private? (filter (fn [{:keys [tags]}] (and (:public tags) (:declare tags)))))]
       (when diagnose?
         (async/put! db/diagnostics-chan
                     {:uri uri
                      :diagnostics (find-diagnostics uri text references excluded-unused-ns-declarations)}))
       references)
     (catch Throwable e
       (log/warn e "Cannot parse: " uri (.getMessage e))
       ;; On purpose
       nil))))

(defn find-unused-aliases [uri]
  (let [usages (safe-find-references uri (slurp uri) false false)
        declarations (usages->declarations usages)
        excludes (-> (get-in @db/db [:settings :linters :unused-namespace :exclude] #{}) set)
        declared-aliases (->> declarations
                              (filter (comp #(contains? % :alias) :tags))
                              (remove (comp excludes :ns)))]
    (process-unused usages declared-aliases)))

(defn find-unused-refers [uri]
  (let [usages (safe-find-references uri (slurp uri) false false)
        declarations (usages->declarations usages)
        excludes (-> (get-in @db/db [:settings :linters :unused-namespace :exclude] #{}) set)
        declared-refers (->> declarations
                              (filter (comp #(contains? % :refer) :tags))
                              (remove (comp excludes :ns)))]
    (process-unused usages declared-refers)))

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
        (if use-cp-cache
          (log/info "skipping classpath scan due to project hash match")
          (do
            (log/info "starting clj-kondo project classpath scan (this takes awhile)")
            (let [results (run-kondo-on-paths! classpath)]
              (log/info "clj-kondo scanned project classpath in" (str (get-in results [:summary :duration]) "ms")))))
        (merge source-envs file-envs jar-envs))
      (let [kondo-source-chan (async/go (run-kondo-on-paths! source-paths))
            crawler-output-chan (async/go (crawl-source-dirs source-paths))]
        (log/info "clj-kondo scanned source paths in"
                  (str (get-in (async/<!! kondo-source-chan) [:summary :duration]) "ms"))
        (async/<!! crawler-output-chan)))))

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
