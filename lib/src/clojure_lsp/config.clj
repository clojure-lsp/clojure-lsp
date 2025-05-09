(ns clojure-lsp.config
  (:require
   [clojure-lsp.logger :as logger]
   [clojure-lsp.shared :as shared]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [medley.core :as medley])
  (:import
   [java.io File]
   (java.util.jar JarFile JarFile$JarFileEntry)))

(set! *warn-on-reflection* true)

(def default-source-path-ignore-regexs ["target.*"])

(defn read-edn-file [^java.io.File file]
  (try
    (->> (slurp file)
         (edn/read-string {:readers {'re re-pattern}})
         shared/keywordize-first-depth)
    (catch Exception e
      (logger/error "WARNING: error while reading" (.getCanonicalPath file) (format "(%s)" (.getMessage e))))))

(defn get-property [p]
  (System/getProperty p))

(defn get-env [p]
  (System/getenv p))

(defn global-config-file []
  (let [xdg-config-home (or (get-env "XDG_CONFIG_HOME")
                            (io/file (get-property "user.home") ".config"))
        xdg-config (io/file xdg-config-home "clojure-lsp")
        home-config (io/file (get-property "user.home") ".lsp")
        path (if (shared/file-exists? xdg-config)
               xdg-config
               home-config)]
    (io/file path  "config.edn")))

(defn global-cache-dir []
  (let [cache-home (or (get-env "XDG_CACHE_HOME")
                       (io/file (get-property "user.home") ".cache"))]
    (io/file cache-home "clojure-lsp")))

(defn local-cache-dir [db]
  (let [project-root (shared/uri->path (:project-root-uri db))
        overwritten-path (some-> (get db [:settings :cache-path])
                                 io/file)
        default (io/file (str project-root) ".lsp" ".cache")]
    ^java.io.File (or overwritten-path default)))

(defn local-project-config-file ^File [project-root-uri]
  (let [project-root-path (io/file (shared/uri->filename project-root-uri))]
    (io/file project-root-path ".lsp" "config.edn")))

(defn ^:private resolve-global-config [^java.io.File global-lsp-config-file]
  (when (shared/file-exists? global-lsp-config-file)
    (read-edn-file global-lsp-config-file)))

(defn ^:private resolve-project-configs [project-root-uri ^java.io.File global-lsp-config-file]
  (loop [dir (io/file (shared/uri->filename project-root-uri))
         configs []]
    (let [file (local-project-config-file project-root-uri)
          parent (.getParentFile dir)]
      (if parent
        (recur parent (cond->> configs
                        (and (shared/file-exists? file)
                             (not (= (.getAbsolutePath global-lsp-config-file) (.getAbsolutePath file))))
                        (concat [(read-edn-file file)])))
        configs))))

(defn resolve-for-root [project-root-uri]
  (when project-root-uri
    (let [global-lsp-config-file (global-config-file)
          project-configs (resolve-project-configs project-root-uri global-lsp-config-file)]
      (reduce shared/deep-merge
              (merge {}
                     (resolve-global-config global-lsp-config-file))
              project-configs))))

(defn ^:private jar-file->config
  [^java.io.File file config-paths]
  (with-open [jar (JarFile. file)]
    (->> (enumeration-seq (.entries jar))
         (filter (fn [^JarFile$JarFileEntry entry]
                   (let [entry-name (.getName entry)]
                     (and (string/starts-with? entry-name "clojure-lsp.exports")
                          (string/includes? entry-name "config.edn")))))
         (mapv (fn [^JarFile$JarFileEntry config-entry]
                 (let [[_ group artifact] (string/split (.getName config-entry) #"/")]
                   (when (some #(and (string/starts-with? % group)
                                     (string/ends-with? % artifact)) config-paths)
                     (logger/info "[classpath-config]" (format "Found config for '%s/%s' in classpath" group artifact))
                     (edn/read-string {:readers {'re re-pattern}}
                                      (slurp (.getInputStream jar config-entry))))))))))

(defn deep-merge-considering-settings
  "Deep merge a with b checking for specific keys related to
  clojure-lsp settings to avoid wrong duplicated values."
  ([a b]
   (let [deep-merged (shared/deep-merge a b)]
     (cond
       (-> deep-merged :cljfmt :indents)
       (let [cljfmt-a (-> a :cljfmt :indents)
             cljfmt-b (-> b :cljfmt :indents)]
         (-> deep-merged
             (update-in [:cljfmt :indents] merge cljfmt-a cljfmt-b)))

       (seq (:paths-ignore-regex deep-merged))
       (update deep-merged :paths-ignore-regex distinct)

       (coll? (:project-specs deep-merged))
       (assoc deep-merged :project-specs
              (medley/distinct-by :project-path (:project-specs deep-merged)))

       :else
       deep-merged)))
  ([a b & more]
   (reduce deep-merge-considering-settings (or a {}) (cons b more))))

(defn ^:private resolve-from-classpath-config-paths-impl [classpath {:keys [classpath-config-paths]}]
  (when-let [cp-config-paths (and (coll? classpath-config-paths)
                                  (seq classpath-config-paths))]
    (when-let [jar-files (->> classpath
                              (filter #(string/ends-with? % ".jar"))
                              distinct
                              (map io/file)
                              (filter shared/file-exists?))]
      (when-let [configs (->> jar-files
                              (map #(jar-file->config % cp-config-paths))
                              flatten
                              (remove nil?)
                              seq)]
        (reduce deep-merge-considering-settings configs)))))

(defn classpath-config-paths? [{:keys [classpath-config-paths]}]
  (and (coll? classpath-config-paths)
       (seq classpath-config-paths)))

(defn resolve-from-classpath-config-paths [classpath settings]
  (shared/logging-task
    :internal/find-classpath-configs
    (loop [{:keys [classpath-config-paths] :as cp-settings} (resolve-from-classpath-config-paths-impl classpath settings)
           merge-config nil]
      (if (and (coll? classpath-config-paths)
               (seq classpath-config-paths))
        (recur (resolve-from-classpath-config-paths-impl classpath cp-settings)
               cp-settings)
        (shared/deep-merge merge-config cp-settings)))))
