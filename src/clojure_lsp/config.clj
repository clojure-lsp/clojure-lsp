(ns clojure-lsp.config
  (:require
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.shared :as shared]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [taoensso.timbre :as log]))

(def diagnostics-debounce-ms 100)
(def change-debounce-ms 300)

(def clojure-lsp-version (string/trim (slurp (io/resource "CLOJURE_LSP_VERSION"))))

(def clj-kondo-version (string/trim (slurp (io/resource "CLJ_KONDO_VERSION"))))

(defn ^:private with-additional-config
  [config settings]
  (cond-> config
    (get-in settings [:linters :clj-kondo :report-duplicates] true)
    (assoc-in [:config :linters] {:unresolved-symbol {:report-duplicates true}
                                  :unresolved-namespace {:report-duplicates true}
                                  :unresolved-var {:report-duplicates true}})))

(defn kondo-for-paths [paths settings]
  (-> {:cache true
       :parallel true
       :copy-configs true
       :lint [(string/join (System/getProperty "path.separator") paths)]
       :custom-lint-fn f.diagnostic/unused-public-var-lint-from-kondo!
       :config {:linters {:clojure-lsp/unused-public-var {:level :info}}
                :output {:analysis {:arglists true
                                    :locals false
                                    :keywords true}
                         :canonical-paths true}}}
      (with-additional-config settings)))

(defn kondo-for-single-file [uri settings]
  (-> {:cache true
       :lint ["-"]
       :copy-configs true
       :lang (shared/uri->file-type uri)
       :filename (shared/uri->filename uri)
       :custom-lint-fn f.diagnostic/unused-public-var-lint-from-kondo!
       :config {:linters {:clojure-lsp/unused-public-var {:level :info}}
                :output {:analysis {:arglists true
                                    :locals true
                                    :keywords true}
                         :canonical-paths true}}}
      (with-additional-config settings)))

(defn read-edn-file [^java.io.File file]
  (try
    (->> (slurp file)
         (edn/read-string {:readers {'re re-pattern}})
         shared/keywordize-first-depth)
    (catch Exception e
      (log/error "WARNING: error while reading" (.getCanonicalPath file) (format "(%s)" (.getMessage e))))))

(defn get-property [p]
  (System/getProperty p))

(defn get-env [p]
  (System/getenv p))

(defn ^:private file-exists? [^java.io.File f]
  (.exists f))

(defn ^:private get-home-config-file []
  (let [xdg-config-home (or (get-env "XDG_CONFIG_HOME")
                            (io/file (get-property "user.home") ".config"))
        xdg-config (io/file xdg-config-home ".lsp" "config.edn")
        home-config (io/file (get-property "user.home") ".lsp" "config.edn")]
    (if (file-exists? xdg-config)
      xdg-config
      home-config)))

(defn ^:private resolve-home-config [^java.io.File home-dir-file]
  (when (file-exists? home-dir-file)
    (read-edn-file home-dir-file)))

(defn ^:private resolve-project-configs [project-root-uri ^java.io.File home-dir-file]
  (loop [dir (io/file (shared/uri->filename project-root-uri))
         configs []]
    (let [file (io/file dir ".lsp" "config.edn")
          parent (.getParentFile dir)]
      (if parent
        (recur parent (cond-> configs
                        (and (file-exists? file)
                             (not (= (.getAbsolutePath home-dir-file) (.getAbsolutePath file))))
                        (conj (read-edn-file file))))
        configs))))

(defn resolve-config [project-root-uri]
  (let [home-dir-file (get-home-config-file)]
    (reduce shared/deep-merge
            (merge {}
                   (resolve-home-config home-dir-file))
            (resolve-project-configs project-root-uri home-dir-file))))

(defn ^:private extract-source-paths [paths extra-paths aliases]
  (->> (concat paths extra-paths)
       (map (fn [path]
              (if (keyword? path)
                (when (or (vector? (get aliases path))
                          (set? (get aliases path)))
                  (get aliases path))
                path)))
       flatten
       (remove nil?)
       set))

(def default-source-aliases #{:dev :test})

(defn resolve-deps-source-paths
  [{:keys [paths extra-paths aliases]}
   settings]
  (let [source-aliases (or (:source-aliases settings) default-source-aliases)
        root-source-paths (extract-source-paths paths extra-paths aliases)]
    (->> source-aliases
         (map #(get aliases % nil))
         (mapcat #(extract-source-paths (:paths %) (:extra-paths %) nil))
         (remove nil?)
         set
         (set/union root-source-paths))))

(def default-lein-source-paths ["src" "src/main/clojure"])
(def default-lein-test-paths ["test" "src/test/clojure"])

(defn ^:private valid-path-config? [config]
  (and config
       (or (vector? config)
           (and (list? config)
                (string? (first config)))
           (set? config))))

(defn resolve-lein-source-paths
  [{:keys [source-paths test-paths profiles] :as project}
   settings]
  (when project
    (let [source-aliases (or (:source-aliases settings) default-source-aliases)
          source-paths (if (valid-path-config? source-paths)
                         source-paths
                         default-lein-source-paths)
          test-paths (if (valid-path-config? test-paths)
                       test-paths
                       default-lein-test-paths)
          root-source-paths (extract-source-paths source-paths test-paths profiles)]
      (->> source-aliases
           (map #(get profiles % nil))
           (mapcat #(extract-source-paths (:source-paths %) (:test-paths %) nil))
           (remove nil?)
           set
           (set/union root-source-paths)))))
