(ns clojure-lsp.config
  (:require
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.shared :as shared]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
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
    (->
      (assoc-in [:config :linters :unresolved-symbol :report-duplicates] true)
      (assoc-in [:config :linters :unresolved-namespace :report-duplicates] true)
      (assoc-in [:config :linters :unresolved-var :report-duplicates] true))))

(defn kondo-for-paths [paths settings external-analysis-only?]
  (-> {:cache true
       :parallel true
       :copy-configs true
       :lint [(string/join (System/getProperty "path.separator") paths)]
       :config {:output {:analysis {:arglists true
                                    :locals false
                                    :keywords true}
                         :canonical-paths true}}}
      (shared/assoc-some :custom-lint-fn (when-not external-analysis-only?
                                           (partial f.diagnostic/post-project-lint! paths)))
      (with-additional-config settings)))

(defn kondo-for-single-file [uri settings]
  (-> {:cache true
       :lint ["-"]
       :copy-configs true
       :lang (shared/uri->file-type uri)
       :filename (shared/uri->filename uri)
       :custom-lint-fn f.diagnostic/unused-public-var-lint-for-single-file!
       :config {:output {:analysis {:arglists true
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
