(ns clojure-lsp.kondo
  (:require
   [babashka.fs :as fs]
   [clj-kondo.core :as kondo]
   [clojure-lsp.config :as config]
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [medley.core :as medley]))

(set! *warn-on-reflection* true)

(defn clj-kondo-version []
  (string/trim (slurp (io/resource "CLJ_KONDO_VERSION"))))

(def clj-kondo-analysis-batch-size 150)

(defn ^:private project-config-dir [project-root-uri]
  ;; TODO: might be better to use clj-kondo.impl.core/config-dir, but it's not
  ;; yet part of the public kondo api.
  #_(kondo/config-dir (shared/uri->filename project-root-uri))
  (let [config-dir (io/file (shared/uri->filename project-root-uri) ".clj-kondo")]
    (when (and (shared/file-exists? config-dir)
               (shared/directory? config-dir))
      config-dir)))

(defn ^:private config-path [config-dir]
  (let [config-file (io/file config-dir "config.edn")]
    (.getCanonicalPath ^java.io.File config-file)))

(defn home-config-path []
  (let [xdg-config-home (or (config/get-env "XDG_CONFIG_HOME")
                            (io/file (config/get-property "user.home") ".config"))]
    (config-path (io/file xdg-config-home "clj-kondo"))))

(defn project-config-path [project-root-uri]
  (config-path (project-config-dir project-root-uri)))

(defn config-hash
  [project-root]
  (let [err (java.io.StringWriter.)]
    (binding [*err* err]
      (let [result (-> project-root
                       (io/file ".clj-kondo")
                       kondo/resolve-config
                       kondo/config-hash)]
        (when-not (string/blank? (str err))
          (logger/error (str err)))
        result))))

(defn db-with-analysis
  "Update `db` with normalized kondo analysis."
  [db {:keys [analysis external?]}]
  (-> db
      (update :analysis merge analysis)
      ;; Whenever the analysis is updated, we refresh the dep graph. This is the
      ;; only place this is done, so to keep the dep graph in sync with the
      ;; analysis, everything that puts analysis in the db must either call this
      ;; function or db-with-results, which calls this function.
      (dep-graph/refresh-analysis (select-keys (:analysis db) (keys analysis))
                                  analysis
                                  (not external?))))

(defn db-with-results
  "Update `db` with normalized kondo result."
  [db {:keys [findings config] :as results}]
  (-> db
      (db-with-analysis results)
      (update :findings merge findings)
      (shared/assoc-some :kondo-config config)))

(defn ^:private element-with-fallback-name-position [element]
  (assoc element
         :name-row (or (:name-row element) (:row element))
         :name-col (or (:name-col element) (:col element))
         :name-end-row (or (:name-end-row element) (:end-row element))
         :name-end-col (or (:name-end-col element) (:end-col element))))

;;;; Normalization

;; The analysis we get back from clj-kondo is indexed by bucket
;; {(bucket [element+])*}
;; We re-index by uri then bucket
;; {(uri {(bucket [element+])+})*}
;; We also normalize elements somewhat, changing their keys, and even spliting
;; some into two elements. As such, the buckets we get from clj-kondo aren't
;; exactly the same as the buckets we have in our db.

(defn ^:private element->normalized-elements [{:keys [bucket arglist-strs] :as element}]
  (case bucket
    :var-definitions
    [(-> element
         (shared/assoc-some :arglist-kws (->> arglist-strs
                                              (keep (fn [arglist-str]
                                                      (try
                                                        (:keys (first (edn/read-string arglist-str)))
                                                        (catch Exception _ nil))))
                                              seq)))]

    ;; We create two elements here (and maybe more for refer)
    :namespace-usages
    (cond-> [(set/rename-keys element {:to :name})]
      (:alias element)
      (conj (-> element
                (assoc :bucket :namespace-alias)
                (set/rename-keys {:alias-row     :name-row
                                  :alias-col     :name-col
                                  :alias-end-row :name-end-row
                                  :alias-end-col :name-end-col}))))

    (:locals :local-usages :java-class-usages :symbols)
    [(element-with-fallback-name-position element)]

    :keywords
    [(-> element
         element-with-fallback-name-position
         (assoc :bucket (if (:reg element) :keyword-definitions :keyword-usages)))]

    (:java-member-definitions
     :java-class-definitions)
    [(-> element
         (assoc :name-row 0
                :name-col 0
                :name-end-row 0
                :name-end-col 0))]


    [element]))

(defn ^:private valid-element? [{:keys [name-row name-col name-end-row name-end-col derived-name-location derived-location]}]
  (and name-row
       name-col
       name-end-row
       name-end-col
       (not derived-name-location)
       (not derived-location)))

(defn ^:private normalize-analysis [external? analysis]
  (reduce-kv
    (fn [result kondo-bucket kondo-elements]
      (transduce
        (comp
          (mapcat #(element->normalized-elements (assoc % :bucket kondo-bucket :external? external?)))
          (filter valid-element?))
        (completing
          (fn [result {:keys [uri bucket] :as element}]
            ;; intentionally use element bucket, since it may have been
            ;; normalized to something besides kondo-bucket
            (update-in result [uri bucket] (fnil conj []) element)))
        result
        kondo-elements))
    ;; TODO: Can result be a transient?
    {}
    analysis))

(defn ^:private normalize
  "Put kondo result in a standard format, with `analysis` normalized and
  `analysis` and `findings` indexed by uri."
  [{:keys [analysis findings] :as kondo-results}
   {:keys [external? ensure-uris filter-analysis] :or {filter-analysis identity}}
   db]
  (let [filename->uri (memoize #(shared/filename->uri % db))
        trade-filename-for-uri (fn [obj]
                                 (-> obj
                                     (assoc :uri (or (:uri obj)
                                                     (filename->uri (:filename obj))))
                                     (dissoc :filename)))
        with-uris (fn [uris default coll]
                    (merge (zipmap uris (repeat default)) coll))
        analysis (->> analysis
                      filter-analysis
                      (medley/map-vals #(map trade-filename-for-uri %))
                      (normalize-analysis external?)
                      (with-uris ensure-uris {}))
        findings (->> findings
                      (map trade-filename-for-uri)
                      (group-by :uri)
                      (with-uris (keys analysis) []))]
    (assoc kondo-results
           :external? external?
           :analysis analysis
           :findings findings)))

(defn ^:private normalize-for-file
  "Normalize kondo result for a single file."
  [kondo-results db filename uri]
  (let [external? (shared/external-filename? filename (settings/get db [:source-paths]))
        normalization-config {:external? external?
                              :ensure-uris [uri]}]
    (normalize kondo-results normalization-config db)))

(defn ^:private with-additional-config
  [config settings]
  (cond-> config
    (get-in settings [:linters :clj-kondo :report-duplicates] true)
    (->
      (assoc-in [:config :linters :unresolved-symbol :report-duplicates] true)
      (assoc-in [:config :linters :unresolved-namespace :report-duplicates] true)
      (assoc-in [:config :linters :unresolved-var :report-duplicates] true))))

(defn ^:private run-custom-lint? [config]
  (not= :off (get-in config [:linters :clojure-lsp/unused-public-var :level])))

(defn ^:private custom-lint-project!
  [db {:keys [config] :as kondo-ctx} normalization-config]
  (when (run-custom-lint? config)
    (shared/logging-time
      "Linting whole project for unused-public-var took %s"
      (let [db (db-with-analysis db (normalize kondo-ctx normalization-config db))]
        (f.diagnostic/custom-lint-project! db kondo-ctx)))))

(defn ^:private custom-lint-files!
  [uris db {:keys [config] :as kondo-ctx} normalization-config]
  (when (run-custom-lint? config)
    (shared/logging-task
      :reference-files/lint
      (let [db (db-with-analysis db (normalize kondo-ctx normalization-config db))]
        (f.diagnostic/custom-lint-uris! uris db kondo-ctx)))))

(defn ^:private custom-lint-file!
  [filename uri db {:keys [config] :as kondo-ctx}]
  (when (run-custom-lint? config)
    (let [db (db-with-analysis db (normalize-for-file kondo-ctx db filename uri))]
      (f.diagnostic/custom-lint-uris! [uri] db kondo-ctx))))

(defn ^:private ignore-path? [db path]
  (let [paths-ignore-regex (get-in db [:settings :paths-ignore-regex] [])]
    (some #(re-matches (re-pattern %) (fs/unixify path)) paths-ignore-regex)))

(def ^:private config-for-shallow-analysis
  {:arglists true
   :keywords true
   :protocol-impls true
   :java-class-definitions true
   :java-member-definitions true
   :var-usages false
   :var-definitions {:shallow true
                     :meta [:arglists :style/indent]}})

(def ^:private config-for-full-analysis
  {:arglists true
   :locals true
   :keywords true
   :protocol-impls true
   :java-class-definitions true
   :java-member-definitions true
   :instance-invocations true
   :java-class-usages true
   :context [:clojure.test
             :re-frame.core]
   :var-definitions {:meta [:arglists :style/indent]}
   :symbols true})

(defn ^:private config-for-paths [paths file-analyzed-fn db]
  (-> {:cache true
       :parallel true
       :config-dir (some-> db :project-root-uri project-config-dir)
       :copy-configs (settings/get db [:copy-kondo-configs?] true)
       :lint [(->> paths
                   (remove (partial ignore-path? db))
                   (string/join (System/getProperty "path.separator")))]
       :config {:output {:canonical-paths true}}
       :file-analyzed-fn file-analyzed-fn}
      (with-additional-config (settings/all db))))

(defn ^:private config-for-internal-paths [paths db custom-lint-fn file-analyzed-fn]
  (-> (config-for-paths paths file-analyzed-fn db)
      (assoc :custom-lint-fn custom-lint-fn)
      (assoc-in [:config :analysis] config-for-full-analysis)))

(defn ^:private config-for-external-paths [paths db file-analyzed-fn]
  (-> (config-for-paths paths file-analyzed-fn db)
      (assoc :skip-lint true)
      (assoc-in [:config :analysis] config-for-shallow-analysis)))

(defn ^:private config-for-copy-configs [paths db]
  {:cache true
   :parallel true
   :skip-lint true
   :config-dir (some-> db :project-root-uri project-config-dir)
   :copy-configs (settings/get db [:copy-kondo-configs?] true)
   :lint [(string/join (System/getProperty "path.separator") paths)]
   :config {:output {:canonical-paths true}}})

(defn ^:private config-for-jdk-source [paths db]
  {:lint paths
   :config-dir (some-> db :project-root-uri project-config-dir)
   :config {:output {:canonical-paths true}
            :analysis {:java-class-definitions true
                       :java-member-definitions true}}})

(defn ^:private config-for-single-file [uri db*]
  (let [db @db*
        filename (shared/uri->filename uri)
        custom-lint-fn #(custom-lint-file! filename uri @db* %)
        lang (shared/uri->file-type uri)]
    (-> {:cache true
         :lint ["-"]
         :copy-configs (settings/get db [:copy-kondo-configs?] true)
         :filename filename
         :config-dir (some-> db :project-root-uri project-config-dir)
         :custom-lint-fn custom-lint-fn
         :config {:output {:canonical-paths true}
                  :analysis config-for-full-analysis}}
        (shared/assoc-in-some [:lang] (when (not= :unknown lang)
                                        lang))
        (with-additional-config (settings/all db)))))

(defn ^:private run-kondo! [config err-hint]
  (let [err-writer (java.io.StringWriter.)]
    (try
      (let [result (binding [*err* err-writer]
                     (kondo/run! config))]
        (when-not (string/blank? (str err-writer))
          (logger/warn "Non-fatal error from clj-kondo:" (str err-writer)))
        result)
      (catch Exception e
        (logger/error e "Error running clj-kondo on" err-hint)))))

(defn run-kondo-on-paths! [paths db* {:keys [external?] :as normalization-config} file-analyzed-fn]
  (let [db @db*
        config (if external?
                 (config-for-external-paths paths db file-analyzed-fn)
                 (config-for-internal-paths paths db
                                            #(custom-lint-project! @db* % normalization-config)
                                            file-analyzed-fn))]
    (-> config
        (run-kondo! (str "paths " (string/join ", " paths)))
        (normalize normalization-config db))))

(defn run-kondo-on-paths-batch!
  "Run kondo on paths by partitioning the paths, with this we should call
  kondo more times but with fewer paths to analyze, improving memory."
  [paths normalization-config file-analyzed-fn db*]
  (let [total (count paths)
        batches (->> paths
                     (partition-all clj-kondo-analysis-batch-size)
                     (map-indexed (fn [index batch-paths]
                                    {:index (inc index)
                                     :paths batch-paths})))
        batch-count (count batches)]
    (logger/info
      (case batch-count
        0 "No new paths to analyze"
        1 (str "Analyzing " total " paths with clj-kondo")
        (str "Analyzing " total " paths with clj-kondo in " batch-count " batches...")))
    (case batch-count
      0 {}
      1 (run-kondo-on-paths! paths db* normalization-config (partial file-analyzed-fn 1 1))
      (->> batches
           (map (fn [{:keys [index paths]}]
                  (logger/info "Analyzing batch" (str index "/" batch-count))
                  (run-kondo-on-paths! paths db* normalization-config (partial file-analyzed-fn index batch-count))))
           (reduce shared/deep-merge)))))

(defn run-kondo-on-reference-uris! [uris db*]
  (let [db @db*
        filenames (map shared/uri->filename uris)
        normalization-config {:external? false
                              :ensure-uris uris}
        custom-lint-fn #(custom-lint-files! uris @db* % normalization-config)]
    (-> (config-for-internal-paths filenames db custom-lint-fn nil)
        (run-kondo! (str "files " (string/join ", " uris)))
        (normalize normalization-config db))))

(defn run-kondo-on-text! [text uri db*]
  (let [filename (shared/uri->filename uri)
        ignore-filename? (ignore-path? @db* filename)
        db @db*]
    (if ignore-filename?
      {}
      (with-in-str text
                   (-> (config-for-single-file uri db*)
                       (run-kondo! filename)
                       (normalize-for-file db filename uri))))))

(defn run-kondo-copy-configs! [paths db]
  (-> (config-for-copy-configs paths db)
      (run-kondo! (str "paths " (string/join ", " paths)))))

(defn run-kondo-on-jdk-source! [paths db]
  (let [normalization-config {:external? true
                              :filter-analysis #(select-keys % [:java-class-definitions
                                                                :java-member-definitions])}]
    (-> (config-for-jdk-source paths db)
        (run-kondo! (str "paths " paths))
        (normalize normalization-config db))))
