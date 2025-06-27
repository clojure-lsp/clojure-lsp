(ns clojure-lsp.kondo
  (:require
   [babashka.fs :as fs]
   [clj-kondo.core :as kondo]
   [clojure-lsp.config :as config]
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [medley.core :as medley]))

(set! *warn-on-reflection* true)

(def logger-tag "[clj-kondo]")

(defn clj-kondo-version []
  (string/trim (slurp (io/resource "CLJ_KONDO_VERSION"))))

(def clj-kondo-analysis-batch-size 120)

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
      (update-in [:diagnostics :clj-kondo] merge findings)
      (shared/assoc-some :kondo-config config)))

(defn ^:private element-with-fallback-name-position [element]
  (assoc element
         :name-row (or (:name-row element) (:row element))
         :name-col (or (:name-col element) (:col element))
         :name-end-row (or (:name-end-row element) (:end-row element))
         :name-end-col (or (:name-end-col element) (:end-col element))))

(defn ^:private full-qualified-namespace?
  [element]
  (when (and (:name-col element)
             (:name-end-col element))
    (let [element-full-name (str (:to element) "/" (:name element))
          element-length (- (:name-end-col element) (:name-col element))]
      (= (count element-full-name) element-length))))

;;;; Normalization

;; The analysis we get back from clj-kondo is indexed by bucket
;; {(bucket [element+])*}
;; We re-index by uri then bucket
;; {(uri {(bucket [element+])+})*}
;; We also normalize elements somewhat, changing their keys, and even spliting
;; some into two elements. As such, the buckets we get from clj-kondo aren't
;; exactly the same as the buckets we have in our db.

(defn ^:private element->normalized-elements
  [{:keys [bucket arglist-strs] :as element}
   keyword-definitions-enabled?
   keyword-usages-enabled?]
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
    (cond
      (and (:reg element) keyword-definitions-enabled?)
      [(-> element
           element-with-fallback-name-position
           (assoc :bucket :keyword-definitions))]

      (and (not (:external? element)) keyword-usages-enabled?)
      [(-> element
           element-with-fallback-name-position
           (assoc :bucket :keyword-usages))])

    (:java-member-definitions
     :java-class-definitions)
    [(-> element
         (assoc :name-row 0
                :name-col 0
                :name-end-row 0
                :name-end-col 0))]

    :var-usages
    [(cond-> element
       (and (not (:alias element))
            (full-qualified-namespace? element))
       (assoc :full-qualified-symbol? true))]

    [element]))

(defn ^:private valid-element?
  [{:keys [name bucket name-row name-col name-end-row name-end-col
           derived-name-location derived-location]}]
  (and name-row
       name-col
       name-end-row
       name-end-col
       (not derived-name-location)
       (not derived-location)
       ;; #1510
       (or (not (identical? :var-definitions bucket))
           name)))

(defn ^:private findings->analysis [findings]
  (shared/logging-task
    :internal/kondo-findings->analysis
    (reduce
      (fn [acc [uri findings]]
        (assoc acc uri
               (reduce
                 (fn [acc2 finding]
                   (if (identical? :unresolved-namespace (:type finding))
                     (update acc2 :var-usages (fnil conj [])
                             {:bucket :var-usages
                              :unresolved? true
                              :external? false
                              :name (:name finding)
                              :to (:ns finding)
                              :full-qualified-symbol? true
                              :name-row (:row finding)
                              :name-col (:col finding)
                              :name-end-row (:end-row finding)
                              :name-end-col (:end-col finding)
                              :uri uri})
                     acc2))
                 {}
                 findings)))
      {}
      findings)))

(defn ^:private normalize-analysis [external? settings findings analysis]
  (let [keyword-definitions-enabled? (get-in settings [:analysis :keywords :definitions] true)
        keyword-usages-enabled? (get-in settings [:analysis :keywords :usages] true)]
    (shared/deep-merge
      (reduce-kv
        (fn [result kondo-bucket kondo-elements]
          (transduce
            (comp
              (mapcat #(element->normalized-elements (assoc % :bucket kondo-bucket :external? external?) keyword-definitions-enabled? keyword-usages-enabled?))
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
        analysis)
      (findings->analysis findings))))

(defn ^:private normalize
  "Put kondo result in a standard format, with `analysis` normalized and
  `analysis` and `findings` indexed by uri."
  [{:keys [analysis findings] :as kondo-results}
   {:keys [external? ensure-uris filter-analysis] :or {filter-analysis identity}}
   db]
  (let [filename->uri (memoize #(shared/filename->uri % db))
        trade-filename-for-uri (fn [obj]
                                 (-> obj
                                     (assoc :uri (or (some-> (:filename obj) filename->uri)
                                                     (some-> (:uri obj) shared/conform-uri-scheme)))
                                     (dissoc :filename)))
        with-uris (fn [uris default coll]
                    (merge (zipmap uris (repeat default)) coll))
        findings (->> findings
                      (map trade-filename-for-uri)
                      (group-by :uri))
        analysis (->> analysis
                      filter-analysis
                      (medley/map-vals #(map trade-filename-for-uri %))
                      (normalize-analysis external? (settings/all db) findings)
                      (with-uris ensure-uris {}))
        findings (with-uris (keys analysis) [] findings)]
    (assoc kondo-results
           :external? external?
           :analysis analysis
           :findings findings)))

(defn ^:private normalize-for-file
  "Normalize kondo result for a single file."
  [kondo-results db filename uri]
  (let [project-dep-files (mapv :project-path (settings/get db [:project-specs]))
        external? (and (shared/external-filename? filename (settings/get db [:source-paths]))
                       (not (some #(= (fs/file-name filename) %) project-dep-files)))
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

(defn ^:private var-definition-metas [db]
  (let [metas (get-in (:kondo-config db) [:linters :clojure-lsp/unused-public-var :exclude-when-contains-meta] #{})]
    (cond-> [:arglists :style/indent]
      (seq metas) (concat metas))))

(defn ^:private kondo-config-dir [db]
  (settings/get db [:kondo-config-dir] (some-> db :project-root-uri project-config-dir)))

(defn ^:private config-for-paths [paths file-analyzed-fn db settings]
  (-> {:cache true
       :parallel true
       :config-dir (kondo-config-dir db)
       :copy-configs (settings/get db [:copy-kondo-configs?] true)
       :lint [(->> paths
                   (remove (partial shared/ignore-path? (settings/all db)))
                   (string/join (System/getProperty "path.separator")))]
       :config {:output {:canonical-paths true}}
       :file-analyzed-fn file-analyzed-fn}
      (with-additional-config settings)))

(defn ^:private config-for-internal-paths [paths db file-analyzed-fn]
  ;; source-paths analysis should always include all code data (full-analysis)
  (let [full-analysis? (not= :project-only (:project-analysis-type db))
        settings (settings/all db)]
    (-> (config-for-paths paths file-analyzed-fn db settings)
        (assoc-in [:config :analysis] {:arglists full-analysis?
                                       :locals full-analysis?
                                       :keywords (and full-analysis? (get-in settings [:analysis :keywords] true))
                                       :protocol-impls full-analysis?
                                       :java-class-definitions (and full-analysis? (get-in settings [:analysis :java :class-definitions] true))
                                       :java-member-definitions (and full-analysis? (get-in settings [:analysis :java :member-definitions] true))
                                       :instance-invocations full-analysis?
                                       :java-class-usages full-analysis?
                                       :context [:clojure.test
                                                 :re-frame.core]
                                       :var-definitions {:meta (var-definition-metas db)
                                                         :callstack true}
                                       :symbols (and full-analysis? (get-in settings [:analysis :symbols] true))}))))

(defn ^:private config-for-external-paths [paths db file-analyzed-fn]
  (let [full-analysis? (not (contains? #{:project-only :project-and-shallow-analysis} (:project-analysis-type db)))
        settings (settings/all db)]
    (-> (config-for-paths paths file-analyzed-fn db settings)
        (assoc :skip-lint true)
        (assoc-in [:config :analysis] {:var-usages false
                                       :keywords (and full-analysis? (get-in settings [:analysis :keywords] true))
                                       :arglists full-analysis?
                                       :protocol-impls full-analysis?
                                       :java-class-definitions (and full-analysis? (get-in settings [:analysis :java :class-definitions] true))
                                       :java-member-definitions (and full-analysis? (get-in settings [:analysis :java :member-definitions] true))
                                       :var-definitions {:shallow true
                                                         :meta (var-definition-metas db)}}))))

(defn ^:private config-for-copy-configs [paths db]
  {:cache true
   :parallel true
   :skip-lint true
   :config-dir (kondo-config-dir db)
   :copy-configs (settings/get db [:copy-kondo-configs?] true)
   :lint [(string/join (System/getProperty "path.separator") paths)]
   :config {:output {:canonical-paths true}}})

(defn ^:private config-for-jdk-source [paths db]
  {:lint paths
   :config-dir (kondo-config-dir db)
   :config {:output {:canonical-paths true}
            :analysis {:java-class-definitions true
                       :java-member-definitions true}}})

(defn ^:private config-for-single-file [uri db*]
  (let [db @db*
        filename (shared/uri->filename uri)
        lang (shared/uri->file-type uri)
        settings (settings/all db)]
    (-> {:cache true
         :lint ["-"]
         :copy-configs (get settings :copy-kondo-configs? true)
         :filename filename
         :config-dir (kondo-config-dir db)
         :config {:output {:canonical-paths true}
                  :analysis {:arglists true
                             :locals true
                             :keywords (get-in settings [:analysis :keywords] true)
                             :protocol-impls true
                             :java-class-definitions (get-in settings [:analysis :java :class-definitions] true)
                             :java-member-definitions (get-in settings [:analysis :java :member-definitions] true)
                             :instance-invocations true
                             :java-class-usages true
                             :context [:clojure.test
                                       :re-frame.core]
                             :var-definitions {:meta (var-definition-metas db)
                                               :callstack true}
                             :symbols (get-in settings [:analysis :symbols] true)}}}
        (shared/assoc-in-some [:lang] (when (not= :unknown lang)
                                        lang))
        (with-additional-config settings))))

(defn ^:private run-kondo! [config err-hint]
  (let [err-writer (java.io.StringWriter.)]
    (try
      (let [result (binding [*err* err-writer]
                     (kondo/run! config))]
        (when-not (string/blank? (str err-writer))
          (logger/warn logger-tag (string/trim-newline (str err-writer))))
        result)
      (catch Exception e
        (logger/error e (str logger-tag " error analysing " err-hint))))))

(defn run-kondo-on-paths! [paths db* {:keys [external?] :as normalization-config} file-analyzed-fn]
  (let [db @db*
        config (if external?
                 (config-for-external-paths paths db file-analyzed-fn)
                 (config-for-internal-paths paths db file-analyzed-fn))
        empty-findings (->> paths
                            (filter (partial shared/ignore-path? (settings/all db)))
                            (reduce (fn [findings path] (assoc findings (shared/filename->uri path db) [])) {}))]
    (-> config
        (run-kondo! (str "paths " (string/join ", " paths)))
        (normalize normalization-config db)
        (update-in [:diagnostics :clj-kondo] merge empty-findings))))

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
      logger-tag
      (case batch-count
        0 "No new paths to analyze"
        1 (str "Analyzing " total " paths with clj-kondo")
        (str "Analyzing " total " paths with clj-kondo in " batch-count " batches...")))
    (case batch-count
      0 {}
      1 (run-kondo-on-paths! paths db* normalization-config (partial file-analyzed-fn 1 1))
      (->> batches
           (map (fn [{:keys [index paths]}]
                  (logger/info logger-tag "Analyzing batch" (str index "/" batch-count))
                  (run-kondo-on-paths! paths db* normalization-config (partial file-analyzed-fn index batch-count))))
           (reduce shared/deep-merge)))))

(defn run-kondo-on-reference-uris! [uris db*]
  (let [db @db*
        filenames (map shared/uri->filename uris)
        normalization-config {:external? false
                              :ensure-uris uris}]
    (-> (config-for-internal-paths filenames db nil)
        (run-kondo! (str "files " (string/join ", " uris)))
        (normalize normalization-config db))))

(defn run-kondo-on-text! [text uri db*]
  (let [filename (shared/uri->filename uri)
        ignore-filename? (shared/ignore-path? (settings/all @db*) filename)
        db @db*]
    (if ignore-filename?
      {:findings {uri []}}
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
