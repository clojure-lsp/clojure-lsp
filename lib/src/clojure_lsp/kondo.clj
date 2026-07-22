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

(defn ^:private clj-kondo-pom-version
  "Version from the clj-kondo artifact pom.properties on the classpath,
  including the git sha for snapshot builds. Accurate for nightly builds,
  unlike the CLJ_KONDO_VERSION resource which is only updated on releases."
  []
  (when-let [pom-properties (io/resource "META-INF/maven/clj-kondo/clj-kondo/pom.properties")]
    (let [content (slurp pom-properties)
          version (some-> (re-find #"(?m)^version=(.+)$" content) second string/trim)
          revision (some-> (re-find #"(?m)^revision=(.+)$" content) second string/trim)]
      (when version
        (if (and revision (string/ends-with? version "-SNAPSHOT"))
          (str version " (" (subs revision 0 (min 7 (count revision))) ")")
          version)))))

(def ^:private clj-kondo-version*
  ;; Eager def so native images resolve it at image build time (Clojure classes
  ;; are initialized at build time via clj-easy/graal-build-time), when the
  ;; clj-kondo artifact resources are available on the classpath.
  (or (clj-kondo-pom-version)
      (some-> (io/resource "CLJ_KONDO_VERSION") slurp string/trim)))

(defn clj-kondo-version []
  clj-kondo-version*)

(def clj-kondo-analysis-batch-size 120)

(defonce ^:private filename->uri-cache
  ;; Process-global cache for filename->uri, which otherwise re-pays a regex and
  ;; a File->Path->URI conversion for every analysis element of every pass. Keyed
  ;; by the uri-affecting settings so distinct projects/dbs don't collide.
  (atom {}))

(def ^:private filename->uri-cache-max 500000)

(defn ^:private filename->uri-fn
  "Returns a filename->uri fn backed by the process-global `filename->uri-cache`,
  scoped to the db's uri-affecting settings."
  [db]
  (let [cache-key [(get-in db [:settings :dependency-scheme])
                   (get-in db [:settings :uri-format])]]
    (fn [filename]
      (or (get-in @filename->uri-cache [cache-key filename])
          (let [uri (shared/filename->uri filename db)]
            (swap! filename->uri-cache update cache-key
                   (fn [m]
                     (let [m (or m {})]
                       (if (>= (count m) filename->uri-cache-max)
                         {filename uri}
                         (assoc m filename uri)))))
            uri)))))

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

(defn ^:private db-with-analysis*
  [db {:keys [analysis external?]} merge-analysis]
  (-> db
      (update :analysis merge-analysis analysis)
      ;; Whenever the analysis is updated, we refresh the dep graph. This is the
      ;; only place this is done, so to keep the dep graph in sync with the
      ;; analysis, everything that puts analysis in the db must either call this
      ;; function or db-with-results, which calls this function.
      (dep-graph/refresh-analysis (select-keys (:analysis db) (keys analysis))
                                  analysis
                                  (not external?))))

(defn db-with-analysis
  "Update `db` with normalized kondo analysis, replacing any existing analysis
  for the same uris."
  [db results]
  (db-with-analysis* db results merge))

(defn db-with-merged-analysis
  "Like `db-with-analysis` but merges new buckets into the existing per-uri
  analysis instead of replacing it, so an additive pass (e.g. the lazy
  java-member-definitions analysis) keeps buckets already present at a uri such
  as java-class-definitions."
  [db results]
  (db-with-analysis* db results #(merge-with merge %1 %2)))

(defn db-with-results
  "Update `db` with normalized kondo result."
  [db {:keys [findings config] :as results}]
  (-> db
      (db-with-analysis results)
      (update-in [:diagnostics :clj-kondo] merge findings)
      (shared/assoc-some :kondo-config config)))

(defn db-without-uris
  "Remove the given `uris` from the analysis, dep-graph, documents and kondo
  diagnostics, keeping the dep-graph in sync."
  [db uris]
  (reduce (fn [db uri]
            (-> db
                (dep-graph/remove-doc uri)
                (shared/dissoc-in [:documents uri])
                (shared/dissoc-in [:analysis uri])
                (shared/dissoc-in [:diagnostics :clj-kondo uri])))
          db
          uris))

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

(defn ^:private canonical-fn
  "Returns a fn that returns a shared canonical instance for equal values,
  pooling them in a mutable map."
  []
  (let [pool (java.util.HashMap.)]
    (fn [v]
      (if-let [pooled (.get pool v)]
        pooled
        (do (.put pool v v)
            v)))))

(def ^:private java-element-canonical-keys
  [:uri :class :name :type :return-type :parameter-types :flags])

(defn ^:private canonicalize-java-element
  "Replaces highly duplicated values of java definition elements with a
  canonical instance from the `canon` pool."
  [canon element]
  (reduce #(medley/update-existing %1 %2 canon)
          element
          java-element-canonical-keys))

(defn canonicalize-java-analysis
  "Canonicalizes java definition elements of a normalized analysis, sharing
  equal values in a single instance to reduce memory usage. Useful after
  reading analysis from the transit cache, which creates new instances even
  for equal values."
  [analysis]
  (shared/logging-task
    :internal/canonicalize-java-analysis
    (let [canon (canonical-fn)
          canonicalize-elements #(mapv (partial canonicalize-java-element canon) %)]
      (reduce-kv
        (fn [result uri buckets]
          (if (or (:java-class-definitions buckets)
                  (:java-member-definitions buckets))
            (do
              ;; pool the analysis key so elements share its uri instance
              (canon uri)
              (assoc result uri (-> buckets
                                    (medley/update-existing :java-class-definitions canonicalize-elements)
                                    (medley/update-existing :java-member-definitions canonicalize-elements))))
            result))
        analysis
        analysis))))

(defn ^:private element->normalized-elements
  [{:keys [bucket arglist-strs] :as element}
   keyword-definitions-enabled?
   keyword-usages-enabled?
   canon]
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

    (:locals :local-usages :symbols)
    [(element-with-fallback-name-position element)]

    ;; clj-kondo merges the expr metadata into these elements, which for forms
    ;; preceded by a `#_{:clj-kondo/ignore [...]}` hint includes rewrite-clj
    ;; nodes holding functions that break the transit db cache. #2380
    :java-class-usages
    [(-> element
         (dissoc :clj-kondo/ignore :clj-kondo/ignore-id)
         element-with-fallback-name-position)]

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

    ;; Unlike other elements, java definitions don't have name positions and
    ;; are highly duplicated, so we canonicalize their values to save memory.
    (:java-member-definitions
     :java-class-definitions)
    [(canonicalize-java-element canon element)]

    :var-usages
    [(cond-> element
       (and (not (:alias element))
            (full-qualified-namespace? element))
       (assoc :full-qualified-symbol? true))]

    [element]))

(defn ^:private valid-element?
  [{:keys [name bucket name-row name-col name-end-row name-end-col
           derived-name-location derived-location]}]
  (or (identical? :java-class-definitions bucket)
      (identical? :java-member-definitions bucket)
      (and name-row
           name-col
           name-end-row
           name-end-col
           (not derived-name-location)
           (not derived-location)
           ;; #1510
           (or (not (identical? :var-definitions bucket))
               name))))

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

(defn ^:private normalize-analysis [external? settings trade-filename-for-uri findings analysis]
  (let [keyword-definitions-enabled? (get-in settings [:analysis :keywords :definitions] true)
        keyword-usages-enabled? (get-in settings [:analysis :keywords :usages] true)
        canon (canonical-fn)]
    (shared/deep-merge
      (persistent!
        (reduce-kv
          (fn [result kondo-bucket kondo-elements]
            (transduce
              (comp
                (mapcat #(-> (assoc % :bucket kondo-bucket :external? external?)
                             trade-filename-for-uri
                             (element->normalized-elements keyword-definitions-enabled? keyword-usages-enabled? canon)))
                (filter valid-element?))
              (completing
                (fn [result {:keys [uri bucket] :as element}]
                  ;; intentionally use element bucket, since it may have been
                  ;; normalized to something besides kondo-bucket
                  (assoc! result uri (update (get result uri) bucket (fnil conj []) element))))
              result
              kondo-elements))
          (transient {})
          analysis))
      (findings->analysis findings))))

(defn ^:private normalize
  "Put kondo result in a standard format, with `analysis` normalized and
  `analysis` and `findings` indexed by uri."
  [{:keys [analysis findings] :as kondo-results}
   {:keys [external? ensure-uris filter-analysis] :or {filter-analysis identity}}
   db]
  (let [filename->uri (filename->uri-fn db)
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
                      (normalize-analysis external? (settings/all db) trade-filename-for-uri findings)
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

(defn ^:private var-definition-metas [db settings]
  (let [kondo-metas (get-in (:kondo-config db) [:linters :clojure-lsp/unused-public-var :exclude-when-contains-meta] #{})
        lsp-metas (get-in settings [:linters :clojure-lsp/unused-public-var :exclude-when-contains-meta] #{})]
    (cond-> [:arglists :style/indent]
      (seq kondo-metas) (concat kondo-metas)
      (seq lsp-metas) (concat lsp-metas))))

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

(defn ^:private java-member-definitions-mode
  "Resolves the `[:analysis :java :member-definitions]` setting to one of
  `:eager` (analyze every dependency's java members up front), `:lazy` (analyze
  a class's members on demand on first navigation/hover/completion) or `:off`.
  Defaults to `:lazy` to keep the heavy java-member-definitions bucket out of
  the upfront classpath analysis."
  [settings]
  (case (get-in settings [:analysis :java :member-definitions] :lazy)
    true :eager
    false :off
    (:lazy :on-demand) :lazy
    :lazy))

(defn lazy-java-member-definitions?
  "Whether external java member definitions are analyzed lazily (on demand)
  instead of up front during the classpath scan."
  [db]
  (identical? :lazy (java-member-definitions-mode (settings/all db))))

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
                                       :java-member-definitions (and full-analysis? (not= :off (java-member-definitions-mode settings)))
                                       :instance-invocations full-analysis?
                                       :java-class-usages full-analysis?
                                       :context [:clojure.test
                                                 :re-frame.core]
                                       :var-definitions {:meta (var-definition-metas db settings)
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
                                       ;; member-definitions default to lazy/on-demand for dependencies;
                                       ;; only emit them up front when explicitly set to eager (true)
                                       :java-member-definitions (and full-analysis? (identical? :eager (java-member-definitions-mode settings)))
                                       :var-definitions {:shallow true
                                                         :meta (var-definition-metas db settings)}}))))

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
                             :java-member-definitions (not= :off (java-member-definitions-mode settings))
                             :instance-invocations true
                             :java-class-usages true
                             :context [:clojure.test
                                       :re-frame.core]
                             :var-definitions {:meta (var-definition-metas db settings)
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

(defn ^:private merge-batch-results
  "Combine two normalized kondo result maps from disjoint path batches. Batches
  never share uris, so a shallow per-key merge of the uri-indexed maps is
  equivalent to a deep merge while avoiding its recursive rebuilds. Scalar and
  config keys (`:external?`, `:config`, `:summary`) are identical across batches,
  so the first batch's are kept."
  [a b]
  (-> a
      (update :analysis merge (:analysis b))
      (update :findings merge (:findings b))
      (update-in [:diagnostics :clj-kondo] merge (get-in b [:diagnostics :clj-kondo]))))

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
           (reduce merge-batch-results)))))

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

(defn ^:private config-for-jar-members [jar-path db]
  {:lint [jar-path]
   :skip-lint true
   :config-dir (kondo-config-dir db)
   :config {:output {:canonical-paths true}
            :analysis {:java-class-definitions true
                       :java-member-definitions true}}})

(defn run-kondo-on-jar-members!
  "Analyze a single dependency `jar-path` for its java member definitions,
  used by the lazy/on-demand java analysis. Returns a normalized result keeping
  only the `:java-member-definitions` bucket (class definitions are already
  analyzed up front)."
  [jar-path db]
  (let [normalization-config {:external? true
                              :filter-analysis #(select-keys % [:java-member-definitions])}]
    (-> (config-for-jar-members jar-path db)
        (run-kondo! (str "jar members " jar-path))
        (normalize normalization-config db))))
