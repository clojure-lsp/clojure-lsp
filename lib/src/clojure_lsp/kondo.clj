(ns clojure-lsp.kondo
  (:require
   [clj-kondo.core :as kondo]
   [clojure-lsp.config :as config]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [lsp4clj.protocols.logger :as logger]))

(set! *warn-on-reflection* true)

(defn clj-kondo-version []
  (string/trim (slurp (io/resource "CLJ_KONDO_VERSION"))))

(def clj-kondo-analysis-batch-size 150)

(defn ^:private project-config-dir [project-root-uri]
  (when project-root-uri
    ;; TODO: might be better to use clj-kondo.impl.core/config-dir, but it's not
    ;; yet part of the public kondo api.
    #_(kondo/config-dir (shared/uri->filename project-root-uri))
    (let [config-dir (io/file (shared/uri->filename project-root-uri) ".clj-kondo")]
      (when (and (shared/file-exists? config-dir)
                 (shared/directory? config-dir))
        config-dir))))

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
  [db analysis]
  (update db :analysis merge analysis))

(defn db-with-results
  "Update `db` with normalized kondo result."
  [db {:keys [analysis findings config]}]
  (-> db
      (db-with-analysis analysis)
      (update :findings merge findings)
      (assoc :kondo-config config)))

(defn entry->normalized-entries [{:keys [bucket arglist-strs] :as element}]
  (cond
    (identical? :var-definitions bucket)
    [(-> element
         (shared/assoc-some :arglist-kws (->> arglist-strs
                                              (keep (fn [arglist-str]
                                                      (try
                                                        (:keys (first (edn/read-string arglist-str)))
                                                        (catch Exception _ nil))))
                                              seq)))]

    ;; We create two entries here (and maybe more for refer)
    (identical? :namespace-usages bucket)
    (cond-> [(set/rename-keys element {:to :name})]
      (:alias element)
      (conj (set/rename-keys (assoc element :bucket :namespace-alias) {:alias-row :name-row
                                                                       :alias-col :name-col
                                                                       :alias-end-row :name-end-row
                                                                       :alias-end-col :name-end-col})))

    (contains? #{:locals :local-usages :keywords} bucket)
    [(-> element
         (assoc :name-row (or (:name-row element) (:row element))
                :name-col (or (:name-col element) (:col element))
                :name-end-row (or (:name-end-row element) (:end-row element))
                :name-end-col (or (:name-end-col element) (:end-col element))))]

    (identical? :java-class-definitions bucket)
    [(-> element
         (dissoc :uri)
         (assoc :name-row 0
                :name-col 0
                :name-end-row 0
                :name-end-col 0))]

    (identical? :java-class-usages bucket)
    [(-> element
         (dissoc :uri)
         (assoc :name-row (or (:name-row element) (:row element))
                :name-col (or (:name-col element) (:col element))
                :name-end-row (or (:name-end-row element) (:end-row element))
                :name-end-col (or (:name-end-col element) (:end-col element))))]

    :else
    [element]))

(defn ^:private valid-element? [{:keys [name-row name-col name-end-row name-end-col]}]
  (and name-row
       name-col
       name-end-row
       name-end-col))

(defn ^:private normalize-analysis [external? analysis]
  (for [[bucket vs] analysis
        v vs
        element (entry->normalized-entries (assoc v :bucket bucket :external? external?))
        :when (valid-element? element)]
    element))

(defn ^:private normalize
  "Put kondo result in a standard format, with `analysis` normalized and
  `analysis` and `findings` indexed by filename."
  [{:keys [analysis findings] :as kondo-results}
   {:keys [external? ensure-filenames filter-analysis] :or {filter-analysis identity}}]
  (let [analysis (->> analysis
                      filter-analysis
                      (normalize-analysis external?)
                      (group-by :filename))
        analysis (reduce (fn [analysis filename]
                           (update analysis filename #(or % [])))
                         analysis
                         ensure-filenames)
        filenames (keys analysis)
        empty-findings (zipmap filenames (repeat []))
        findings (merge empty-findings (group-by :filename findings))]
    (assoc kondo-results
           :analysis analysis
           :findings findings)))

(defn ^:private normalize-for-filename
  "Normalize kondo result for a single file."
  [kondo-results db filename]
  (let [external? (shared/external-filename? filename (settings/get db [:source-paths]))]
    (normalize kondo-results {:external? external?
                              :ensure-filenames [filename]})))

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
  [db {:keys [config analysis] :as kondo-ctx}]
  (when (run-custom-lint? config)
    (shared/logging-time
      "Linting whole project for unused-public-var took %s"
      (f.diagnostic/custom-lint-project! (db-with-analysis db analysis) kondo-ctx))))

(defn ^:private custom-lint-files!
  [files db {:keys [config analysis] :as kondo-ctx}]
  (when (run-custom-lint? config)
    (shared/logging-task
      :reference-files/lint
      (f.diagnostic/custom-lint-files! files (db-with-analysis db analysis) kondo-ctx))))

(defn ^:private custom-lint-file!
  [filename db {:keys [analysis config] :as kondo-ctx}]
  (when (run-custom-lint? config)
    (f.diagnostic/custom-lint-file! filename (db-with-analysis db analysis) kondo-ctx)))

(def ^:private config-for-internal-analysis
  {:arglists true
   :locals true
   :keywords true
   :protocol-impls true
   :java-class-definitions true
   :instance-invocations true
   :java-class-usages true
   :context [:clojure.test
             :re-frame.core]})

(defn ^:private config-for-paths [paths file-analyzed-fn db]
  (-> {:cache true
       :parallel true
       :copy-configs (settings/get db [:copy-kondo-configs?] true)
       :lint [(string/join (System/getProperty "path.separator") paths)]
       :config {:output {:canonical-paths true}}
       :file-analyzed-fn file-analyzed-fn}
      (with-additional-config (settings/all db))))

(defn ^:private config-for-internal-paths [paths db custom-lint-fn file-analyzed-fn]
  (-> (config-for-paths paths file-analyzed-fn db)
      (assoc :custom-lint-fn custom-lint-fn)
      (assoc-in [:config :analysis] config-for-internal-analysis)))

(defn ^:private config-for-external-paths [paths db file-analyzed-fn]
  (-> (config-for-paths paths file-analyzed-fn db)
      (assoc :skip-lint true)
      (assoc-in [:config :analysis]
                {:arglists true
                 :keywords true
                 :protocol-impls true
                 :java-class-definitions true
                 :var-usages false
                 :var-definitions {:shallow true}})))

(defn ^:private config-for-copy-configs [paths db]
  {:cache true
   :parallel true
   :skip-lint true
   :copy-configs (settings/get db [:copy-kondo-configs?] true)
   :lint [(string/join (System/getProperty "path.separator") paths)]
   :config {:output {:canonical-paths true}}})

(defn ^:private config-for-jdk-source [paths]
  {:lint paths
   :config {:output {:canonical-paths true}
            :analysis {:java-class-definitions true}}})

(defn ^:private config-for-single-file [uri db*]
  (let [db @db*
        filename (shared/uri->filename uri)
        custom-lint-fn #(let [db @db*]
                          (custom-lint-file! filename db (normalize-for-filename % db filename)))]
    (-> {:cache true
         :lint ["-"]
         :copy-configs (settings/get db [:copy-kondo-configs?] true)
         :lang (shared/uri->file-type uri)
         :filename filename
         :config-dir (project-config-dir (:project-root-uri db))
         :custom-lint-fn custom-lint-fn
         :config {:output {:canonical-paths true}
                  :analysis config-for-internal-analysis}}
        (with-additional-config (settings/all db)))))

(defn ^:private run-kondo! [config err-hint db]
  (let [err-writer (java.io.StringWriter.)
        out-writer (java.io.StringWriter.)]
    (try
      (let [result (if (:api? db)
                     (kondo/run! config)
                     (binding [*err* err-writer
                               *out* out-writer]
                       (kondo/run! config)))]
        (when-not (string/blank? (str err-writer))
          (logger/warn "Non-fatal error from clj-kondo:" (str err-writer)))
        (when-not (string/blank? (str out-writer))
          (logger/warn "Output from clj-kondo:" (str out-writer)))
        result)
      (catch Exception e
        (logger/error e "Error running clj-kondo on" err-hint)))))

(defn run-kondo-on-paths! [paths db* {:keys [external?] :as normalization-config} file-analyzed-fn]
  (let [db @db*
        config (if external?
                 (config-for-external-paths paths db file-analyzed-fn)
                 (config-for-internal-paths paths db
                                            #(custom-lint-project! @db* (normalize % normalization-config))
                                            file-analyzed-fn))]
    (-> config
        (run-kondo! (str "paths " (string/join ", " paths)) db)
        (normalize normalization-config))))

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
                  (logger/info "Analyzing" (str index "/" batch-count) "batch paths with clj-kondo...")
                  (run-kondo-on-paths! paths db* normalization-config (partial file-analyzed-fn index batch-count))))
           (reduce shared/deep-merge)))))

(defn run-kondo-on-reference-filenames! [filenames db*]
  (let [db @db*
        normalization-config {:external? false
                              :ensure-filenames filenames}
        custom-lint-fn #(custom-lint-files! filenames @db* (normalize % normalization-config))]
    (-> (config-for-internal-paths filenames db custom-lint-fn nil)
        (run-kondo! (str "files " (string/join ", " filenames)) db)
        (normalize normalization-config))))

(defn run-kondo-on-text! [text uri db*]
  (let [filename (shared/uri->filename uri)
        db @db*]
    (with-in-str text
                 (-> (config-for-single-file uri db*)
                     (run-kondo! filename db)
                     (normalize-for-filename db filename)))))

(defn run-kondo-copy-configs! [paths db]
  (-> (config-for-copy-configs paths db)
      (run-kondo! (str "paths " (string/join ", " paths)) db)))

(defn run-kondo-on-jdk-source! [paths db]
  (-> (config-for-jdk-source paths)
      (run-kondo! (str "paths " paths) db)
      (normalize {:external? true
                  :filter-analysis #(select-keys % [:java-class-definitions])})))
