(ns clojure-lsp.kondo
  (:require
   [clj-kondo.core :as kondo]
   [clojure-lsp.config :as config]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [lsp4clj.protocols.logger :as logger]))

(set! *warn-on-reflection* true)

(defn clj-kondo-version []
  (string/trim (slurp (io/resource "CLJ_KONDO_VERSION"))))

(def clj-kondo-analysis-batch-size 50)

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

(defmacro catch-kondo-errors [err-hint & body]
  (let [m (meta &form)
        err-sym (gensym "err")
        out-sym (gensym "out")
        e-sym (gensym "e")]
    `(let [~err-sym (java.io.StringWriter.)
           ~out-sym (java.io.StringWriter.)]
       (try
         (binding [*err* ~err-sym
                   *out* ~out-sym]
           (let [result# (do ~@body)]
             (when-not (string/blank? (str ~err-sym))
               ~(with-meta `(logger/warn "Non-fatal error from clj-kondo:" (str ~err-sym)) m))
             (when-not (string/blank? (str ~out-sym))
               ~(with-meta `(logger/warn "Output from clj-kondo:" (str ~out-sym)) m))
             result#))
         (catch Exception ~e-sym
           ~(with-meta `(logger/error ~e-sym "Error running clj-kondo on" ~err-hint) m))))))

(defn entry->normalized-entries [{:keys [bucket] :as element}]
  (cond
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

(defn normalize-analysis [external? analysis]
  (for [[bucket vs] analysis
        v vs
        element (entry->normalized-entries (assoc v :bucket bucket :external? external?))
        :when (valid-element? element)]
    element))

(defn ^:private with-additional-config
  [config settings]
  (cond-> config
    (get-in settings [:linters :clj-kondo :report-duplicates] true)
    (->
      (assoc-in [:config :linters :unresolved-symbol :report-duplicates] true)
      (assoc-in [:config :linters :unresolved-namespace :report-duplicates] true)
      (assoc-in [:config :linters :unresolved-var :report-duplicates] true))))

(defn ^:private custom-lint-project!
  [external-analysis-only? {:keys [analysis config] :as kondo-ctx}]
  (when-not (= :off (get-in config [:linters :clojure-lsp/unused-public-var :level]))
    (let [new-analysis (group-by :filename (normalize-analysis external-analysis-only? analysis))]
      (f.diagnostic/custom-lint-project! new-analysis kondo-ctx))))

(defn ^:private custom-lint-files!
  [files db* {:keys [analysis] :as kondo-ctx}]
  (shared/logging-task
    :reference-files/lint
    (let [db @db*
          new-analysis (group-by :filename (normalize-analysis false analysis))
          updated-analysis (merge (:analysis db) new-analysis)]
      (f.diagnostic/custom-lint-files! files updated-analysis kondo-ctx))))

(defn ^:private custom-lint-file!
  [{:keys [analysis config] :as kondo-ctx} uri db*]
  (when-not (= :off (get-in config [:linters :clojure-lsp/unused-public-var :level]))
    (let [db @db*
          filename (-> analysis :var-definitions first :filename)
          source-paths (settings/get db [:source-paths])
          external-filename? (shared/external-filename? filename source-paths)
          updated-analysis (assoc (:analysis db) filename (normalize-analysis external-filename? analysis))]
      (if (settings/get db [:linters :clj-kondo :async-custom-lint?] false)
        (async/go-loop [tries 1]
          (if (>= tries 200)
            (logger/info "Max tries reached when async custom linting" uri)
            (if (contains? (:processing-changes @db*) uri)
              (do
                (Thread/sleep 50)
                (recur (inc tries)))
              (let [db @db*
                    old-findings (get-in db [:findings filename])
                    new-findings (f.diagnostic/custom-lint-file-merging-findings! filename updated-analysis kondo-ctx db)]
                ;; This equality check doesn't seem necessary, but it helps
                ;; avoid an infinite loop. See
                ;; https://github.com/clojure-lsp/clojure-lsp/issues/796#issuecomment-1065830737
                ;; and the surrounding discussion. Even if the new-findings are
                ;; `=` to the old-findings, they never seem to be `identical?`.
                ;; (TODO: understand why?). If we swap them in, the
                ;; `compare-and-set!` in `file-management.analyze-changes` is
                ;; guaranteed to fail (since `compare-and-set!` is based on
                ;; object identity, not equality). That will trigger a
                ;; re-analysis and re-linting, bringing us back to this line and
                ;; starting the loop again.
                (when (not= old-findings new-findings)
                  (swap! db* assoc-in [:findings filename] new-findings))
                (when (not= :unknown (shared/uri->file-type uri))
                  (f.diagnostic/sync-publish-diagnostics! uri @db*))))))
        (f.diagnostic/custom-lint-file! filename updated-analysis kondo-ctx)))))

(defn kondo-for-paths [paths db* external-analysis-only?]
  (let [db @db*]
    (-> {:cache true
         :parallel true
         :copy-configs (settings/get db [:copy-kondo-configs?] true)
         :lint [(string/join (System/getProperty "path.separator") paths)]
         :config {:output {:analysis {:arglists true
                                      :locals false
                                      :keywords true
                                      :protocol-impls true
                                      :java-class-definitions true}
                           :canonical-paths true}}}
        (shared/assoc-in-some [:custom-lint-fn] (when-not external-analysis-only?
                                                  (partial custom-lint-project! external-analysis-only?)))
        (shared/assoc-in-some [:config :output :analysis :java-class-usages] (not external-analysis-only?))
        (with-additional-config (settings/all db)))))

(defn kondo-copy-configs [paths db]
  {:cache true
   :parallel true
   :skip-lint true
   :copy-configs (settings/get db [:copy-kondo-configs?] true)
   :lint [(string/join (System/getProperty "path.separator") paths)]
   :config {:output {:canonical-paths true}}})

(defn kondo-jdk-source [paths]
  {:lint paths
   :config {:output {:analysis {:java-class-definitions true}
                     :canonical-paths true}}})

(defn kondo-for-reference-filenames [filenames db*]
  (-> (kondo-for-paths filenames db* false)
      (assoc :custom-lint-fn (partial custom-lint-files! filenames db*))))

(defn kondo-for-single-file [uri db*]
  (let [db @db*]
    (-> {:cache true
         :lint ["-"]
         :copy-configs (settings/get db [:copy-kondo-configs?] true)
         :lang (shared/uri->file-type uri)
         :filename (shared/uri->filename uri)
         :config-dir (project-config-dir (:project-root-uri db))
         :custom-lint-fn #(custom-lint-file! % uri db*)
         :config {:output {:analysis {:arglists true
                                      :locals true
                                      :keywords true
                                      :protocol-impls true
                                      :java-class-definitions true
                                      :java-class-usages true
                                      :context [:clojure.test
                                                :re-frame.core]}
                           :canonical-paths true}}}
        (with-additional-config (settings/all db)))))

(defn run-kondo-on-paths! [paths external-analysis-only? db*]
  (catch-kondo-errors (str "paths " (string/join ", " paths))
    (kondo/run! (kondo-for-paths paths db* external-analysis-only?))))

(defn run-kondo-on-paths-batch!
  "Run kondo on paths by partitioning the paths, with this we should call
  kondo more times but with fewer paths to analyze, improving memory."
  [paths public-only? update-callback db*]
  (let [total (count paths)
        batch-count (int (Math/ceil (float (/ total clj-kondo-analysis-batch-size))))]
    (logger/info (str "Analyzing " total " paths with clj-kondo with batch size of " batch-count " ..."))
    (if (<= total clj-kondo-analysis-batch-size)
      (run-kondo-on-paths! paths public-only? db*)
      (->> paths
           (partition-all clj-kondo-analysis-batch-size)
           (map-indexed (fn [index batch-paths]
                          (logger/info "Analyzing" (str (inc index) "/" batch-count) "batch paths with clj-kondo...")
                          (update-callback (inc index) batch-count)
                          (run-kondo-on-paths! batch-paths public-only? db*)))
           (reduce shared/deep-merge)))))

(defn run-kondo-on-reference-filenames! [filenames db*]
  (catch-kondo-errors (str "files " (string/join ", " filenames))
    (kondo/run! (kondo-for-reference-filenames filenames db*))))

(defn run-kondo-on-text! [text uri db*]
  (catch-kondo-errors (shared/uri->filename uri)
    (with-in-str text (kondo/run! (kondo-for-single-file uri db*)))))

(defn run-kondo-copy-configs! [paths db]
  (catch-kondo-errors (str "paths " (string/join ", " paths))
    (kondo/run! (kondo-copy-configs paths db))))

(defn run-kondo-on-jdk-source! [paths]
  (catch-kondo-errors (str "paths " paths)
    (kondo/run! (kondo-jdk-source paths))))
