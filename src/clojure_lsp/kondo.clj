(ns clojure-lsp.kondo
  (:require
   [clj-kondo.core :as kondo]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(defn clj-kondo-version []
  (string/trim (slurp (io/resource "CLJ_KONDO_VERSION"))))

(def clj-kondo-analysis-batch-size 50)

(defmacro catch-kondo-errors [err-hint & body]
  `(let [err# (java.io.StringWriter.)
         out# (java.io.StringWriter.)]
     (try
       (binding [*err* err#
                 *out* out#]
         (let [result# (do ~@body)]
           (when-not (string/blank? (str err#))
             (log/warn "Non-fatal error from clj-kondo:" (str err#)))
           (when-not (string/blank? (str out#))
             (log/warn "Output from clj-kondo:" (str out#)))
           result#))
       (catch Exception e#
         (log/error e# "Error running clj-kondo on" ~err-hint)))))

(defn entry->normalized-entries [{:keys [bucket] :as element}]
  (cond
    ;; We create two entries here (and maybe more for refer)
    (= :namespace-usages bucket)
    (cond-> [(set/rename-keys element {:to :name})]
      (:alias element)
      (conj (set/rename-keys (assoc element :bucket :namespace-alias) {:alias-row :name-row :alias-col :name-col :alias-end-row :name-end-row :alias-end-col :name-end-col})))

    (contains? #{:locals :local-usages :keywords} bucket)
    [(-> element
         (assoc :name-row (or (:name-row element) (:row element))
                :name-col (or (:name-col element) (:col element))
                :name-end-row (or (:name-end-row element) (:end-row element))
                :name-end-col (or (:name-end-col element) (:end-col element))))]

    :else
    [element]))

(defn ^:private valid-element? [{:keys [name-row name-col name-end-row name-end-col] :as _element}]
  (and name-row
       name-col
       name-end-row
       name-end-col))

(defn normalize-analysis [analysis]
  (for [[bucket vs] analysis
        v vs
        element (entry->normalized-entries (assoc v :bucket bucket))
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

(defn ^:private project-custom-lint!
  [paths db {:keys [analysis] :as kondo-ctx}]
  (let [new-analysis (group-by :filename (normalize-analysis analysis))]
    (if (:api? @db)
      (do
        (log/info (format "Starting to lint whole project files..."))
        (shared/logging-time
          "Linting whole project files took %s secs"
          (f.diagnostic/lint-project-diagnostics! new-analysis kondo-ctx db)))
      (when (settings/get db [:lint-project-files-after-startup?] true)
        (async/go
          (shared/logging-time
            "Linting whole project files took %s secs"
            (f.diagnostic/lint-and-publish-project-diagnostics! paths new-analysis kondo-ctx db)))))))

(defn ^:private custom-lint-for-reference-files!
  [files db {:keys [analysis] :as kondo-ctx}]
  (shared/logging-time
    "Linting references took %s secs"
    (let [new-analysis (group-by :filename (normalize-analysis analysis))
          updated-analysis (merge (:analysis @db) new-analysis)]
      (doseq [file files]
        (f.diagnostic/unused-public-var-lint-for-single-file! file updated-analysis kondo-ctx db)))))

(defn ^:private single-file-custom-lint!
  [{:keys [analysis config] :as kondo-ctx} uri db]
  (when-not (= :off (get-in config [:linters :clojure-lsp/unused-public-var :level]))
    (let [filename (-> analysis :var-definitions first :filename)
          updated-analysis (assoc (:analysis @db) filename (normalize-analysis analysis))]
      (if (settings/get db [:linters :clj-kondo :async-custom-lint?] true)
        (async/go-loop [tries 1]
          (if (>= tries 200)
            (log/info "Max tries reached when async custom linting" uri)
            (if (:processing-changes @db)
              (do
                (Thread/sleep 50)
                (recur (inc tries)))
              (let [new-findings (f.diagnostic/unused-public-var-lint-for-single-file-merging-findings! filename updated-analysis kondo-ctx db)]
                (swap! db assoc-in [:findings filename] new-findings)
                (when (not= :unknown (shared/uri->file-type uri))
                  (f.diagnostic/sync-lint-file! uri db))))))
        (f.diagnostic/unused-public-var-lint-for-single-file! filename updated-analysis kondo-ctx db)))))

(defn kondo-for-paths [paths db external-analysis-only?]
  (-> {:cache true
       :parallel true
       :copy-configs true
       :lint [(string/join (System/getProperty "path.separator") paths)]
       :config {:output {:analysis {:arglists true
                                    :locals false
                                    :keywords true}
                         :canonical-paths true}}}
      (shared/assoc-some :custom-lint-fn (when-not external-analysis-only?
                                           (partial project-custom-lint! paths db)))
      (with-additional-config (settings/all db))))

(defn kondo-for-reference-filenames [filenames db]
  (-> (kondo-for-paths filenames db false)
      (assoc :custom-lint-fn (partial custom-lint-for-reference-files! filenames db))))

(defn kondo-for-single-file [uri db]
  (-> {:cache true
       :lint ["-"]
       :copy-configs true
       :lang (shared/uri->file-type uri)
       :filename (shared/uri->filename uri)
       :custom-lint-fn #(single-file-custom-lint! % uri db)
       :config {:output {:analysis {:arglists true
                                    :locals true
                                    :keywords true
                                    :context [:clojure.test]}
                         :canonical-paths true}}}
      (with-additional-config (settings/all db))))

(defn run-kondo-on-paths! [paths external-analysis-only? db]
  (catch-kondo-errors (str "paths " (string/join ", " paths))
    (kondo/run! (kondo-for-paths paths db external-analysis-only?))))

(defn run-kondo-on-paths-batch!
  "Run kondo on paths by partitioning the paths, with this we should call
  kondo more times but with fewer paths to analyze, improving memory."
  [paths public-only? update-callback db]
  (let [total (count paths)
        batch-count (int (Math/ceil (float (/ total clj-kondo-analysis-batch-size))))]
    (log/info "Analyzing" total "paths with clj-kondo with batch size of" batch-count "...")
    (if (<= total clj-kondo-analysis-batch-size)
      (run-kondo-on-paths! paths public-only? db)
      (->> paths
           (partition-all clj-kondo-analysis-batch-size)
           (map-indexed (fn [index batch-paths]
                          (log/info "Analyzing" (str (inc index) "/" batch-count) "batch paths with clj-kondo...")
                          (update-callback (inc index) batch-count)
                          (run-kondo-on-paths! batch-paths public-only? db)))
           (reduce shared/deep-merge)))))

(defn run-kondo-on-reference-filenames! [filenames db]
  (catch-kondo-errors (str "files " (string/join ", " filenames))
    (kondo/run! (kondo-for-reference-filenames filenames db))))

(defn run-kondo-on-text! [text uri db]
  (catch-kondo-errors (shared/uri->filename uri)
    (with-in-str text (kondo/run! (kondo-for-single-file uri db)))))

(defn config-hash
  [project-root]
  (let [err (java.io.StringWriter.)]
    (binding [*err* err]
      (let [result (-> project-root
                       (io/file ".clj-kondo")
                       kondo/resolve-config
                       kondo/config-hash)]
        (when-not (string/blank? (str err))
          (log/error (str err)))
        result))))
