(ns clojure-lsp.kondo
  (:require
   [clj-kondo.core :as kondo]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [taoensso.timbre :as log]))

(defn clj-kondo-version []
  (string/trim (slurp (io/resource "CLJ_KONDO_VERSION"))))

(def clj-kondo-analysis-batch-size 50)

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
  (when (get-in db [:settings :lint-project-files-after-startup?] true)
    (async/go
      (let [start-time (System/nanoTime)]
        (f.diagnostic/lint-project-diagnostics! paths (normalize-analysis analysis) kondo-ctx db)
        (log/info (format "Linting whole project files took %sms" (quot (- (System/nanoTime) start-time) 1000000)))))))

(defn ^:private single-file-custom-lint!
  [{:keys [analysis] :as kondo-ctx} db]
  (let [filename (-> analysis :var-definitions first :filename)
        updated-analysis (assoc (:analysis @db) filename (normalize-analysis analysis))]
    (f.diagnostic/unused-public-var-lint-for-single-file! filename updated-analysis kondo-ctx)))

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
      (with-additional-config (:settings @db))))

(defn kondo-for-single-file [uri db]
  (-> {:cache true
       :lint ["-"]
       :copy-configs true
       :lang (shared/uri->file-type uri)
       :filename (shared/uri->filename uri)
       :custom-lint-fn #(single-file-custom-lint! % db)
       :config {:output {:analysis {:arglists true
                                    :locals true
                                    :keywords true}
                         :canonical-paths true}}}
      (with-additional-config (:settings @db))))

(defn run-kondo-on-paths! [paths external-analysis-only? db]
  (let [err (java.io.StringWriter.)]
    (binding [*err* err]
      (let [result (kondo/run! (kondo-for-paths paths db external-analysis-only?))]
        (when-not (string/blank? (str err))
          (log/info (str err)))
        result))))

(defn run-kondo-on-paths-batch!
  "Run kondo on paths by partition the paths, with this we should call
  kondo more times but we fewer paths to analyze, improving memory."
  [paths public-only? db]
  (let [total (count paths)
        batch-count (int (Math/ceil (float (/ total clj-kondo-analysis-batch-size))))]
    (log/info "Analyzing" total "paths with clj-kondo with batch size of" batch-count "...")
    (if (<= total clj-kondo-analysis-batch-size)
      (run-kondo-on-paths! paths public-only? db)
      (->> paths
           (partition-all clj-kondo-analysis-batch-size)
           (map-indexed (fn [index batch-paths]
                          (log/info "Analyzing" (str (inc index) "/" batch-count) "batch paths with clj-kondo...")
                          (run-kondo-on-paths! batch-paths public-only? db)))
           (reduce shared/deep-merge)))))

(defn run-kondo-on-text! [text uri db]
  (let [err (java.io.StringWriter.)]
    (binding [*err* err]
      (let [result (with-in-str
                     text
                     (kondo/run! (kondo-for-single-file uri db)))]
        (when-not (string/blank? (str err))
          (log/error (str err)))
        result))))
