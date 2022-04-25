(ns clojure-lsp.db
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [cognitect.transit :as transit]
   [lsp4clj.protocols.logger :as logger]))

(set! *warn-on-reflection* true)

(def ^:private db-logger-tag "[DB]")

(def initial-db {:documents {}
                 :processing-changes #{}})
(defonce db* (atom initial-db))
(defonce current-changes-chan (async/chan 1))
(defonce diagnostics-chan (async/chan 1))
(defonce created-watched-files-chan (async/chan 1))
(defonce edits-chan (async/chan 1))

(def version 2)

(defn ^:private sqlite-db-file [project-root]
  (io/file (str project-root) ".lsp" ".cache" "sqlite.db"))

(defn ^:private datalevin-db-files [db]
  (let [cache-dir ^java.io.File (config/local-cache-dir db)]
    [(io/file cache-dir "data.mdb")
     (io/file cache-dir "lock.mdb")]))

(defn ^:private transit-local-db-file [db]
  (io/file (config/local-cache-dir db) "db.transit.json"))

(defn ^:private transit-global-db-file []
  (io/file (config/global-cache-dir) "db.transit.json"))

(defn ^:private remove-old-sqlite-db-file! [project-root-path]
  (let [old-db-file (sqlite-db-file project-root-path)]
    (when (shared/file-exists? old-db-file)
      (io/delete-file old-db-file true))))

(defn ^:private remove-old-datalevin-db-file! []
  (->> (datalevin-db-files @db*)
       (filter shared/file-exists?)
       (mapv #(io/delete-file % true))))

(defn db-exists? [db]
  (shared/file-exists? (transit-local-db-file db)))

(defn remove-db! [db]
  (io/delete-file (transit-local-db-file db)))

(defn ^:private upsert-cache! [cache cache-file]
  (try
    (shared/logging-time
      (str db-logger-tag " Upserting transit analysis to " cache-file " cache took %s")
      (with-open [;; first we write to a baos as a workaround for transit-clj #43
                  bos (java.io.ByteArrayOutputStream. 1024)
                  os (io/output-stream bos)]
        (let [writer (transit/writer os :json)]
          (io/make-parents cache-file)
          (transit/write writer cache)
          (io/copy (.toByteArray bos) cache-file))))
    (catch Throwable e
      (logger/error db-logger-tag "Could not upsert db cache" e))))

(defn ^:private read-cache [cache-file]
  (try
    (shared/logging-time
      (str db-logger-tag " Reading transit analysis cache from " cache-file " db took %s")
      (if (shared/file-exists? cache-file)
        (let [cache (with-open [is (io/input-stream cache-file)]
                      (transit/read (transit/reader is :json)))]
          (when (= version (:version cache))
            cache))
        (logger/error db-logger-tag "No cache DB file found")))
    (catch Throwable e
      (logger/error db-logger-tag "Could not load global cache from DB" e))))

(defn upsert-local-cache! [{:keys [project-root] :as project-cache} db]
  (remove-old-sqlite-db-file! project-root)
  (remove-old-datalevin-db-file!)
  (upsert-cache! project-cache (transit-local-db-file db)))

(defn read-local-cache [project-root db]
  (let [project-analysis (read-cache (transit-local-db-file db))]
    (when (= (str project-root) (:project-root project-analysis))
      project-analysis)))

(defn read-and-update-cache! [db db-change-fn]
  (-> (shared/uri->path (:project-root-uri db))
      (read-local-cache db)
      (db-change-fn)
      (upsert-local-cache! db)))

(defn ^:private upsert-global-cache! [global-cache]
  (upsert-cache! global-cache (transit-global-db-file)))

(defn read-global-cache []
  (read-cache (transit-global-db-file)))

(defn read-and-update-global-cache! [db-change-fn]
  (-> (read-global-cache)
      (db-change-fn)
      (upsert-global-cache!)))

;; DATA

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

(defn ^:private normalize-analysis [external? analysis]
  (for [[bucket vs] analysis
        v vs
        element (entry->normalized-entries (assoc v :bucket bucket :external? external?))
        :when (valid-element? element)]
    element))

(defn normalize-kondo
  "Put kondo result in a standard format, with `analysis` normalized and
  `analysis` and `findings` indexed by filename."
  [{:keys [analysis findings config]} {:keys [external? ensure-filenames]}]
  (let [analysis (->> analysis
                      (normalize-analysis external?)
                      (group-by :filename))
        analysis (reduce (fn [analysis filename]
                           (update analysis filename #(or % [])))
                         analysis
                         ensure-filenames)
        filenames (keys analysis)
        empty-findings (zipmap filenames (repeat []))
        findings (merge empty-findings (group-by :filename findings))]
    {:analysis analysis
     :findings findings
     :config config}))

(defn merge-kondo-results
  "Update `db` with normalized kondo result."
  [db {:keys [analysis findings config]}]
  (-> db
      (update :analysis merge analysis)
      (update :findings merge findings)
      (assoc :kondo-config config)))

(defn with-kondo-results
  "Update `db` with raw kondo results, which has not yet been normalized."
  [db kondo-results results-config]
  (merge-kondo-results db (normalize-kondo kondo-results results-config)))

(defn with-kondo-for-filename
  "Update `db` with raw kondo result for a single file."
  [db kondo-results filename]
  (let [external? (shared/external-filename? filename (settings/get db [:source-paths]))]
    (with-kondo-results db kondo-results {:external? external?
                                          :ensure-filenames [filename]})))

(defn with-kondo-for-uri
  "Update `db` with raw kondo result for a single uri."
  [db kondo-results uri]
  (with-kondo-for-filename db kondo-results (shared/uri->filename uri)))
