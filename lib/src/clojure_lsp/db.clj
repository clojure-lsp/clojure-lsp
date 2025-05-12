(ns clojure-lsp.db
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [cognitect.transit :as transit])
  (:import
   [java.io IOException OutputStream]))

(set! *warn-on-reflection* true)

(def ^:private db-logger-tag "[DB]")

(def initial-db {:documents {}
                 :dep-graph {}
                 :diagnostics {:clj-kondo nil
                               :clj-depend nil
                               :custom nil}})
(defonce db* (atom initial-db))

(def version 12)

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

(defn ^:private remove-old-datalevin-db-file! [db]
  (->> (datalevin-db-files db)
       (filter shared/file-exists?)
       (mapv #(io/delete-file % true))))

(defn db-exists? [db]
  (shared/file-exists? (transit-local-db-file db)))

(defn remove-db! [db]
  (try
    (io/delete-file (transit-local-db-file db))
    (catch IOException _ nil)))

(defn ^:private no-flush-output-stream [^OutputStream os]
  (proxy [java.io.BufferedOutputStream] [os]
    (flush [])
    (close []
      (let [^java.io.BufferedOutputStream this this]
        (proxy-super flush)
        (proxy-super close)))))

(defn ^:private upsert-cache! [cache cache-file]
  (try
    (shared/logging-task
      :db/manual-gc-before-update-db
      ;; Reduce a little bit Out of memory exceptions when writing the cache for huge caches.
      (System/gc))
    (shared/logging-task
      :db/upsert-cache
      (io/make-parents cache-file)
      ;; https://github.com/cognitect/transit-clj/issues/43
      (with-open [os ^OutputStream (no-flush-output-stream (io/output-stream cache-file))]
        (let [writer (transit/writer os :json)]
          (transit/write writer cache))))
    (catch Throwable e
      (logger/error db-logger-tag (str "Could not upsert db cache to " cache-file) e))))

(defn ^:private read-cache [cache-file]
  (try
    (shared/logging-task
      :db/read-cache
      (if (shared/file-exists? cache-file)
        (let [cache (with-open [is (io/input-stream cache-file)]
                      (transit/read (transit/reader is :json)))]
          (when (= version (:version cache))
            cache))
        (logger/error db-logger-tag (str "No cache DB file found for " cache-file))))
    (catch Throwable e
      (logger/error db-logger-tag "Could not load global cache from DB" e))))

(defn upsert-local-cache! [{:keys [project-root] :as project-cache} db]
  (remove-old-sqlite-db-file! project-root)
  (remove-old-datalevin-db-file! db)
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
  (let [global-cache (read-cache (transit-global-db-file))]
    (when (= version (:version global-cache))
      global-cache)))

(defn read-and-update-global-cache! [db-change-fn]
  (-> (read-global-cache)
      (db-change-fn)
      (upsert-global-cache!)))
