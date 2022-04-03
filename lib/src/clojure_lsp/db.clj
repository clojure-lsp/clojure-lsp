(ns clojure-lsp.db
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
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

(def version 1)

(defn ^:private sqlite-db-file [project-root]
  (io/file (str project-root) ".lsp" ".cache" "sqlite.db"))

(defn ^:private datalevin-db-files [db]
  (let [cache-dir ^java.io.File (config/cache-file db)]
    [(io/file cache-dir "data.mdb")
     (io/file cache-dir "lock.mdb")]))

(defn ^:private transit-db-file [db]
  (io/file (config/cache-file db) "db.transit.json"))

(defn ^:private remove-old-sqlite-db-file! [project-root-path]
  (let [old-db-file (sqlite-db-file project-root-path)]
    (when (shared/file-exists? old-db-file)
      (io/delete-file old-db-file true))))

(defn ^:private remove-old-datalevin-db-file! []
  (->> (datalevin-db-files @db*)
       (filter shared/file-exists?)
       (mapv #(io/delete-file % true))))

(defn db-exists? [db]
  (shared/file-exists? (transit-db-file db)))

(defn remove-db! [db]
  (io/delete-file (transit-db-file db)))

(defn upsert-cache! [{:keys [project-root] :as project-cache} db]
  (remove-old-sqlite-db-file! project-root)
  (remove-old-datalevin-db-file!)
  (try
    (shared/logging-time
      (str db-logger-tag " Upserting transit analysis cache took %s")
      (let [cache-file (transit-db-file db)]
        (with-open [;; first we write to a baos as a workaround for transit-clj #43
                    bos (java.io.ByteArrayOutputStream. 1024)
                    os (io/output-stream bos)]
          (let [writer (transit/writer os :json)]
            (io/make-parents cache-file)
            (transit/write writer project-cache)
            (io/copy (.toByteArray bos) cache-file)))))
    (catch Throwable e
      (logger/error db-logger-tag "Could not upsert db cache" e))))

(defn read-cache [project-root db]
  (try
    (shared/logging-time
      (str db-logger-tag " Reading transit analysis cache from db took %s")
      (let [db-file (transit-db-file db)]
        (if (shared/file-exists? db-file)
          (let [project-analysis (with-open [is (io/input-stream db-file)]
                                   (transit/read (transit/reader is :json)))]
            (when (and (= (str project-root) (:project-root project-analysis))
                       (= version (:version project-analysis)))
              project-analysis))
          (logger/error db-logger-tag "No cache DB file found"))))
    (catch Throwable e
      (logger/error db-logger-tag "Could not load project cache from DB" e))))
