(ns clojure-lsp.db
  (:require
   [clojure-lsp.settings :as settings]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [cognitect.transit :as transit]
   [clojure-lsp.shared :as shared]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(def initial-db {:documents {}
                 :processing-changes #{}})
(defonce db (atom initial-db))
(defonce current-changes-chan (async/chan 1))
(defonce diagnostics-chan (async/chan 1))
(defonce created-watched-files-chan (async/chan 1))
(defonce edits-chan (async/chan 1))

(def version 1)

(defn ^:private sqlite-db-file [project-root]
  (io/file (str project-root) ".lsp" ".cache" "sqlite.db"))

(defn ^:private datalevin-db-files [project-root-path db]
  (let [configured (some-> (settings/get db [:cache-path])
                           io/file)
        default (io/file (str project-root-path) ".lsp" ".cache")
        cache-dir ^java.io.File (or configured default)]
    [(io/file cache-dir "data.mdb")
     (io/file cache-dir "lock.mdb")]))

(defn ^:private transit-db-file [project-root db]
  (let [overwritten-path (some-> (settings/get db [:cache-path])
                                 io/file)
        default (io/file (str project-root) ".lsp" ".cache" "db.transit.json")]
    ^java.io.File (or overwritten-path default)))

(defn ^:private remove-old-sqlite-db-file! [project-root-path]
  (let [old-db-file (sqlite-db-file project-root-path)]
    (when (shared/file-exists? old-db-file)
      (io/delete-file old-db-file true))))

(defn ^:private remove-old-datalevin-db-file! [project-root-path]
  (->> (datalevin-db-files project-root-path db)
       (filter shared/file-exists?)
       (mapv #(io/delete-file % true))))

(defn db-exists? [project-root-path db]
  (shared/file-exists? (transit-db-file project-root-path db)))

(defn remove-db! [project-root-path db]
  (io/delete-file (transit-db-file project-root-path db)))

(defn upsert-cache! [{:keys [project-root] :as project-cache} db]
  (remove-old-sqlite-db-file! project-root)
  (remove-old-datalevin-db-file! project-root)
  (try
    (shared/logging-time
      "Upserting transit analysis cache took %s secs"
      (let [cache-file (transit-db-file project-root db)]
        (with-open [;; first we write to a baos as a workaround for transit-clj #43
                    bos (java.io.ByteArrayOutputStream. 1024)
                    os (io/output-stream bos)]
          (let [writer (transit/writer os :json)]
            (io/make-parents cache-file)
            (transit/write writer project-cache)
            (io/copy (.toByteArray bos) cache-file)))))
    (catch Throwable e
      (log/error "Could not upsert db cache" e))))

(defn read-cache [project-root db]
  (try
    (shared/logging-time
      "Reading transit analysis cache from db took %s secs"
      (let [db-file (transit-db-file project-root db)]
        (if (shared/file-exists? db-file)
          (let [project-analysis (with-open [is (io/input-stream db-file)]
                                   (transit/read (transit/reader is :json)))]
            (when (and (= (str project-root) (:project-root project-analysis))
                       (= version (:version project-analysis)))
              project-analysis))
          (log/error "No cache DB file found"))))
    (catch Throwable e
      (log/error "Could not load project cache from DB" e))))
