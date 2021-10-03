(ns clojure-lsp.db
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [datalevin.core :as d]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(def analysis-table-name "analysis-table")

(defonce db (atom {:documents {}}))
(defonce current-changes-chan (async/chan 1))
(defonce diagnostics-chan (async/chan 1))
(defonce edits-chan (async/chan 1))

(def version 1)

(defn ^:private get-sqlite-db-file [project-root]
  (io/file (str project-root) ".lsp" ".cache" "sqlite.db"))

(defn ^:private get-datalevin-db-file [project-root-path db]
  (let [configured (some-> (get-in @db [:settings :cache-path])
                           io/file)
        default (io/file (str project-root-path) ".lsp" ".cache")]
    ^java.io.File (or configured default)))

(defn ^:private get-db-file-path [project-root-path db]
  (let [file ^java.io.File (get-datalevin-db-file project-root-path db)]
    (if (.isAbsolute file)
      (.getAbsolutePath file)
      (.getAbsolutePath (io/file (str project-root-path) file)))))

(defn make-db [project-root db]
  (d/open-kv (get-db-file-path project-root db)))

(defn ^:private remove-old-db-file! [project-root-path]
  (let [old-db-file (get-sqlite-db-file project-root-path)]
    (when (shared/file-exists? old-db-file)
      (io/delete-file old-db-file))))

(defn save-deps! [project-root-path project-hash kondo-config-hash classpath analysis db]
  (remove-old-db-file! project-root-path)
  (shared/logging-time
    "DB datalevin save took %s secs"
    (let [datalevin-db (make-db project-root-path db)]
      (d/open-dbi datalevin-db analysis-table-name)
      (d/transact-kv datalevin-db [[:del analysis-table-name :project-analysis]])
      (d/transact-kv datalevin-db [[:put analysis-table-name :project-analysis {:version version
                                                                                :project-root (str project-root-path)
                                                                                :project-hash project-hash
                                                                                :kondo-config-hash kondo-config-hash
                                                                                :classpath classpath
                                                                                :analysis analysis}]])
      (d/close-kv datalevin-db))))

(defn read-deps [project-root-path db]
  (try
    (shared/logging-time
      "DB datalevin read took %s secs"
      (let [db (make-db project-root-path db)]
        (d/open-dbi db analysis-table-name)
        (when-let [project-analysis (d/get-value db analysis-table-name :project-analysis)]
          (when (and (= (str project-root-path) (:project-root project-analysis))
                     (= version (:version project-analysis)))
            project-analysis))))
    (catch Throwable e
      (log/error "Could not load project cache from DB" e))))

(defn db-exists? [project-root-path db]
  (shared/file-exists? (get-datalevin-db-file project-root-path db)))

(defn remove-db! [project-root-path db]
  (io/delete-file (get-datalevin-db-file project-root-path db)))
