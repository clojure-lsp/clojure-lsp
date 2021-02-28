(ns clojure-lsp.db
  (:require
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [datalevin.lmdb :as l]
   [datalevin.binding.java]
   [datalevin.binding.graal]
   [taoensso.timbre :as log]))

(defonce db (atom {:documents {}}))
(defonce current-changes-chan (async/chan 1))
(defonce diagnostics-chan (async/chan 1))
(defonce edits-chan (async/chan 1))

(def version 1)
(def table "analysis-table")

(defn ^:private get-db-file-path [project-root]
  (let [configured (some-> (get-in @db [:settings :db-path])
                           io/file)
        default (io/file (str project-root) ".lsp" "db")
        file (or configured default)]
    (if (.isAbsolute file)
      (.getAbsolutePath file)
      (.getAbsolutePath (io/file (str project-root) file)))))

(defn make-db [project-root]
  (l/open-kv (get-db-file-path project-root)))

(defn save-deps [project-root project-hash classpath analysis]
  (let [db (make-db project-root)]
    (l/open-dbi db table)
    (l/transact-kv db [[:del table :project-analysis]])
    (l/transact-kv db [[:put table :project-analysis {:version      version
                                                      :project-root (str project-root)
                                                      :project-hash project-hash
                                                      :classpath    classpath
                                                      :analysis     analysis}]])
    (l/close-kv db)))

(defn read-deps [project-root]
  (let [db (make-db project-root)]
    (l/open-dbi db table)
    (when-let [project-analysis (l/get-value db table :project-analysis)]
      (when (and (= (str project-root) (:project-root project-analysis))
                 (= version (:version project-analysis)))
        project-analysis))))
