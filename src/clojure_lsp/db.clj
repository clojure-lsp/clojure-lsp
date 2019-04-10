(ns clojure-lsp.db
  (:require
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [clojure.java.io :as io]
   [jdbc.core :as jdbc]
   [clojure.core.async :as async]))

(defonce db (atom {:documents {}}))
(defonce diagnostics-chan (async/chan 1))
(defonce edits-chan (async/chan 1))

(def version 1)

(defn make-spec [project-root]
  (let [lsp-db (io/file (str project-root) ".lsp" "sqlite.1.db")]
    {:subprotocol "sqlite"
     :subname (.getAbsolutePath lsp-db)}))

(defn save-deps [project-root project-hash classpath jar-envs]
  (let [db-spec (make-spec project-root)]
    (io/make-parents (:subname db-spec))
    (with-open [conn (jdbc/connection db-spec)]
      (jdbc/execute conn "drop table if exists project;")
      (jdbc/execute conn "create table project (version text, root text unique, hash text, classpath text, jar_envs text);")
      (jdbc/execute conn ["insert or replace into project
                          (version, root, hash, classpath, jar_envs)
                          values (?,?,?,?,?);" (str version) (str project-root) project-hash (pr-str classpath) (pr-str jar-envs)]))))

(defn read-deps [project-root]
  (try
    (with-open [conn (jdbc/connection (make-spec project-root))]
      (let [project-row
            (->> (jdbc/fetch conn ["select root, hash, classpath, jar_envs from project where root = ? and version = ?" (str project-root) (str version)])
                 (first))]
        {:jar-envs (edn/read-string (:jar_envs project-row))
         :classpath (edn/read-string (:classpath project-row))
         :project-hash (:hash project-row)}))
    (catch Exception e
      (log/warn "Could not load db" (.getMessage e)))))

(comment
  (do
    (save-db! "/Users/case/dev/lsp")
    (load-db! "/Users/case/dev/lsp")
    (:jar-envs @db)))
