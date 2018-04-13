(ns clojure-lsp.db
  (:require
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [clojure.java.io :as io]
   [jdbc.core :as jdbc]))

(defonce db (atom {:documents {}}))

(defn make-spec [project-root]
  (let [lsp-db (io/file project-root ".lsp" "sqlite.db")]
    {:subprotocol "sqlite"
     :subname (.getAbsolutePath lsp-db)}))

(defn save-db! [project-root]
  (let [db-spec (make-spec project-root)]
    (io/make-parents (:subname db-spec))
    (with-open [conn (jdbc/connection db-spec)]
      (jdbc/execute conn "drop table if exists project;")
      (jdbc/execute conn "create table project (root text unique, db text);")
      (jdbc/execute conn ["insert or replace into project (root, db) values (?,?);" project-root (pr-str @db)]))))

(defn load-db! [project-root]
  (try
    (with-open [conn (jdbc/connection (make-spec project-root))]
      (->> (jdbc/fetch conn ["select root, db from project where root = ?" project-root])
           (first)
           (:db)
           (edn/read-string)
           (reset! db)))
    (catch Exception e
      (log/warn "Could not load db" (.getMessage e)))))

(comment
  (do
    (save-db! "/Users/case/dev/lsp")
    (load-db! "/Users/case/dev/lsp")
    ))
