(ns clojure-lsp.db
  (:require
   [clojure.core.async :as async]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [taoensso.timbre :as log]))

(defonce db (atom {:documents {}}))
(defonce diagnostics-chan (async/chan 1))
(defonce edits-chan (async/chan 1))

(def version 1)

(defn ^:private get-sqlite-db-file [project-root]
  (let [configured (some-> (get-in @db [:settings :sqlite-db-path])
                           io/file)
        default (io/file (str project-root) ".lsp" "sqlite.db")
        file (or configured default)]
    (if (.isAbsolute file)
      file
      (io/file (str project-root) file))))

(defn ^:private make-spec [project-root]
  (let [lsp-db (get-sqlite-db-file project-root)]
    {:dbtype "sqlite"
     :dbname (.getAbsolutePath lsp-db)}))

(defn save-deps [project-root project-hash classpath analysis]
  (let [db-spec (make-spec project-root)]
    (io/make-parents (:dbname db-spec))
    (with-open [conn (jdbc/get-connection db-spec)]
      (jdbc/execute! conn ["drop table if exists project;"])
      (jdbc/execute! conn ["create table project (version text, root text unique, hash text, classpath text, analysis text);"])
      (jdbc/execute! conn ["insert or replace into project
                            (version, root, hash, classpath, analysis)
                            values (?,?,?,?,?);" (str version) (str project-root) project-hash (pr-str classpath) (pr-str analysis)]))))

(defn read-deps [project-root]
  (try
    (with-open [conn (jdbc/get-connection (make-spec project-root))]
      (let [project-row
            (-> (jdbc/execute! conn
                                ["select root, hash, classpath, analysis from project where root = ? and version = ?"
                                 (str project-root)
                                 (str version)]
                                {:builder-fn rs/as-unqualified-lower-maps})
                 (nth 0))]
        {:analysis (edn/read-string (:analysis project-row))
         :classpath (edn/read-string (:classpath project-row))
         :project-hash (:hash project-row)}))
    (catch Throwable e
      (log/warn "Could not load db" (.getMessage e)))))
