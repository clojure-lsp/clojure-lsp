(ns clojure-lsp.db
  (:require
   [clojure.core.async :as async]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [taoensso.timbre :as log]
   [clojure-lsp.shared :as shared]))

(defonce db (atom {:documents {}}))
(defonce current-changes-chan (async/chan 1))
(defonce diagnostics-chan (async/chan 1))
(defonce edits-chan (async/chan 1))

(def version 1)

(defn ^:private get-sqlite-db-file [project-root-path db]
  (let [configured (or (some-> (get-in @db [:settings :sqlite-db-path])
                               io/file)
                       (some-> (get-in @db [:settings :cache-path])
                               (io/file "sqlite.db")))
        default (io/file (str project-root-path) ".lsp" ".cache" "sqlite.db")]
    ^java.io.File (or configured default)))

(defn ^:private get-sqlite-db-file-path [project-root-path db]
  (let [file (get-sqlite-db-file project-root-path db)]
    (if (.isAbsolute file)
      (.getAbsolutePath file)
      (.getAbsolutePath (io/file (str project-root-path) file)))))

(defn ^:private make-spec [lsp-db-path]
  {:dbtype "sqlite"
   :dbname lsp-db-path})

(defn ^:private migrate-old-db-file!
  [project-root-path]
  (let [old-file (io/file (str project-root-path) ".lsp" "sqlite.db")
        new-file (io/file (str project-root-path) ".lsp" ".cache" "sqlite.db")]
    (when (shared/file-exists? old-file)
      (log/info "Migrating old db file to new .cache folder...")
      (try
        (io/make-parents new-file)
        (io/copy old-file new-file)
        (io/delete-file old-file)
        (catch Exception e
          (log/error "Failed to migrate old db file" e))))))

(defn save-deps! [project-root-path project-hash classpath analysis db]
  (let [lsp-db-path (get-sqlite-db-file-path project-root-path db)
        db-spec (make-spec lsp-db-path)]
    (io/make-parents (:dbname db-spec))
    (with-open [conn (jdbc/get-connection db-spec)]
      (jdbc/execute! conn ["drop table if exists project;"])
      (jdbc/execute! conn ["create table project (version text, root text unique, hash text, classpath text, analysis text);"])
      (jdbc/execute! conn ["insert or replace into project
                            (version, root, hash, classpath, analysis)
                            values (?,?,?,?,?);" (str version) (str project-root-path) project-hash (pr-str classpath) (pr-str analysis)]))))

(defn read-deps [project-root-path db]
  (migrate-old-db-file! project-root-path)
  (let [lsp-db-path (get-sqlite-db-file-path project-root-path db)]
    (try
      (with-open [conn (jdbc/get-connection (make-spec lsp-db-path))]
        (let [project-row
              (-> (jdbc/execute! conn
                                 ["select root, hash, classpath, analysis from project where root = ? and version = ?"
                                  (str project-root-path)
                                  (str version)]
                                 {:builder-fn rs/as-unqualified-lower-maps})
                  (nth 0))]
          {:analysis (edn/read-string (:analysis project-row))
           :classpath (edn/read-string (:classpath project-row))
           :project-hash (:hash project-row)}))
      (catch org.sqlite.SQLiteException _
        (log/warn (format "Could not load project cache from '%s'. This usually happens the first time project is being analyzed." lsp-db-path)))
      (catch Throwable e
        (log/error e (format "Could not load project cache from '%s'" lsp-db-path))))))

(defn db-exists? [project-root-path db]
  (.exists (get-sqlite-db-file project-root-path db)))

(defn remove-db! [project-root-path db]
  (io/delete-file (get-sqlite-db-file project-root-path db)))
