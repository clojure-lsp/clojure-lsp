(ns clojure-lsp.nrepl
  (:require
   [borkdude.dynaload :refer [dynaload]]
   [clojure-lsp.db :as db]
   [taoensso.timbre :as log]))

(def start-server (dynaload 'nrepl.server/start-server))

(defn ^:private find-dot-nrepl-port-file []
  (try
    (slurp ".nrepl-port")
    (catch Exception _)))

(defn ^:private repl-port []
  (or (find-dot-nrepl-port-file)
      (:port (start-server))))

(defn setup-nrepl []
  (try
    (when-let [port (repl-port)]
      (log/info "====== LSP nrepl server started on port" port)
      (swap! db/db assoc :port port))
    (catch Throwable _
      (log/debug "nrepl not found, skipping nrepl server start..."))))
