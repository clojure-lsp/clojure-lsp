(ns clojure-lsp.nrepl
  (:require
   [borkdude.dynaload :refer [dynaload]]
   [taoensso.timbre :as log]))

(def start-server (dynaload 'nrepl.server/start-server))

(defn ^:private repl-port []
  (:port (start-server)))

(defn setup-nrepl [db]
  (try
    (when-let [port (repl-port)]
      (log/info "====== LSP nrepl server started on port" port)
      (swap! db assoc :port port))
    (catch Throwable _
      (log/debug "nrepl not found, skipping nrepl server start..."))))
