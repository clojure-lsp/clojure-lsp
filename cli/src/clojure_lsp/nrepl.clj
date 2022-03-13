(ns clojure-lsp.nrepl
  (:require
   [borkdude.dynaload :refer [dynaload]]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(def start-server (dynaload 'nrepl.server/start-server))

(def cider-nrepl-handler (dynaload 'cider.nrepl/cider-nrepl-handler))

(defn ^:private repl-port []
  (:port (start-server :feature-handler cider-nrepl-handler)))

(defn setup-nrepl [db]
  (try
    (when-let [port (repl-port)]
      (log/info "====== LSP nrepl server started on port" port)
      (swap! db assoc :port port))
    (catch Throwable _
      (log/debug "nrepl not found, skipping nrepl server start..."))))
