(ns clojure-lsp.nrepl
  (:require
   [borkdude.dynaload :refer [dynaload]]
   [cider.nrepl :refer [cider-nrepl-handler]]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(def start-server (dynaload 'nrepl.server/start-server))

(defn ^:private repl-port []
  (:port (start-server :handler cider-nrepl-handler)))

(defn setup-nrepl [db]
  (try
    (when-let [port (repl-port)]
      (log/info "====== LSP nrepl server started on port" port)
      (swap! db assoc :port port))
    (catch Throwable _
      (log/debug "nrepl not found, skipping nrepl server start..."))))
