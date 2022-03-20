(ns clojure-lsp.nrepl
  (:require
   [borkdude.dynaload :refer [dynaload]]
   [lsp4clj.protocols.logger :as logger]))

(set! *warn-on-reflection* true)

(def start-server (dynaload 'nrepl.server/start-server))

(def cider-nrepl-handler (dynaload 'cider.nrepl/cider-nrepl-handler))

(defn ^:private repl-port []
  (:port (start-server :feature-handler cider-nrepl-handler)))

(defn setup-nrepl [db]
  (try
    (when-let [port (repl-port)]
      (logger/info (:logger @db) "====== LSP nrepl server started on port" port)
      (swap! db assoc :port port))
    (catch Throwable _
      (logger/debug (:logger @db) "nrepl not found, skipping nrepl server start..."))))
