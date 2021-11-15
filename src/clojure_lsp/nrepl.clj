(ns clojure-lsp.nrepl
  (:require
   [borkdude.dynaload :refer [dynaload]]
   [nrepl.core :as nrepl]
   [nrepl.server :as nrepl.server]
   [taoensso.timbre :as log])
  (:import
   (java.io Closeable)))

(set! *warn-on-reflection* true)

(def cider-nrepl-handler (dynaload 'cider.nrepl/cider-nrepl-handler))

(defn ^:private start-nrepl []
  (nrepl.server/start-server :handler cider-nrepl-handler))

(defn ^:private find-dot-nrepl-port-file []
  (try
    (some-> (slurp ".nrepl-port")
            Integer/parseInt)
    (catch Exception _)))

(defn test-eval [db]
  (try
    (with-open [conn ^nrepl.transport.FnTransport (nrepl/connect :port ^Integer (find-dot-nrepl-port-file))]
      (-> (nrepl/client conn 1000)
          (nrepl/message {:op "eval" :code "(+ 2 5)"})
          nrepl/response-values))
    (catch Exception e
      (log/error "-->" e)
      e)))

(defn setup-dev-nrepl [db]
  (try
    (when-let [port (:port (nrepl.server/start-server))]
      (log/info "====== LSP nrepl server started on port" port)
      (swap! db assoc :port port))
    (catch Throwable _
      (log/debug "nrepl not found, skipping nrepl server start..."))))
