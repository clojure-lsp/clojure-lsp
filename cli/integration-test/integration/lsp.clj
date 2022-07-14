(ns integration.lsp
  (:require
   [babashka.process :as p]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.test :refer [use-fixtures]]
   [integration.client :as client]
   [integration.fixture :as fixture]))

(def ^:dynamic *clojure-lsp-process* nil)
(def ^:dynamic *mock-client* nil)

(defn start-server
  ([binary]
   (start-server binary []))
  ([binary args]
   (p/process (into [(.getCanonicalPath (io/file binary))] args)
              {:dir "integration-test/sample-test/"})))

(defn start-process! []
  (let [server (start-server (first *command-line-args*))
        client (client/client (:in server) (:out server))]
    (client/start client nil)
    (async/go-loop []
      (when-let [log (async/<! (:log-ch client))]
        (println log)
        (recur)))
    (alter-var-root #'*clojure-lsp-process* (constantly server))
    (alter-var-root #'*mock-client* (constantly client))))

(defn cli! [& args]
  (let [server (start-server (first *command-line-args*) args)]
    (alter-var-root #'*clojure-lsp-process* (constantly server))
    (io/reader (:out server))))

(defn clean! []
  (flush)
  (some-> *mock-client* client/shutdown)
  (some-> *clojure-lsp-process* deref) ;; wait for shutdown of client to shutdown server
  (some-> *mock-client* client/exit)
  (alter-var-root #'*clojure-lsp-process* (constantly nil))
  (alter-var-root #'*mock-client* (constantly nil)))

(defn clean-after-test []
  (use-fixtures :each (fn [f] (clean!) (f)))
  (use-fixtures :once (fn [f] (f) (clean!))))

(defn notify! [[method body]]
  (client/send-notification *mock-client* method body))

(defn request! [[method body]]
  (client/request-and-await-server-response! *mock-client* method body))

(defn client-awaits-server-diagnostics [path]
  (client/await-server-diagnostics *mock-client* path))

(defn client-awaits-server-notification [method]
  (client/await-server-notification *mock-client* method))

(defn client-awaits-server-request [method]
  (client/await-server-request *mock-client* method))

(defn client-awaits-open-diagnostics [path]
  (notify! (fixture/did-open-notification path))
  (client-awaits-server-diagnostics path))

(defn mock-response [method resp]
  (client/mock-response *mock-client* method resp))
