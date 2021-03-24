(ns integration.fixture
  (:require
    [cheshire.core :as json]
    [integration.helper :as h]))

(defonce request-id (atom 0))

(defn ^:private inc-id []
  (swap! request-id inc))

(defn initialize-request []
  (json/generate-string
    {:jsonrpc "2.0"
     :method :initialize
     :params {:rootUri (str "file://" h/root-project-path)}
     :id (inc-id)}))

(defn initialized-request []
  (json/generate-string
    {:jsonrpc "2.0"
     :method :initialized
     :params {}
     :id (inc-id)}))
