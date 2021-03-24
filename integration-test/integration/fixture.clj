(ns integration.fixture
  (:require
    [cheshire.core :as json]
    [integration.helper :as h]
    [clojure.java.io :as io]))

(defn ^:private lsp-json-rpc [method params]
  (json/generate-string
    {:jsonrpc "2.0"
     :method method
     :params params
     :id (h/inc-request-id)}))

(defn initialize-request []
  (lsp-json-rpc :initialize
                {:rootUri (str "file://" h/root-project-path)}))

(defn initialized-notification []
  (lsp-json-rpc :initialized {}))

(defn did-open-notification [file-path]
  (let [file (io/file (io/as-relative-path (str "integration-test/sample-test/src/" file-path) #_(str "integration-test/sample-test/src/" file-path)))
        uri (str (io/as-url file))
        text (slurp (.getAbsolutePath file))]
    (lsp-json-rpc :textDocument/didOpen
                  {:textDocument
                   {:uri uri
                    :languageId "clojure"
                    :version 0
                    :text text}})))
