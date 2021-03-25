(ns integration.fixture
  (:require
    [cheshire.core :as json]
    [clojure.java.io :as io]
    [integration.helper :as h]
    [integration.lsp :as lsp]))

(defn ^:private to-file [path]
  (->> path
       (str "integration-test/sample-test/src/")
       io/as-relative-path
       io/file))

(defn ^:private lsp-json-rpc [method params]
  (json/generate-string
    {:jsonrpc "2.0"
     :method method
     :params params
     :id (lsp/inc-request-id)}))

(defn initialize-request []
  (lsp-json-rpc :initialize
                {:rootUri (str (io/as-url (io/file h/root-project-path)))}))

(defn formatting-full-request [path]
  (let [file (to-file path)]
    (lsp-json-rpc :textDocument/formatting
                  {:textDocument {:uri (str (io/as-url file))}
                   :options {:tabSize 2
                             :insertSpaces true}})))

(defn formatting-range-request [path start-row start-col end-row end-col]
  (let [file (to-file path)]
    (lsp-json-rpc :textDocument/rangeFormatting
                  {:textDocument {:uri (str (io/as-url file))}
                   :options {:tabSize 2
                             :insertSpaces true}
                   :range {:start {:line start-row :character start-col}
                           :end {:line end-row :character end-col}}})))

(defn initialized-notification []
  (lsp-json-rpc :initialized {}))

(defn did-open-notification [path]
  (let [file (to-file path)
        uri (str (io/as-url file))
        text (slurp (.getAbsolutePath file))]
    (lsp-json-rpc :textDocument/didOpen
                  {:textDocument
                   {:uri uri
                    :languageId "clojure"
                    :version 0
                    :text text}})))
