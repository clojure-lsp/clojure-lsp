(ns integration.fixture
  (:require
    [cheshire.core :as json]
    [clojure.java.io :as io]
    [integration.helper :as h]
    [integration.lsp :as lsp]
    [clojure.string :as str]))

(defn ^:private to-source-file [path]
  (->> path
       (str "integration-test/sample-test/src/")
       io/as-relative-path
       io/file))

(defn ^:private file->uri [file]
  (let [path (.getAbsolutePath file)]
    (if (str/starts-with? path "/")
      (str "file://" path)
      (str "file:///" path))))

(defn ^:private lsp-json-rpc [method params]
  (json/generate-string
    {:jsonrpc "2.0"
     :method method
     :params params
     :id (lsp/inc-request-id)}))

(defn initialize-request []
  (lsp-json-rpc :initialize
                {:rootUri (file->uri (io/file h/root-project-path))}))

(defn formatting-full-request [path]
  (let [file (to-source-file path)]
    (lsp-json-rpc :textDocument/formatting
                  {:textDocument {:uri (file->uri file)}
                   :options {:tabSize 2
                             :insertSpaces true}})))

(defn rename-request [path new-name row col]
  (let [file (to-source-file path)]
    (lsp-json-rpc :textDocument/rename
                  {:textDocument {:uri (file->uri file)}
                   :position {:line row :character col}
                   :newName new-name})))

(defn formatting-range-request [path start-row start-col end-row end-col]
  (let [file (to-source-file path)]
    (lsp-json-rpc :textDocument/rangeFormatting
                  {:textDocument {:uri (file->uri file)}
                   :options {:tabSize 2
                             :insertSpaces true}
                   :range {:start {:line start-row :character start-col}
                           :end {:line end-row :character end-col}}})))

(defn initialized-notification []
  (lsp-json-rpc :initialized {}))

(defn did-open-notification [path]
  (let [file (to-source-file path)
        uri (file->uri file)
        text (slurp (.getAbsolutePath file))]
    (lsp-json-rpc :textDocument/didOpen
                  {:textDocument
                   {:uri uri
                    :languageId "clojure"
                    :version 0
                    :text text}})))
