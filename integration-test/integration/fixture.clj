(ns integration.fixture
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(defn ^:private lsp-json-rpc [method params]
  (json/generate-string
    {:jsonrpc "2.0"
     :method method
     :params params
     :id (lsp/inc-request-id)}))

(defn initialize-request
  ([]
   (initialize-request {:lint-project-files-after-startup? false}))
  ([settings]
   (lsp-json-rpc :initialize
                 {:rootUri (h/file->uri (io/file h/root-project-path))
                  :initializationOptions settings})))

(defn definition-request [path row col]
  (lsp-json-rpc :textDocument/definition
                {:textDocument {:uri (h/file->uri (h/source-path->file path))}
                 :position {:line row :character col}}))

(defn formatting-full-request [path]
  (lsp-json-rpc :textDocument/formatting
                {:textDocument {:uri (h/source-path->uri path)}
                 :options {:tabSize 2
                           :insertSpaces true}}))

(defn rename-request [path new-name row col]
  (lsp-json-rpc :textDocument/rename
                {:textDocument {:uri (h/file->uri (h/source-path->file path))}
                 :position {:line row :character col}
                 :newName new-name}))

(defn formatting-range-request [path start-row start-col end-row end-col]
  (lsp-json-rpc :textDocument/rangeFormatting
                {:textDocument {:uri (h/source-path->uri path)}
                 :options {:tabSize 2
                           :insertSpaces true}
                 :range {:start {:line start-row :character start-col}
                         :end {:line end-row :character end-col}}}))

(defn document-symbol-request [path]
  (lsp-json-rpc :textDocument/documentSymbol
                {:textDocument {:uri (h/file->uri (h/source-path->file path))}}))

(defn document-highlight-request [path row col]
  (lsp-json-rpc :textDocument/documentHighlight
                {:textDocument {:uri (h/file->uri (h/source-path->file path))}
                 :position {:line row :character col}}))

(defn linked-editing-range-request [path row col]
  (lsp-json-rpc :textDocument/linkedEditingRange
                {:textDocument {:uri (h/file->uri (h/source-path->file path))}
                 :position {:line row :character col}}))

(defn initialized-notification []
  (lsp-json-rpc :initialized {}))

(defn did-open-notification [path]
  (let [file (h/source-path->file path)
        uri (h/file->uri file)
        text (slurp (.getAbsolutePath file))]
    (lsp-json-rpc :textDocument/didOpen
                  {:textDocument
                   {:uri uri
                    :languageId "clojure"
                    :version 0
                    :text text}})))
