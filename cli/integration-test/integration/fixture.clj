(ns integration.fixture
  (:require
   [clojure.java.io :as io]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(defn ^:private lsp-rpc [method params]
  {:jsonrpc "2.0"
   :method method
   :params params
   :id (lsp/inc-request-id)})

(def default-init-options {:lint-project-files-after-startup? false
                           :java false})

(defn initialize-request
  ([]
   (initialize-request {:initializationOptions default-init-options}))
  ([params]
   (lsp-rpc :initialize
            (merge {:rootUri (h/file->uri (io/file h/root-project-path))}
                   params))))

(defn completion-request [path row col]
  (lsp-rpc :textDocument/completion
           {:textDocument {:uri (h/source-path->uri path)}
            :position {:line row :character col}}))

(defn definition-request [path row col]
  (lsp-rpc :textDocument/definition
           {:textDocument {:uri (h/source-path->uri path)}
            :position {:line row :character col}}))

(defn declaration-request [path row col]
  (lsp-rpc :textDocument/declaration
           {:textDocument {:uri (h/source-path->uri path)}
            :position {:line row :character col}}))

(defn implementation-request [path row col]
  (lsp-rpc :textDocument/implementation
           {:textDocument {:uri (h/source-path->uri path)}
            :position {:line row :character col}}))

(defn formatting-full-request [path]
  (lsp-rpc :textDocument/formatting
           {:textDocument {:uri (h/source-path->uri path)}
            :options {:tabSize 2
                      :insertSpaces true}}))

(defn prepare-rename-request [path row col]
  (lsp-rpc :textDocument/prepareRename
           {:textDocument {:uri (h/source-path->uri path)}
            :position {:line row :character col}}))

(defn rename-request [path new-name row col]
  (lsp-rpc :textDocument/rename
           {:textDocument {:uri (h/source-path->uri path)}
            :position {:line row :character col}
            :newName new-name}))

(defn formatting-range-request [path start-row start-col end-row end-col]
  (lsp-rpc :textDocument/rangeFormatting
           {:textDocument {:uri (h/source-path->uri path)}
            :options {:tabSize 2
                      :insertSpaces true}
            :range {:start {:line start-row :character start-col}
                    :end {:line end-row :character end-col}}}))

(defn document-symbol-request [path]
  (lsp-rpc :textDocument/documentSymbol
           {:textDocument {:uri (h/source-path->uri path)}}))

(defn document-highlight-request [path row col]
  (lsp-rpc :textDocument/documentHighlight
           {:textDocument {:uri (h/source-path->uri path)}
            :position {:line row :character col}}))

(defn linked-editing-range-request [path row col]
  (lsp-rpc :textDocument/linkedEditingRange
           {:textDocument {:uri (h/source-path->uri path)}
            :position {:line row :character col}}))

(defn code-action-request [path row col]
  (lsp-rpc :textDocument/codeAction
           {:textDocument {:uri (h/source-path->uri path)}
            :context      {:diagnostics []}
            :range        {:start {:line row :character col}}}))

(defn execute-command-request [command & args]
  (lsp-rpc :workspace/executeCommand
           {:command   command
            :arguments args}))

(defn cursor-info-raw-request [path row col]
  (lsp-rpc "clojure/cursorInfo/raw"
           {:textDocument {:uri (h/source-path->uri path)}
            :position {:line row :character col}}))

(defn clojure-dependency-contents-request [uri]
  (lsp-rpc "clojure/dependencyContents"
           {:uri uri}))

(defn initialized-notification []
  (lsp-rpc :initialized {}))

(defn did-open-notification [path]
  (let [file (h/source-path->file path)
        uri (h/file->uri file)
        text (slurp (.getAbsolutePath file))]
    (lsp-rpc :textDocument/didOpen
             {:textDocument
              {:uri uri
               :languageId "clojure"
               :version 0
               :text text}})))
