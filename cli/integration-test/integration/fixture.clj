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

(def default-init-options {:lint-project-files-after-startup? false})

(defn initialize-request
  ([]
   (initialize-request default-init-options))
  ([settings]
   (initialize-request settings nil))
  ([settings capabilities]
   (lsp-json-rpc :initialize
                 (cond-> {:rootUri (h/file->uri (io/file h/root-project-path))
                          :initializationOptions settings}
                   capabilities (assoc :capabilities capabilities)))))

(defn initialize-emacs-request []
  (initialize-request
   default-init-options
   ;; Emacs' advertised client capabilities
   ;; LSP :: lsp-mode 20220115.1742, Emacs 27.2, darwin
   ;; Derived from a printout of client capabilities that was halfway through
   ;; the json -> java -> coerce process. It was messy. I hope this is accurate.
   ;; Emacs advertises many other capabilities, but this includes only the ones
   ;; that clojure-lsp cares about as of 2022-01-28.
   {:experimental {:testTree true}
    :textDocument {:hover      {:contentFormat ["markdown" "plaintext"]}
                   :completion {:completionItem {:snippetSupport true}}}
    :window       {:showDocument {:support true}}
    :workspace    {:workspaceEdit {:documentChanges true}
                   ;; does not advertise code lens refresh support, though clojure-lsp would use it
                   #_#_:codeLens  {:refreshSupport true}}}))

(defn definition-request [path row col]
  (lsp-json-rpc :textDocument/definition
                {:textDocument {:uri (h/source-path->uri path)}
                 :position {:line row :character col}}))

(defn declaration-request [path row col]
  (lsp-json-rpc :textDocument/declaration
                {:textDocument {:uri (h/source-path->uri path)}
                 :position {:line row :character col}}))

(defn formatting-full-request [path]
  (lsp-json-rpc :textDocument/formatting
                {:textDocument {:uri (h/source-path->uri path)}
                 :options {:tabSize 2
                           :insertSpaces true}}))

(defn rename-request [path new-name row col]
  (lsp-json-rpc :textDocument/rename
                {:textDocument {:uri (h/source-path->uri path)}
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
                {:textDocument {:uri (h/source-path->uri path)}}))

(defn document-highlight-request [path row col]
  (lsp-json-rpc :textDocument/documentHighlight
                {:textDocument {:uri (h/source-path->uri path)}
                 :position {:line row :character col}}))

(defn linked-editing-range-request [path row col]
  (lsp-json-rpc :textDocument/linkedEditingRange
                {:textDocument {:uri (h/source-path->uri path)}
                 :position {:line row :character col}}))

(defn code-action-request [path row col]
  (lsp-json-rpc :textDocument/codeAction
                {:textDocument {:uri (h/source-path->uri path)}
                 :context      {:diagnostics []}
                 :range        {:start {:line row :character col}}}))

(defn execute-command-request [command & args]
  (lsp-json-rpc :workspace/executeCommand
                {:command   command
                 :arguments args}))

(defn cursor-info-raw-request [path row col]
  (lsp-json-rpc "clojure/cursorInfo/raw"
                {:textDocument {:uri (h/source-path->uri path)}
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
