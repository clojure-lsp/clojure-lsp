(ns integration.fixture
  (:require
   [clojure.java.io :as io]
   [integration.helper :as h]))

(def default-init-options {:lint-project-files-after-startup? false
                           :java false})

(defn initialize-request
  ([]
   (initialize-request {:initializationOptions default-init-options}))
  ([params]
   (initialize-request params (h/file->uri (io/file h/root-project-path))))
  ([params root-uri]
   [:initialize
    (merge (if root-uri {:rootUri root-uri} {})
           params)]))

(defn shutdown-request
  []
  [:shutdown {}])

(defn completion-request [path line character]
  [:textDocument/completion
   {:textDocument {:uri (h/source-path->uri path)}
    :position {:line line :character character}}])

(defn completion-item-resolve-request [params]
  [:completionItem/resolve
   params])

(defn definition-request [uri line character]
  [:textDocument/definition
   {:textDocument {:uri uri}
    :position {:line line :character character}}])

(defn declaration-request [path line character]
  [:textDocument/declaration
   {:textDocument {:uri (h/source-path->uri path)}
    :position {:line line :character character}}])

(defn implementation-request [path line character]
  [:textDocument/implementation
   {:textDocument {:uri (h/source-path->uri path)}
    :position {:line line :character character}}])

(defn formatting-full-request [path]
  [:textDocument/formatting
   {:textDocument {:uri (h/source-path->uri path)}
    :options {:tabSize 2
              :insertSpaces true}}])

(defn prepare-rename-request [path line character]
  [:textDocument/prepareRename
   {:textDocument {:uri (h/source-path->uri path)}
    :position {:line line :character character}}])

(defn rename-request [path new-name line character]
  [:textDocument/rename
   {:textDocument {:uri (h/source-path->uri path)}
    :position {:line line :character character}
    :newName new-name}])

(defn formatting-range-request [path start-line start-character end-line end-character]
  [:textDocument/rangeFormatting
   {:textDocument {:uri (h/source-path->uri path)}
    :options {:tabSize 2
              :insertSpaces true}
    :range {:start {:line start-line :character start-character}
            :end {:line end-line :character end-character}}}])

(defn document-symbol-request [path]
  [:textDocument/documentSymbol
   {:textDocument {:uri (h/source-path->uri path)}}])

(defn document-highlight-request [path line character]
  [:textDocument/documentHighlight
   {:textDocument {:uri (h/source-path->uri path)}
    :position {:line line :character character}}])

(defn linked-editing-range-request [path line character]
  [:textDocument/linkedEditingRange
   {:textDocument {:uri (h/source-path->uri path)}
    :position {:line line :character character}}])

(defn code-action-request [path line character]
  [:textDocument/codeAction
   {:textDocument {:uri (h/source-path->uri path)}
    :context      {:diagnostics []}
    :range        {:start {:line line :character character}}}])

(defn hover-external-uri-request [uri line character]
  [:textDocument/hover
   {:textDocument {:uri uri}
    :position     {:line line :character character}}])

(defn hover-source-path-request [path line character]
  [:textDocument/hover
   {:textDocument {:uri (h/source-path->uri path)}
    :position     {:line line :character character}}])

(defn execute-command-request [command & args]
  [:workspace/executeCommand
   {:command   command
    :arguments args}])

(defn cursor-info-raw-request [path line character]
  ["clojure/cursorInfo/raw"
   {:textDocument {:uri (h/source-path->uri path)}
    :position {:line line :character character}}])

(defn clojure-dependency-contents-request [uri]
  ["clojure/dependencyContents"
   {:uri uri}])

(defn initialized-notification []
  [:initialized {}])

(defn ^:private did-open-notification [uri text]
  [:textDocument/didOpen
   {:textDocument
    {:uri uri
     :languageId "clojure"
     :version 0
     :text text}}])

(defn did-open-external-path-notification [uri text]
  (did-open-notification uri text))

(defn did-open-source-path-notification [path]
  (let [file (h/source-path->file path)]
    (did-open-notification (h/file->uri file) (slurp (.getAbsolutePath file)))))

(defn did-change-notification [path version changes]
  [:textDocument/didChange
   {:textDocument {:uri (h/source-path->uri path)
                   :version version}
    :contentChanges (map (fn [[text start-line start-character end-line end-character]]
                           {:text text
                            :range {:start {:line start-line :character start-character}
                                    :end {:line end-line :character end-character}}})
                         changes)}])
