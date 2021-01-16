(ns clojure-lsp.main
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.feature.semantic-tokens :as semantic-tokens]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.interop :as interop]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.tools.logging :as log]
   [nrepl.server :as nrepl.server]
   [trptcolin.versioneer.core :as version]
   [clojure-lsp.producer :as producer])
  (:import
   (clojure_lsp ClojureExtensions)
   (org.eclipse.lsp4j
     CallHierarchyIncomingCallsParams
     CallHierarchyPrepareParams
     CodeActionParams
     CodeAction
     CodeActionOptions
     CodeLens
     CodeLensParams
     CodeLensOptions
     CompletionItem
     CompletionOptions
     CompletionParams
     DefinitionParams
     DidChangeConfigurationParams
     DidChangeTextDocumentParams
     DidChangeWatchedFilesParams
     DidChangeWatchedFilesRegistrationOptions
     DidCloseTextDocumentParams
     DidOpenTextDocumentParams
     DidSaveTextDocumentParams
     DocumentFormattingParams
     DocumentHighlightParams
     DocumentRangeFormattingParams
     DocumentSymbolParams
     ExecuteCommandOptions
     ExecuteCommandParams
     FileSystemWatcher
     HoverParams
     InitializeParams
     InitializeResult
     InitializedParams
     ParameterInformation
     ReferenceParams
     Registration
     RegistrationParams
     RenameParams
     SaveOptions
     SemanticTokensLegend
     SemanticTokensParams
     SemanticTokensRangeParams
     SemanticTokensWithRegistrationOptions
     ServerCapabilities
     SignatureHelp
     SignatureHelpParams
     SignatureInformation
     TextDocumentContentChangeEvent
     TextDocumentSyncKind
     TextDocumentSyncOptions
     WorkspaceSymbolParams)
   (org.eclipse.lsp4j.launch LSPLauncher)
   (org.eclipse.lsp4j.services LanguageServer TextDocumentService WorkspaceService LanguageClient)
   (java.util.concurrent CompletableFuture)
   (java.util.function Supplier))
  (:gen-class))

(defonce formatting (atom false))

(defonce status (atom {}))

(defmacro go [id & body]
  `(let [~'_start-time (System/nanoTime)
         ~'_id ~id]
     (swap! status update ~id (fnil conj #{}) ~'_start-time)
     (do ~@body)))

(defmacro end [expr]
  `(try
     ~expr
     (catch Throwable ex#
       (log/error ex#))
     (finally
       (try
         (swap! status update ~'_id disj ~'_start-time)
         (let [duration# (quot (- (System/nanoTime) ~'_start-time) 1000000)
               running# (filter (comp seq val) @status)]
           (when (or (> duration# 100) (seq running#))
             (log/debug ~'_id duration# running#)))
         (catch Throwable ex#
           (log/error ex#))))))

(deftype LSPTextDocumentService []
  TextDocumentService
  (^void didOpen [_ ^DidOpenTextDocumentParams params]
    (go :didOpen
        (end
          (let [document (.getTextDocument params)]
            (#'handlers/did-open (interop/document->decoded-uri document) (.getText document))))))

  (^void didChange [_ ^DidChangeTextDocumentParams params]
    (go :didChange
        (end
          (let [textDocument (.getTextDocument params)
                version (.getVersion textDocument)
                changes (.getContentChanges params)
                text (.getText ^TextDocumentContentChangeEvent (.get changes 0))
                uri (interop/document->decoded-uri textDocument)]
            (#'handlers/did-change uri text version)))))

  (^void didSave [_ ^DidSaveTextDocumentParams _params]
    (go :didSave
        (end nil)))

  (^void didClose [_ ^DidCloseTextDocumentParams params]
    (go :didClose
        (end (-> (.getTextDocument params)
                 interop/document->decoded-uri
                 handlers/did-close))))

  (^CompletableFuture references [this ^ReferenceParams params]
    (go :references
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (->> params
                     interop/java->clj
                     handlers/references
                     (interop/conform-or-log ::interop/references))))))))

  (^CompletableFuture completion [this ^CompletionParams params]
    (go :completion
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (let [doc-id (interop/document->decoded-uri (.getTextDocument params))
                      pos (.getPosition params)
                      line (inc (int (.getLine pos)))
                      column (inc (int (.getCharacter pos)))]
                  (interop/conform-or-log ::interop/completion-items (#'handlers/completion doc-id line column)))))))))

  (^CompletableFuture resolveCompletionItem [this ^CompletionItem item]
    (go :resolveCompletionItem
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (let [label (.getLabel item)
                      sym-wanted (interop/json->clj (.getData item))]
                  (interop/conform-or-log ::interop/completion-item (#'handlers/resolve-completion-item label sym-wanted)))))))))

  (^CompletableFuture rename [this ^RenameParams params]
    (go :rename
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (let [doc-id (interop/document->decoded-uri (.getTextDocument params))
                      pos (.getPosition params)
                      line (inc (.getLine pos))
                      column (inc (.getCharacter pos))
                      new-name (.getNewName params)
                      edit (#'handlers/rename doc-id line column new-name)]
                  (interop/conform-or-log ::interop/workspace-edit edit))))))))

  (^CompletableFuture hover [this ^HoverParams params]
    (go :hover
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (let [doc-id (interop/document->decoded-uri (.getTextDocument params))
                      pos (.getPosition params)
                      line (inc (.getLine pos))
                      column (inc (.getCharacter pos))]
                  (interop/conform-or-log ::interop/hover (#'handlers/hover doc-id line column)))))))))

  (^CompletableFuture signatureHelp [_ ^SignatureHelpParams _params]
    (go :signatureHelp
        (CompletableFuture/completedFuture
          (end
            (SignatureHelp. [(doto (SignatureInformation. "sign-label")
                               (.setDocumentation "docs")
                               (.setParameters [(ParameterInformation. "param label" "param doc")]))]
                            0 0)))))

  (^CompletableFuture formatting [this ^DocumentFormattingParams params]
    (go :formatting
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (let [doc-id (interop/document->decoded-uri (.getTextDocument params))]
                  (interop/conform-or-log ::interop/edits (#'handlers/formatting doc-id)))))))))

  (^CompletableFuture rangeFormatting [_this ^DocumentRangeFormattingParams params]
    (go :rangeFormatting
        (end
          (let [result (when (compare-and-set! formatting false true)
                         (try
                           (let [doc-id (interop/document->decoded-uri (.getTextDocument params))
                                 range (.getRange params)
                                 start (.getStart range)
                                 end (.getEnd range)]
                             (interop/conform-or-log ::interop/edits (#'handlers/range-formatting
                                                                      doc-id
                                                                      {:row (inc (.getLine start))
                                                                       :col (inc (.getCharacter start))
                                                                       :end-row (inc (.getLine end))
                                                                       :end-col (inc (.getCharacter end))})))
                           (catch Exception e
                             (log/error e))
                           (finally
                             (reset! formatting false))))]
            (CompletableFuture/completedFuture
              result)))))

  (^CompletableFuture codeAction [_ ^CodeActionParams params]
    (go :codeAction
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [_this]
              (end
                (->> params
                     interop/java->clj
                     handlers/code-actions
                     (interop/conform-or-log ::interop/code-actions))))))))

  (^CompletableFuture resolveCodeAction [_ ^CodeAction unresolved]
    (go :resolveCodeAction
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [_this]
              (end
                (->> unresolved
                     interop/java->clj
                     handlers/resolve-code-action
                     (interop/conform-or-log ::interop/code-action))))))))

  (^CompletableFuture codeLens [_ ^CodeLensParams params]
    (go :codeLens
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [_this]
              (end
                (let [doc-id (interop/document->decoded-uri (.getTextDocument params))]
                  (interop/conform-or-log ::interop/code-lenses (#'handlers/code-lens doc-id)))))))))

  (^CompletableFuture resolveCodeLens [_ ^CodeLens params]
    (go :resolveCodeLens
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [_this]
              (end
                (->> (.getData params)
                     interop/json->clj
                     (handlers/code-lens-resolve (-> params .getRange shared/range->clj))
                     (interop/conform-or-log ::interop/code-lens))))))))

  (^CompletableFuture definition [this ^DefinitionParams params]
    (go :definition
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (let [doc-id (interop/document->decoded-uri (.getTextDocument params))
                      pos (.getPosition params)
                      line (inc (.getLine pos))
                      column (inc (.getCharacter pos))]
                  (interop/conform-or-log ::interop/location (handlers/definition doc-id line column)))))))))

  (^CompletableFuture documentSymbol [this ^DocumentSymbolParams params]
    (go :documentSymbol
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (let [doc-id (interop/document->decoded-uri (.getTextDocument params))]
                  (interop/conform-or-log ::interop/document-symbols (#'handlers/document-symbol doc-id)))))))))

  (^CompletableFuture documentHighlight [this ^DocumentHighlightParams params]
    (go :documentHighlight
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (let [doc-id (interop/document->decoded-uri (.getTextDocument params))
                      pos (.getPosition params)
                      line (inc (.getLine pos))
                      column (inc (.getCharacter pos))]
                  (interop/conform-or-log ::interop/document-highlights (#'handlers/document-highlight doc-id line column)))))))))

  (^CompletableFuture semanticTokensFull [_ ^SemanticTokensParams params]
    (go :semanticTokensFull
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [_this]
              (end
                (let [doc-id (interop/document->decoded-uri (.getTextDocument params))]
                  (interop/conform-or-log ::interop/semantic-tokens (handlers/semantic-tokens-full doc-id)))))))))

  (^CompletableFuture semanticTokensRange [_ ^SemanticTokensRangeParams params]
    (go :semanticTokensRange
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [_this]
              (end
                (let [doc-id (interop/document->decoded-uri (.getTextDocument params))
                      start (-> params .getRange .getStart)
                      end (-> params .getRange .getEnd)
                      range {:row (inc (.getLine start))
                             :col (inc (.getCharacter start))
                             :end-row (inc (.getLine end))
                             :end-col (inc (.getCharacter end))}]
                  (interop/conform-or-log ::interop/semantic-tokens (handlers/semantic-tokens-range doc-id range)))))))))

  (^CompletableFuture prepareCallHierarchy [_ ^CallHierarchyPrepareParams params]
    (go :prepareCallHierarchy
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [_this]
              (end
                (let [doc-id (interop/document->decoded-uri (.getTextDocument params))
                      pos (.getPosition params)
                      row (inc (.getLine pos))
                      col (inc (.getCharacter pos))]
                  (interop/conform-or-log ::interop/call-hierarchy-items (handlers/prepare-call-hierarchy doc-id row col)))))))))

  (^CompletableFuture callHierarchyIncomingCalls [_ ^CallHierarchyIncomingCallsParams params]
    (go :callHierarchyIncomingCalls
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [_this]
              (end
                (let [item (.getItem params)]
                  (interop/conform-or-log ::interop/call-hierarchy-incoming-calls (handlers/call-hierarchy-incoming item))))))))))

(deftype LSPWorkspaceService []
  WorkspaceService

  (^CompletableFuture executeCommand [_ ^ExecuteCommandParams params]
    (go :executeCommand
        (future
          (end
            (-> params interop/java->clj handlers/execute-command))))
    (CompletableFuture/completedFuture 0))

  (^void didChangeConfiguration [_ ^DidChangeConfigurationParams params]
    (log/warn params))

  (^void didChangeWatchedFiles [_ ^DidChangeWatchedFilesParams params]
    (go :didChangeWatchedFiles
        (end
          (some->> params
                   (.getChanges)
                   (interop/conform-or-log ::interop/watched-files-changes)
                   (handlers/did-change-watched-files)))))

  (^CompletableFuture symbol [this ^WorkspaceSymbolParams params]
    (go :workspaceSymbol
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (let [query (.getQuery params)]
                  (interop/conform-or-log ::interop/workspace-symbols (#'handlers/workspace-symbols query))))))))))

(defn client-settings [^InitializeParams params]
  (-> params
      (.getInitializationOptions)
      (interop/json->clj)
      (or {})
      shared/keywordize-first-depth
      (interop/clean-client-settings)))

(defn client-capabilities [^InitializeParams params]
  (some->> params
           (.getCapabilities)
           (interop/conform-or-log ::interop/client-capabilities)))

;; Called from java
(defn extension [method & args]
  (go :extension
      (CompletableFuture/completedFuture
        (end
          (apply #'handlers/extension method args)))))

(defn ^:private start-parent-process-liveness-probe!
  [ppid server]
  (async/go-loop []
    (async/<! (async/timeout 5000))
    (log/debug "Checking parent process" ppid "liveness")
    (if (shared/process-alive? ppid)
      (do
        (log/debug "Parent process" ppid "is running")
        (recur))
      (do
        (log/info "Parent process" ppid "is not running - exiting server")
        (.exit server)))))

(def server
  (proxy [ClojureExtensions LanguageServer] []
    (^CompletableFuture initialize [^InitializeParams params]
      (go :initialize
          (end
            (do
              (log/info "Initializing...")
              (#'handlers/initialize (.getRootUri params)
                                     (client-capabilities params)
                                     (client-settings params))
              (when-let [parent-process-id (.getProcessId params)]
                (start-parent-process-liveness-probe! parent-process-id this))
              (let [settings (:settings @db/db)]
                (CompletableFuture/completedFuture
                  (InitializeResult. (doto (ServerCapabilities.)
                                       (.setHoverProvider true)
                                       (.setCallHierarchyProvider true)
                                       (.setCodeActionProvider (doto (CodeActionOptions. interop/code-action-kind)
                                                                 (.setResolveProvider true)))
                                       (.setCodeLensProvider (CodeLensOptions. true))
                                       (.setReferencesProvider true)
                                       (.setRenameProvider true)
                                       (.setDefinitionProvider true)
                                       (.setDocumentFormattingProvider (:document-formatting? settings))
                                       (.setDocumentRangeFormattingProvider (:document-range-formatting? settings))
                                       (.setDocumentSymbolProvider true)
                                       (.setDocumentHighlightProvider true)
                                       (.setWorkspaceSymbolProvider true)
                                       (.setSemanticTokensProvider (when (or (not (contains? settings :semantic-tokens?))
                                                                             (:semantic-tokens? settings))
                                                                     (doto (SemanticTokensWithRegistrationOptions.)
                                                                       (.setLegend (doto (SemanticTokensLegend.
                                                                                           semantic-tokens/token-types-str
                                                                                           semantic-tokens/token-modifiers)))
                                                                       (.setRange true)
                                                                       (.setFull true))))
                                       (.setExecuteCommandProvider (doto (ExecuteCommandOptions.)
                                                                     (.setCommands f.refactor/available-refactors)))
                                       (.setTextDocumentSync (doto (TextDocumentSyncOptions.)
                                                               (.setOpenClose true)
                                                               (.setChange TextDocumentSyncKind/Full)
                                                               (.setSave (SaveOptions. true))))
                                       (.setCompletionProvider (CompletionOptions. true []))))))))))

    (^void initialized [^InitializedParams params]
      (log/info "Initialized" params)
      (go :initialized
          (end
            (doto
             (:client @db/db)
             (.registerCapability
               (RegistrationParams. [(Registration. "id" "workspace/didChangeWatchedFiles"
                                                    (DidChangeWatchedFilesRegistrationOptions. [(FileSystemWatcher. "**")]))]))))))
    (^CompletableFuture shutdown []
      (log/info "Shutting down")
      (reset! db/db {:documents {}}) ;; TODO confirm this is correct
      (CompletableFuture/completedFuture
       {:result nil}))
    (exit []
      (log/info "Exit")
      (shutdown-agents)
      (System/exit 0))
    (getTextDocumentService []
      (LSPTextDocumentService.))
    (getWorkspaceService []
      (LSPWorkspaceService.))))

(defn- dot-nrepl-port-file
  []
  (try
    (slurp  ".nrepl-port")
    (catch Exception _)))

(defn- embedded-nrepl-server
  []
  (let [repl-server (nrepl.server/start-server)
        port (:port repl-server)]
    port))

(defn- repl-port
  []
  (or (dot-nrepl-port-file)
      (embedded-nrepl-server)))

(defn- run []
  (log/info "Starting server...")
  (let [launcher (LSPLauncher/createServerLauncher server System/in System/out)
        port (repl-port)]
    (log/info "====== LSP nrepl server started on port" port)
    (swap! db/db assoc
           :client ^LanguageClient (.getRemoteProxy launcher)
           :port port)
    (async/go
      (loop [edit (async/<! db/edits-chan)]
        (producer/workspace-apply-edit edit)
        (recur (async/<! db/edits-chan))))
    (async/go
      (loop [diagnostic (async/<! db/diagnostics-chan)]
        (producer/publish-diagnostic diagnostic)
        (recur (async/<! db/diagnostics-chan))))
    (.startListening launcher)))

(defn -main [& args]
  (if (empty? args)
    (run)
    (println "clojure-lsp" (version/get-version "clojure-lsp" "clojure-lsp"))))
