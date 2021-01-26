(ns clojure-lsp.main
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.refactor :as f.refactor]
    [clojure-lsp.feature.semantic-tokens :as semantic-tokens]
    [clojure-lsp.handlers :as handlers]
    [clojure-lsp.interop :as interop]
    [clojure-lsp.producer :as producer]
    [clojure-lsp.shared :as shared]
    [clojure.core.async :as async]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [nrepl.server :as nrepl.server]
    [trptcolin.versioneer.core :as version])
  (:import
    (clojure_lsp ClojureExtensions)
    (java.lang.management ManagementFactory)
    (java.util.concurrent CompletableFuture)
    (java.util.function Supplier)
    (org.apache.log4j MDC)
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
    (org.eclipse.lsp4j.services LanguageServer TextDocumentService WorkspaceService LanguageClient))
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

(defmacro ^:private sync-handler
  ([params handler]
   `(end
      (->> ~params
           interop/java->clj
           ~handler)))
  ([params handler response-spec]
   `(end
      (->> ~params
           interop/java->clj
           ~handler
           (interop/conform-or-log ~response-spec)))))

(defmacro ^:private async-handler
  [params handler response-spec]
  `(CompletableFuture/supplyAsync
     (reify Supplier
       (get [this]
         (sync-handler ~params ~handler ~response-spec)))))

(deftype LSPTextDocumentService []
  TextDocumentService
  (^void didOpen [_ ^DidOpenTextDocumentParams params]
    (go :didOpen
        (sync-handler params handlers/did-open)))

  (^void didChange [_ ^DidChangeTextDocumentParams params]
    (go :didChange
        (end
          (let [textDocument (.getTextDocument params)
                version (.getVersion textDocument)
                changes (.getContentChanges params)
                text (.getText ^TextDocumentContentChangeEvent (.get changes 0))
                uri (interop/document->decoded-uri textDocument)]
            (#'handlers/did-change uri text version)))))

  (^void didSave [_ ^DidSaveTextDocumentParams params]
    (go :didSave
        (sync-handler params handlers/did-save)))

  (^void didClose [_ ^DidCloseTextDocumentParams _params]
    (go :didClose
        nil))

  (^CompletableFuture references [_ ^ReferenceParams params]
    (go :references
        (async-handler params handlers/references ::interop/references)))

  (^CompletableFuture completion [_ ^CompletionParams params]
    (go :completion
        (async-handler params handlers/completion ::interop/completion-items)))

  (^CompletableFuture resolveCompletionItem [_ ^CompletionItem item]
    (go :resolveCompletionItem
        (async-handler item handlers/resolve-completion-item ::interop/completion-item)))

  (^CompletableFuture rename [_ ^RenameParams params]
    (go :rename
        (async-handler params handlers/rename ::interop/workspace-edit)))

  (^CompletableFuture hover [_ ^HoverParams params]
    (go :hover
        (async-handler params handlers/hover ::interop/hover)))

  (^CompletableFuture signatureHelp [_ ^SignatureHelpParams _params]
    (go :signatureHelp
        (CompletableFuture/completedFuture
          (end
            (SignatureHelp. [(doto (SignatureInformation. "sign-label")
                               (.setDocumentation "docs")
                               (.setParameters [(ParameterInformation. "param label" "param doc")]))]
                            0 0)))))

  (^CompletableFuture formatting [_ ^DocumentFormattingParams params]
    (go :formatting
        (async-handler params handlers/formatting ::interop/edits)))

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
        (async-handler params handlers/code-actions ::interop/code-actions)))

  (^CompletableFuture resolveCodeAction [_ ^CodeAction unresolved]
    (go :resolveCodeAction
        (async-handler unresolved handlers/resolve-code-action ::interop/code-action)))

  (^CompletableFuture codeLens [_ ^CodeLensParams params]
    (go :codeLens
        (async-handler params handlers/code-lens ::interop/code-lenses)))

  (^CompletableFuture resolveCodeLens [_ ^CodeLens params]
    (go :resolveCodeLens
        (async-handler params handlers/code-lens-resolve ::interop/code-lens)))

  (^CompletableFuture definition [_ ^DefinitionParams params]
    (go :definition
        (async-handler params handlers/definition ::interop/location)))

  (^CompletableFuture documentSymbol [_ ^DocumentSymbolParams params]
    (go :documentSymbol
        (async-handler params handlers/document-symbol ::interop/document-symbols)))

  (^CompletableFuture documentHighlight [_ ^DocumentHighlightParams params]
    (go :documentHighlight
        (async-handler params handlers/document-highlight ::interop/document-highlights)))

  (^CompletableFuture semanticTokensFull [_ ^SemanticTokensParams params]
    (go :semanticTokensFull
        (async-handler params handlers/semantic-tokens-full ::interop/semantic-tokens)))

  (^CompletableFuture semanticTokensRange [_ ^SemanticTokensRangeParams params]
    (go :semanticTokensRange
        (async-handler params handlers/semantic-tokens-range ::interop/semantic-tokens)))

  (^CompletableFuture prepareCallHierarchy [_ ^CallHierarchyPrepareParams params]
    (go :prepareCallHierarchy
        (async-handler params handlers/prepare-call-hierarchy ::interop/call-hierarchy-items)))

  (^CompletableFuture callHierarchyIncomingCalls [_ ^CallHierarchyIncomingCallsParams params]
    (go :callHierarchyIncomingCalls
        (async-handler params handlers/call-hierarchy-incoming ::interop/call-hierarchy-incoming-calls))))

(deftype LSPWorkspaceService []
  WorkspaceService

  (^CompletableFuture executeCommand [_ ^ExecuteCommandParams params]
    (go :executeCommand
        (future
          (sync-handler params handlers/execute-command)))
    (CompletableFuture/completedFuture 0))

  (^void didChangeConfiguration [_ ^DidChangeConfigurationParams params]
    (log/warn (interop/java->clj params)))

  (^void didChangeWatchedFiles [_ ^DidChangeWatchedFilesParams params]
    (go :didChangeWatchedFiles
        (end
          (some->> params
                   (.getChanges)
                   (interop/conform-or-log ::interop/watched-files-changes)
                   (handlers/did-change-watched-files)))))

  ;; TODO wait for lsp4j release
  #_(^void didDeleteFiles [_ ^DeleteFilesParams params]
    (go :didSave
        (sync-handler params handlers/did-delete-files)))

  (^CompletableFuture symbol [_ ^WorkspaceSymbolParams params]
    (go :workspaceSymbol
        (async-handler params handlers/workspace-symbols ::interop/workspace-symbols))))

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
    (if (shared/process-alive? ppid)
      (recur)
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
                                       (.setDocumentHighlightProvider true)
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

(defn tee-system-in [system-in]
  (let [buffer-size 1024
        os (java.io.PipedOutputStream.)
        is (java.io.PipedInputStream. os)]
    (async/thread
      (try
        (let [buffer (byte-array buffer-size)]
          (loop [chs (.read system-in buffer 0 buffer-size)]
            (when (pos? chs)
              (log/warn "FROM STDIN" chs (String. (java.util.Arrays/copyOfRange buffer 0 chs)))
              (.write os buffer 0 chs)
              (recur (.read system-in buffer 0 buffer-size)))))
        (catch Exception e
          (log/error e "in thread"))))
    is))

(defn tee-system-out [system-out]
  (let [buffer-size 1024
        is (java.io.PipedInputStream.)
        os (java.io.PipedOutputStream. is)]
    (async/thread
      (try
        (let [buffer (byte-array buffer-size)]
          (loop [chs (.read is buffer 0 buffer-size)]
            (when (pos? chs)
              (log/warn "FROM STDOUT" chs (String. (java.util.Arrays/copyOfRange buffer 0 chs)))
              (.write system-out buffer)
              (recur (.read is buffer 0 buffer-size)))))
        (catch Exception e
          (log/error e "in thread"))))
    os))

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

(defn- inject-pid-for-log4j
  "Inject a PID context value so we can include it in log4j
  logs."
  []
  (let [pid (-> (ManagementFactory/getRuntimeMXBean)
                (.getName)
                (string/split #"@")
                (first))]
    (MDC/put "PID" pid)))

(defn- run []
  (inject-pid-for-log4j)
  (log/info "Starting server...")
  (let [is (or System/in (tee-system-in System/in))
        os (or System/out (tee-system-out System/out))
        launcher (LSPLauncher/createServerLauncher server is os)
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
