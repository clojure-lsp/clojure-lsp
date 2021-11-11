(ns clojure-lsp.server
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.feature.semantic-tokens :as semantic-tokens]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.interop :as interop]
   [clojure-lsp.nrepl :as nrepl]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :refer [<! go-loop thread timeout]]
   [taoensso.timbre :as log])
  (:import
   (clojure_lsp
     ClojureExtensions
     ExtraMethods
     CursorInfoParams
     ClojuredocsParams)
   (java.util.concurrent CompletableFuture
                         CompletionException)
   (java.util.function Supplier)
   (org.eclipse.lsp4j
     CallHierarchyIncomingCallsParams
     CallHierarchyOutgoingCallsParams
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
     LinkedEditingRangeParams
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
     SignatureHelpOptions
     SignatureHelpParams
     TextDocumentSyncKind
     TextDocumentSyncOptions
     WorkspaceSymbolParams)
   (org.eclipse.lsp4j.jsonrpc ResponseErrorException)
   (org.eclipse.lsp4j.launch LSPLauncher)
   (org.eclipse.lsp4j.services LanguageServer TextDocumentService WorkspaceService LanguageClient))
  (:gen-class))

(set! *warn-on-reflection* true)

(defonce formatting (atom false))

(defmacro ^:private start [id & body]
  `(let [~'_start-time (System/nanoTime)
         ~'_id ~id]
     (do ~@body)))

(defmacro ^:private end [expr]
  `(try
     ~expr
     (catch Throwable ex#
       (if (instance? ResponseErrorException ex#)
         (throw (CompletionException. ex#))
         (log/error ex#)))
     (finally
       (try
         (let [duration# (quot (- (System/nanoTime) ~'_start-time) 1000000)]
           (log/debug ~'_id (format "%sms" duration#)))
         (catch Throwable ex#
           (log/error ex#))))))

(defmacro ^:private sync-notification
  [params handler]
  `(end
     (->> ~params
          interop/java->clj
          ~handler)))

(defmacro ^:private sync-request
  [params handler response-spec]
  `(end
     (->> ~params
          interop/java->clj
          ~handler
          (interop/conform-or-log ~response-spec))))

(defmacro ^:private async-request
  [params handler response-spec]
  `(CompletableFuture/supplyAsync
     (reify Supplier
       (get [this]
         (sync-request ~params ~handler ~response-spec)))))

(defmacro ^:private async-notification
  [params handler]
  `(CompletableFuture/supplyAsync
     (reify Supplier
       (get [this]
         (sync-notification ~params ~handler)))))

(deftype LSPTextDocumentService []
  TextDocumentService
  (^void didOpen [_ ^DidOpenTextDocumentParams params]
    (start :didOpen
           (sync-notification params handlers/did-open)))

  (^void didChange [_ ^DidChangeTextDocumentParams params]
    (start :didChange
           (sync-notification params handlers/did-change)))

  (^void didSave [_ ^DidSaveTextDocumentParams params]
    (start :didSave
           (future
             (sync-notification params handlers/did-save)))
    (CompletableFuture/completedFuture 0))

  (^void didClose [_ ^DidCloseTextDocumentParams params]
    (start :didClose
           (async-notification params handlers/did-close)))

  (^CompletableFuture references [_ ^ReferenceParams params]
    (start :references
           (async-request params handlers/references ::interop/references)))

  (^CompletableFuture completion [_ ^CompletionParams params]
    (start :completion
           (async-request params handlers/completion ::interop/completion-items)))

  (^CompletableFuture resolveCompletionItem [_ ^CompletionItem item]
    (start :resolveCompletionItem
           (async-request item handlers/completion-resolve-item ::interop/completion-item)))

  (^CompletableFuture rename [_ ^RenameParams params]
    (start :rename
           (async-request params handlers/rename ::interop/workspace-edit-or-error)))

  (^CompletableFuture hover [_ ^HoverParams params]
    (start :hover
           (async-request params handlers/hover ::interop/hover)))

  (^CompletableFuture signatureHelp [_ ^SignatureHelpParams params]
    (start :signatureHelp
           (async-request params handlers/signature-help ::interop/signature-help)))

  (^CompletableFuture formatting [_ ^DocumentFormattingParams params]
    (start :formatting
           (async-request params handlers/formatting ::interop/edits)))

  (^CompletableFuture rangeFormatting [_this ^DocumentRangeFormattingParams params]
    (start :rangeFormatting
           (end
             (let [result (when (compare-and-set! formatting false true)
                            (try
                              (let [doc-id (interop/document->uri (.getTextDocument params))
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
    (start :codeAction
           (async-request params handlers/code-actions ::interop/code-actions)))

  (^CompletableFuture resolveCodeAction [_ ^CodeAction unresolved]
    (start :resolveCodeAction
           (async-request unresolved handlers/resolve-code-action ::interop/code-action)))

  (^CompletableFuture codeLens [_ ^CodeLensParams params]
    (start :codeLens
           (async-request params handlers/code-lens ::interop/code-lenses)))

  (^CompletableFuture resolveCodeLens [_ ^CodeLens params]
    (start :resolveCodeLens
           (async-request params handlers/code-lens-resolve ::interop/code-lens)))

  (^CompletableFuture definition [_ ^DefinitionParams params]
    (start :definition
           (async-request params handlers/definition ::interop/location)))

  (^CompletableFuture documentSymbol [_ ^DocumentSymbolParams params]
    (start :documentSymbol
           (async-request params handlers/document-symbol ::interop/document-symbols)))

  (^CompletableFuture documentHighlight [_ ^DocumentHighlightParams params]
    (start :documentHighlight
           (async-request params handlers/document-highlight ::interop/document-highlights)))

  (^CompletableFuture semanticTokensFull [_ ^SemanticTokensParams params]
    (start :semanticTokensFull
           (async-request params handlers/semantic-tokens-full ::interop/semantic-tokens)))

  (^CompletableFuture semanticTokensRange [_ ^SemanticTokensRangeParams params]
    (start :semanticTokensRange
           (async-request params handlers/semantic-tokens-range ::interop/semantic-tokens)))

  (^CompletableFuture prepareCallHierarchy [_ ^CallHierarchyPrepareParams params]
    (start :prepareCallHierarchy
           (async-request params handlers/prepare-call-hierarchy ::interop/call-hierarchy-items)))

  (^CompletableFuture callHierarchyIncomingCalls [_ ^CallHierarchyIncomingCallsParams params]
    (start :callHierarchyIncomingCalls
           (async-request params handlers/call-hierarchy-incoming ::interop/call-hierarchy-incoming-calls)))

  (^CompletableFuture callHierarchyOutgoingCalls [_ ^CallHierarchyOutgoingCallsParams params]
    (start :callHierarchyOutgoingCalls
           (async-request params handlers/call-hierarchy-outgoing ::interop/call-hierarchy-outgoing-calls)))

  (^CompletableFuture linkedEditingRange [_ ^LinkedEditingRangeParams params]
    (start :linkedEditingRange
           (async-request params handlers/linked-editing-ranges ::interop/linked-editing-ranges-or-error))))

(deftype LSPWorkspaceService []
  WorkspaceService
  (^CompletableFuture executeCommand [_ ^ExecuteCommandParams params]
    (start :executeCommand
           (future
             (sync-notification params handlers/execute-command)))
    (CompletableFuture/completedFuture 0))

  (^void didChangeConfiguration [_ ^DidChangeConfigurationParams params]
    (log/warn (interop/java->clj params)))

  (^void didChangeWatchedFiles [_ ^DidChangeWatchedFilesParams params]
    (start :didChangeWatchedFiles
           (async-notification params handlers/did-change-watched-files)))

  ;; TODO wait for lsp4j release
  #_(^void didDeleteFiles [_ ^DeleteFilesParams params]
                          (start :didSave
                                 (sync-handler params handlers/did-delete-files)))

  (^CompletableFuture symbol [_ ^WorkspaceSymbolParams params]
    (start :workspaceSymbol
           (async-request params handlers/workspace-symbols ::interop/workspace-symbols))))

(defn ^:private client-settings [^InitializeParams params]
  (-> params
      interop/java->clj
      :initializationOptions
      (or {})
      shared/keywordize-first-depth
      (interop/clean-client-settings)))

(defn ^:private client-capabilities [^InitializeParams params]
  (some->> params
           .getCapabilities
           (interop/conform-or-log ::interop/client-capabilities)))

;; Called from java
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn extension [method & args]
  (start :extension
         (CompletableFuture/completedFuture
           (end
             (apply #'handlers/extension method args)))))

(defn ^:private start-parent-process-liveness-probe!
  [ppid server]
  (go-loop []
    (<! (timeout 5000))
    (if (shared/process-alive? ppid)
      (recur)
      (do
        (log/info "Parent process" ppid "is not running - exiting server")
        (.exit ^LanguageServer server)))))

(def server
  (proxy [ClojureExtensions LanguageServer ExtraMethods] []
    (^CompletableFuture initialize [^InitializeParams params]
      (start :initialize
             (end
               (do
                 (log/info "Initializing...")
                 (handlers/initialize (.getRootUri params)
                                      (client-capabilities params)
                                      (client-settings params)
                                      (some-> (.getWorkDoneToken params) .get str))
                 (when-let [parent-process-id (.getProcessId params)]
                   (start-parent-process-liveness-probe! parent-process-id this))
                 (let [settings (settings/all db/db)]
                   (CompletableFuture/completedFuture
                     (InitializeResult. (doto (ServerCapabilities.)
                                          (.setDocumentHighlightProvider true)
                                          (.setHoverProvider true)
                                          (.setSignatureHelpProvider (SignatureHelpOptions. []))
                                          (.setCallHierarchyProvider true)
                                          (.setLinkedEditingRangeProvider true)
                                          (.setCodeActionProvider (doto (CodeActionOptions. interop/code-action-kind)
                                                                    (.setResolveProvider true)))
                                          (.setCodeLensProvider (CodeLensOptions. true))
                                          (.setReferencesProvider true)
                                          (.setRenameProvider true)
                                          (.setDefinitionProvider true)
                                          (.setDocumentFormattingProvider ^Boolean (:document-formatting? settings))
                                          (.setDocumentRangeFormattingProvider ^Boolean (:document-range-formatting? settings))
                                          (.setDocumentSymbolProvider true)
                                          (.setWorkspaceSymbolProvider true)
                                          (.setSemanticTokensProvider (when (or (not (contains? settings :semantic-tokens?))
                                                                                (:semantic-tokens? settings))
                                                                        (doto (SemanticTokensWithRegistrationOptions.)
                                                                          (.setLegend (doto (SemanticTokensLegend.
                                                                                              semantic-tokens/token-types-str
                                                                                              semantic-tokens/token-modifiers-str)))
                                                                          (.setRange true)
                                                                          (.setFull true))))
                                          (.setExecuteCommandProvider (doto (ExecuteCommandOptions.)
                                                                        (.setCommands f.refactor/available-refactors)))
                                          (.setTextDocumentSync (doto (TextDocumentSyncOptions.)
                                                                  (.setOpenClose true)
                                                                  (.setChange (case (:text-document-sync-kind settings)
                                                                                :full TextDocumentSyncKind/Full
                                                                                :incremental TextDocumentSyncKind/Incremental
                                                                                TextDocumentSyncKind/Full))
                                                                  (.setSave (SaveOptions. true))))
                                          (.setCompletionProvider (CompletionOptions. true [":" "/"]))))))))))

    (^void initialized [^InitializedParams params]
      (start :initialized
             (end
               (do
                 (log/info "Initialized!")
                 (let [client ^LanguageClient (:client @db/db)]
                   (.registerCapability client
                                        (RegistrationParams. [(Registration. "id" "workspace/didChangeWatchedFiles"
                                                                             (DidChangeWatchedFilesRegistrationOptions. [(FileSystemWatcher. "**/*.{clj,cljs,cljc,edn}")]))])))))))

    (^CompletableFuture serverInfoRaw []
      (CompletableFuture/completedFuture
        (->> (handlers/server-info-raw)
             (interop/conform-or-log ::interop/server-info-raw))))

    (^void serverInfoLog []
      (start :server-info-log
             (future
               (end
                 (handlers/server-info-log)))))

    (^void cursorInfoLog [^CursorInfoParams params]
      (start :cursor-info-log
             (future
               (sync-notification params handlers/cursor-info-log))))

    (^CompletableFuture clojuredocsRaw [^ClojuredocsParams params]
      (start :clojuredocsRaw
             (CompletableFuture/completedFuture
               (sync-request params handlers/clojuredocs-raw ::interop/clojuredocs-raw))))

    (^CompletableFuture shutdown []
      (log/info "Shutting down")
      (reset! db/db {:documents {}})
      (CompletableFuture/completedFuture
        {:result nil}))
    (exit []
      (log/info "Exitting...")
      (shutdown-agents)
      (System/exit 0))
    (getTextDocumentService []
      (LSPTextDocumentService.))
    (getWorkspaceService []
      (LSPWorkspaceService.))))

(defn ^:private tee-system-in [^java.io.InputStream system-in]
  (let [buffer-size 1024
        os (java.io.PipedOutputStream.)
        is (java.io.PipedInputStream. os)]
    (thread
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

(defn ^:private tee-system-out [^java.io.OutputStream system-out]
  (let [buffer-size 1024
        is (java.io.PipedInputStream.)
        os (java.io.PipedOutputStream. is)]
    (thread
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

(defn run-server! []
  (log/info "Starting server...")
  (let [is (or System/in (tee-system-in System/in))
        os (or System/out (tee-system-out System/out))
        launcher (LSPLauncher/createServerLauncher server is os)
        debounced-diags (shared/debounce-by db/diagnostics-chan config/diagnostics-debounce-ms :uri)
        debounced-changes (shared/debounce-by db/current-changes-chan config/change-debounce-ms :uri)]
    (nrepl/setup-nrepl db/db)
    (swap! db/db assoc :client ^LanguageClient (.getRemoteProxy launcher))
    (go-loop [edit (<! db/edits-chan)]
      (producer/workspace-apply-edit edit db/db)
      (recur (<! db/edits-chan)))
    (go-loop []
      (producer/publish-diagnostic (<! debounced-diags) db/db)
      (recur))
    (go-loop []
      (try
        (f.file-management/analyze-changes (<! debounced-changes) db/db)
        (catch Exception e
          (log/error e "Error during analyzing buffer file changes")))
      (recur))
    (.startListening launcher)))
