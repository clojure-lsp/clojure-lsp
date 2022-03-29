(ns lsp4clj.core
  (:require
   [clojure.core.async :refer [<! go-loop thread timeout]]
   [clojure.java.shell :as shell]
   [clojure.string :as string]
   [lsp4clj.coercer :as coercer]
   [lsp4clj.protocols.feature-handler :as feature-handler]
   [lsp4clj.protocols.logger :as logger]
   [lsp4clj.protocols.producer :as producer])
  (:import
   (java.util.concurrent
     CompletableFuture
     CompletionException)
   (java.util.function Supplier)
   (lsp4clj.protocols.feature_handler ILSPFeatureHandler)
   (org.eclipse.lsp4j
     ApplyWorkspaceEditParams
     CallHierarchyIncomingCallsParams
     CallHierarchyOutgoingCallsParams
     CallHierarchyPrepareParams
     CodeActionParams
     CodeLens
     CodeLensParams
     CodeLensWorkspaceCapabilities
     CompletionItem
     CompletionParams
     DeclarationParams
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
     ExecuteCommandParams
     FileSystemWatcher
     HoverParams
     ImplementationParams
     InitializeParams
     InitializeResult
     InitializedParams
     LinkedEditingRangeParams
     MessageActionItem
     PrepareRenameParams
     ReferenceParams
     Registration
     RegistrationParams
     RenameParams
     SemanticTokensParams
     SemanticTokensRangeParams
     SignatureHelpParams
     WindowClientCapabilities
     WorkspaceSymbolParams)
   (org.eclipse.lsp4j.jsonrpc ResponseErrorException)
   (org.eclipse.lsp4j.services
     LanguageClient
     LanguageServer
     TextDocumentService
     WorkspaceService))
  (:gen-class))

(set! *warn-on-reflection* true)

(defonce formatting (atom false))

(defmacro handle-request [params f handler response-spec]
  `(coercer/conform-or-log
     ~response-spec
     (~f ~handler (coercer/java->clj ~params))))

(defmacro handle-notification [params f handler]
  `(~f ~handler (coercer/java->clj ~params)))

(defmacro in-completable-future [& body]
  (let [m (meta &form)
        ex-sym (gensym "ex")]
    `(CompletableFuture/supplyAsync
       (reify Supplier
         (get [this#]
           (try
             ~@body
             (catch Throwable ~ex-sym
               (if (instance? ResponseErrorException ~ex-sym)
                 (throw (CompletionException. ~ex-sym))
                 ~(with-meta `(logger/error ~ex-sym) m)))))))))

(deftype LSPTextDocumentService
         [^ILSPFeatureHandler handler]
  TextDocumentService
  (^void didOpen [_ ^DidOpenTextDocumentParams params]
    (handle-notification params feature-handler/did-open handler))

  (^void didChange [_ ^DidChangeTextDocumentParams params]
    (handle-notification params feature-handler/did-change handler))

  (^void didSave [_ ^DidSaveTextDocumentParams params]
    (future
      (handle-notification params feature-handler/did-save handler))
    (CompletableFuture/completedFuture 0))

  (^void didClose [_ ^DidCloseTextDocumentParams params]
    (in-completable-future
      (handle-notification params feature-handler/did-close handler)))

  (^CompletableFuture references [_ ^ReferenceParams params]
    (in-completable-future
      (handle-request params feature-handler/references handler ::coercer/locations)))

  (^CompletableFuture completion [_ ^CompletionParams params]
    (in-completable-future
      (handle-request params feature-handler/completion handler ::coercer/completion-items)))

  (^CompletableFuture resolveCompletionItem [_ ^CompletionItem item]
    (in-completable-future
      (handle-request item feature-handler/completion-resolve-item handler ::coercer/completion-item)))

  (^CompletableFuture prepareRename [_ ^PrepareRenameParams params]
    (in-completable-future
      (handle-request params feature-handler/prepare-rename handler ::coercer/prepare-rename-or-error)))

  (^CompletableFuture rename [_ ^RenameParams params]
    (in-completable-future
      (handle-request params feature-handler/rename handler ::coercer/workspace-edit-or-error)))

  (^CompletableFuture hover [_ ^HoverParams params]
    (in-completable-future
      (handle-request params feature-handler/hover handler ::coercer/hover)))

  (^CompletableFuture signatureHelp [_ ^SignatureHelpParams params]
    (in-completable-future
      (handle-request params feature-handler/signature-help handler ::coercer/signature-help)))

  (^CompletableFuture formatting [_ ^DocumentFormattingParams params]
    (in-completable-future
      (handle-request params feature-handler/formatting handler ::coercer/edits)))

  (^CompletableFuture rangeFormatting [_this ^DocumentRangeFormattingParams params]
    (CompletableFuture/completedFuture
      (when (compare-and-set! formatting false true)
        (try
          (handle-request params feature-handler/range-formatting handler ::coercer/edits)
          (catch Exception e
            (logger/error e))
          (finally
            (reset! formatting false))))))

  (^CompletableFuture codeAction [_ ^CodeActionParams params]
    (in-completable-future
      (handle-request params feature-handler/code-actions handler ::coercer/code-actions)))

  (^CompletableFuture codeLens [_ ^CodeLensParams params]
    (in-completable-future
      (handle-request params feature-handler/code-lens handler ::coercer/code-lenses)))

  (^CompletableFuture resolveCodeLens [_ ^CodeLens params]
    (in-completable-future
      (handle-request params feature-handler/code-lens-resolve handler ::coercer/code-lens)))

  (^CompletableFuture definition [_ ^DefinitionParams params]
    (in-completable-future
      (handle-request params feature-handler/definition handler ::coercer/location)))

  (^CompletableFuture declaration [_ ^DeclarationParams params]
    (in-completable-future
      (handle-request params feature-handler/declaration handler ::coercer/location)))

  (^CompletableFuture implementation [_ ^ImplementationParams params]
    (in-completable-future
      (handle-request params feature-handler/implementation handler ::coercer/locations)))

  (^CompletableFuture documentSymbol [_ ^DocumentSymbolParams params]
    (in-completable-future
      (handle-request params feature-handler/document-symbol handler ::coercer/document-symbols)))

  (^CompletableFuture documentHighlight [_ ^DocumentHighlightParams params]
    (in-completable-future
      (handle-request params feature-handler/document-highlight handler ::coercer/document-highlights)))

  (^CompletableFuture semanticTokensFull [_ ^SemanticTokensParams params]
    (in-completable-future
      (handle-request params feature-handler/semantic-tokens-full handler ::coercer/semantic-tokens)))

  (^CompletableFuture semanticTokensRange [_ ^SemanticTokensRangeParams params]
    (in-completable-future
      (handle-request params feature-handler/semantic-tokens-range handler ::coercer/semantic-tokens)))

  (^CompletableFuture prepareCallHierarchy [_ ^CallHierarchyPrepareParams params]
    (in-completable-future
      (handle-request params feature-handler/prepare-call-hierarchy handler ::coercer/call-hierarchy-items)))

  (^CompletableFuture callHierarchyIncomingCalls [_ ^CallHierarchyIncomingCallsParams params]
    (in-completable-future
      (handle-request params feature-handler/call-hierarchy-incoming handler ::coercer/call-hierarchy-incoming-calls)))

  (^CompletableFuture callHierarchyOutgoingCalls [_ ^CallHierarchyOutgoingCallsParams params]
    (in-completable-future
      (handle-request params feature-handler/call-hierarchy-outgoing handler ::coercer/call-hierarchy-outgoing-calls)))

  (^CompletableFuture linkedEditingRange [_ ^LinkedEditingRangeParams params]
    (in-completable-future
      (handle-request params feature-handler/linked-editing-ranges handler ::coercer/linked-editing-ranges-or-error))))

(deftype LSPWorkspaceService
         [^ILSPFeatureHandler handler]
  WorkspaceService
  (^CompletableFuture executeCommand [_ ^ExecuteCommandParams params]
    (future
      (handle-notification params feature-handler/execute-command handler))
    (CompletableFuture/completedFuture 0))

  (^void didChangeConfiguration [_ ^DidChangeConfigurationParams params]
    (logger/warn (coercer/java->clj params)))

  (^void didChangeWatchedFiles [_ ^DidChangeWatchedFilesParams params]
    (in-completable-future
      (handle-notification params feature-handler/did-change-watched-files handler)))

  ;; TODO wait for lsp4j release
  #_(^void didDeleteFiles [_ ^DeleteFilesParams params]
                          (in-completable-future
                            (handle-notification params handler/did-delete-files handler)))

  (^CompletableFuture symbol [_ ^WorkspaceSymbolParams params]
    (in-completable-future
      (handle-request params feature-handler/workspace-symbols handler ::coercer/workspace-symbols))))

(defn client-capabilities
  [^InitializeParams params]
  (some->> params
           .getCapabilities
           (coercer/conform-or-log ::coercer/client-capabilities)))

(defn ^:private windows-process-alive?
  [pid]
  (let [{:keys [out]} (shell/sh "tasklist" "/fi" (format "\"pid eq %s\"" pid))]
    (string/includes? out (str pid))))

(defn ^:private unix-process-alive?
  [pid]
  (let [{:keys [exit]} (shell/sh "kill" "-0" (str pid))]
    (zero? exit)))

(defn ^:private process-alive?
  [pid]
  (try
    (if (.contains (System/getProperty "os.name") "Windows")
      (windows-process-alive? pid)
      (unix-process-alive? pid))
    (catch Exception e
      (logger/warn "Checking if process is alive failed." e)
      ;; Return true since the check failed. Assume the process is alive.
      true)))

(defn start-parent-process-liveness-probe!
  [ppid server]
  (go-loop []
    (<! (timeout 5000))
    (if (process-alive? ppid)
      (recur)
      (do
        (logger/info "Parent process" ppid "is not running - exiting server")
        (.exit ^LanguageServer server)))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(deftype LSPServer
         [^ILSPFeatureHandler feature-handler
          producer*
          db
          initial-db
          capabilities-fn
          client-settings
          files]
  LanguageServer
  (^CompletableFuture initialize [this ^InitializeParams params]
    (logger/info "Initializing...")
    (feature-handler/initialize feature-handler
                                (.getRootUri params)
                                (client-capabilities params)

                                (-> params
                                    coercer/java->clj
                                    client-settings)
                                (some-> (.getWorkDoneToken params) .get str))
    (when-let [parent-process-id (.getProcessId params)]
      (start-parent-process-liveness-probe! parent-process-id this))
    (let [capabilities (capabilities-fn db)]
      (CompletableFuture/completedFuture
        (InitializeResult. (coercer/conform-or-log ::coercer/server-capabilities capabilities)))))

  (^void initialized [_ ^InitializedParams _params]
    (logger/info "Initialized!")
    (producer/register-capability
      @producer*
      (RegistrationParams.
        [(Registration. "id" "workspace/didChangeWatchedFiles"
                        (DidChangeWatchedFilesRegistrationOptions.
                          [(FileSystemWatcher. files)]))])))

  (^CompletableFuture shutdown [_]
    (logger/info "Shutting down")
    (reset! db initial-db)
    (CompletableFuture/completedFuture
      {:result nil}))
  (exit [_]
    (logger/info "Exiting...")
    (shutdown-agents)
    (System/exit 0))
  (getTextDocumentService [_]
    (LSPTextDocumentService. feature-handler))
  (getWorkspaceService [_]
    (LSPWorkspaceService. feature-handler)))

(defn tee-system-in [^java.io.InputStream system-in]
  (let [buffer-size 1024
        os (java.io.PipedOutputStream.)
        is (java.io.PipedInputStream. os)]
    (thread
      (try
        (let [buffer (byte-array buffer-size)]
          (loop [chs (.read system-in buffer 0 buffer-size)]
            (when (pos? chs)
              (logger/warn "FROM STDIN" chs (String. (java.util.Arrays/copyOfRange buffer 0 chs)))
              (.write os buffer 0 chs)
              (recur (.read system-in buffer 0 buffer-size)))))
        (catch Exception e
          (logger/warn e "in thread"))))
    is))

(defn tee-system-out [^java.io.OutputStream system-out]
  (let [buffer-size 1024
        is (java.io.PipedInputStream.)
        os (java.io.PipedOutputStream. is)]
    (thread
      (try
        (let [buffer (byte-array buffer-size)]
          (loop [chs (.read is buffer 0 buffer-size)]
            (when (pos? chs)
              (logger/warn "FROM STDOUT" chs (String. (java.util.Arrays/copyOfRange buffer 0 chs)))
              (.write system-out buffer)
              (recur (.read is buffer 0 buffer-size)))))
        (catch Exception e
          (logger/error e "in thread"))))
    os))

(defrecord LSPProducer [^LanguageClient client db]
  producer/ILSPProducer

  (publish-diagnostic [_this diagnostic]
    (->> diagnostic
         (coercer/conform-or-log ::coercer/publish-diagnostics-params)
         (.publishDiagnostics client)))

  (refresh-code-lens [_this]
    (when-let [code-lens-capability ^CodeLensWorkspaceCapabilities (get-in @db [:client-capabilities :workspace :code-lens])]
      (when (.getRefreshSupport code-lens-capability)
        (.refreshCodeLenses client))))

  (publish-workspace-edit [_this edit]
    (->> edit
         (coercer/conform-or-log ::coercer/workspace-edit-or-error)
         ApplyWorkspaceEditParams.
         (.applyEdit client)))

  (show-document-request [_this document-request]
    (logger/info "Requesting to show on editor the document" document-request)
    (when (.getShowDocument ^WindowClientCapabilities (get-in @db [:client-capabilities :window]))
      (->> document-request
           (coercer/conform-or-log ::coercer/show-document-request)
           (.showDocument client))))

  (publish-progress [_this percentage message progress-token]
    (let [progress (case (int percentage)
                     0 {:kind :begin
                        :title message
                        :percentage percentage}
                     100 {:kind :end
                          :message message
                          :percentage 100}
                     {:kind :report
                      :message message
                      :percentage percentage})]
      (->> {:token (or progress-token "clojure-lsp")
            :value progress}
           (coercer/conform-or-log ::coercer/notify-progress)
           (.notifyProgress client))))

  (show-message-request [_this message type actions]
    (let [result (->> {:type type
                       :message message
                       :actions actions}
                      (coercer/conform-or-log ::coercer/show-message-request)
                      (.showMessageRequest client)
                      .get)]
      (.getTitle ^MessageActionItem result)))

  (show-message [_this message type extra]
    (let [message-content {:message message
                           :type type
                           :extra extra}]
      (logger/info message-content)
      (->> message-content
           (coercer/conform-or-log ::coercer/show-message)
           (.showMessage client))))

  (register-capability [_this capability]
    (.registerCapability
      client
      capability)))
