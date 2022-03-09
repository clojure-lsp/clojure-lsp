(ns clojure-lsp.server
  (:require
   [clojure-lsp.coercer :as coercer]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.feature.semantic-tokens :as semantic-tokens]
   [clojure-lsp.handler :as handler]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.lsp :as lsp]
   [clojure-lsp.nrepl :as nrepl]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.shared-config :as config]
   [clojure.core.async :refer [<! go-loop]]
   [taoensso.timbre :as log])
  (:import
   (clojure_lsp
     ClojureLanguageServer
     ClojureLanguageClient)
   (clojure_lsp.feature.clojuredocs ClojuredocsParams)
   (clojure_lsp.feature.cursor_info CursorInfoParams)
   (clojure_lsp.lsp LSPTextDocumentService LSPWorkspaceService)
   (java.util.concurrent CompletableFuture
                         CompletionException)
   (org.eclipse.lsp4j
     CodeActionOptions
     CodeLensOptions
     CompletionOptions
     DidChangeWatchedFilesRegistrationOptions
     ExecuteCommandOptions
     FileSystemWatcher
     InitializeParams
     InitializeResult
     InitializedParams
     Registration
     RegistrationParams
     RenameOptions
     SaveOptions
     SemanticTokensLegend
     SemanticTokensWithRegistrationOptions
     ServerCapabilities
     SignatureHelpOptions
     TextDocumentSyncKind
     TextDocumentSyncOptions)
   (org.eclipse.lsp4j.jsonrpc ResponseErrorException
                              Launcher))
  (:gen-class))

(set! *warn-on-reflection* true)

(defmacro ^:private start [id & body]
  `(let [~'_start-time (System/nanoTime)
         ~'_id ~id]
     (do ~@body)))

(defmacro ^:private end
  ([expr]
   `(end ~expr false))
  ([expr extra-log-fn]
   `(let [~'_result (try
                      ~expr
                      (catch Throwable ex#
                        (if (instance? ResponseErrorException ex#)
                          (throw (CompletionException. ex#))
                          (log/error ex#))))]
      (try
        (let [duration# (quot (- (System/nanoTime) ~'_start-time) 1000000)]
          (if ~extra-log-fn
            (log/debug ~'_id (format "%sms - %s" duration# (~extra-log-fn ~'_result)))
            (log/debug ~'_id (format "%sms" duration#))))
        (catch Throwable ex#
          (log/error ex#)))
      ~'_result)))

(deftype ClojureLSPServer [handler]
  ClojureLanguageServer
  (^CompletableFuture initialize [this ^InitializeParams params]
    (start :initialize
           (end
             (do
               (log/info "Initializing...")
               (handler/initialize handler (.getRootUri params)
                                   (lsp/client-capabilities params)
                                   (lsp/client-settings params)
                                   (some-> (.getWorkDoneToken params) .get str))
               (when-let [parent-process-id (.getProcessId params)]
                 (lsp/start-parent-process-liveness-probe! parent-process-id this))
               (let [settings (settings/all db/db)]
                 (CompletableFuture/completedFuture
                   (InitializeResult. (doto (ServerCapabilities.)
                                        (.setDocumentHighlightProvider true)
                                        (.setHoverProvider true)
                                        (.setDeclarationProvider true)
                                        (.setImplementationProvider true)
                                        (.setSignatureHelpProvider (SignatureHelpOptions. []))
                                        (.setCallHierarchyProvider true)
                                        (.setLinkedEditingRangeProvider true)
                                        (.setCodeActionProvider (CodeActionOptions. (vec (vals coercer/code-action-kind))))
                                        (.setCodeLensProvider (CodeLensOptions. true))
                                        (.setReferencesProvider true)
                                        (.setRenameProvider (RenameOptions. true))
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
                                        (.setCompletionProvider (CompletionOptions. true [":" "/"]))
                                        (.setExperimental {"testTree" true
                                                           "cursorInfo" true
                                                           "serverInfo" true
                                                           "clojuredocs" true})))))))))

  (^void initialized [_ ^InitializedParams _params]
    (start :initialized
           (end
             (do
               (log/info "Initialized!")
               (producer/register-capability
                 (:producer @db/db)
                 (RegistrationParams.
                   [(Registration. "id" "workspace/didChangeWatchedFiles"
                                   (DidChangeWatchedFilesRegistrationOptions.
                                     [(FileSystemWatcher. "**/*.{clj,cljs,cljc,edn}")]))]))))))

  (^CompletableFuture serverInfoRaw [_]
    (CompletableFuture/completedFuture
      (->> (handler/server-info-raw handler)
           (coercer/conform-or-log ::coercer/server-info-raw))))

  (^void serverInfoLog [_]
    (start :server-info-log
           (future
             (end
               (handler/server-info-log handler)))))

  (^CompletableFuture cursorInfoRaw [_ ^CursorInfoParams params]
    (start :cursorInfoRaw
           (CompletableFuture/completedFuture
             (lsp/sync-request params handler/cursor-info-raw handler ::coercer/cursor-info-raw))))

  (^void cursorInfoLog [_ ^CursorInfoParams params]
    (start :cursor-info-log
           (future
             (lsp/sync-notification params handler/cursor-info-log handler))))

  (^CompletableFuture clojuredocsRaw [_ ^ClojuredocsParams params]
    (start :clojuredocsRaw
           (CompletableFuture/completedFuture
             (lsp/sync-request params handler/clojuredocs-raw handler ::coercer/clojuredocs-raw))))

  (^CompletableFuture shutdown [_]
    (log/info "Shutting down")
    (reset! db/db {:documents {}})
    (CompletableFuture/completedFuture
      {:result nil}))
  (exit [_]
    (log/info "Exitting...")
    (shutdown-agents)
    (System/exit 0))
  (getTextDocumentService [_]
    (LSPTextDocumentService. handler))
  (getWorkspaceService [_]
    (LSPWorkspaceService. handler)))

(defn run-server! []
  (log/info "Starting server...")
  (let [is (or System/in (lsp/tee-system-in System/in))
        os (or System/out (lsp/tee-system-out System/out))
        handler (handlers/->ClojureFeatureHandler)
        launcher (Launcher/createLauncher (ClojureLSPServer. handler) ClojureLanguageClient is os)
        debounced-diags (shared/debounce-by db/diagnostics-chan config/diagnostics-debounce-ms :uri)
        debounced-changes (shared/debounce-by db/current-changes-chan config/change-debounce-ms :uri)
        debounced-created-watched-files (shared/debounce-all db/created-watched-files-chan config/created-watched-files-debounce-ms)
        producer (lsp/->LSPProducer ^ClojureLanguageClient (.getRemoteProxy launcher) db/db)]
    (nrepl/setup-nrepl db/db)
    (swap! db/db assoc :producer producer)
    (swap! db/db assoc :handler handler)
    (go-loop [edit (<! db/edits-chan)]
      (producer/publish-workspace-edit producer edit)
      (recur (<! db/edits-chan)))
    (go-loop []
      (producer/publish-diagnostic producer (<! debounced-diags))
      (recur))
    (go-loop []
      (try
        (f.file-management/analyze-changes (<! debounced-changes) db/db)
        (catch Exception e
          (log/error e "Error during analyzing buffer file changes")))
      (recur))
    (go-loop []
      (try
        (f.file-management/analyze-watched-created-files! (<! debounced-created-watched-files) db/db)
        (catch Exception e
          (log/error e "Error during analyzing created watched files")))
      (recur))
    (.startListening launcher)))
