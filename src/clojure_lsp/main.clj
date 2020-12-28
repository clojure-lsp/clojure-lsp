(ns clojure-lsp.main
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.features.refactor :as f.refactor]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.interop :as interop]
   [clojure-lsp.semantic-tokens :as semantic-tokens]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.tools.logging :as log]
   [nrepl.server :as nrepl.server]
   [trptcolin.versioneer.core :as version])
  (:import
   (clojure_lsp ClojureExtensions)
   (org.eclipse.lsp4j.services LanguageServer TextDocumentService WorkspaceService LanguageClient)
   (org.eclipse.lsp4j
     ApplyWorkspaceEditParams
     CodeActionParams
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

(defn ^:private execute-server-info []
  (.showMessage (:client @db/db)
                (interop/conform-or-log ::interop/show-message (#'handlers/server-info))))

(defn ^:private execute-refactor [command args]
  (let [[doc-id line col & args] (map interop/json->clj args)]
    (when-let [result (#'handlers/refactor doc-id
                                           (inc (int line))
                                           (inc (int col))
                                           command
                                           args)]
      (.get (.applyEdit (:client @db/db)
                        (ApplyWorkspaceEditParams.
                          (interop/conform-or-log ::interop/workspace-edit result)))))))

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
    (log/warn "DidCloseTextDocumentParams")
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
                (try
                  (let [doc-id (interop/document->decoded-uri (.getTextDocument params))
                        pos (.getPosition params)
                        line (inc (.getLine pos))
                        column (inc (.getCharacter pos))]
                    (interop/conform-or-log ::interop/references (#'handlers/references doc-id line column)))
                  (catch Exception e
                    (log/error e)))))))))

  (^CompletableFuture completion [this ^CompletionParams params]
    (go :completion
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (try
                  (let [doc-id (interop/document->decoded-uri (.getTextDocument params))
                        pos (.getPosition params)
                        line (inc (int (.getLine pos)))
                        column (inc (int (.getCharacter pos)))]
                    (interop/conform-or-log ::interop/completion-items (#'handlers/completion doc-id line column)))
                  (catch Exception e
                    (log/error e)))))))))

  (^CompletableFuture resolveCompletionItem [this ^CompletionItem item]
    (go :resolveCompletionItem
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (try
                  (let [label (.getLabel item)
                        sym-wanted (interop/json->clj (.getData item))]
                    (interop/conform-or-log ::interop/completion-item (#'handlers/resolve-completion-item label sym-wanted)))
                  (catch Exception e
                    (log/error e)))))))))

  (^CompletableFuture rename [this ^RenameParams params]
    (go :rename
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (try
                  (let [doc-id (interop/document->decoded-uri (.getTextDocument params))
                        pos (.getPosition params)
                        line (inc (.getLine pos))
                        column (inc (.getCharacter pos))
                        new-name (.getNewName params)
                        edit (#'handlers/rename doc-id line column new-name)]
                    (interop/conform-or-log ::interop/workspace-edit edit))
                  (catch Exception e
                    (log/error e)))))))))

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
                (try
                  (let [doc-id (interop/document->decoded-uri (.getTextDocument params))]
                    (interop/conform-or-log ::interop/edits (#'handlers/formatting doc-id)))
                  (catch Exception e
                    (log/error e)))))))))

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
               (try
                 (let [doc-id          (interop/document->decoded-uri (.getTextDocument params))
                       diagnostics     (.getDiagnostics (.getContext params))
                       start           (.getStart (.getRange params))
                       start-line      (.getLine start)
                       start-character (.getCharacter start)]
                   (interop/conform-or-log ::interop/code-actions (#'handlers/code-actions doc-id diagnostics start-line start-character)))
                 (catch Exception e
                   (log/error e)))))))))

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
                (try
                  (let [doc-id (interop/document->decoded-uri (.getTextDocument params))
                        pos (.getPosition params)
                        line (inc (.getLine pos))
                        column (inc (.getCharacter pos))]
                    (interop/conform-or-log ::interop/location (#'handlers/definition doc-id line column)))
                  (catch Exception e
                    (log/error e)))))))))

  (^CompletableFuture documentSymbol [this ^DocumentSymbolParams params]
    (go :documentSymbol
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (try
                  (let [doc-id (interop/document->decoded-uri (.getTextDocument params))]
                    (interop/conform-or-log ::interop/document-symbols (#'handlers/document-symbol doc-id)))
                  (catch Exception e
                    (log/error e)))))))))

  (^CompletableFuture documentHighlight [this ^DocumentHighlightParams params]
    (go :documentHighlight
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (try
                  (let [doc-id (interop/document->decoded-uri (.getTextDocument params))
                        pos (.getPosition params)
                        line (inc (.getLine pos))
                        column (inc (.getCharacter pos))]
                    (interop/conform-or-log ::interop/document-highlights (#'handlers/document-highlight doc-id line column)))
                  (catch Exception e
                    (log/error e)))))))))

  (^CompletableFuture semanticTokensFull [_ ^SemanticTokensParams params]
    (go :semanticTokensFull
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [_this]
              (end
                (let [doc-id (interop/document->decoded-uri (.getTextDocument params))]
                  (interop/conform-or-log ::interop/semantic-tokens (#'handlers/semantic-tokens-full doc-id))))))))))

(deftype LSPWorkspaceService []
  WorkspaceService

  (^CompletableFuture executeCommand [_ ^ExecuteCommandParams params]
    (go :executeCommand
        (future
          (end
            (let [command (.getCommand params)
                  args (.getArguments params)]
              (log/info "Executing command" command "with args" args)
              (cond
                (= command "server-info")
                (execute-server-info)

                (some #(= % command) f.refactor/available-refactors)
                (execute-refactor command args))))))
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
                (try
                  (let [query (.getQuery params)]
                    (interop/conform-or-log ::interop/workspace-symbols (#'handlers/workspace-symbols query)))
                  (catch Exception e
                    (log/error e))))))))))

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
              (let [settings (:settings @db/db)]
                (CompletableFuture/completedFuture
                  (InitializeResult. (doto (ServerCapabilities.)
                                       (.setHoverProvider true)
                                       (.setCodeActionProvider true)
                                       (.setCodeLensProvider (CodeLensOptions. true))
                                       (.setReferencesProvider true)
                                       (.setRenameProvider true)
                                       (.setDefinitionProvider true)
                                       (.setDocumentFormattingProvider (:document-formatting? settings))
                                       (.setDocumentRangeFormattingProvider (:document-range-formatting? settings))
                                       (.setDocumentSymbolProvider true)
                                       (.setDocumentHighlightProvider true)
                                       (.setWorkspaceSymbolProvider true)
                                       (.setSemanticTokensProvider (when (:semantic-tokens? settings)
                                                                     (doto (SemanticTokensWithRegistrationOptions.)
                                                                       (.setLegend (doto (SemanticTokensLegend.
                                                                                           semantic-tokens/token-types-str
                                                                                           semantic-tokens/token-modifiers)))
                                                                       (.setRange false)
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

(defn- run []
  (log/info "Starting server...")
  (let [launcher (LSPLauncher/createServerLauncher server System/in System/out)
        repl-server (nrepl.server/start-server)
        port (:port repl-server)]
    (log/info "====== LSP nrepl server started on port" port)
    (swap! db/db assoc
           :client ^LanguageClient (.getRemoteProxy launcher)
           :port port)
    (async/go
      (loop [edit (async/<! db/edits-chan)]
        (log/warn "edit applied?" (.get (.applyEdit (:client @db/db) (ApplyWorkspaceEditParams. (interop/conform-or-log ::interop/workspace-edit edit)))))
        (recur (async/<! db/edits-chan))))
    (async/go
      (loop [diagnostic (async/<! db/diagnostics-chan)]
        (.publishDiagnostics (:client @db/db) (interop/conform-or-log ::interop/publish-diagnostics-params diagnostic))
        (recur (async/<! db/diagnostics-chan))))
    (.startListening launcher)))

(defn -main [& args]
  (if (empty? args)
    (run)
    (println "clojure-lsp" (version/get-version "clojure-lsp" "clojure-lsp"))))
