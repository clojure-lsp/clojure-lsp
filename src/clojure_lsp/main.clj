(ns clojure-lsp.main
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.handlers :as handlers]
    [clojure-lsp.interop :as interop]
    [clojure-lsp.parser :as parser]
    [clojure.core.async :as async]
    [clojure.tools.logging :as log]
    [clojure.tools.nrepl.server :as nrepl.server]
    [trptcolin.versioneer.core :as version])
  (:import
    (clojure_lsp ClojureExtensions)
    (org.eclipse.lsp4j.services LanguageServer TextDocumentService WorkspaceService LanguageClient)
    (org.eclipse.lsp4j
      ApplyWorkspaceEditParams
      CodeActionParams
      Command
      CompletionItemKind
      CompletionOptions
      CompletionParams
      ConfigurationItem
      ConfigurationParams
      DidChangeConfigurationParams
      DidChangeTextDocumentParams
      DidChangeWatchedFilesParams
      DidCloseTextDocumentParams
      DidOpenTextDocumentParams
      DidSaveTextDocumentParams
      DocumentFormattingParams
      DocumentRangeFormattingParams
      ExecuteCommandOptions
      ExecuteCommandParams
      InitializeParams
      InitializeResult
      InitializedParams
      ParameterInformation
      ReferenceParams
      RenameParams
      SaveOptions
      ServerCapabilities
      SignatureHelp
      SignatureHelpOptions
      SignatureInformation
      TextDocumentContentChangeEvent
      TextDocumentPositionParams
      TextDocumentSyncKind
      TextDocumentSyncOptions)
    (org.eclipse.lsp4j.launch LSPLauncher)
    (org.eclipse.lsp4j.jsonrpc.services JsonSegment JsonRequest)
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
             (log/info ~'_id duration# running#)))
         (catch Throwable ex#
           (log/error ex#))))))

(deftype LSPTextDocumentService []
  TextDocumentService
  (^void didOpen [_ ^DidOpenTextDocumentParams params]
    (go :didOpen
        (end
          (let [document (.getTextDocument params)]
            (#'handlers/did-open (parser/document->decoded-uri document) (.getText document))))))

  (^void didChange [_ ^DidChangeTextDocumentParams params]
    (go :didChange
        (end
          (let [textDocument (.getTextDocument params)
                version (.getVersion textDocument)
                changes (.getContentChanges params)
                text (.getText ^TextDocumentContentChangeEvent (.get changes 0))
                uri (parser/document->decoded-uri textDocument)]
            (#'handlers/did-change uri text version)))))

  (^void didSave [_ ^DidSaveTextDocumentParams _params]
    (go :didSave
        (end nil)))

  (^void didClose [_ ^DidCloseTextDocumentParams params]
    (log/warn "DidCloseTextDocumentParams")
    (go :didClose
        (end (swap! db/db update :documents dissoc (parser/document->decoded-uri (.getTextDocument params))))))

  (^CompletableFuture references [this ^ReferenceParams params]
    (go :references
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (try
                  (let [doc-id (parser/document->decoded-uri (.getTextDocument params))
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
                  (let [doc-id (parser/document->decoded-uri (.getTextDocument params))
                        pos (.getPosition params)
                        line (inc (int (.getLine pos)))
                        column (inc (int (.getCharacter pos)))]
                    (interop/conform-or-log ::interop/completion-items (#'handlers/completion doc-id line column)))
                  (catch Exception e
                    (log/error e)))))))))

  (^CompletableFuture rename [this ^RenameParams params]
    (go :rename
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (try
                  (let [doc-id (parser/document->decoded-uri (.getTextDocument params))
                        pos (.getPosition params)
                        line (inc (.getLine pos))
                        column (inc (.getCharacter pos))
                        new-name (.getNewName params)]
                    (interop/conform-or-log ::interop/workspace-edit (#'handlers/rename doc-id line column new-name)))
                  (catch Exception e
                    (log/error e)))))))))

  (^CompletableFuture hover [this ^TextDocumentPositionParams params]
    (go :hover
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (try
                  (let [doc-id (parser/document->decoded-uri (.getTextDocument params))
                        pos (.getPosition params)
                        line (inc (.getLine pos))
                        column (inc (.getCharacter pos))]
                    (interop/conform-or-log ::interop/hover (#'handlers/hover doc-id line column)))
                  (catch Exception e
                    (log/error e)))))))))

  (^CompletableFuture signatureHelp [_ ^TextDocumentPositionParams _params]
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
                  (let [doc-id (parser/document->decoded-uri (.getTextDocument params))]
                    (interop/conform-or-log ::interop/edits (#'handlers/formatting doc-id)))
                  (catch Exception e
                    (log/error e)))))))))

  (^CompletableFuture rangeFormatting [_this ^DocumentRangeFormattingParams params]
    (go :rangeFormatting
        (end
          (let [result (when (compare-and-set! formatting false true)
                         (try
                           (let [doc-id (parser/document->decoded-uri (.getTextDocument params))
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
        (end
          (CompletableFuture/completedFuture
            (let [start (.getStart (.getRange params))]
              [(Command. "add-missing-libspec" "add-missing-libspec"
                         [(parser/document->decoded-uri (.getTextDocument params)) (.getLine start) (.getCharacter start)])])))))

  (^CompletableFuture definition [this ^TextDocumentPositionParams params]
    (go :definition
        (CompletableFuture/supplyAsync
          (reify Supplier
            (get [this]
              (end
                (try
                  (let [doc-id (parser/document->decoded-uri (.getTextDocument params))
                        pos (.getPosition params)
                        line (inc (.getLine pos))
                        column (inc (.getCharacter pos))]
                    (interop/conform-or-log ::interop/location (#'handlers/definition doc-id line column)))
                  (catch Exception e
                    (log/error e))))))))))

(deftype LSPWorkspaceService []
  WorkspaceService
  (^CompletableFuture executeCommand [_ ^ExecuteCommandParams params]
    (go :executeCommand
        (let [[doc-id line col & args] (map interop/json->clj (.getArguments params))
              command (.getCommand params)]
          (future
            (end
              (try
                (when-let [result (#'handlers/refactor doc-id
                                                       (inc (int line))
                                                       (inc (int col))
                                                       command
                                                       args)]
                  (.get (.applyEdit (:client @db/db)
                                    (ApplyWorkspaceEditParams.
                                      (interop/conform-or-log ::interop/workspace-edit result)))))
                (catch Exception e
                  (log/error e)))))))
    (CompletableFuture/completedFuture 0))
  (^void didChangeConfiguration [_ ^DidChangeConfigurationParams params]
    (log/warn params))
  (^void didChangeWatchedFiles [_ ^DidChangeWatchedFilesParams _params]
    (log/warn "DidChangeWatchedFilesParams")))

(defn client-settings [^InitializeParams params]
  (-> params
      (.getInitializationOptions)
      (interop/json->clj)
      (or {})
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
              (log/warn "Initialize")
              (#'handlers/initialize (.getRootUri params)
                                     (client-capabilities params)
                                     (client-settings params))
              (CompletableFuture/completedFuture
                (InitializeResult. (doto (ServerCapabilities.)
                                     (.setHoverProvider true)
                                     (.setCodeActionProvider true)
                                     (.setReferencesProvider true)
                                     (.setRenameProvider true)
                                     (.setDefinitionProvider true)
                                     (.setDocumentFormattingProvider true)
                                     (.setDocumentRangeFormattingProvider true)
                                     (.setExecuteCommandProvider (doto (ExecuteCommandOptions.)
                                                                   (.setCommands (keys handlers/refactorings))))
                                     (.setTextDocumentSync (doto (TextDocumentSyncOptions.)
                                                             (.setOpenClose true)
                                                             (.setChange TextDocumentSyncKind/Full)
                                                             (.setSave (SaveOptions. true))))
                                     (.setCompletionProvider (CompletionOptions. false [\c])))))))))
    (^void initialized [^InitializedParams params]
      (log/warn "Initialized" params))
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
  (log/info "Server started")
  (let [launcher (LSPLauncher/createServerLauncher server System/in System/out)
        repl-server (nrepl.server/start-server)]
    (log/info "====== LSP nrepl server started on port" (:port repl-server))
    (swap! db/db assoc :client ^LanguageClient (.getRemoteProxy launcher))
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
