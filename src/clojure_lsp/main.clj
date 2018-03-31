(ns clojure-lsp.main
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.handlers :as handlers]
    [clojure.tools.logging :as log])
  (:import
    (org.eclipse.lsp4j.services LanguageServer TextDocumentService WorkspaceService)
    (org.eclipse.lsp4j
      InitializedParams InitializeParams InitializeResult ServerCapabilities CompletionOptions DidOpenTextDocumentParams DidChangeTextDocumentParams DidSaveTextDocumentParams DidCloseTextDocumentParams TextDocumentPositionParams CompletionItem TextEdit Range Position DidChangeConfigurationParams DidChangeWatchedFilesParams TextDocumentSyncOptions TextDocumentSyncKind SaveOptions CompletionItemKind ReferenceParams Location TextDocumentContentChangeEvent RenameParams)
    (org.eclipse.lsp4j.launch LSPLauncher)
    (java.util.concurrent CompletableFuture)
    (java.util.function Supplier)))

(deftype LSPTextDocumentService []
  TextDocumentService
  (^void didOpen [this ^DidOpenTextDocumentParams params]
    (log/warn "DidOpenTextDocumentParams")
    (let [document (.getTextDocument params)]
      (handlers/did-open (.getUri document) (.getText document))))

  (^void didChange [this ^DidChangeTextDocumentParams params]
    (log/warn "DidChangeTextDocumentParams")
    (let [textDocument (.getTextDocument params)
          version (.getVersion textDocument)
          changes (.getContentChanges params)
          text (.getText ^TextDocumentContentChangeEvent (.get changes 0))
          uri (.getUri textDocument)]
      (handlers/did-change uri text version)))

  (^void didSave [this ^DidSaveTextDocumentParams params]
    (log/warn "DidSaveTextDocumentParams"))
  (^void didClose [this ^DidCloseTextDocumentParams params]
    (log/warn "DidCloseTextDocumentParams")
    (swap! db/db update :documents dissoc (.getUri (.getTextDocument params))))

  (^CompletableFuture references [this ^ReferenceParams params]
    (log/warn params)
    (CompletableFuture/supplyAsync
      (reify Supplier
        (get [this]
          (try
            (let [doc-id (.getUri (.getTextDocument params))
                  pos (.getPosition params)
                  line (inc (.getLine pos))
                  column (inc (.getCharacter pos))]
              (handlers/references doc-id line column))
            (catch Exception e
              (log/error e)))))))

  (^CompletableFuture completion [this ^TextDocumentPositionParams params]
    (log/warn params)
    (CompletableFuture/supplyAsync
      (reify Supplier
        (get [this]
          (try
            (let [doc-id (.getUri (.getTextDocument params))
                  pos (.getPosition params)
                  line (inc (.getLine pos))
                  column (inc (.getCharacter pos))]
              (handlers/completion doc-id line column))
            (catch Exception e
              (log/error e)))))))

  (^CompletableFuture rename [this ^RenameParams params]
    (log/warn params)
    (CompletableFuture/supplyAsync
      (reify Supplier
        (get [this]
          (try
            (let [doc-id (.getUri (.getTextDocument params))
                  pos (.getPosition params)
                  line (inc (.getLine pos))
                  column (inc (.getCharacter pos))
                  new-name (.getNewName params)]
              (handlers/rename doc-id line column new-name))
            (catch Exception e
              (log/error e))))))
    ))

(deftype LSPWorkspaceService []
  WorkspaceService
  (^void didChangeConfiguration [this ^DidChangeConfigurationParams params]
    (log/warn params))
  (^void didChangeWatchedFiles [this ^DidChangeWatchedFilesParams params]
    (log/warn "DidChangeWatchedFilesParams")))

(defrecord LSPServer []
  LanguageServer
  (^CompletableFuture initialize [this ^InitializeParams params]
    (handlers/initialize (.getRootUri params))
    (CompletableFuture/completedFuture
      (InitializeResult. (doto (ServerCapabilities.)
                           (.setReferencesProvider true)
                           (.setRenameProvider true)
                           (.setTextDocumentSync (doto (TextDocumentSyncOptions.)
                                                   (.setOpenClose true)
                                                   (.setChange TextDocumentSyncKind/Full)
                                                   (.setSave (SaveOptions. true))))
                           (.setCompletionProvider (CompletionOptions. false [\c]))))))
  (^void initialized [this ^InitializedParams params]
    (log/warn "HELLO" params))
  (^CompletableFuture shutdown [this]
    (log/warn "bye")
    (reset! db/db {:documents {}}) ;; TODO confirm this is correct
    (CompletableFuture/completedFuture
      {:result nil}))
  (exit [this]
    (System/exit 1))
  (getTextDocumentService [this]
    (LSPTextDocumentService.))
  (getWorkspaceService [this]
    (LSPWorkspaceService.)))

(defn -main [& args]
  (let [server (LSPServer.)
        launcher (LSPLauncher/createServerLauncher server System/in System/out)]
    (swap! db/db assoc :client (.getRemoteProxy launcher))
    (.startListening launcher)))
