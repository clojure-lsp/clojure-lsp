(ns clojure-lsp.main
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.handlers :as handlers]
    [clojure.tools.logging :as log]
    [clojure.spec.alpha :as s])
  (:import
    (org.eclipse.lsp4j.services LanguageServer TextDocumentService WorkspaceService)
    (org.eclipse.lsp4j
      CompletionItem
      CompletionItemKind
      CompletionOptions
      DidChangeConfigurationParams
      DidChangeTextDocumentParams
      DidChangeWatchedFilesParams
      DidCloseTextDocumentParams
      DidOpenTextDocumentParams
      DidSaveTextDocumentParams
      InitializeParams
      InitializeResult
      InitializedParams
      Location
      Position
      Range
      ReferenceParams
      RenameParams
      SaveOptions
      ServerCapabilities
      TextDocumentContentChangeEvent
      TextDocumentEdit
      TextDocumentPositionParams
      TextDocumentSyncKind
      TextDocumentSyncOptions
      TextEdit
      VersionedTextDocumentIdentifier
      WorkspaceEdit)
    (org.eclipse.lsp4j.launch LSPLauncher)
    (java.util.concurrent CompletableFuture)
    (java.util.function Supplier)))


(s/def ::line integer?)
(s/def ::character integer?)
(s/def ::position (s/and (s/keys :req-un [::line ::character])
                         (s/conformer #(Position. (:line %1) (:character %1)))))
(s/def ::start ::position)
(s/def ::end ::position)
(s/def ::range (s/and (s/keys :req-un [::start ::end])
                      (s/conformer #(Range. (:start %1) (:end %1)))))
(s/def ::new-text string?)
(s/def ::text-edit (s/and (s/keys :req-un [::new-text ::range])
                          (s/conformer #(TextEdit. (:range %1) (:new-text %1)))))
(s/def ::additional-text-edits (s/coll-of ::text-edit))
(s/def ::completion-item (s/and (s/keys :req-un [::label]
                                        :opt-un [::additional-text-edits])
                                (s/conformer (fn [{:keys [label additional-text-edits]}]
                                               (let [item (CompletionItem. label)]
                                                 (when additional-text-edits
                                                   (.setAdditionalTextEdits item additional-text-edits))
                                                 item)))))
(s/def ::completion-items (s/coll-of ::completion-item))
(s/def ::version integer?)
(s/def ::uri string?)
(s/def ::text-edit ::text-edit)
(s/def ::text-document (s/and (s/keys :req-un [::version ::uri])
                              (s/conformer #(doto (VersionedTextDocumentIdentifier. (:version %1))
                                              (.setUri (:uri %1))))))
(s/def ::text-document-edit (s/and (s/keys :req-un [::text-document ::edits])
                                   (s/conformer #(TextDocumentEdit. (:text-document %1) (:edits %1)))))
(s/def ::document-changes (s/coll-of ::text-document-edit))
(s/def ::workspace-edit (s/and (s/keys :opt-un [::document-changes])
                               (s/conformer #(WorkspaceEdit. (:document-changes %1)))))
(s/def ::location (s/and (s/keys :req-un [::uri ::range])
                         (s/conformer #(Location. (:uri %1) (:range %1)))))

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
              (s/conform ::location (handlers/references doc-id line column)))
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
              (s/conform ::completion-items (handlers/completion doc-id line column)))
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
              (s/conform ::workspace-edit (handlers/rename doc-id line column new-name)))
            (catch Exception e
              (log/error e)))))))
  (^CompletableFuture definition [this ^TextDocumentPositionParams params]
    (CompletableFuture/supplyAsync
      (reify Supplier
        (get [this]
          (try (let [doc-id (.getUri (.getTextDocument params))
                     pos (.getPosition params)
                     line (inc (.getLine pos))
                     column (inc (.getCharacter pos))]
                 (s/conform ::location (handlers/definition doc-id line column)))))))))

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
                           (.setDefinitionProvider true)
                           (.setTextDocumentSync (doto (TextDocumentSyncOptions.)
                                                   (.setOpenClose true)
                                                   (.setChange TextDocumentSyncKind/Full)
                                                   (.setSave (SaveOptions. true))))
                           (.setCompletionProvider (CompletionOptions. false [\c]))))))
  (^void initialized [this ^InitializedParams params]
    (log/warn "Initialized" params))
  (^CompletableFuture shutdown [this]
    (log/info "Shutting down")
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
  (log/info "Server started")
  (let [server (LSPServer.)
        launcher (LSPLauncher/createServerLauncher server System/in System/out)]
    (swap! db/db assoc :client (.getRemoteProxy launcher))
    (.startListening launcher)))
