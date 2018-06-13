(ns clojure-lsp.main
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]
   [clojure.tools.logging :as log]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [clojure.core.async :as async]
   [medley.core :as medley])
  (:import
   (org.eclipse.lsp4j.services LanguageServer TextDocumentService WorkspaceService LanguageClient)
   (org.eclipse.lsp4j
     CompletionItem
     CompletionItemKind
     CompletionOptions
     CompletionParams
     ConfigurationParams
     DidChangeConfigurationParams
     DidChangeTextDocumentParams
     DidChangeWatchedFilesParams
     DidCloseTextDocumentParams
     DidOpenTextDocumentParams
     DidSaveTextDocumentParams
     DocumentFormattingParams
     DocumentRangeFormattingParams
     Hover
     InitializeParams
     InitializeResult
     InitializedParams
     Location
     MarkupContent
     ParameterInformation
     Position
     Range
     ReferenceParams
     RenameParams
     SaveOptions
     ServerCapabilities
     SignatureHelp
     SignatureHelpOptions
     SignatureInformation
     TextDocumentContentChangeEvent
     TextDocumentEdit
     TextDocumentPositionParams
     TextDocumentSyncKind
     TextDocumentSyncOptions
     TextEdit
     VersionedTextDocumentIdentifier
     WorkspaceEdit
     ExecuteCommandParams
     ApplyWorkspaceEditParams
     PublishDiagnosticsParams
     Diagnostic
     DiagnosticSeverity MarkedString CodeActionParams Command ConfigurationItem)
   (org.eclipse.lsp4j.launch LSPLauncher)
   (java.util.concurrent CompletableFuture)
   (java.util.function Supplier)
   (org.eclipse.lsp4j.jsonrpc.messages Either))
  (:gen-class))

(s/def ::line (s/and integer? (s/conformer int)))
(s/def ::character (s/and integer? (s/conformer int)))
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
(s/def ::version (s/and integer? (s/conformer int)))
(s/def ::uri string?)
(s/def ::edits (s/coll-of ::text-edit))
(s/def ::text-document (s/and (s/keys :req-un [::version ::uri])
                              (s/conformer #(doto (VersionedTextDocumentIdentifier. (:version %1))
                                              (.setUri (:uri %1))))))
(s/def ::text-document-edit (s/and (s/keys :req-un [::text-document ::edits])
                                   (s/conformer #(TextDocumentEdit. (:text-document %1) (:edits %1)))))
(s/def ::changes (s/coll-of (s/tuple string? ::edits) :kind map?))
(s/def ::document-changes (s/coll-of ::text-document-edit))
(s/def ::workspace-edit (s/and (s/keys :opt-un [::document-changes ::changes])
                               (s/conformer #(WorkspaceEdit. (:changes %1) (:document-changes %1)))))
(s/def ::location (s/and (s/keys :req-un [::uri ::range])
                         (s/conformer #(Location. (:uri %1) (:range %1)))))
(s/def ::references (s/coll-of ::location))

(s/def ::severity (s/and integer?
                         (s/conformer #(DiagnosticSeverity/forValue %1))))

(s/def ::code (s/conformer name))

(s/def ::diagnostic (s/and (s/keys :req-un [::range ::message]
                                   :opt-un [::severity ::code ::source ::message])
                           (s/conformer #(Diagnostic. (:range %1) (:message %1) (:severity %1) (:source %1) (:code %1)))))
(s/def ::diagnostics (s/coll-of ::diagnostic))
(s/def ::publish-diagnostics-params (s/and (s/keys :req-un [::uri ::diagnostics])
                                           (s/conformer #(PublishDiagnosticsParams. (:uri %1) (:diagnostics %1)))))

(s/def ::marked-string (s/and (s/or :string string?
                                    :marked-string (s/and (s/keys :req-un [::language ::value])
                                                          (s/conformer #(MarkedString. (:language %1) (:value %1)))))
                              (s/conformer (fn [v]
                                             (case (first v)
                                               :string (Either/forLeft (second v))
                                               :marked-string (Either/forRight (second v)))))))

(s/def :markup/kind #{"plaintext" "markdown"})
(s/def :markup/value string?)
(s/def ::markup-content (s/and (s/keys :req-un [:markup/kind :markup/value])
                               (s/conformer #(doto (MarkupContent.)
                                              (.setKind (:kind %1))
                                              (.setValue (:value %1))))))

(s/def ::contents (s/and (s/or :marked-strings (s/coll-of ::marked-string)
                               :markup-content ::markup-content)
                         (s/conformer second)))

(s/def ::hover (s/and (s/keys :req-un [::contents]
                              :opt-un [::range])
                      (s/conformer #(Hover. (:contents %1) (:range %1)))))

(defn debeaner [inst]
  (when inst
    (->> (dissoc (bean inst) :class)
         (into {})
         (medley/remove-vals nil?)
         (medley/map-keys #(as-> % map-key
                             (name map-key)
                             (string/split map-key #"(?=[A-Z])")
                             (string/join "-" map-key)
                             (string/lower-case map-key)
                             (keyword map-key))))))

(s/def ::debean (s/conformer debeaner))
(s/def ::value-set (s/conformer (fn [value-set]
                                  (set (map #(.getValue %) value-set)))))
(s/def :capabilities/code-action ::debean)
(s/def :capabilities/code-lens ::debean)
(s/def :capabilities/color-provider ::debean)
(s/def :capabilities/completion-item ::debean)
(s/def :capabilities/definition ::debean)
(s/def :capabilities/document-highlight ::debean)
(s/def :capabilities/document-link ::debean)
(s/def :capabilities/formatting ::debean)
(s/def :capabilities/implementation ::debean)
(s/def :capabilities/on-type-formatting ::debean)
(s/def :capabilities/publish-diagnostics ::debean)
(s/def :capabilities/range-formatting ::debean)
(s/def :capabilities/references ::debean)
(s/def :capabilities/rename ::debean)
(s/def :capabilities/signature-information ::debean)
(s/def :capabilities/synchronization ::debean)
(s/def :capabilities/type-definition ::debean)

(s/def :capabilities/symbol-kind (s/and ::debean
                                        (s/keys :opt-un [::value-set])))
(s/def :capabilities/document-symbol (s/and ::debean
                                            (s/keys :opt-un [:capabilities/symbol-kind])))
(s/def :capabilities/signature-help (s/and ::debean
                                           (s/keys :opt-un [:capabilities/signature-information])))

(s/def :capabilities/completion-item-kind (s/and ::debean
                                                 (s/keys :opt-un [::value-set])))
(s/def :capabilities/completion (s/and ::debean
                                       (s/keys :opt-un [:capabilities/completion-item
                                                        :capabilities/completion-item-kind])))
(s/def :capabilities/hover (s/and ::debean
                                  (s/keys :opt-un [:capabilities/content-format])))
(s/def :capabilities/text-document (s/and ::debean
                                          (s/keys :opt-un [:capabilities/hover
                                                           :capabilities/completion
                                                           :capabilities/definition
                                                           :capabilities/formatting
                                                           :capabilities/publish-diagnostics
                                                           :capabilities/code-action
                                                           :capabilities/document-symbol
                                                           :capabilities/code-lens
                                                           :capabilities/document-highlight
                                                           :capabilities/color-provider
                                                           :capabilities/type-definition
                                                           :capabilities/rename
                                                           :capabilities/references
                                                           :capabilities/document-link
                                                           :capabilities/synchronization
                                                           :capabilities/range-formatting
                                                           :capabilities/on-type-formatting
                                                           :capabilities/signature-help
                                                           :capabilities/implementation])))

(s/def :capabilities/workspace-edit ::debean)
(s/def :capabilities/workspace-edit ::debean)
(s/def :capabilities/did-change-configuration ::debean)
(s/def :capabilities/did-change-watched-files ::debean)
(s/def :capabilities/execute-command ::debean)
(s/def :capabilities/symbol (s/and ::debean
                                  (s/keys :opt-un [:capabilities/symbol-kind])))
(s/def :capabilities/workspace (s/and ::debean
                                      (s/keys :opt-un [:capabilities/workspace-edit
                                                       :capabilities/did-change-configuration
                                                       :capabilities/did-change-watched-files
                                                       :capabilities/execute-command
                                                       :capabilities/symbol])))
(s/def ::client-capabilities (s/and ::debean
                                    (s/keys :opt-un [:capabilities/workspace :capabilities/text-document])))

(defn conform-or-log [spec value]
  (when value
    (try
      (let [result (s/conform spec value)]
        (if (= :clojure.spec.alpha/invalid result)
          (log/error (s/explain-data spec value))
          result))
      (catch Exception ex
        (log/error ex spec value)))))

(defonce formatting (atom false))

(deftype LSPTextDocumentService []
  TextDocumentService
  (^void didOpen [this ^DidOpenTextDocumentParams params]
    (log/warn "DidOpenTextDocumentParams")
    (let [document (.getTextDocument params)]
      (#'handlers/did-open (.getUri document) (.getText document))))

  (^void didChange [this ^DidChangeTextDocumentParams params]
    (let [textDocument (.getTextDocument params)
          version (.getVersion textDocument)
          changes (.getContentChanges params)
          text (.getText ^TextDocumentContentChangeEvent (.get changes 0))
          uri (.getUri textDocument)]
      (#'handlers/did-change uri text version)))

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
             (conform-or-log ::references (#'handlers/references doc-id line column)))
           (catch Exception e
             (log/error e)))))))

  (^CompletableFuture completion [this ^CompletionParams params]
    (CompletableFuture/supplyAsync
     (reify Supplier
       (get [this]
         (try
           (let [doc-id (.getUri (.getTextDocument params))
                 pos (.getPosition params)
                 line (inc (.getLine pos))
                 column (inc (.getCharacter pos))]
             (conform-or-log ::completion-items (#'handlers/completion doc-id line column)))
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
             (conform-or-log ::workspace-edit (#'handlers/rename doc-id line column new-name)))
           (catch Exception e
             (log/error e)))))))

  (^CompletableFuture hover [this ^TextDocumentPositionParams params]
    (CompletableFuture/supplyAsync
     (reify Supplier
       (get [this]
         (try
           (let [doc-id (.getUri (.getTextDocument params))
                 pos (.getPosition params)
                 line (inc (.getLine pos))
                 column (inc (.getCharacter pos))]
             (conform-or-log ::hover (#'handlers/hover doc-id line column)))
           (catch Exception e
             (log/error e)))))))

  (^CompletableFuture signatureHelp [this ^TextDocumentPositionParams params]
    (CompletableFuture/completedFuture
     (SignatureHelp. [(doto (SignatureInformation. "sign-label")
                        (.setDocumentation "docs")
                        (.setParameters [(ParameterInformation. "param label" "param doc")]))]
                     0 0)))

  (^CompletableFuture formatting [this ^DocumentFormattingParams params]
    (CompletableFuture/supplyAsync
     (reify Supplier
       (get [this]
         (try
           (let [doc-id (.getUri (.getTextDocument params))]
             (conform-or-log ::edits (#'handlers/formatting doc-id)))
           (catch Exception e
             (log/error e)))))))

  (^CompletableFuture rangeFormatting [this ^DocumentRangeFormattingParams params]
    (let [result (when (compare-and-set! formatting false true)
                   (try
                     (let [doc-id (.getUri (.getTextDocument params))
                           range (.getRange params)
                           start (.getStart range)
                           end (.getEnd range)]
                       (conform-or-log ::edits (#'handlers/range-formatting
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
       result)))

  (^CompletableFuture codeAction [this ^CodeActionParams params]
    (log/warn params)
    (CompletableFuture/completedFuture
     (let [start (.getStart (.getRange params))]
       [(Command. "add-missing-libspec" "add-missing-libspec"
                  [(.getUri (.getTextDocument params)) (.getLine start) (.getCharacter start)])])))

  (^CompletableFuture definition [this ^TextDocumentPositionParams params]
    (CompletableFuture/supplyAsync
     (reify Supplier
       (get [this]
         (try
           (let [doc-id (.getUri (.getTextDocument params))
                 pos (.getPosition params)
                 line (inc (.getLine pos))
                 column (inc (.getCharacter pos))]
             (conform-or-log ::location (#'handlers/definition doc-id line column)))
           (catch Exception e
             (log/error e))))))))

(defn- json->clj [json-element]
  (cond
    (nil? json-element)
    nil

    (.isJsonNull json-element)
    nil

    (.isJsonArray json-element)
    (mapv json->clj (iterator-seq (.iterator (.getAsJsonArray json-element))))

    (.isJsonObject json-element)
    (->> json-element
         (.getAsJsonObject)
         (.entrySet)
         (map (juxt key (comp json->clj val)))
         (into {}))

    (.isJsonPrimitive json-element)
    (let [json-primitive (.getAsJsonPrimitive json-element)]
      (cond
        (.isString json-primitive) (.getAsString json-primitive)
        (.isNumber json-primitive) (.getAsLong json-primitive)
        (.isBoolean json-primitive) (.getAsBoolean json-primitive)
        :else json-primitive))

    :else
    json-element))

(defn- path->uri [path]
  (if (string/starts-with? path "/")
    (str "file://" path)
    path))

(deftype LSPWorkspaceService []
  WorkspaceService
  (^CompletableFuture executeCommand [this ^ExecuteCommandParams params]
    (log/warn params)
    (let [[doc-id line col & args] (map json->clj (.getArguments params))]
      (future
        (try
          (let [result (#'handlers/refactor (path->uri doc-id)
                                            (inc (int line))
                                            (inc (int col))
                                            (.getCommand params)
                                            args)]
            (.get (.applyEdit (:client @db/db)
                              (ApplyWorkspaceEditParams.
                               (conform-or-log ::workspace-edit result)))))
          (catch Exception e
            (log/error e)))))
    (CompletableFuture/completedFuture 0))
  (^void didChangeConfiguration [this ^DidChangeConfigurationParams params]
    (log/warn params))
  (^void didChangeWatchedFiles [this ^DidChangeWatchedFilesParams params]
    (log/warn "DidChangeWatchedFilesParams")))

(comment
  (s/conform ::client-capabilities (doto (ClientCapabilities.)
                                     (.setTextDocument (doto (TextDocumentClientCapabilities.)
                                                         (.setHover (HoverCapabilities. ["plaintext" "markdown"] false))))
                                     (.setWorkspace (doto (WorkspaceClientCapabilities.)
                                                      (.setWorkspaceEdit (WorkspaceEditCapabilities. true)))))))

(defrecord LSPServer []
  LanguageServer
  (^CompletableFuture initialize [this ^InitializeParams params]
    (log/warn "Initialize" params)
    (let [client-capabilities (some->> params (.getCapabilities) (conform-or-log ::client-capabilities))]
      (#'handlers/initialize (.getRootUri params) client-capabilities (json->clj (.getInitializationOptions params))))
    (CompletableFuture/completedFuture
     (InitializeResult. (doto (ServerCapabilities.)
                          (.setHoverProvider true)
                          (.setCodeActionProvider true)
                          (.setReferencesProvider true)
                          (.setRenameProvider true)
                          (.setDefinitionProvider true)
                          (.setDocumentFormattingProvider true)
                          (.setDocumentRangeFormattingProvider true)
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
    (log/info "Exit")
    (shutdown-agents)
    (System/exit 0))
  (getTextDocumentService [this]
    (LSPTextDocumentService.))
  (getWorkspaceService [this]
    (LSPWorkspaceService.)))

(defn -main [& args]
  (log/info "Server started")
  (let [server (LSPServer.)
        launcher (LSPLauncher/createServerLauncher server System/in System/out)]
    (swap! db/db assoc :client ^LanguageClient (.getRemoteProxy launcher))
    (async/go
      (loop [edit (async/<! handlers/edits-chan)]
        (log/warn "edit applied?" (.get (.applyEdit (:client @db/db) (ApplyWorkspaceEditParams. (conform-or-log ::workspace-edit edit)))))
        (recur (async/<! handlers/edits-chan))))
    (async/go
      (loop [diagnostic (async/<! handlers/diagnostics-chan)]
        (.publishDiagnostics (:client @db/db) (conform-or-log ::publish-diagnostics-params diagnostic))
        (recur (async/<! handlers/diagnostics-chan))))
    (.startListening launcher)))
