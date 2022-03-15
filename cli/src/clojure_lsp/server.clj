(ns clojure-lsp.server
  (:require
   [clojure-lsp.clojure-coercer :as clojure-coercer]
   [clojure-lsp.clojure-feature :as clojure-feature]
   [clojure-lsp.clojure-producer :as clojure-producer]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.feature.semantic-tokens :as semantic-tokens]
   [clojure-lsp.feature.test-tree :as f.test-tree]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.nrepl :as nrepl]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :refer [<! go go-loop]]
   [clojure.java.data :as j]
   [lsp4clj.coercer :as coercer]
   [lsp4clj.core :as lsp]
   [lsp4clj.protocols :as protocols]
   [taoensso.timbre :as log])
  (:import
   (clojure_lsp
     ClojureLanguageClient
     ClojureLanguageServer)
   (clojure_lsp.feature.clojuredocs ClojuredocsParams)
   (clojure_lsp.feature.cursor_info CursorInfoParams)
   (java.util.concurrent CompletableFuture)
   (lsp4clj.core LSPServer)
   (org.eclipse.lsp4j
     InitializeParams
     InitializedParams)
   (org.eclipse.lsp4j.jsonrpc Launcher))
  (:gen-class))

(set! *warn-on-reflection* true)

(def diagnostics-debounce-ms 100)
(def change-debounce-ms 300)
(def created-watched-files-debounce-ms 500)

;; Called from java
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn extension [method & args]
  (lsp/start :extension
             (CompletableFuture/completedFuture
               (lsp/end
                 (apply #'clojure-feature/extension (:feature-handler @db/db) method (coercer/java->clj args))))))

(defrecord ^:private ClojureLspProducer [lsp-producer ^ClojureLanguageClient client db]
  protocols/ILSPProducer
  (publish-diagnostic [_this diagnostic]
    (protocols/publish-diagnostic lsp-producer diagnostic))
  (refresh-code-lens [_this]
    (protocols/refresh-code-lens lsp-producer))
  (publish-workspace-edit [_this edit]
    (protocols/publish-workspace-edit lsp-producer edit))
  (show-document-request [_this document-request]
    (protocols/show-document-request lsp-producer document-request))
  (publish-progress [_this percentage message progress-token]
    (protocols/publish-progress lsp-producer percentage message progress-token))
  (show-message-request [_this message type actions]
    (protocols/show-message-request lsp-producer message type actions))
  (show-message [_this message type extra]
    (protocols/show-message lsp-producer message type extra))
  (register-capability [_this capability]
    (protocols/register-capability lsp-producer capability))

  clojure-producer/IClojureProducer
  (refresh-test-tree [_this uris]
    (go
      (when (some-> @db/db :client-capabilities :experimental j/from-java :testTree)
        (shared/logging-time
          "Refreshing testTree took %s secs"
          (doseq [uri uris]
            (when-let [test-tree (f.test-tree/tree uri db)]
              (->> test-tree
                   (coercer/conform-or-log ::clojure-coercer/publish-test-tree-params)
                   (.publishTestTree client)))))))))

(deftype ClojureLspServer [^LSPServer lsp-server feature-handler]
  ClojureLanguageServer
  (^CompletableFuture initialize [_ ^InitializeParams params]
    (.initialize lsp-server params))
  (^void initialized [_ ^InitializedParams _params]
    (.initialized lsp-server _params))
  (^CompletableFuture shutdown [_]
    (.shutdown lsp-server))
  (exit [_]
    (.exit lsp-server))
  (getTextDocumentService [_]
    (.getTextDocumentService lsp-server))
  (getWorkspaceService [_]
    (.getWorkspaceService lsp-server))
  (^CompletableFuture serverInfoRaw [_]
    (CompletableFuture/completedFuture
      (->> (clojure-feature/server-info-raw feature-handler)
           (coercer/conform-or-log ::clojure-coercer/server-info-raw))))

  (^void serverInfoLog [_]
    (lsp/start :server-info-log
               (future
                 (lsp/end
                   (clojure-feature/server-info-log feature-handler)))))

  (^CompletableFuture cursorInfoRaw [_ ^CursorInfoParams params]
    (lsp/start :cursorInfoRaw
               (CompletableFuture/completedFuture
                 (lsp/sync-request params clojure-feature/cursor-info-raw feature-handler ::clojure-coercer/cursor-info-raw))))

  (^void cursorInfoLog [_ ^CursorInfoParams params]
    (lsp/start :cursor-info-log
               (future
                 (lsp/sync-notification params clojure-feature/cursor-info-log feature-handler))))

  (^CompletableFuture clojuredocsRaw [_ ^ClojuredocsParams params]
    (lsp/start :clojuredocsRaw
               (CompletableFuture/completedFuture
                 (lsp/sync-request params clojure-feature/clojuredocs-raw feature-handler ::clojure-coercer/clojuredocs-raw)))))

(defn client-settings [params]
  (-> params
      :initializationOptions
      (or {})
      shared/keywordize-first-depth
      (settings/clean-client-settings)))

(defn capabilites [db]
  (let [settings (settings/all db)]
    {:document-highlight-provider true
     :hover-provider true
     :declaration-provider true
     :implementation-provider true
     :signature-help-provider []
     :call-hierarchy-provider true
     :linked-editing-range-provider true
     :code-action-provider (vec (vals coercer/code-action-kind))
     :code-lens-provider true
     :references-provider true
     :rename-provider true
     :definition-provider true
     :document-formatting-provider ^Boolean (:document-formatting? settings)
     :document-range-formatting-provider ^Boolean (:document-range-formatting? settings)
     :document-symbol-provider true
     :workspace-symbol-provider true
     :semantic-tokens-provider (when (or (not (contains? settings :semantic-tokens?))
                                         (:semantic-tokens? settings))
                                 {:token-types semantic-tokens/token-types-str
                                  :token-modifiers semantic-tokens/token-modifiers-str
                                  :range true
                                  :full true})
     :execute-command-provider f.refactor/available-refactors
     :text-document-sync (:text-document-sync-kind settings)
     :completion-provider {:resolve-provider true :trigger-characters [":" "/"]}
     :experimental {"testTree" true
                    "cursorInfo" true
                    "serverInfo" true
                    "clojuredocs" true}}))

(defn run-server! []
  (log/info "Starting server...")
  (let [is (or System/in (lsp/tee-system-in System/in))
        os (or System/out (lsp/tee-system-out System/out))
        clojure-feature-handler (handlers/->ClojureLSPFeatureHandler)
        server (ClojureLspServer. (LSPServer. clojure-feature-handler
                                              db/db
                                              db/initial-db
                                              capabilites
                                              client-settings)
                                  clojure-feature-handler)
        launcher (Launcher/createLauncher server ClojureLanguageClient is os)
        debounced-diags (shared/debounce-by db/diagnostics-chan diagnostics-debounce-ms :uri)
        debounced-changes (shared/debounce-by db/current-changes-chan change-debounce-ms :uri)
        debounced-created-watched-files (shared/debounce-all db/created-watched-files-chan created-watched-files-debounce-ms)
        language-client ^ClojureLanguageClient (.getRemoteProxy launcher)
        producer (->ClojureLspProducer (lsp/->LSPProducer language-client db/db) language-client db/db)]
    (nrepl/setup-nrepl db/db)
    (swap! db/db assoc :producer producer)
    (swap! db/db assoc :feature-handler clojure-feature-handler)
    (go-loop [edit (<! db/edits-chan)]
      (protocols/publish-workspace-edit producer edit)
      (recur (<! db/edits-chan)))
    (go-loop []
      (protocols/publish-diagnostic producer (<! debounced-diags))
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
