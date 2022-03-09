(ns clojure-lsp.server
  (:require
   [clojure-lsp.coercer :as coercer]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.feature.semantic-tokens :as semantic-tokens]
   [clojure-lsp.feature.test-tree :as f.test-tree]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.lsp :as lsp]
   [clojure-lsp.nrepl :as nrepl]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.shared-config :as config]
   [clojure.core.async :refer [<! go go-loop]]
   [clojure.java.data :as j]
   [taoensso.timbre :as log]
   [clojure-lsp.settings :as settings])
  (:import
   (clojure_lsp
     ClojureLanguageClient)
   (clojure_lsp.lsp ClojureLSPServer)
   (org.eclipse.lsp4j.jsonrpc Launcher))
  (:gen-class))

(set! *warn-on-reflection* true)

(defrecord ClojureLspProducer [lsp-producer ^ClojureLanguageClient client db]
  producer/IProducer
  (publish-diagnostic [_this diagnostic]
    (producer/publish-diagnostic lsp-producer diagnostic))
  (refresh-code-lens [_this]
    (producer/refresh-code-lens lsp-producer))
  (publish-workspace-edit [_this edit]
    (producer/publish-workspace-edit lsp-producer edit))
  (show-document-request [_this document-request]
    (producer/show-document-request lsp-producer document-request))
  (publish-progress [_this percentage message progress-token]
    (producer/publish-progress lsp-producer percentage message progress-token))
  (show-message-request [_this message type actions]
    (producer/show-message-request lsp-producer message type actions))
  (show-message [_this message type extra]
    (producer/show-message lsp-producer message type extra))
  (register-capability [_this capability]
    (producer/register-capability lsp-producer capability))

  producer/IClojureProducer
  (refresh-test-tree [_this uris]
    (go
      (when (some-> @db/db :client-capabilities :experimental j/from-java :testTree)
        (shared/logging-time
          "Refreshing testTree took %s secs"
          (doseq [uri uris]
            (when-let [test-tree (f.test-tree/tree uri db)]
              (->> test-tree
                   (coercer/conform-or-log ::coercer/publish-test-tree-params)
                   (.publishTestTree client)))))))))

(defn client-settings [params]
  (-> params
      :initializationOptions
      (or {})
      shared/keywordize-first-depth
      (settings/clean-client-settings)))

(defn run-server! []
  (log/info "Starting server...")
  (let [is (or System/in (lsp/tee-system-in System/in))
        os (or System/out (lsp/tee-system-out System/out))
        handler (handlers/->ClojureFeatureHandler)
        server (ClojureLSPServer. handler
                                  semantic-tokens/token-types-str
                                  semantic-tokens/token-modifiers-str
                                  f.refactor/available-refactors
                                  settings/all
                                  client-settings)
        launcher (Launcher/createLauncher server ClojureLanguageClient is os)
        debounced-diags (shared/debounce-by db/diagnostics-chan config/diagnostics-debounce-ms :uri)
        debounced-changes (shared/debounce-by db/current-changes-chan config/change-debounce-ms :uri)
        debounced-created-watched-files (shared/debounce-all db/created-watched-files-chan config/created-watched-files-debounce-ms)
        language-client ^ClojureLanguageClient (.getRemoteProxy launcher)
        producer (->ClojureLspProducer (lsp/->LSPProducer language-client db/db) language-client db/db)]
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
