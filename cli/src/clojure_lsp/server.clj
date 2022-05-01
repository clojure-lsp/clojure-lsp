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
   [lsp4clj.components :as components]
   [lsp4clj.core :as lsp]
   [lsp4clj.protocols.logger :as logger]
   [lsp4clj.protocols.producer :as producer]
   [taoensso.timbre :as timbre])
  (:import
   (clojure_lsp
     ClojureLanguageClient
     ClojureLanguageServer)
   (clojure_lsp.feature.clojuredocs ClojuredocsParams)
   (clojure_lsp.feature.cursor_info CursorInfoParams)
   (java.util.concurrent CompletableFuture)
   (lsp4clj.core LSPServer)
   (lsp4clj.protocols.feature_handler ILSPFeatureHandler)
   (lsp4clj.protocols.producer ILSPProducer)
   (org.eclipse.lsp4j
     InitializeParams
     InitializedParams
     TextDocumentIdentifier)
   (org.eclipse.lsp4j.jsonrpc Launcher))
  (:gen-class))

(set! *warn-on-reflection* true)

(def diagnostics-debounce-ms 100)
(def change-debounce-ms 300)
(def created-watched-files-debounce-ms 500)

(def known-files-pattern "**/*.{clj,cljs,cljc,cljd,edn,bb}")

(defn log! [level args fmeta]
  (timbre/log! level :p args {:?line (:line fmeta)
                              :?file (:file fmeta)
                              :?ns-str (:ns-str fmeta)}))

(defrecord TimbreLogger [db]
  logger/ILSPLogger

  (setup [this]
    (let [log-path (str (java.io.File/createTempFile "clojure-lsp." ".out"))]
      (timbre/merge-config! {:middleware [#(assoc % :hostname_ "")]
                             :appenders {:println {:enabled? false}
                                         :spit (timbre/spit-appender {:fname log-path})}})
      (timbre/handle-uncaught-jvm-exceptions!)
      (swap! db assoc :log-path log-path)
      (logger/set-logger! this)))

  (set-log-path [_this log-path]
    (timbre/merge-config! {:appenders {:spit (timbre/spit-appender {:fname log-path})}}))

  (-info [_this fmeta arg1] (log! :info [arg1] fmeta))
  (-info [_this fmeta arg1 arg2] (log! :info [arg1 arg2] fmeta))
  (-info [_this fmeta arg1 arg2 arg3] (log! :info [arg1 arg2 arg3] fmeta))
  (-warn [_this fmeta arg1] (log! :warn [arg1] fmeta))
  (-warn [_this fmeta arg1 arg2] (log! :warn [arg1 arg2] fmeta))
  (-warn [_this fmeta arg1 arg2 arg3] (log! :warn [arg1 arg2 arg3] fmeta))
  (-error [_this fmeta arg1] (log! :error [arg1] fmeta))
  (-error [_this fmeta arg1 arg2] (log! :error [arg1 arg2] fmeta))
  (-error [_this fmeta arg1 arg2 arg3] (log! :error [arg1 arg2 arg3] fmeta))
  (-debug [_this fmeta arg1] (log! :debug [arg1] fmeta))
  (-debug [_this fmeta arg1 arg2] (log! :debug [arg1 arg2] fmeta))
  (-debug [_this fmeta arg1 arg2 arg3] (log! :debug [arg1 arg2 arg3] fmeta)))

(defrecord ^:private ClojureLspProducer
           [^ClojureLanguageClient client
            ^ILSPProducer lsp-producer
            db*]
  producer/ILSPProducer
  (publish-diagnostic [_this diagnostic]
    (producer/publish-diagnostic lsp-producer diagnostic))
  (refresh-code-lens [_this]
    (producer/refresh-code-lens lsp-producer))
  (publish-workspace-edit [_this edit]
    (some-> (producer/publish-workspace-edit lsp-producer edit)
            deref))
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

  clojure-producer/IClojureProducer
  (refresh-test-tree [_this uris]
    (go
      (let [db @db*]
        (when (some-> db :client-capabilities :experimental j/from-java :testTree)
          (shared/logging-task
            :refreshing-test-tree
            (doseq [uri uris]
              (when-let [test-tree (f.test-tree/tree uri db)]
                (->> test-tree
                     (coercer/conform-or-log ::clojure-coercer/publish-test-tree-params)
                     (.publishTestTree client))))))))))

(deftype ClojureLspServer
         [^LSPServer lsp-server
          ^ILSPFeatureHandler feature-handler]
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
  (^CompletableFuture dependencyContents [_ ^TextDocumentIdentifier uri]
    (CompletableFuture/completedFuture
      (lsp/handle-request uri clojure-feature/dependency-contents feature-handler ::coercer/uri)))

  (^CompletableFuture serverInfoRaw [_]
    (CompletableFuture/completedFuture
      (->> (clojure-feature/server-info-raw feature-handler)
           (coercer/conform-or-log ::clojure-coercer/server-info-raw))))

  (^void serverInfoLog [_]
    (future
      (try
        (clojure-feature/server-info-log feature-handler)
        (catch Throwable e
          (logger/error e)
          (throw e))))
    nil)

  (^CompletableFuture cursorInfoRaw [_ ^CursorInfoParams params]
    (CompletableFuture/completedFuture
      (lsp/handle-request params clojure-feature/cursor-info-raw feature-handler ::clojure-coercer/cursor-info-raw)))

  (^void cursorInfoLog [_ ^CursorInfoParams params]
    (future
      (try
        (lsp/handle-notification params clojure-feature/cursor-info-log feature-handler)
        (catch Throwable e
          (logger/error e)
          (throw e)))))

  (^CompletableFuture clojuredocsRaw [_ ^ClojuredocsParams params]
    (CompletableFuture/completedFuture
      (lsp/handle-request params clojure-feature/clojuredocs-raw feature-handler ::clojure-coercer/clojuredocs-raw))))

(defn client-settings [params]
  (-> params
      :initializationOptions
      (or {})
      shared/keywordize-first-depth
      (settings/clean-client-settings)))

(defn capabilites [db*]
  (let [settings (settings/all @db*)]
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
     :workspace {:file-operations {:will-rename {:filters [{:scheme "file"
                                                            :pattern {:glob known-files-pattern
                                                                      :matches "file"}}]}}}
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

(defonce ^:private components* (atom {}))

(defn run-server! []
  (let [producer* (atom nil)
        db* db/db*
        timbre-logger (doto (->TimbreLogger db*)
                        (logger/setup))
        _ (logger/info "[SERVER]" "Starting server...")
        _ (reset! components* (components/->components db* timbre-logger nil))
        clojure-feature-handler (handlers/->ClojureLSPFeatureHandler components*)
        server (ClojureLspServer. (LSPServer. clojure-feature-handler
                                              producer*
                                              db*
                                              db/initial-db
                                              capabilites
                                              client-settings
                                              known-files-pattern)
                                  clojure-feature-handler)
        ;; For debugging, it's possible to trace all I/O through lsp4j.
        ;; tracer (java.io.PrintWriter. (io/writer (io/file "path/to/some/log/file")))
        ;; launcher (Launcher/createLauncher server ClojureLanguageClient System/in System/out false tracer)
        launcher (Launcher/createLauncher server ClojureLanguageClient System/in System/out)
        language-client ^ClojureLanguageClient (.getRemoteProxy launcher)
        producer (->ClojureLspProducer language-client
                                       (lsp/->LSPProducer language-client db*)
                                       db*)
        debounced-diags (shared/debounce-by db/diagnostics-chan diagnostics-debounce-ms :uri)
        debounced-changes (shared/debounce-by db/current-changes-chan change-debounce-ms :uri)
        debounced-created-watched-files (shared/debounce-all db/created-watched-files-chan created-watched-files-debounce-ms)]
    ;; TODO remove atom, think in a way to build all components in the same place and not need to assoc to atom later.
    (reset! producer* producer)
    (swap! components* assoc :producer producer)
    (nrepl/setup-nrepl db*)
    (go-loop [edit (<! db/edits-chan)]
      (producer/publish-workspace-edit producer edit)
      (recur (<! db/edits-chan)))
    (go-loop []
      (producer/publish-diagnostic producer (<! debounced-diags))
      (recur))
    (go-loop []
      (try
        (let [changes (<! debounced-changes)] ;; do not put inside shared/logging-task; parked time gets included in task time
          (shared/logging-task
            :analyze-file
            (f.file-management/analyze-changes changes @components*)))
        (catch Exception e
          (logger/error e "Error during analyzing buffer file changes")))
      (recur))
    (go-loop []
      (try
        (let [created-watched-files (<! debounced-created-watched-files)] ;; do not put inside shared/logging-task; parked time gets included in task time
          (shared/logging-task
            :analyze-created-files-in-watched-dir
            (f.file-management/analyze-watched-created-files! created-watched-files @components*)))
        (catch Exception e
          (logger/error e "Error during analyzing created watched files")))
      (recur))
    (.startListening launcher)))
