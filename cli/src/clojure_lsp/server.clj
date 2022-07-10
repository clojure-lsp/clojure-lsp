(ns clojure-lsp.server
  (:require
   [clojure-lsp.clojure-coercer :as clojure-coercer]
   [clojure-lsp.clojure-producer :as clojure-producer]
   [clojure-lsp.coercer-v1 :as coercer-v1]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.feature.semantic-tokens :as semantic-tokens]
   [clojure-lsp.feature.test-tree :as f.test-tree]
   [clojure-lsp.handlers :as handler]
   [clojure-lsp.nrepl :as nrepl]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [lsp4clj.json-rpc.messages :as lsp.messages]
   [lsp4clj.liveness-probe :as lsp.liveness-probe]
   [lsp4clj.protocols.endpoint :as lsp.endpoint]
   [lsp4clj.protocols.logger :as logger]
   [lsp4clj.protocols.producer :as producer]
   [lsp4clj.server :as lsp.server]
   [taoensso.timbre :as timbre])
  (:import
   (lsp4clj.protocols.endpoint IEndpoint)))

(set! *warn-on-reflection* true)

(def diagnostics-debounce-ms 100)
(def change-debounce-ms 300)
(def created-watched-files-debounce-ms 500)

(def known-files-pattern "**/*.{clj,cljs,cljc,cljd,edn,bb,clj_kondo}")

(defn log! [level args fmeta]
  (timbre/log! level :p args {:?line (:line fmeta)
                              :?file (:file fmeta)
                              :?ns-str (:ns-str fmeta)}))

(defrecord TimbreLogger []
  logger/ILSPLogger
  (setup [this]
    (let [log-path (str (java.io.File/createTempFile "clojure-lsp." ".out"))]
      (timbre/merge-config! {:middleware [#(assoc % :hostname_ "")]
                             :appenders {:println {:enabled? false}
                                         :spit (timbre/spit-appender {:fname log-path})}})
      (timbre/handle-uncaught-jvm-exceptions!)
      (logger/set-logger! this)
      log-path))

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

;; TODO: lsp2clj bring the ILSPProducer protocol from lsp4clj into clojure-lsp. We
;; need it so that we can provide dummy implementations on CLI and tests. Other
;; servers can organize however they wish.
(defrecord ^:private ClojureLspProducer
           [^IEndpoint server
            db*]
  producer/ILSPProducer

  (publish-diagnostic [_this diagnostic]
    (logger/debug (format "Publishing %s diagnostics for %s" (count (:diagnostics diagnostic)) (:uri diagnostic)))
    (shared/logging-task
      :publish-diagnostics
      (->> diagnostic
           (coercer-v1/conform-or-log ::coercer-v1/publish-diagnostics-params)
           (lsp.endpoint/send-notification server "textDocument/publishDiagnostics"))))

  (refresh-code-lens [_this]
    (when (get-in @db* [:client-capabilities :workspace :code-lens :refresh-support])
      (lsp.endpoint/send-request server "workspace/codeLens/refresh" nil)))

  (publish-workspace-edit [_this edit]
    (let [request (->> edit
                       (coercer-v1/conform-or-log ::coercer-v1/workspace-edit-or-error)
                       (lsp.endpoint/send-request server "workspace/applyEdit"))
          response (lsp.server/deref-or-cancel request 10e3 ::timeout)]
      (if (= ::timeout response)
        (logger/error "No reponse from client after 10 seconds.")
        response)))

  (show-document-request [_this document-request]
    (when (get-in @db* [:client-capabilities :window :show-document])
      (logger/info "Requesting to show on editor the document" document-request)
      (->> document-request
           (coercer-v1/conform-or-log ::coercer-v1/show-document-request)
           (lsp.endpoint/send-request server "window/showDocument"))))

  (publish-progress [_this percentage message progress-token]
    ;; ::coercer/notify-progress
    (->> (lsp.messages/work-done-progress percentage message (or progress-token "clojure-lsp"))
         (lsp.endpoint/send-notification server "$/progress")))

  (show-message-request [_this message type actions]
    (let [request (->> {:type type
                        :message message
                        :actions actions}
                       (coercer-v1/conform-or-log ::coercer-v1/show-message-request)
                       (lsp.endpoint/send-request server "window/showMessageRequest"))
          response (lsp.server/deref-or-cancel request 10e3 ::timeout)]
      (when-not (= response ::timeout)
        (:title response))))

  (show-message [_this message type extra]
    (let [message-content {:message message
                           :type type
                           :extra extra}]
      (logger/info message-content)
      (->> message-content
           (coercer-v1/conform-or-log ::coercer-v1/show-message)
           (lsp.endpoint/send-notification server "window/showMessage"))))

  ;; TODO: lsp2clj capabilities are now registered directly by "initialized"
  ;; handler, meaning this is unused by clojure-lsp; remove from its version of
  ;; protocol
  (register-capability [_this _capability]
    ;; (.registerCapability client capability)
    )

  clojure-producer/IClojureProducer
  (refresh-test-tree [_this uris]
    (async/go
      (let [db @db*]
        (when (some-> db :client-capabilities :experimental :test-tree)
          (shared/logging-task
            :refreshing-test-tree
            (doseq [uri uris]
              (when-let [test-tree (f.test-tree/tree uri db)]
                (->> test-tree
                     (coercer-v1/conform-or-log ::clojure-coercer/publish-test-tree-params)
                     (lsp.endpoint/send-notification server "clojure/textDocument/testTree"))))))))))

;;;; clojure experimental features

(defmethod lsp.server/handle-request "clojure/dependencyContents" [_ components uri]
  (coercer-v1/conform-or-log
    ::coercer-v1/uri
    (handler/dependency-contents uri components)))
(defmethod lsp.server/handle-request "clojure/serverInfo/raw" [_ components _params]
  (coercer-v1/conform-or-log
    ::clojure-coercer/server-info-raw
    (handler/server-info-raw components)))
(defmethod lsp.server/handle-notification "clojure/serverInfo/log" [_ components _params]
  (future
    (try
      (handler/server-info-log components)
      (catch Throwable e
        (logger/error e)
        (throw e)))))
(defmethod lsp.server/handle-request "clojure/cursorInfo/raw" [_ components params]
  (coercer-v1/conform-or-log
    ::clojure-coercer/cursor-info-raw
    (handler/cursor-info-raw params components)))
(defmethod lsp.server/handle-notification "clojure/cursorInfo/log" [_ components params]
  (future
    (try
      (handler/cursor-info-log params components)
      (catch Throwable e
        (logger/error e)
        (throw e)))))
(defmethod lsp.server/handle-request "clojure/clojuredocs/raw" [_ components params]
  (coercer-v1/conform-or-log
    ::clojure-coercer/clojuredocs-raw
    (handler/clojuredocs-raw params components)))

;;;; Document sync features

;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_synchronization

(defmethod lsp.server/handle-notification "textDocument/didOpen" [_ components params]
  (handler/did-open params components))

(defmethod lsp.server/handle-notification "textDocument/didChange" [_ components params]
  (handler/did-change params components))

(defmethod lsp.server/handle-notification "textDocument/didSave" [_ components params]
  (future
    (try
      (handler/did-save params components)
      (catch Throwable e
        (logger/error e)
        (throw e)))))

(defmethod lsp.server/handle-notification "textDocument/didClose" [_ components params]
  (handler/did-close params components))

(defmethod lsp.server/handle-request "textDocument/references" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/locations
    (handler/references params components)))

(defmethod lsp.server/handle-request "textDocument/completion" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/completion-items
    (handler/completion params components)))

(defmethod lsp.server/handle-request "completionItem/resolve" [_ components item]
  (coercer-v1/conform-or-log
    ::coercer-v1/completion-item
    (handler/completion-resolve-item item components)))

(defmethod lsp.server/handle-request "textDocument/prepareRename" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/prepare-rename-or-error
    (handler/prepare-rename params components)))

(defmethod lsp.server/handle-request "textDocument/rename" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/workspace-edit-or-error
    (handler/rename params components)))

(defmethod lsp.server/handle-request "textDocument/hover" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/hover
    (handler/hover params components)))

(defmethod lsp.server/handle-request "textDocument/signatureHelp" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/signature-help
    (handler/signature-help params components)))

(defmethod lsp.server/handle-request "textDocument/formatting" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/edits
    (handler/formatting params components)))

(def ^:private formatting (atom false))

(defmethod lsp.server/handle-request "textDocument/rangeFormatting" [_this components params]
  (when (compare-and-set! formatting false true)
    (try
      (coercer-v1/conform-or-log
        ::coercer-v1/edits
        (handler/range-formatting params components))
      (catch Exception e
        (logger/error e))
      (finally
        (reset! formatting false)))))

(defmethod lsp.server/handle-request "textDocument/codeAction" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/code-actions
    (handler/code-actions params components)))

(defmethod lsp.server/handle-request "textDocument/codeLens" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/code-lenses
    (handler/code-lens params components)))

(defmethod lsp.server/handle-request "codeLens/resolve" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/code-lens
    (handler/code-lens-resolve params components)))

(defmethod lsp.server/handle-request "textDocument/definition" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/location
    (handler/definition params components)))

(defmethod lsp.server/handle-request "textDocument/declaration" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/location
    (handler/declaration params components)))

(defmethod lsp.server/handle-request "textDocument/implementation" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/locations
    (handler/implementation params components)))

(defmethod lsp.server/handle-request "textDocument/documentSymbol" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/document-symbols
    (handler/document-symbol params components)))

(defmethod lsp.server/handle-request "textDocument/documentHighlight" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/document-highlights
    (handler/document-highlight params components)))

(defmethod lsp.server/handle-request "textDocument/semanticTokens/full" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/semantic-tokens
    (handler/semantic-tokens-full params components)))

(defmethod lsp.server/handle-request "textDocument/semanticTokens/range" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/semantic-tokens
    (handler/semantic-tokens-range params components)))

(defmethod lsp.server/handle-request "textDocument/prepareCallHierarchy" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/call-hierarchy-items
    (handler/prepare-call-hierarchy params components)))

(defmethod lsp.server/handle-request "callHierarchy/incomingCalls" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/call-hierarchy-incoming-calls
    (handler/call-hierarchy-incoming params components)))

(defmethod lsp.server/handle-request "callHierarchy/outgoingCalls" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/call-hierarchy-outgoing-calls
    (handler/call-hierarchy-outgoing params components)))

(defmethod lsp.server/handle-request "textDocument/linkedEditingRange" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/linked-editing-ranges-or-error
    (handler/linked-editing-ranges params components)))

;;;; Workspace features

;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFeatures

(defmethod lsp.server/handle-request "workspace/executeCommand" [_ components params]
  (future
    (try
      (handler/execute-command params components)
      (catch Throwable e
        (logger/error e)
        (throw e))))
  nil)

(defmethod lsp.server/handle-notification "workspace/didChangeConfiguration" [_ _components params]
  (logger/warn params))

(defmethod lsp.server/handle-notification "workspace/didChangeWatchedFiles" [_ components params]
  (handler/did-change-watched-files (coercer-v1/conform-or-log ::coercer-v1/did-change-watched-files-params params) components))

(defmethod lsp.server/handle-request "workspace/symbol" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/workspace-symbols
    (handler/workspace-symbols params components)))

(defmethod lsp.server/handle-request "workspace/willRenameFiles" [_ components params]
  (coercer-v1/conform-or-log
    ::coercer-v1/workspace-edit
    (handler/will-rename-files params components)))

(defn capabilities [settings]
  (coercer-v1/conform-or-log
    ::coercer-v1/server-capabilities
    {:document-highlight-provider true
     :hover-provider true
     :declaration-provider true
     :implementation-provider true
     :signature-help-provider []
     :call-hierarchy-provider true
     :linked-editing-range-provider true
     :code-action-provider (vec (vals coercer-v1/code-action-kind))
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
     :experimental {:test-tree true
                    :cursor-info true
                    :server-info true
                    :clojuredocs true}}))

(defn client-settings [params]
  (-> params
      :initialization-options
      (or {})
      (settings/clean-client-settings)))

(defn ^:private exit []
  (logger/info "Exiting...")
  (shutdown-agents)
  (System/exit 0))

;;;; Lifecycle messages

;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#lifeCycleMessages

(defmethod lsp.server/handle-request "initialize" [_ components params]
  (logger/info "Initializing...")
  (handler/initialize (:root-uri params)
                              ;; TODO: lsp2clj do we need any of the client capabilities
                              ;; coercion that used to happen?
                      (:capabilities params)
                      (client-settings params)
                      (some-> params :work-done-token str)
                      components)
  (when-let [parent-process-id (:process-id params)]
    (lsp.liveness-probe/start! parent-process-id exit))
  ;; TODO: lsp2clj do we need any of the server capabilities coercion that used to happen?
  {:capabilities (capabilities (settings/all (deref (:db* components))))})

(defmethod lsp.server/handle-notification "initialized" [_ {:keys [server]} _params]
  (logger/info "Initialized!")
  (->> {:registrations [{:id "id"
                         :method "workspace/didChangeWatchedFiles"
                         :register-options {:watchers [{:glob-pattern known-files-pattern}]}}]}
       (lsp.endpoint/send-request server "client/registerCapability")))

(defmethod lsp.server/handle-request "shutdown" [_ components _params]
  (logger/info "Shutting down...")
  (reset! (:db* components) db/initial-db)
  nil)

(defmethod lsp.server/handle-notification "exit" [_ _components _params]
  (logger/info "Shut down")
  (exit))

(defmethod lsp.server/handle-notification "$/cancelRequest" [_ _ _])

(defmacro ^:private safe-async-task [task-name & task-body]
  `(async/go-loop []
     (try
       ~@task-body
       (catch Exception e#
         (logger/error e# (format "Error during async task %s" ~task-name))))
     (recur)))

(defn ^:private spawn-async-tasks!
  [{:keys [producer] :as components}]
  (let [debounced-diags (shared/debounce-by db/diagnostics-chan diagnostics-debounce-ms :uri)
        debounced-changes (shared/debounce-by db/current-changes-chan change-debounce-ms :uri)
        debounced-created-watched-files (shared/debounce-all db/created-watched-files-chan created-watched-files-debounce-ms)]
    (safe-async-task
      :edits
      (producer/publish-workspace-edit producer (async/<! db/edits-chan)))
    (safe-async-task
      :diagnostics
      (producer/publish-diagnostic producer (async/<! debounced-diags)))
    (safe-async-task
      :changes
      (let [changes (async/<! debounced-changes)] ;; do not put inside shared/logging-task; parked time gets included in task time
        (shared/logging-task
          :analyze-file
          (f.file-management/analyze-changes changes components))))
    (safe-async-task
      :watched-files
      (let [created-watched-files (async/<! debounced-created-watched-files)] ;; do not put inside shared/logging-task; parked time gets included in task time
        (shared/logging-task
          :analyze-created-files-in-watched-dir
          (f.file-management/analyze-watched-created-files! created-watched-files components))))))

(defn run-server! []
  (let [timbre-logger (->TimbreLogger)
        log-path (logger/setup timbre-logger)
        db (assoc db/initial-db :log-path log-path)
        db* (atom db)
        server (lsp.server/stdio-server {:trace? false
                                         :in System/in
                                         :out System/out})
        producer (ClojureLspProducer. server db*)
        components {:db* db*
                    :logger timbre-logger
                    :producer producer
                    :server server}]
    (logger/info "[SERVER]" "Starting server...")
    (nrepl/setup-nrepl db*)
    (spawn-async-tasks! components)
    (lsp.endpoint/start server components)))
