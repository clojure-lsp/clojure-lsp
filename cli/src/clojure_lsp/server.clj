(ns clojure-lsp.server
  (:require
   [clojure-lsp.clojure-coercer :as clojure-coercer]
   [clojure-lsp.components :as components]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.feature.semantic-tokens :as semantic-tokens]
   [clojure-lsp.feature.test-tree :as f.test-tree]
   [clojure-lsp.handlers :as handler]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.nrepl :as nrepl]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [lsp4clj.coercer :as coercer]
   [lsp4clj.io-server :as lsp.io-server]
   [lsp4clj.liveness-probe :as lsp.liveness-probe]
   [lsp4clj.lsp.requests :as lsp.requests]
   [lsp4clj.server :as lsp.server]
   [taoensso.timbre :as timbre]))

(set! *warn-on-reflection* true)

(def snapc components/snapc)

(def diagnostics-debounce-ms 100)
(def change-debounce-ms 300)
(def watched-files-debounce-ms 1000)

(def known-files-pattern "**/*.{clj,cljs,cljc,cljd,edn,bb,clj_kondo}")

(defn log! [level args fmeta]
  (timbre/log! level :p args {:?line (:line fmeta)
                              :?file (:file fmeta)
                              :?ns-str (:ns-str fmeta)}))

(defn log-wrapper-fn
  [level & args]
  ;; NOTE: this does not do compile-time elision because the level isn't a constant.
  ;; We don't really care because we always log all levels.
  (timbre/log! level :p args))

(defmacro conform-or-log
  "Provides log function for conformation, while preserving line numbers."
  [spec value]
  (let [fmeta (assoc (meta &form)
                     :file *file*
                     :ns-str (str *ns*))]
    `(coercer/conform-or-log
       (fn [& args#]
         (log! :error args# ~fmeta))
       ~spec
       ~value)))

(defrecord TimbreLogger []
  logger/ILogger
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

(defrecord ^:private ClojureLspProducer
           [server db*]
  producer/IProducer

  (publish-diagnostic [_this diagnostic]
    (lsp.server/discarding-stdout
      (logger/debug (format "Publishing %s diagnostics for %s" (count (:diagnostics diagnostic)) (:uri diagnostic)))
      (shared/logging-task
        :publish-diagnostics
        (->> diagnostic
             (conform-or-log ::coercer/publish-diagnostics-params)
             (lsp.server/send-notification server "textDocument/publishDiagnostics")))))

  (refresh-code-lens [_this]
    (lsp.server/discarding-stdout
      (when (get-in @db* [:client-capabilities :workspace :code-lens :refresh-support])
        (lsp.server/send-request server "workspace/codeLens/refresh" nil))))

  (publish-workspace-edit [_this edit]
    (lsp.server/discarding-stdout
      (let [request (->> {:edit edit}
                         (conform-or-log ::coercer/workspace-edit-params)
                         (lsp.server/send-request server "workspace/applyEdit"))
            response (lsp.server/deref-or-cancel request 10e3 ::timeout)]
        (if (= ::timeout response)
          (logger/error "No reponse from client after 10 seconds while applying workspace-edit.")
          response))))

  (show-document-request [_this document-request]
    (lsp.server/discarding-stdout
      (when (get-in @db* [:client-capabilities :window :show-document])
        (logger/info "Requesting to show on editor the document" document-request)
        (->> document-request
             (conform-or-log ::coercer/show-document-request)
             (lsp.server/send-request server "window/showDocument")))))

  (publish-progress [_this percentage message progress-token]
    (lsp.server/discarding-stdout
      ;; ::coercer/notify-progress
      (->> (lsp.requests/work-done-progress percentage message (or progress-token "clojure-lsp"))
           (lsp.server/send-notification server "$/progress"))))

  (show-message-request [_this message type actions]
    (lsp.server/discarding-stdout
      (let [request (->> {:type    type
                          :message message
                          :actions actions}
                         (conform-or-log ::coercer/show-message-request)
                         (lsp.server/send-request server "window/showMessageRequest"))
            ;; High timeout as we probably want to wait some time for user input
            response (lsp.server/deref-or-cancel request 10e5 ::timeout)]
        (when-not (= response ::timeout)
          (:title response)))))

  (show-message [_this message type extra]
    (lsp.server/discarding-stdout
      (let [message-content {:message message
                             :type type
                             :extra extra}]
        (logger/info message-content)
        (->> message-content
             (conform-or-log ::coercer/show-message)
             (lsp.server/send-notification server "window/showMessage")))))

  (refresh-test-tree [_this uris]
    (async/thread
      (lsp.server/discarding-stdout
        (let [db @db*]
          (when (some-> db :client-capabilities :experimental :test-tree)
            (shared/logging-task
              :refreshing-test-tree
              (doseq [uri uris]
                (some->> (f.test-tree/tree uri db)
                         (conform-or-log ::clojure-coercer/publish-test-tree-params)
                         (lsp.server/send-notification server "clojure/textDocument/testTree"))))))))))

;;;; clojure extra features

(defmethod lsp.server/receive-request "clojure/dependencyContents" [_ components params]
  (->> params
       (handler/dependency-contents (snapc components))
       (conform-or-log ::coercer/uri)))

(defmethod lsp.server/receive-request "clojure/serverInfo/raw" [_ components _params]
  (handler/server-info-raw components))

(defmethod lsp.server/receive-notification "clojure/serverInfo/log" [_ components _params]
  (future
    (try
      (handler/server-info-log (snapc components))
      (catch Throwable e
        (logger/error e)
        (throw e)))))

(defmethod lsp.server/receive-request "clojure/cursorInfo/raw" [_ components params]
  (handler/cursor-info-raw (snapc components) params))

(defmethod lsp.server/receive-notification "clojure/cursorInfo/log" [_ components params]
  (future
    (try
      (handler/cursor-info-log (snapc components) params)
      (catch Throwable e
        (logger/error e)
        (throw e)))))

(defmethod lsp.server/receive-request "clojure/clojuredocs/raw" [_ components params]
  (handler/clojuredocs-raw (snapc components) params))

;;;; Document sync features

;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_synchronization

(defmethod lsp.server/receive-notification "textDocument/didOpen" [_ components params]
  (handler/did-open (snapc components) params))

(defmethod lsp.server/receive-notification "textDocument/didChange" [_ components params]
  (handler/did-change (snapc components) params))

(defmethod lsp.server/receive-notification "textDocument/didSave" [_ components params]
  (future
    (try
      (handler/did-save (snapc components) params)
      (catch Throwable e
        (logger/error e)
        (throw e)))))

(defmethod lsp.server/receive-notification "textDocument/didClose" [_ components params]
  (handler/did-close (snapc components) params))

(defmethod lsp.server/receive-request "textDocument/references" [_ components params]
  (->> params
       (handler/references (snapc components))
       (conform-or-log ::coercer/locations)))

(defmethod lsp.server/receive-request "textDocument/completion" [_ components params]
  (->> params
       (handler/completion (snapc components))
       (conform-or-log ::coercer/completion-items)))

(defmethod lsp.server/receive-request "completionItem/resolve" [_ components item]
  (->> item
       (conform-or-log ::coercer/input.completion-item)
       (handler/completion-resolve-item (snapc components))
       (conform-or-log ::coercer/completion-item)))

(defmethod lsp.server/receive-request "textDocument/prepareRename" [_ components params]
  (->> params
       (handler/prepare-rename (snapc components))
       (conform-or-log ::coercer/prepare-rename-or-error)))

(defmethod lsp.server/receive-request "textDocument/rename" [_ components params]
  (->> params
       (handler/rename (snapc components))
       (conform-or-log ::coercer/workspace-edit-or-error)))

(defmethod lsp.server/receive-request "textDocument/hover" [_ components params]
  (->> params
       (handler/hover (snapc components))
       (conform-or-log ::coercer/hover)))

(defmethod lsp.server/receive-request "textDocument/signatureHelp" [_ components params]
  (->> params
       (handler/signature-help (snapc components))
       (conform-or-log ::coercer/signature-help)))

(defmethod lsp.server/receive-request "textDocument/formatting" [_ components params]
  (->> params
       (handler/formatting (snapc components))
       (conform-or-log ::coercer/edits)))

(def ^:private formatting (atom false))

(defmethod lsp.server/receive-request "textDocument/rangeFormatting" [_ components params]
  (when (compare-and-set! formatting false true)
    (try
      (->> params
           (handler/range-formatting (snapc components))
           (conform-or-log ::coercer/edits))
      (catch Exception e
        (logger/error e))
      (finally
        (reset! formatting false)))))

(defmethod lsp.server/receive-request "textDocument/codeAction" [_ components params]
  (->> params
       (handler/code-actions (snapc components))
       (conform-or-log ::coercer/code-actions)))

(defmethod lsp.server/receive-request "textDocument/codeLens" [_ components params]
  (->> params
       (handler/code-lens (snapc components))
       (conform-or-log ::coercer/code-lenses)))

(defmethod lsp.server/receive-request "codeLens/resolve" [_ components params]
  (->> params
       (handler/code-lens-resolve (snapc components))
       (conform-or-log ::coercer/code-lens)))

(defmethod lsp.server/receive-request "textDocument/definition" [_ components params]
  (->> params
       (handler/definition (snapc components))
       (conform-or-log ::coercer/location)))

(defmethod lsp.server/receive-request "textDocument/declaration" [_ components params]
  (->> params
       (handler/declaration (snapc components))
       (conform-or-log ::coercer/location)))

(defmethod lsp.server/receive-request "textDocument/implementation" [_ components params]
  (->> params
       (handler/implementation (snapc components))
       (conform-or-log ::coercer/locations)))

(defmethod lsp.server/receive-request "textDocument/documentSymbol" [_ components params]
  (->> params
       (handler/document-symbol (snapc components))
       (conform-or-log ::coercer/document-symbols)))

(defmethod lsp.server/receive-request "textDocument/documentHighlight" [_ components params]
  (->> params
       (handler/document-highlight (snapc components))
       (conform-or-log ::coercer/document-highlights)))

(defmethod lsp.server/receive-request "textDocument/semanticTokens/full" [_ components params]
  (->> params
       (handler/semantic-tokens-full (snapc components))
       (conform-or-log ::coercer/semantic-tokens)))

(defmethod lsp.server/receive-request "textDocument/semanticTokens/range" [_ components params]
  (->> params
       (handler/semantic-tokens-range (snapc components))
       (conform-or-log ::coercer/semantic-tokens)))

(defmethod lsp.server/receive-request "textDocument/prepareCallHierarchy" [_ components params]
  (->> params
       (handler/prepare-call-hierarchy (snapc components))
       (conform-or-log ::coercer/call-hierarchy-items)))

(defmethod lsp.server/receive-request "callHierarchy/incomingCalls" [_ components params]
  (->> params
       (handler/call-hierarchy-incoming (snapc components))
       (conform-or-log ::coercer/call-hierarchy-incoming-calls)))

(defmethod lsp.server/receive-request "callHierarchy/outgoingCalls" [_ components params]
  (->> params
       (handler/call-hierarchy-outgoing (snapc components))
       (conform-or-log ::coercer/call-hierarchy-outgoing-calls)))

(defmethod lsp.server/receive-request "textDocument/linkedEditingRange" [_ components params]
  (->> params
       (handler/linked-editing-ranges (snapc components))
       (conform-or-log ::coercer/linked-editing-ranges-or-error)))

;;;; Workspace features

;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFeatures

(defmethod lsp.server/receive-request "workspace/executeCommand" [_ components params]
  (future
    (try
      (handler/execute-command (snapc components) params)
      (catch Throwable e
        (logger/error e)
        (throw e))))
  nil)

(defmethod lsp.server/receive-notification "workspace/didChangeConfiguration" [_ _components params]
  (logger/warn params))

(defmethod lsp.server/receive-notification "workspace/didChangeWatchedFiles" [_ components params]
  (->> params
       (conform-or-log ::coercer/did-change-watched-files-params)
       (handler/did-change-watched-files (snapc components))))

(defmethod lsp.server/receive-request "workspace/symbol" [_ components params]
  (->> params
       (handler/workspace-symbols (snapc components))
       (conform-or-log ::coercer/workspace-symbols)))

(defmethod lsp.server/receive-request "workspace/willRenameFiles" [_ components params]
  (->> params
       (handler/will-rename-files (snapc components))
       (conform-or-log ::coercer/workspace-edit)))

(defn capabilities [settings]
  (conform-or-log
    ::coercer/server-capabilities
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
     :experimental {:test-tree true
                    :cursor-info true
                    :server-info true
                    :clojuredocs true}}))

;;;; Lifecycle messages

;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#lifeCycleMessages

(defn client-settings [params]
  (-> params
      :initialization-options
      (or {})
      (settings/clean-client-settings)))

(defn ^:private exit [server]
  (logger/info "Exiting...")
  (lsp.server/shutdown server) ;; blocks, waiting up to 10s for previously received messages to be processed
  (shutdown-agents)
  (System/exit 0))

(defmethod lsp.server/receive-request "initialize" [_ components params]
  (logger/info "Initializing...")
  ;; TODO: According to the spec, we shouldn't process any other requests or
  ;; notifications until we've received this request. Furthermore, we shouldn't
  ;; send any requests or notifications (except for $/progress and a few others)
  ;; until after responding to this request. However, we start sending
  ;; diagnostics notifications during `handler/initialize`. This particular case
  ;; might be fixed by delaying `spawn-async-tasks!` until the end of this
  ;; method, or to `initialized`, but the more general case of being selective
  ;; about which messages are sent when probably needs to be handled in lsp4clj.
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  (handler/initialize components
                      (:root-uri params)
                      (:capabilities params)
                      (client-settings params)
                      (some-> params :work-done-token str))
  (let [{:keys [server settings]} (snapc components)]
    (when-let [parent-process-id (:process-id params)]
      (lsp.liveness-probe/start! parent-process-id log-wrapper-fn #(exit server)))
    {:capabilities (capabilities settings)}))

(defmethod lsp.server/receive-notification "initialized" [_ components _params]
  (let [{:keys [server]} (snapc components)]
    (logger/info "Initialized!")
    (->> {:registrations [{:id "id"
                           :method "workspace/didChangeWatchedFiles"
                           :register-options {:watchers [{:glob-pattern known-files-pattern}]}}]}
         (lsp.server/send-request server "client/registerCapability"))))

(defmethod lsp.server/receive-request "shutdown" [_ {:keys [db*]} _params]
  (logger/info "Shutting down...")
  (reset! db* db/initial-db) ;; resets db for dev
  nil)

(defmethod lsp.server/receive-notification "exit" [_ components _params]
  (let [{:keys [server]} (snapc components)]
    (exit server)))

(defmacro ^:private safe-async-task [task-name & task-body]
  `(async/thread
     (loop []
       (try
         ~@task-body
         (catch Exception e#
           (logger/error e# (format "Error during async task %s" ~task-name))))
       (recur))))

(defn ^:private spawn-async-tasks!
  [{:keys [producer current-changes-chan diagnostics-chan
           watched-files-chan edits-chan] :as components}]
  (let [debounced-diags (shared/debounce-by diagnostics-chan diagnostics-debounce-ms :uri)
        debounced-changes (shared/debounce-by current-changes-chan change-debounce-ms :uri)
        debounced-watched-files (shared/debounce-all watched-files-chan watched-files-debounce-ms)]
    (safe-async-task
      :edits
      (when-let [edit (async/<!! edits-chan)]
        (producer/publish-workspace-edit producer edit)))
    (safe-async-task
      :diagnostics
      (when-let [diagnostic (async/<!! debounced-diags)]
        (producer/publish-diagnostic producer diagnostic)))
    (safe-async-task
      :changes
      (when-let [changes (async/<!! debounced-changes)] ;; do not put inside shared/logging-task; parked time gets included in task time
        (shared/logging-task
          :analyze-file
          (f.file-management/analyze-changes changes components))))
    (safe-async-task
      :watched-files
      (when-let [watched-files (async/<!! debounced-watched-files)] ;; do not put inside shared/logging-task; parked time gets included in task time
        (shared/logging-task
          :analyze-files-in-watched-dir
          (f.file-management/analyze-watched-files! watched-files components))))))

(defn ^:private monitor-server-logs [log-ch]
  ;; NOTE: if this were moved to `initialize`, after timbre has been configured,
  ;; the server's startup logs and traces would appear in the regular log file
  ;; instead of the temp log file. We don't do this though because if anything
  ;; bad happened before `initialize`, we wouldn't get any logs.
  (async/go-loop []
    (when-let [log-args (async/<! log-ch)]
      (apply log-wrapper-fn log-args)
      (recur))))

(defn ^:private setup-dev-environment [db*]
  ;; We don't have an ENV=development flag, so the next best indication that
  ;; we're in a development environment is whether we're able to start an nREPL.
  (when-let [nrepl-port (nrepl/setup-nrepl)]
    ;; Save the port in the db, so it can be reported in server-info.
    (swap! db* assoc :port nrepl-port)
    ;; In the development environment, make the db* atom available globally as
    ;; db/db*, so it can be inspected in the nREPL.
    (alter-var-root #'db/db* (constantly db*))))

(defn run-server! []
  (lsp.server/discarding-stdout
    (let [timbre-logger (->TimbreLogger)
          log-path (logger/setup timbre-logger)
          db (assoc db/initial-db :log-path log-path)
          db* (atom db)
          log-ch (async/chan (async/sliding-buffer 20))
          server (lsp.io-server/stdio-server {:log-ch log-ch
                                              ;; uncomment for server-side traces
                                              #_#_:trace-ch log-ch})
          producer (ClojureLspProducer. server db*)
          components {:db* db*
                      :logger timbre-logger
                      :producer producer
                      :server server
                      :current-changes-chan (async/chan 1)
                      :diagnostics-chan (async/chan 1)
                      :watched-files-chan (async/chan 1)
                      :edits-chan (async/chan 1)}]
      (logger/info "[SERVER]" "Starting server...")
      (monitor-server-logs log-ch)
      (setup-dev-environment db*)
      (spawn-async-tasks! components)
      (lsp.server/start server components))))
