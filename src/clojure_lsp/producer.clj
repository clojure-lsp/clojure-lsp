(ns clojure-lsp.producer
  (:require
   [clojure-lsp.feature.test-tree :as f.test-tree]
   [clojure-lsp.interop :as interop]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [taoensso.timbre :as log])
  (:import
   (clojure_lsp ClojureLanguageClient)
   (org.eclipse.lsp4j
     MessageActionItem
     ApplyWorkspaceEditParams
     CodeLensWorkspaceCapabilities
     WindowClientCapabilities)))

(set! *warn-on-reflection* true)

(defn window-show-message
  ([message type extra db]
   (window-show-message {:message message :type type :extra extra} db))
  ([message-content db]
   (log/info message-content)
   (if-let [client ^ClojureLanguageClient (:client @db)]
     (->> message-content
          (interop/conform-or-log ::interop/show-message)
          (.showMessage client))
     (when-let [messages-fn (:messages-fn @db)]
       (messages-fn message-content)))))

(defn window-show-message-request
  [message type actions db]
  (when-let [client ^ClojureLanguageClient (:client @db)]
    (let [result (->> {:type type
                       :message message
                       :actions actions}
                      (interop/conform-or-log ::interop/show-message-request)
                      (.showMessageRequest client)
                      .get)]
      (.getTitle ^MessageActionItem result))))

(defn workspace-apply-edit [edit db]
  (let [client ^ClojureLanguageClient (:client @db)]
    (->> edit
         (interop/conform-or-log ::interop/workspace-edit-or-error)
         ApplyWorkspaceEditParams.
         (.applyEdit client)
         .get)))

(defn publish-diagnostic [diagnostic db]
  (let [client ^ClojureLanguageClient (:client @db)]
    (->> diagnostic
         (interop/conform-or-log ::interop/publish-diagnostics-params)
         (.publishDiagnostics client))))

(defn refresh-code-lens [db]
  (when-let [code-lens-capability ^CodeLensWorkspaceCapabilities (get-in @db [:client-capabilities :workspace :code-lens])]
    (when (.getRefreshSupport code-lens-capability)
      (let [client ^ClojureLanguageClient (:client @db)]
        (.refreshCodeLenses client)))))

(defn notify-progress [progress db]
  (when-let [client ^ClojureLanguageClient (:client @db)]
    (->> progress
         (interop/conform-or-log ::interop/notify-progress)
         (.notifyProgress client))))

(defn show-document-request [document-request db]
  (log/info "Requesting to show on editor the document" document-request)
  (when-let [client ^ClojureLanguageClient (:client @db)]
    (when (.getShowDocument ^WindowClientCapabilities (get-in @db [:client-capabilities :window]))
      (->> (update document-request :range #(or (some-> % shared/->range)
                                                (shared/full-file-range)))
           (interop/conform-or-log ::interop/show-document-request)
           (.showDocument client)))))

(defn refresh-test-tree [uri db]
  (async/go
    (shared/logging-time
      ":testTree %s secs"
      (when-let [client ^ClojureLanguageClient (:client @db)]
        (when-let [test-tree (f.test-tree/tree uri db)]
          (->> test-tree
               (interop/conform-or-log ::interop/publish-test-tree-params)
               (.publishTestTree client)))))))
