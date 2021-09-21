(ns clojure-lsp.producer
  (:require
   [clojure-lsp.interop :as interop]
   [taoensso.timbre :as log]
   [clojure-lsp.db :as db])
  (:import
   (org.eclipse.lsp4j
     ApplyWorkspaceEditParams)
   (org.eclipse.lsp4j.services LanguageClient)))

(defn window-show-message
  ([message type db]
   (window-show-message {:message message :type type} db))
  ([message-content db]
   (log/info message-content)
   (when-let [client ^LanguageClient (:client @db)]
     (->> message-content
          (interop/conform-or-log ::interop/show-message)
          (.showMessage client)))))

(defn workspace-apply-edit [edit db]
  (let [client ^LanguageClient (:client @db)]
    (->> edit
         (interop/conform-or-log ::interop/workspace-edit)
         ApplyWorkspaceEditParams.
         (.applyEdit client)
         .get)))

(defn publish-diagnostic [diagnostic db]
  (let [client ^LanguageClient (:client @db)]
    (->> diagnostic
         (interop/conform-or-log ::interop/publish-diagnostics-params)
         (.publishDiagnostics client))))

(defn refresh-code-lens [db]
  (when-let [code-lens-capability (get-in @db [:client-capabilities :workspace :code-lens])]
    (when (.getRefreshSupport code-lens-capability)
      (let [client ^LanguageClient (:client @db)]
        (.refreshCodeLenses client)))))

(defn notify-progress [progress db]
  (when-let [client ^LanguageClient (:client @db)]
     (->> progress
          (interop/conform-or-log ::interop/notify-progress)
          (.notifyProgress client))))
