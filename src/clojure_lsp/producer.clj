(ns clojure-lsp.producer
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.interop :as interop]
    [taoensso.timbre :as log])
  (:import
   (org.eclipse.lsp4j
     ApplyWorkspaceEditParams)
   (org.eclipse.lsp4j.services LanguageClient)))

(defn window-show-message
  ([message type]
   (window-show-message {:message message :type type}))
  ([message-content]
   (log/info message-content)
   (let [client ^LanguageClient (:client @db/db)]
     (->> message-content
          (interop/conform-or-log ::interop/show-message)
          (.showMessage client)))))

(defn workspace-apply-edit [edit]
  (let [client ^LanguageClient (:client @db/db)]
    (->> edit
         (interop/conform-or-log ::interop/workspace-edit)
         ApplyWorkspaceEditParams.
         (.applyEdit client)
         .get)))

(defn publish-diagnostic [diagnostic]
  (let [client ^LanguageClient (:client @db/db)]
    (->> diagnostic
         (interop/conform-or-log ::interop/publish-diagnostics-params)
         (.publishDiagnostics client))))
