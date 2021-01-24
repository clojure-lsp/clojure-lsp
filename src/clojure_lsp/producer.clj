(ns clojure-lsp.producer
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.interop :as interop]
    [taoensso.timbre :as log])
  (:import
   (org.eclipse.lsp4j
     ApplyWorkspaceEditParams)))

(defn window-show-message
  ([message type]
   (window-show-message {:message message :type type}))
  ([message-content]
   (log/info message-content)
   (->> message-content
        (interop/conform-or-log ::interop/show-message)
        (.showMessage (:client @db/db)))))

(defn workspace-apply-edit [edit]
  (->> edit
       (interop/conform-or-log ::interop/workspace-edit)
       ApplyWorkspaceEditParams.
       (.applyEdit (:client @db/db))
       .get))

(defn publish-diagnostic [diagnostic]
  (->> diagnostic
       (interop/conform-or-log ::interop/publish-diagnostics-params)
       (.publishDiagnostics (:client @db/db))))
