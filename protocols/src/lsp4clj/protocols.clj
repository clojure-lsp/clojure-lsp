(ns lsp4clj.protocols)

(set! *warn-on-reflection* true)

(defprotocol ILSPFeatureHandler
  (initialize [this project-root-uri client-capabilities client-settings work-done-token logger])
  (did-open [this doc])
  (did-change [this doc])
  (did-save [this doc])
  (execute-command [this doc])
  (did-close [this doc])
  (did-change-watched-files [this doc])
  (references [this doc])
  (completion [this doc])
  (completion-resolve-item [this doc])
  (prepare-rename [this doc])
  (rename [this doc])
  (hover [this doc])
  (signature-help [this doc])
  (formatting [this doc])
  (code-actions [this doc])
  (code-lens [this doc])
  (code-lens-resolve [this doc])
  (definition [this doc])
  (declaration [this doc])
  (implementation [this doc])
  (document-symbol [this doc])
  (document-highlight [this doc])
  (semantic-tokens-full [this doc])
  (semantic-tokens-range [this doc])
  (prepare-call-hierarchy [this doc])
  (call-hierarchy-incoming [this doc])
  (call-hierarchy-outgoing [this doc])
  (linked-editing-ranges [this doc])
  ;; (did-delete-files [this doc])
  (workspace-symbols [this doc])
  (range-formatting [this doc-id format-pos]))

(defprotocol ILSPProducer
  (refresh-code-lens [this])
  (publish-diagnostic [this diagnostic])
  (publish-workspace-edit [this edit])
  (publish-progress [this percentage message progress-token])
  (show-document-request [this document-request])
  (show-message-request [this message type actions])
  (show-message [this message type extra])
  (register-capability [this capability]))

(defprotocol ILSPLogger
  (setup [this])

  (set-log-path [_this log-path])

  (info
    [this arg1]
    [this arg1 arg2]
    [this arg1 arg2 arg3])
  (warn
    [this arg1]
    [this arg1 arg2]
    [this arg1 arg2 arg3])
  (error
    [this arg1]
    [this arg1 arg2]
    [this arg1 arg2 arg3])
  (debug
    [this arg1]
    [this arg1 arg2]
    [this arg1 arg2 arg3]))
