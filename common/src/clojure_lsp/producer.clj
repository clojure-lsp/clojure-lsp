(ns clojure-lsp.producer)

(set! *warn-on-reflection* true)

(defprotocol IProducer
  (refresh-code-lens [this])
  (publish-diagnostic [this diagnostic])
  (publish-workspace-edit [this edit])
  (publish-progress [this percentage message progress-token])
  (show-document-request [this document-request])
  (show-message-request [this message type actions])
  (show-message [this message type extra])
  (register-capability [this capability]))
