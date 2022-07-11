(ns clojure-lsp.producer
  "An interface for sending messages to a 'client', whether that's an editor,
  the CLI, or a no-op producer for tests.")

(defprotocol IProducer
  (refresh-code-lens [this])
  (publish-diagnostic [this diagnostic])
  (publish-workspace-edit [this edit])
  (publish-progress [this percentage message progress-token])
  (show-document-request [this document-request])
  (show-message-request [this message type actions])
  (show-message [this message type extra])
  (refresh-test-tree [this uris]))
