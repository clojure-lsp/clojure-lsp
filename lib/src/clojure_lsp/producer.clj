(ns clojure-lsp.producer
  "An interface for sending messages to a 'client', whether that's an editor,
  the CLI, or a no-op producer for tests.")

(set! *warn-on-reflection* true)

(defprotocol IProducer
  (refresh-code-lens [this])
  (publish-diagnostic [this diagnostic])
  (publish-workspace-edit [this edit])
  (create-work-done-progress [this progress-token])
  (publish-progress [this percentage message progress-token])
  (show-document-request [this document-request])
  (show-message-request [this message type actions])
  (show-message [this message type extra])
  (refresh-test-tree [this uris]))

(defn with-work-done-progress [producer title f]
  (let [progress-token (str (random-uuid))]
    (if (create-work-done-progress producer progress-token)
      (do
        (publish-progress producer 0 title progress-token)
        (try
          (f)
          (finally
            (publish-progress producer 100 nil progress-token))))
      (f))))
