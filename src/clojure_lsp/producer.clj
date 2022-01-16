(ns clojure-lsp.producer
  (:require
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(defprotocol IProducer
  (refresh-code-lens [this])
  (refresh-test-tree [this uri])
  (publish-diagnostic [this diagnostic])
  (publish-workspace-edit [this edit])
  (publish-progress [this percentage message progress-token])
  (show-document-request [this document-request])
  (show-message-request [this message type actions])
  (show-message [this message type extra])
  (register-capability [this capability]))
