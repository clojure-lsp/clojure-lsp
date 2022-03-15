(ns clojure-lsp.clojure-producer)

(defprotocol IClojureProducer
  (refresh-test-tree [this uris]))
