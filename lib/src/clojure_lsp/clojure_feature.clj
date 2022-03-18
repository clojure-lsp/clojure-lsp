(ns clojure-lsp.clojure-feature)

(defprotocol IClojureLSPFeature
  (server-info-raw [this])
  (server-info-log [this])
  (cursor-info-raw [this doc])
  (cursor-info-log [this doc])
  (clojuredocs-raw [this doc])
  (extension [this method doc-id]))