(ns clojure-lsp.handler)

(set! *warn-on-reflection* true)

(defprotocol IHandler
  (did-open [this doc])
  (did-change [this doc])
  (did-save [this doc])
  (execute-command [this doc])
  (did-close [this doc])
  (did-change-watched-files [this doc])
  (cursor-info-log [this doc])
  )
