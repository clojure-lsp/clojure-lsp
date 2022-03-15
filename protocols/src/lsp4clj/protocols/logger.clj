(ns lsp4clj.protocols.logger)

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
