(ns lsp4clj.protocols.logger)

(defprotocol ILSPLogger
  (setup [this])

  (set-log-path [_this log-path])

  (-info [this arg1] [this arg1 arg2] [this arg1 arg2 arg3])
  (-warn [this arg1] [this arg1 arg2] [this arg1 arg2 arg3])
  (-error [this arg1] [this arg1 arg2] [this arg1 arg2 arg3])
  (-debug [this arg1] [this arg1 arg2] [this arg1 arg2 arg3]))

(def ^:dynamic *logger*
  "Optional logger state to avoid having component available everywhere."
  nil)

(defn set-logger! [logger]
  (alter-var-root #'*logger* (constantly logger)))

(defn info [& args]
  (when *logger*
    (apply -info *logger* args)))

(defn warn [& args]
  (when *logger*
    (apply -warn *logger* args)))

(defn error [& args]
  (when *logger*
    (apply -error *logger* args)))

(defn debug [& args]
  (when *logger*
    (apply -debug *logger* args)))
