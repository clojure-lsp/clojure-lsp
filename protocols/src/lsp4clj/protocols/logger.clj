(ns lsp4clj.protocols.logger
  (:require
   [clojure.string :as string]))

(defprotocol ILSPLogger
  (setup [this])

  (set-log-path [_this log-path])

  (-info [this message])
  (-warn [this message])
  (-error [this message])
  (-debug [this message]))

(def ^:dynamic *logger*
  "Optional logger state to avoid having component available everywhere."
  nil)

(defn set-logger! [logger]
  (alter-var-root #'*logger* (constantly logger)))

(defn info [& args]
  (when *logger*
    (-info *logger* (string/join " " args))))

(defn warn [& args]
  (when *logger*
    (-warn *logger* (string/join " " args))))

(defn error [& args]
  (when *logger*
    (-error *logger* (string/join " " args))))

(defn debug [& args]
  (when *logger*
    (-debug *logger* (string/join " " args))))
