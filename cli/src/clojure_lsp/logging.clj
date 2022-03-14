(ns clojure-lsp.logging
  (:require
   [lsp4clj.protocols :as protocols]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(defrecord TimbreLogger [db]
  protocols/ILSPLogger
  (setup [_this]
    (let [log-path (str (java.io.File/createTempFile "clojure-lsp." ".out"))]
      (log/merge-config! {:middleware [#(assoc % :hostname_ "")]
                          :appenders {:println {:enabled? false}
                                      :spit (log/spit-appender {:fname log-path})}})
      (log/handle-uncaught-jvm-exceptions!)
      (swap! db assoc :log-path log-path)))

  (set-log-path [_this log-path]
    (log/merge-config! {:appenders {:spit (log/spit-appender {:fname log-path})}}))

  (info [_this arg1] (log/info arg1))
  (info [_this arg1 arg2] (log/info arg1 arg2))
  (info [_this arg1 arg2 arg3] (log/info arg1 arg2 arg3))

  (warn [_this arg1] (log/warn arg1))
  (warn [_this arg1 arg2] (log/warn arg1 arg2))
  (warn [_this arg1 arg2 arg3] (log/warn arg1 arg2 arg3))

  (error [_this arg1] (log/error arg1))
  (error [_this arg1 arg2] (log/error arg1 arg2))
  (error [_this arg1 arg2 arg3] (log/error arg1 arg2 arg3))

  (debug [_this arg1] (log/debug arg1))
  (debug [_this arg1 arg2] (log/debug arg1 arg2))
  (debug [_this arg1 arg2 arg3] (log/debug arg1 arg2 arg3)))
