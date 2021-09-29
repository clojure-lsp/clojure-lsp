(ns clojure-lsp.logging
  (:require
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

#_{:clj-kondo/ignore [:unresolved-var]}
(defn setup-logging [db]
  (let [log-path (str (java.io.File/createTempFile "clojure-lsp." ".out"))]
    (log/merge-config! {:appenders {:println {:enabled? false}
                                    :spit (log/spit-appender {:fname log-path})}})
    (log/handle-uncaught-jvm-exceptions!)
    (swap! db assoc :log-path log-path)))

#_{:clj-kondo/ignore [:unresolved-var]}
(defn update-log-path [log-path db]
  (log/merge-config! {:appenders {:spit (log/spit-appender {:fname log-path})}})
  (swap! db assoc :log-path log-path))

#_{:clj-kondo/ignore [:unresolved-var]}
(defn set-log-to-stdout []
  (log/merge-config! {:appenders {:println (log/println-appender {:stream :auto})}}))
