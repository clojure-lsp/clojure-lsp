(ns clojure-lsp.logging
  (:require
   [taoensso.timbre :as log]))

(defn setup-logging [db]
  (let [log-path (str (java.io.File/createTempFile "clojure-lsp." ".out"))]
    (log/merge-config! {:appenders {:println {:enabled? false}
                                    :spit (log/spit-appender {:fname log-path})}})
    (log/handle-uncaught-jvm-exceptions!)
    (swap! db assoc :log-path log-path)))

(defn update-log-path [log-path db]
  (log/merge-config! {:appenders {:spit (log/spit-appender {:fname log-path})}})
  (swap! db assoc :log-path log-path))

(defn set-log-to-stdout []
  (log/merge-config! {:appenders {:println (log/println-appender {:stream :auto})}}))
