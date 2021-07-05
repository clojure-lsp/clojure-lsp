(ns clojure-lsp.logging
  (:require
   [clojure-lsp.db :as db]
   [taoensso.timbre :as log]))

(defn setup-logging []
  (let [log-path (str (java.io.File/createTempFile "clojure-lsp." ".out"))]
    (log/merge-config! {:appenders {:println {:enabled? false}
                                    :spit (log/spit-appender {:fname log-path})}})
    (log/handle-uncaught-jvm-exceptions!)
    (swap! db/db assoc :log-path log-path)))

(defn update-log-path [log-path]
  (log/merge-config! {:appenders {:spit (log/spit-appender {:fname log-path})}})
  (swap! db/db assoc :log-path log-path))

(defn set-log-to-stdout []
  (log/merge-config! {:appenders {:println (log/println-appender {:stream :auto})}}))
