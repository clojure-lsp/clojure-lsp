(ns clojure-lsp.window
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.interop :as interop]
    [clojure.tools.logging :as log]))

(defn show-message
  ([message type]
   (show-message {:message message :type type}))
  ([message-type]
   (log/info message-type)
   (->> message-type
        (interop/conform-or-log ::interop/show-message)
        (.showMessage (:client @db/db)))))
