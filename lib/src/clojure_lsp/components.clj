(ns clojure-lsp.components
  (:require
   [clojure-lsp.settings :as settings]))

(defn snapc
  "Snapshot components. Puts the current value of db* in `:db` and the current
  settings in `:settings`."
  [components]
  (let [db @(:db* components)]
    (assoc components
           :db db
           :settings (settings/all db)
           :client-capabilities (:client-capabilities db))))
