(ns clojure-lsp.settings
  (:refer-clojure :exclude [get]))

(defn all [db]
  (:settings @db))

(defn get
  "Memorize get settings from db.
  Re-set settings in db if reaches memoize threshold."
  ([db kws]
   (get db kws nil))
  ([db kws default]
   (let [settings (:settings @db)]
     (get-in settings kws default))))
