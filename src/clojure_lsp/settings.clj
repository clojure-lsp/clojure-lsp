(ns clojure-lsp.settings
  (:refer-clojure :exclude [get])
  (:require
   [clojure-lsp.config :as config]
   [clojure.core.memoize :as memoize]
   [medley.core :as medley]
   [taoensso.timbre :as log]))

(defn ^:private get-refreshed-settings [db]
  (let [{:keys [project-root-uri settings force-settings]} @db
        new-project-settings (config/resolve-for-root project-root-uri)]
    (medley/deep-merge settings
                       new-project-settings
                       force-settings)))

(def ttl-threshold-milis 1000)

(def ^:private memoized-settings
  (memoize/ttl get-refreshed-settings :ttl/threshold ttl-threshold-milis))

(defn all [db]
  (if (= :test (:env @db))
    (get-refreshed-settings db)
    (memoized-settings db)))

(defn get
  "Memorize get settings from db.
  Re-set settings in db if reaches memoize threshold."
  ([db kws]
   (get db kws nil))
  ([db kws default]
   (let [settings (if (= :test (:env @db))
                    (get-refreshed-settings db)
                    (memoized-settings db))]
     (get-in settings kws default))))
