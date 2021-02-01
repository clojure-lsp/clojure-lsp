(ns clojure-lsp.feature.diagnostics
  (:require
    [clojure-lsp.shared :as shared]
    [clojure-lsp.db :as db]
    [clojure.core.async :as async]
    [taoensso.timbre :as log]))

(defn ^:private kondo-finding->diagnostic [{:keys [type message level row col] :as finding}]
  (let [expression? (not= row (:end-row finding))
        finding (cond-> (merge {:end-row row :end-col col} finding)
                  expression? (assoc :end-row row :end-col col))]
    {:range (shared/->range finding)
     :message message
     :code (name type)
     :severity (case level
                 :error   1
                 :warning 2
                 :info    3)
     :source "clj-kondo"}))

(defn notify [uri {:keys [findings]}]
  (async/put! db/diagnostics-chan
                {:uri uri
                 :diagnostics (->> findings
                                   (filter #(= (shared/uri->filename uri) (:filename %)))
                                   (mapv kondo-finding->diagnostic))}))
