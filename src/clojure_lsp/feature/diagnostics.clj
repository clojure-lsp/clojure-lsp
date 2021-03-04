(ns clojure-lsp.feature.diagnostics
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.shared :as shared]
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

(defn ^:private valid-finding? [{:keys [row col end-row end-col] :as finding}]
  (or (and row
           col
           end-row
           end-col)
      (log/warn "Invalid clj-kondo finding. Cannot find position data for" finding)))

(defn notify [uri {:keys [findings]}]
  (async/put! db/diagnostics-chan
              {:uri uri
               :diagnostics (->> findings
                                 (filter #(= (shared/uri->filename uri) (:filename %)))
                                 (filter valid-finding?)
                                 (mapv kondo-finding->diagnostic))}))
