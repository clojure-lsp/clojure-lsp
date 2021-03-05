(ns clojure-lsp.feature.diagnostics
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.set :as set]
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

(defn ^:private valid-finding? [{:keys [row col] :as finding}]
  (or (and row col)
      (log/warn "Invalid clj-kondo finding. Cannot find position data for" finding)))

(defn ^:private kondo-findings->diagnostics [uri findings]
  (->> (get findings (shared/uri->filename uri))
       (filter #(= (shared/uri->filename uri) (:filename %)))
       (filter valid-finding?)
       (mapv kondo-finding->diagnostic)))

(defn ^:private unused-public-var->diagnostic [settings var]
  {:range (shared/->range var)
   :message (format "Unused public var '%s/%s'" (:ns var) (:name var))
   :code "unused-public-var"
   :severity (case (get-in settings [:linters :unused-public-var :level] :info)
               :error   1
               :warning 2
               :info    3)
   :source "clojure-lsp"})

(def default-public-vars-to-exclude
  '#{clojure.test/deftest})

(defn ^:private exclude-public-var? [settings var]
  (let [excluded-syms (get-in settings [:linters :unused-public-var :exclude] #{})
        excluded-vars (filter qualified-ident? excluded-syms)
        excluded-ns (filter simple-ident? excluded-syms)]
    (not (or (-> excluded-ns
                 set
                 (contains? (:ns var)))
             (-> excluded-vars
                 set
                 (set/union default-public-vars-to-exclude)
                 (contains? (symbol (-> var :ns str) (-> var :name str))))))))

(defn ^:private lint-public-vars [uri analysis settings]
  (when (not (= :off (get-in settings [:linters :unused-public-var :level])))
    (let [filename (shared/uri->filename uri)]
      (->> (q/find-vars analysis filename false)
           (filter (partial exclude-public-var? settings))
           (filter (comp #(= (count %) 0)
                         #(q/find-references-from-cursor analysis filename (:name-row %) (:name-col %) false)))
           (mapv (partial unused-public-var->diagnostic settings))))))

(defn notify [uri db]
  (let [settings (get db :settings)]
    (async/put! db/diagnostics-chan
                {:uri uri
                 :diagnostics (into
                               (kondo-findings->diagnostics uri (:findings db))
                               (lint-public-vars uri (:analysis db) settings))})))
