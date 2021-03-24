(ns clojure-lsp.feature.diagnostics
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.set :as set]
   [taoensso.timbre :as log]))

(def unnecessary-diagnostic-types
  #{:redefined-var
    :redundant-do
    :redundant-expression
    :redundant-let
    :unused-binding
    :unreachable-code
    :unused-import
    :unused-namespace
    :unused-private-var
    :unused-referred-var})

(defn ^:private kondo-finding->diagnostic
  [{:keys [type message level row col end-row] :as finding}]
  (let [expression? (not= row end-row)
        finding (cond-> (merge {:end-row row :end-col col} finding)
                  expression? (assoc :end-row row :end-col col))]
    {:range (shared/->range finding)
     :tags (cond-> []
             (unnecessary-diagnostic-types type) (conj 1)
             (#{:deprecated-var} type) (conj 2))
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
   :tags [1]
   :severity (case (get-in settings [:linters :unused-public-var :level] :info)
               :error   1
               :warning 2
               :info    3)
   :source "clojure-lsp"})

(def default-public-vars-defined-by-to-exclude
  '#{clojure.test/deftest
     state-flow.cljtest/defflow})

(def default-public-vars-name-to-exclude
  '#{-main})

(defn ^:private exclude-public-var? [settings var]
  (let [excluded-syms (get-in settings [:linters :unused-public-var :exclude] #{})
        excluded-defined-by-syms (get-in settings [:linters :unused-public-var :exclude-when-defined-by] #{})
        excluded-vars (filter qualified-ident? excluded-syms)
        excluded-ns (filter simple-ident? excluded-syms)]
    (not (or (contains? (set/union default-public-vars-defined-by-to-exclude excluded-defined-by-syms)
                        (:defined-by var))
             (contains? default-public-vars-name-to-exclude (:name var))
             (-> excluded-ns
                 set
                 (contains? (:ns var)))
             (-> excluded-vars
                 set
                 (contains? (symbol (-> var :ns str) (-> var :name str))))))))

(defn ^:private lint-public-vars [uri analysis settings]
  (when (not (= :off (get-in settings [:linters :unused-public-var :level])))
    (let [filename (shared/uri->filename uri)
          public-vars (->>
                        (q/find-vars analysis filename false)
                        (filter (partial exclude-public-var? settings))
                        (map (juxt (juxt :ns :name) identity))
                        (into {}))
          unused-vars (reduce
                        (fn [without-reference element]
                          (let [element-key [(or (:ns element) (:to element)) (:name element)]]
                            (if (and (not= :keywords (:bucket element))
                                     (not= :var-definitions (:bucket element))
                                     (contains? without-reference element-key))
                              (let [next-accum (dissoc without-reference element-key)]
                                (if (empty? next-accum)
                                  (reduced next-accum)
                                  next-accum))
                              without-reference)))
                        public-vars
                        (mapcat val analysis))]
      (mapv (partial unused-public-var->diagnostic settings) (vals unused-vars)))))

(defn ^:private find-diagnostics [uri db]
  (let [#_#_start-time (System/nanoTime)
        settings (get db :settings)
        diagnostics (cond-> []
                      (not (= :off (get-in settings [:linters :clj-kondo :level])))
                      (into (kondo-findings->diagnostics uri (:findings db)))

                      (not (= :off (get-in settings [:linters :unused-public-var :level])))
                      (into (lint-public-vars uri (:analysis db) settings)))]
    #_(log/info "Find diagnostics took: " (float (/ (- (System/nanoTime) start-time) 1000000000)) "seconds " uri)
    diagnostics))

(defn lint-file [uri db]
  (async/put! db/diagnostics-chan
              {:uri uri
               :diagnostics (find-diagnostics uri db)}))
