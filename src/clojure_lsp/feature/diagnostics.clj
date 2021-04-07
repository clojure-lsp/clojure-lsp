(ns clojure-lsp.feature.diagnostics
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.set :as set]
   [rewrite-clj.zip :as z]
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
  [{:keys [type message level row col end-row] :as finding}
   text]
  (let [start-char (-> (parser/loc-at-pos text row col)
                       z/string
                       (nth 0 nil))
        expression? (or (not= row end-row)
                        (identical? \( start-char))
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

(defn ^:private kondo-findings->diagnostics [uri findings text]
  (->> (get findings (shared/uri->filename uri))
       (filter #(= (shared/uri->filename uri) (:filename %)))
       (filter valid-finding?)
       (mapv #(kondo-finding->diagnostic % text))))

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
    (let [filename (shared/uri->filename uri)]
      (->> (q/find-vars analysis filename false)
           (filter (partial exclude-public-var? settings))
           (filter (comp #(= (count %) 0)
                         #(q/find-references-from-cursor analysis filename (:name-row %) (:name-col %) false)))
           (mapv (partial unused-public-var->diagnostic settings))))))

(defn ^:private find-diagnostics [uri db]
  (let [settings (get db :settings)]
    (cond-> []
      (not (= :off (get-in settings [:linters :clj-kondo :level])))
      (concat (kondo-findings->diagnostics uri (:findings db) (get-in db [:documents uri :text])))

      (not (= :off (get-in settings [:linters :unused-public-var :level])))
      (concat (lint-public-vars uri (:analysis db) settings)))))

(defn lint-file [uri db]
  (async/put! db/diagnostics-chan
              {:uri uri
               :diagnostics (find-diagnostics uri db)}))
