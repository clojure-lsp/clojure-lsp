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

(def deprecated-diagnostic-types
  #{:deprecated-var})

(defn ^:private kondo-finding->diagnostic
  [{:keys [type message level row col end-row] :as finding}]
  (let [expression? (not= row end-row)
        finding (cond-> (merge {:end-row row :end-col col} finding)
                  expression? (assoc :end-row row :end-col col))]
    {:range (shared/->range finding)
     :tags (cond-> []
             (unnecessary-diagnostic-types type) (conj 1)
             (deprecated-diagnostic-types type) (conj 2))
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

(defn ^:private kondo-findings->diagnostics [filename findings]
  (->> (get findings filename)
       (filter #(= filename (:filename %)))
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
        excluded-full-qualified-vars (set (filter qualified-ident? excluded-syms))
        excluded-ns-or-var (set (filter simple-ident? excluded-syms))]
    (not (or (contains? (set/union default-public-vars-defined-by-to-exclude excluded-defined-by-syms)
                        (:defined-by var))
             (contains? (set/union excluded-ns-or-var default-public-vars-name-to-exclude)
                        (:name var))
             (contains? (set excluded-ns-or-var) (:ns var))
             (-> excluded-full-qualified-vars
                 set
                 (contains? (symbol (-> var :ns str) (-> var :name str))))))))

(defn ^:private lint-public-vars [filename analysis settings]
  (let [start-time (System/nanoTime)
        public-vars (->> (q/find-var-definitions analysis filename false)
                         (filter (partial exclude-public-var? settings))
                         (filter (comp #(= (count %) 0)
                                       #(q/find-references-from-cursor analysis filename (:name-row %) (:name-col %) false)))
                         (mapv (partial unused-public-var->diagnostic settings)))]
    (log/info (format "Linting public vars took %sms for %s" (quot (- (System/nanoTime) start-time) 1000000) filename))
    public-vars))

(defn ^:private find-diagnostics [uri db]
  (let [settings (get db :settings)
        filename (shared/uri->filename uri)]
    (cond-> []
      (not (= :off (get-in settings [:linters :clj-kondo :level])))
      (concat (kondo-findings->diagnostics filename (:findings db)))

      #_(not (= :off (get-in settings [:linters :unused-public-var :level])))
      #_(concat (lint-public-vars filename (:analysis db) settings)))))

(defn sync-lint-file [uri db]
  (async/put! db/diagnostics-chan
              {:uri uri
               :diagnostics (find-diagnostics uri db)}))

(defn async-lint-file [uri db]
  (async/go
    (async/put! db/diagnostics-chan
                {:uri uri
                 :diagnostics (find-diagnostics uri db)})))

(defn unused-public-var-lint-from-kondo!
  [{:keys [analysis reg-finding!]}]
  (let [filename (-> analysis :var-definitions first :filename)
        ;; new-analysis (crawler/normalize-analysis analysis)
        #__ #_(swap! db/db assoc-in [:analysis filename] new-analysis)
        #_elements #_(->> (q/find-var-definitions (:analysis @db/db) filename false)
                      (filter (comp #(= (count %) 0)
                                   #(q/find-references (:analysis @db/db) % false))))]
    ;; (swap! db/db assoc-in [:analysis filename] new-analysis)
    (log/info "---> updated analysis")
    #_(doseq [element elements]
      (reg-finding! {:filename (:filename element)
                     :row (:name-row element)
                     :col (:name-col element)
                     :end-row (:name-end-row element)
                     :end-col (:name-end-col element)
                     :message (format "Unused public var '%s/%s'" (:ns element) (:name element))
                     :type :unused-private-var}))))
