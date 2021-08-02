(ns clojure-lsp.feature.diagnostics
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [medley.core :as medley]
   [taoensso.timbre :as log]))

(def default-public-vars-defined-by-to-exclude
  '#{clojure.test/deftest
     state-flow.cljtest/defflow})

(def default-public-vars-name-to-exclude
  '#{-main})

(def unnecessary-diagnostic-types
  #{:clojure-lsp/unused-public-var
    :redefined-var
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

(defn ^:private reg-unused-public-var-elements! [elements reg-finding! config]
  (mapv (fn [element]
          (let [finding {:filename (:filename element)
                         :row (:name-row element)
                         :col (:name-col element)
                         :end-row (:name-end-row element)
                         :end-col (:name-end-col element)
                         :level (or (-> config :linters :clojure-lsp/unused-public-var :level) :info)
                         :message (format "Unused public var '%s/%s'" (:ns element) (:name element))
                         :type :clojure-lsp/unused-public-var}]
            (reg-finding! finding)
            finding))
        elements))

(defn ^:private exclude-public-var? [kondo-config var]
  (let [excluded-syms (get-in kondo-config [:linters :clojure-lsp/unused-public-var :exclude] #{})
        excluded-defined-by-syms (get-in kondo-config [:linters :clojure-lsp/unused-public-var :exclude-when-defined-by] #{})
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
     :source (if (= :clojure-lsp/unused-public-var type)
               "clojure-lsp"
               "clj-kondo")}))

(defn ^:private valid-finding? [{:keys [row col] :as finding}]
  (or (and row col)
      (log/warn "Invalid clj-kondo finding. Cannot find position data for" finding)))

(defn ^:private kondo-findings->diagnostics [filename findings]
  (->> (get findings filename)
       (filter #(= filename (:filename %)))
       (filter valid-finding?)
       (mapv kondo-finding->diagnostic)))

(defn ^:private find-diagnostics [uri db]
  (let [settings (get db :settings)
        filename (shared/uri->filename uri)]
    (cond-> []
      (not (= :off (get-in settings [:linters :clj-kondo :level])))
      (concat (kondo-findings->diagnostics filename (:findings db))))))

(defn sync-lint-file [uri db]
  (async/put! db/diagnostics-chan
              {:uri uri
               :diagnostics (find-diagnostics uri db)}))

(defn async-lint-file [uri db]
  (async/go
    (async/put! db/diagnostics-chan
                {:uri uri
                 :diagnostics (find-diagnostics uri db)})))

(defn ^:private lint-project-files [paths]
  (doseq [path paths]
    (doseq [file (file-seq (io/file path))]
      (let [filename (.getAbsolutePath ^java.io.File file)
            uri (shared/filename->uri filename)]
        (when (not= :unknown (shared/uri->file-type uri))
          (sync-lint-file uri @db/db))))))

(defn unused-public-var-lint-for-paths!
  [paths
   {:keys [analysis reg-finding! config]}]
  (async/go
    (let [start-time (System/nanoTime)
          project-analysis (->> (lsp.kondo/normalize-analysis analysis)
                                (group-by :filename)
                                q/filter-project-analysis)
          elements (->> project-analysis
                        q/find-all-var-definitions
                        (filter (partial exclude-public-var? config))
                        (filter (comp #(= (count %) 0)
                                      #(q/find-references project-analysis % false))))
          kondo-findings (group-by :filename (reg-unused-public-var-elements! elements reg-finding! config))
          cur-findings (:findings @db/db)]
      (swap! db/db assoc :findings (merge-with #(->> (into %1 %2)
                                                     (medley/distinct-by (juxt :row :col :end-row :end-col)))
                                               cur-findings
                                               kondo-findings))
      (when (get-in @db/db [:settings :lint-project-files-after-startup?] true)
        (lint-project-files paths))
      (log/info (format "Linting unused public vars for whole project took %sms" (quot (- (System/nanoTime) start-time) 1000000))))))

(defn unused-public-var-lint-for-single-file!
  [{:keys [analysis reg-finding! config]}]
  (let [filename (-> analysis :var-definitions first :filename)
        project-analysis (->> (lsp.kondo/normalize-analysis analysis)
                              (assoc (:analysis @db/db) filename)
                              q/filter-project-analysis)
        elements (->> (q/find-var-definitions project-analysis filename false)
                      (filter (partial exclude-public-var? config))
                      (filter (comp #(= (count %) 0)
                                    #(q/find-references project-analysis % false))))]
    (reg-unused-public-var-elements! elements reg-finding! config)))
