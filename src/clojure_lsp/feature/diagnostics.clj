(ns clojure-lsp.feature.diagnostics
  (:require
   [clojure-lsp.db :as db]
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

(defn ^:private reg-unused-public-var-elements! [elements reg-finding! kondo-config]
  (mapv (fn [element]
          (let [finding {:filename (:filename element)
                         :row (:name-row element)
                         :col (:name-col element)
                         :end-row (:name-end-row element)
                         :end-col (:name-end-col element)
                         :level (or (-> kondo-config :linters :clojure-lsp/unused-public-var :level) :info)
                         :message (format "Unused public var '%s/%s'" (:ns element) (:name element))
                         :type :clojure-lsp/unused-public-var}]
            (reg-finding! finding)
            finding))
        elements))

(defn exclude-public-var? [kondo-config var]
  (let [excluded-syms (get-in kondo-config [:linters :clojure-lsp/unused-public-var :exclude] #{})
        excluded-defined-by-syms (get-in kondo-config [:linters :clojure-lsp/unused-public-var :exclude-when-defined-by] #{})
        excluded-full-qualified-vars (set (filter qualified-ident? excluded-syms))
        excluded-ns-or-var (set (filter simple-ident? excluded-syms))]
    (or (contains? (set/union default-public-vars-defined-by-to-exclude excluded-defined-by-syms)
                   (:defined-by var))
        (contains? (set/union excluded-ns-or-var default-public-vars-name-to-exclude)
                   (:name var))
        (contains? (set excluded-ns-or-var) (:ns var))
        (-> excluded-full-qualified-vars
            set
            (contains? (symbol (-> var :ns str) (-> var :name str)))))))

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
  (let [settings (:settings @db)
        filename (shared/uri->filename uri)]
    (cond-> []
      (not (= :off (get-in settings [:linters :clj-kondo :level])))
      (concat (kondo-findings->diagnostics filename (:findings @db))))))

(defn sync-lint-file! [uri db]
  (async/>!! db/diagnostics-chan
             {:uri uri
              :diagnostics (find-diagnostics uri db)}))

(defn async-lint-file! [uri db]
  (if (= :test (:env @db)) ;; Avoid async on test which cause flakeness
    (async/put! db/diagnostics-chan
                {:uri uri
                 :diagnostics (find-diagnostics uri db)})
    (async/go
      (async/>! db/diagnostics-chan
                {:uri uri
                 :diagnostics (find-diagnostics uri db)}))))

(defn clean! [uri]
  (async/>!! db/diagnostics-chan
             {:uri uri
              :diagnostics []}))

(defn ^:private lint-project-files [paths db]
  (doseq [path paths]
    (doseq [file (file-seq (io/file path))]
      (let [filename (.getAbsolutePath ^java.io.File file)
            uri (shared/filename->uri filename db)]
        (when (not= :unknown (shared/uri->file-type uri))
          (sync-lint-file! uri db))))))

(defn ^:private unused-public-vars-lint!
  [var-definitions project-analysis {:keys [config reg-finding!]}]
  (let [elements (->> var-definitions
                      (remove (partial exclude-public-var? config))
                      (filter (comp #(= (count %) 0)
                                    #(q/find-references project-analysis % false))))]
    (->> (reg-unused-public-var-elements! elements reg-finding! config)
         (group-by :filename))))

(defn lint-project-diagnostics!
  [paths new-analysis kondo-ctx db]
  (let [project-analysis (q/filter-project-analysis new-analysis)
        var-definitions (q/find-all-var-definitions project-analysis)
        kondo-findings (unused-public-vars-lint! var-definitions project-analysis kondo-ctx)]
    (loop [state-db @db]
      (let [cur-findings (:findings state-db)
            new-findings (merge-with #(->> (into %1 %2)
                                           (medley/distinct-by (juxt :row :col :end-row :end-col)))
                                     cur-findings
                                     kondo-findings)]
        (if (compare-and-set! db state-db (assoc state-db :findings new-findings))
          (lint-project-files paths db)
          (recur @db))))))

(defn unused-public-var-lint-for-single-file!
  [filename analysis kondo-ctx]
  (let [project-analysis (q/filter-project-analysis analysis)
        var-definitions (q/find-var-definitions project-analysis filename false)]
    (unused-public-vars-lint! var-definitions project-analysis kondo-ctx)))
