(ns clojure-lsp.feature.diagnostics
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [medley.core :as medley]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(def default-public-vars-defined-by-to-exclude
  '#{clojure.test/deftest
     cljs.test/deftest
     state-flow.cljtest/defflow
     potemkin/import-vars})

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
            (reg-finding! finding)))
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
     :code (if-let [n (namespace type)]
             (str n "/" (name type))
             (name type))
     :severity (case level
                 :error   1
                 :warning 2
                 :info    3)
     :source (if (= :clojure-lsp/unused-public-var type)
               "clojure-lsp"
               "clj-kondo")}))

(defn ^:private valid-finding? [{:keys [row col level] :as finding}]
  (when (not= level :off)
    (or (and row col)
        (log/warn "Invalid clj-kondo finding. Cannot find position data for" finding))))

(defn ^:private exclude-ns? [filename linter db]
  (when-let [namespace (shared/filename->namespace filename db)]
    (when-let [ns-exclude-regex-str (settings/get db [:linters linter :ns-exclude-regex])]
      (re-matches (re-pattern ns-exclude-regex-str) (str namespace)))))

(defn ^:private kondo-findings->diagnostics [filename linter db]
  (when-not (exclude-ns? filename linter db)
    (->> (get (:findings @db) filename)
         (filter #(= filename (:filename %)))
         (filter valid-finding?)
         (mapv kondo-finding->diagnostic))))

(defn severity->level [severity]
  (case (int severity)
    1 :error
    2 :warning
    3 :info))

(defn severity->color [severity]
  (case (int severity)
    1 :red
    2 :yellow
    3 :cyan))

(defn find-diagnostics [uri db]
  (let [filename (shared/uri->filename uri)]
    (cond-> []
      (not (= :off (settings/get db [:linters :clj-kondo :level])))
      (concat (kondo-findings->diagnostics filename :clj-kondo db)))))

(defn sync-lint-file! [uri db]
  (async/>!! db/diagnostics-chan
             {:uri uri
              :diagnostics (find-diagnostics uri db)}))

(defn async-lint-file! [uri db]
  (if (#{:unit-test :api-test} (:env @db)) ;; Avoid async on test which cause flakeness
    (async/put! db/diagnostics-chan
                {:uri uri
                 :diagnostics (find-diagnostics uri db)})
    (async/go
      (async/>! db/diagnostics-chan
                {:uri uri
                 :diagnostics (find-diagnostics uri db)}))))

(defn clean! [uri db]
  (if (#{:unit-test :api-test} (:env @db))
    (async/put! db/diagnostics-chan
                {:uri uri
                 :diagnostics []})
    (async/go
      (async/>! db/diagnostics-chan
                {:uri uri
                 :diagnostics []}))))

(defn ^:private lint-project-files [paths db]
  (doseq [path paths]
    (doseq [file (file-seq (io/file path))]
      (let [filename (.getAbsolutePath ^java.io.File file)
            uri (shared/filename->uri filename db)]
        (when (not= :unknown (shared/uri->file-type uri))
          (sync-lint-file! uri db))))))

(defn ^:private unused-public-vars-lint!
  [var-definitions project-analysis {:keys [config reg-finding!]} db]
  (let [elements (->> var-definitions
                      (remove (partial exclude-public-var? config))
                      (filter (comp #(= (count %) 0)
                                    #(q/find-references project-analysis % false db))))]
    (->> (reg-unused-public-var-elements! elements reg-finding! config)
         (remove nil?)
         (group-by :filename))))

(defn lint-project-diagnostics!
  [new-analysis kondo-ctx db]
  (let [project-analysis (q/filter-project-analysis new-analysis db)
        var-definitions (q/find-all-var-definitions project-analysis)]
    (unused-public-vars-lint! var-definitions project-analysis kondo-ctx db)))

(defn lint-and-publish-project-diagnostics!
  [paths new-analysis kondo-ctx db]
  (let [project-analysis (q/filter-project-analysis new-analysis db)
        var-definitions (q/find-all-var-definitions project-analysis)
        kondo-findings (unused-public-vars-lint! var-definitions project-analysis kondo-ctx db)]
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
  [filename analysis kondo-ctx db]
  (let [project-analysis (q/filter-project-analysis analysis db)
        var-definitions (q/find-var-definitions project-analysis filename false)]
    (unused-public-vars-lint! var-definitions project-analysis kondo-ctx db)))

(defn unused-public-var-lint-for-single-file-merging-findings!
  [filename analysis kondo-ctx db]
  (let [kondo-findings (-> (unused-public-var-lint-for-single-file! filename analysis kondo-ctx db)
                           (get filename))
        cur-findings (get-in @db [:findings filename])]
    (->> cur-findings
         (remove #(= :clojure-lsp/unused-public-var (:type %)))
         (concat kondo-findings)
         vec)))
