(ns clojure-lsp.feature.diagnostics
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [com.climate.claypoole :as cp]
   [lsp4clj.protocols.logger :as logger]
   [medley.core :as medley]))

(set! *warn-on-reflection* true)

(def diagnostic-types-of-unnecessary-type
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

(defn ^:private reg-unused-public-var-element! [element reg-finding! kondo-config]
  (let [keyword-def? (boolean (:reg element))
        finding {:filename (:filename element)
                 :row (:name-row element)
                 :col (:name-col element)
                 :end-row (:name-end-row element)
                 :end-col (:name-end-col element)
                 :level (or (-> kondo-config :linters :clojure-lsp/unused-public-var :level) :info)
                 :message (if keyword-def?
                            (if (:ns element)
                              (format "Unused public keyword ':%s/%s'" (:ns element) (:name element))
                              (format "Unused public keyword ':%s'" (:name element)))
                            (format "Unused public var '%s/%s'" (:ns element) (:name element)))
                 :type :clojure-lsp/unused-public-var}]
    (reg-finding! finding)))

(defn ^:private exclude-public-diagnostic-definition? [kondo-config definition]
  (let [excluded-syms-regex (get-in kondo-config [:linters :clojure-lsp/unused-public-var :exclude-regex] #{})
        excluded-defined-by-syms-regex (get-in kondo-config [:linters :clojure-lsp/unused-public-var :exclude-when-defined-by-regex] #{})
        fqsn (symbol (-> definition :ns str) (-> definition :name str))]
    (or (q/exclude-public-definition? kondo-config definition)
        (some #(re-matches (re-pattern (str %)) (str fqsn)) excluded-syms-regex)
        (some #(re-matches (re-pattern (str %)) (str (:defined-by definition))) excluded-defined-by-syms-regex)
        (:export definition))))

(defn ^:private kondo-finding->diagnostic
  [{:keys [type message level row col end-row] :as finding}]
  (let [expression? (not= row end-row)
        finding (cond-> (merge {:end-row row :end-col col} finding)
                  expression? (assoc :end-row row :end-col col))]
    {:range (shared/->range finding)
     :tags (cond-> []
             (diagnostic-types-of-unnecessary-type type) (conj 1)
             (deprecated-diagnostic-types type) (conj 2))
     :message message
     :code (if-let [n (namespace type)]
             (str n "/" (name type))
             (name type))
     :severity (case level
                 :error   1
                 :warning 2
                 :info    3)
     :source (if (identical? :clojure-lsp/unused-public-var type)
               "clojure-lsp"
               "clj-kondo")}))

(defn ^:private valid-finding? [logger {:keys [row col level] :as finding}]
  (when (not= level :off)
    (or (and row col)
        (logger/warn logger "Invalid clj-kondo finding. Cannot find position data for" finding))))

(defn ^:private exclude-ns? [filename linter db]
  (when-let [namespace (shared/filename->namespace filename db)]
    (when-let [ns-exclude-regex-str (settings/get db [:linters linter :ns-exclude-regex])]
      (re-matches (re-pattern ns-exclude-regex-str) (str namespace)))))

(defn ^:private kondo-findings->diagnostics [filename linter db]
  (when-not (exclude-ns? filename linter db)
    (->> (get (:findings @db) filename)
         (filter #(= filename (:filename %)))
         (filter (partial valid-finding? (:logger @db)))
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

(defn find-diagnostics [^String uri db]
  (let [filename (shared/uri->filename uri)
        source-paths (settings/get db [:source-paths])]
    (cond-> []
      (and (not= :off (settings/get db [:linters :clj-kondo :level]))
           (not (shared/external-filename? filename source-paths)))
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

(defn ^:private pmap-light
  "Call claypoole pmap with less threads than pmap to avoid topping cpu."
  [f coll]
  (let [threadpool-size (int (Math/ceil (/ (.. Runtime getRuntime availableProcessors) 3)))]
    (cp/upmap threadpool-size f coll)))

(defn ^:private unused-public-vars-lint!
  [definitions project-analysis {:keys [config reg-finding!]} max-parallelize? db]
  (let [parallelize-fn (if max-parallelize? pmap pmap-light)]
    (->> definitions
         (remove (partial exclude-public-diagnostic-definition? config))
         (parallelize-fn #(when (= 0 (count (q/find-references project-analysis % false db)))
                            %))
         (remove nil?)
         (mapv #(reg-unused-public-var-element! % reg-finding! config))
         (group-by :filename))))

(defn ^:private project-definitions [project-analysis]
  (concat (q/find-all-var-definitions project-analysis)
          (q/find-all-keyword-definitions project-analysis)))

(defn ^:private file-definitions [project-analysis filename]
  (concat (q/find-var-definitions project-analysis filename false)
          (q/find-keyword-definitions project-analysis filename)))

(defn lint-project-diagnostics!
  [new-analysis kondo-ctx db]
  (let [project-analysis (q/filter-project-analysis new-analysis db)
        definitions (project-definitions project-analysis)]
    (unused-public-vars-lint! definitions project-analysis kondo-ctx true db)))

(defn lint-and-publish-project-diagnostics!
  [paths new-analysis kondo-ctx db]
  (let [project-analysis (q/filter-project-analysis new-analysis db)
        definitions (project-definitions project-analysis)
        kondo-findings (unused-public-vars-lint! definitions project-analysis kondo-ctx false db)]
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
        definitions (file-definitions project-analysis filename)]
    (unused-public-vars-lint! definitions project-analysis kondo-ctx false db)))

(defn unused-public-var-lint-for-single-file-merging-findings!
  [filename analysis kondo-ctx db]
  (let [kondo-findings (-> (unused-public-var-lint-for-single-file! filename analysis kondo-ctx db)
                           (get filename))
        cur-findings (get-in @db [:findings filename])]
    (->> cur-findings
         (remove #(identical? :clojure-lsp/unused-public-var (:type %)))
         (concat kondo-findings)
         vec)))
