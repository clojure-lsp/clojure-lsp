(ns clojure-lsp.feature.diagnostics
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [lsp4clj.protocols.logger :as logger]))

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

(defn ^:private unused-public-var->finding [element kondo-config]
  (let [keyword-def? (boolean (:reg element))]
    {:filename (:filename element)
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
     :type :clojure-lsp/unused-public-var}))

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

(defn ^:private valid-finding? [{:keys [row col level] :as finding}]
  (when (not= level :off)
    (or (and row col)
        (logger/warn "Invalid clj-kondo finding. Cannot find position data for" finding))))

(defn ^:private exclude-ns? [filename linter db]
  (when-let [namespace (shared/filename->namespace filename db)]
    (when-let [ns-exclude-regex-str (settings/get db [:linters linter :ns-exclude-regex])]
      (re-matches (re-pattern ns-exclude-regex-str) (str namespace)))))

(defn ^:private kondo-findings->diagnostics [filename linter db]
  (when-not (exclude-ns? filename linter db)
    (->> (get (:findings db) filename)
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

(defn find-diagnostics [^String uri db]
  (let [filename (shared/uri->filename uri)
        source-paths (settings/get db [:source-paths])]
    (cond-> []
      (and (not= :off (settings/get db [:linters :clj-kondo :level]))
           (not (shared/external-filename? filename source-paths)))
      (concat (kondo-findings->diagnostics filename :clj-kondo db)))))

(defn sync-publish-diagnostics! [uri db]
  (async/>!! db/diagnostics-chan
             {:uri uri
              :diagnostics (find-diagnostics uri db)}))

(defn async-publish-diagnostics! [uri db]
  (if (#{:unit-test :api-test} (:env db)) ;; Avoid async on test which cause flakeness
    (async/put! db/diagnostics-chan
                {:uri uri
                 :diagnostics (find-diagnostics uri db)})
    (async/go
      (async/>! db/diagnostics-chan
                {:uri uri
                 :diagnostics (find-diagnostics uri db)}))))

(defn publish-all-diagnostics! [paths db]
  (doseq [path paths]
    (doseq [file (file-seq (io/file path))]
      (let [filename (.getAbsolutePath ^java.io.File file)
            uri (shared/filename->uri filename db)]
        (when (not= :unknown (shared/uri->file-type uri))
          (sync-publish-diagnostics! uri db))))))

(defn publish-empty-diagnostics! [uri db]
  (if (#{:unit-test :api-test} (:env db))
    (async/put! db/diagnostics-chan
                {:uri uri
                 :diagnostics []})
    (async/go
      (async/>! db/diagnostics-chan
                {:uri uri
                 :diagnostics []}))))

(defn ^:private lint-defs!
  [var-defs kw-defs project-analysis {:keys [config reg-finding!]}]
  (let [var-definitions (remove (partial exclude-public-diagnostic-definition? config) var-defs)
        var-nses (set (map :ns var-definitions)) ;; optimization to limit usages to internal namespaces, or in the case of a single file, to its namespaces
        var-usages (into #{}
                         (comp
                           (q/xf-all-var-usages-to-namespaces var-nses)
                           (map q/var-usage-signature))
                         project-analysis)
        var-used? (fn [var-def]
                    (some var-usages (q/var-definition-signatures var-def)))
        kw-definitions (remove (partial exclude-public-diagnostic-definition? config) kw-defs)
        kw-usages (into #{}
                        (comp
                          q/xf-all-keyword-usages
                          (map q/kw-signature))
                        project-analysis)
        kw-used? (fn [kw-def]
                   (contains? kw-usages (q/kw-signature kw-def)))
        findings (->> (concat (remove var-used? var-definitions)
                              (remove kw-used? kw-definitions))
                      (map (fn [unused-var]
                             (unused-public-var->finding unused-var config))))]
    (doseq [finding findings] ;; side-effect to register findings
      (reg-finding! finding))
    (group-by :filename findings)))

(defn ^:private file-var-definitions [project-analysis filename]
  (q/find-var-definitions project-analysis filename false))
(def ^:private file-kw-definitions q/find-keyword-definitions)
(def ^:private all-var-definitions q/find-all-var-definitions)
(def ^:private all-kw-definitions q/find-all-keyword-definitions)

(defn custom-lint-project!
  [new-analysis kondo-ctx]
  (let [project-analysis (into {}
                               q/filter-project-analysis-xf
                               new-analysis)]
    (shared/logging-time
      "Linting whole project for unused-public-var took %s"
      (lint-defs! (all-var-definitions project-analysis)
                  (all-kw-definitions project-analysis)
                  project-analysis kondo-ctx))))

(defn custom-lint-files!
  [filenames new-analysis kondo-ctx]
  (let [project-analysis (into {}
                               q/filter-project-analysis-xf
                               new-analysis)
        file-analyses (select-keys project-analysis filenames)]
    (lint-defs! (all-var-definitions file-analyses)
                (all-kw-definitions file-analyses)
                project-analysis kondo-ctx)))

(defn custom-lint-file!
  [filename analysis kondo-ctx]
  (let [project-analysis (into {}
                               q/filter-project-analysis-xf
                               analysis)]
    (lint-defs! (file-var-definitions project-analysis filename)
                (file-kw-definitions project-analysis filename)
                project-analysis kondo-ctx)))

(defn custom-lint-file-merging-findings!
  [filename analysis kondo-ctx db]
  (let [kondo-findings (-> (custom-lint-file! filename analysis kondo-ctx)
                           (get filename))
        cur-findings (get-in db [:findings filename])]
    (->> cur-findings
         (remove #(identical? :clojure-lsp/unused-public-var (:type %)))
         (concat kondo-findings)
         vec)))
