(ns clojure-lsp.feature.diagnostics
  (:require
   [clj-kondo.impl.config :as kondo.config]
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.string :as string]
   [rewrite-clj.zip :as z])
  (:gen-class))

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

(defn ^:private kondo-config-for-ns [kondo-config ns-name filename]
  (let [ns-groups (cons ns-name (kondo.config/ns-groups kondo-config ns-name filename))
        configs-in-ns (seq (keep #(get (:config-in-ns kondo-config) %) ns-groups))
        kondo-config (if configs-in-ns
                       (apply kondo.config/merge-config! kondo-config configs-in-ns)
                       kondo-config)]
    kondo-config))

(defn ^:private unused-public-var->finding [element kondo-config]
  (let [keyword-def? (identical? :keyword-definitions (:bucket element))
        kondo-config (if (:ns element)
                       (kondo-config-for-ns kondo-config (:ns element) (:filename element))
                       kondo-config)]
    {:uri (:uri element)
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

(defn ^:private exclude-public-diagnostic-definition? [db kondo-config definition]
  (let [kondo-config (kondo-config-for-ns kondo-config (:ns definition) (:filename definition))
        excluded-syms-regex (get-in kondo-config [:linters :clojure-lsp/unused-public-var :exclude-regex] #{})
        excluded-defined-by-syms-regex (get-in kondo-config [:linters :clojure-lsp/unused-public-var :exclude-when-defined-by-regex] #{})
        fqsn (symbol (-> definition :ns str) (-> definition :name str))
        starts-with-dash? (string/starts-with? (:name definition) "-")]
    (or (q/exclude-public-definition? kondo-config definition)
        (some #(re-matches (re-pattern (str %)) (str fqsn)) excluded-syms-regex)
        (some #(re-matches (re-pattern (str %)) (str (:defined-by definition))) excluded-defined-by-syms-regex)
        (:export definition)
        (when starts-with-dash?
          ;; check if if namespace has :gen-class
          (some-> (parser/zloc-of-file db (:uri definition))
                  edit/find-namespace
                  (z/find-next-value z/next :gen-class))))))

(defn ^:private kondo-finding->diagnostic
  [range-type
   {:keys [type message level row col end-row] :as finding}]
  (let [full-range? (or (not= row end-row)
                        (not (identical? :full range-type)))
        finding (cond-> (merge {:end-row row :end-col col} finding)
                  full-range? (assoc :end-row row :end-col col))]
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

(defn ^:private exclude-ns? [uri linter db]
  ;; TODO: it's possible, though unusual, to have several namespaces in a file.
  ;; What should we do in that case?
  (when-let [namespace (first (dep-graph/ns-names-for-uri db uri))]
    (when-let [ns-exclude-regex-str (settings/get db [:linters linter :ns-exclude-regex])]
      (re-matches (re-pattern ns-exclude-regex-str) (str namespace)))))

(defn ^:private kondo-findings->diagnostics [uri linter db]
  (let [range-type (settings/get db [:diagnostics :range-type] :full)]
    (when-not (exclude-ns? uri linter db)
      (->> (get-in db [:findings uri])
           (filter valid-finding?)
           (mapv (partial kondo-finding->diagnostic range-type))))))

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

(defn ^:private clj-depend-violations->diagnostics [uri level db]
  ;; TODO: it's possible, though unusual, to have several namespaces in a file.
  ;; What should we do in that case?
  (when-let [namespace (first (dep-graph/ns-names-for-uri db uri))]
    (mapv (fn [{:keys [message]}]
            (let [ns-definition (q/find-namespace-definition-by-uri db uri)]
              {:range (shared/->range ns-definition)
               :tags []
               :message message
               :code "clj-depend"
               :severity (case level
                           :error   1
                           :warning 2
                           :info    3)
               :source "clj-depend"}))
          (get-in db [:clj-depend-violations (symbol namespace)]))))

(defn find-diagnostics [^String uri db]
  (let [kondo-level (settings/get db [:linters :clj-kondo :level])
        depend-level (settings/get db [:linters :clj-depend :level] :info)]
    (if (shared/jar-file? uri)
      []
      (cond-> []
        (not= :off kondo-level)
        (concat (kondo-findings->diagnostics uri :clj-kondo  db))

        (not= :off depend-level)
        (concat (clj-depend-violations->diagnostics uri depend-level db))))))

(defn ^:private publish-diagnostic!* [{:keys [diagnostics-chan]} diagnostic]
  (async/put! diagnostics-chan diagnostic))

(defn ^:private publish-all-diagnostics!* [{:keys [diagnostics-chan]} diagnostics]
  (async/onto-chan! diagnostics-chan diagnostics false))

(defn ^:private diagnostics-of-uri [uri db]
  {:uri uri
   :diagnostics (find-diagnostics uri db)})

(defn ^:private empty-diagnostics-of-uri [uri]
  {:uri uri
   :diagnostics []})

(defn publish-diagnostics! [uri {:keys [db*], :as components}]
  (publish-diagnostic!* components (diagnostics-of-uri uri @db*)))

(defn publish-all-diagnostics! [uris {:keys [db*], :as components}]
  (let [db @db*]
    (publish-all-diagnostics!*
      components
      (->> uris
           (remove #(= :unknown (shared/uri->file-type %)))
           (map #(diagnostics-of-uri % db))))))

(defn publish-empty-diagnostics! [uris components]
  (publish-all-diagnostics!* components (map empty-diagnostics-of-uri uris)))

(defn ^:private unused-public-vars [narrowed-db project-db kondo-config]
  (let [exclude-def? (partial exclude-public-diagnostic-definition? project-db kondo-config)
        var-definitions (->> (q/find-all-var-definitions narrowed-db)
                             (remove exclude-def?))
        var-nses (set (map :ns var-definitions)) ;; optimization to limit usages to internal namespaces, or in the case of a single file, to its namespaces
        var-usages (into #{}
                         (comp
                           (q/xf-all-var-usages-to-namespaces var-nses)
                           (map q/var-usage-signature))
                         (q/nses-and-dependents-analysis project-db var-nses))
        var-used? (fn [var-def]
                    (some var-usages (q/var-definition-signatures var-def)))
        kw-definitions (->> (q/find-all-keyword-definitions narrowed-db)
                            (remove exclude-def?))
        kw-usages (if (seq kw-definitions) ;; avoid looking up thousands of keyword usages if these files don't define any keywords
                    (into #{}
                          (comp
                            q/xf-all-keyword-usages
                            (map q/kw-signature))
                          (:analysis project-db))
                    #{})
        kw-used? (fn [kw-def]
                   (contains? kw-usages (q/kw-signature kw-def)))]
    (->> (concat (remove var-used? var-definitions)
                 (remove kw-used? kw-definitions))
         (map (fn [unused-var]
                (unused-public-var->finding unused-var kondo-config))))))

(defn ^:private findings-of-project
  [db kondo-config]
  (let [project-db (q/db-with-internal-analysis db)]
    (unused-public-vars project-db project-db kondo-config)))

(defn ^:private findings-of-uris
  [uris db kondo-config]
  (let [project-db (q/db-with-internal-analysis db)
        db-of-uris (update project-db :analysis select-keys uris)]
    (unused-public-vars db-of-uris project-db kondo-config)))

(defn ^:private finalize-findings! [findings reg-finding!]
  (let [uri->filename (memoize shared/uri->filename)]
    (run! #(reg-finding! (assoc % :filename (uri->filename (:uri %)))) findings))
  nil)

(defn custom-lint-project!
  [db {:keys [reg-finding! config]}]
  (-> (findings-of-project db config)
      (finalize-findings! reg-finding!)))

(defn custom-lint-uris!
  [uris db {:keys [reg-finding! config]}]
  (-> (findings-of-uris uris db config)
      (finalize-findings! reg-finding!)))
