(ns clojure-lsp.queries
  (:refer-clojure :exclude [ns-aliases])
  (:require
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [clojure.string :as string]
   [medley.core :as medley]))

(set! *warn-on-reflection* true)

(defn elem-langs [element]
  (or (some-> element :lang list set)
      (shared/uri->available-langs (:filename element))))

;;;; Filter analysis, using dep-graph

;; TODO: Remove this when the dep graph experiment is done.
;; NOTE: Do not make this public. It will make it harder to end the dep-graph
;; experiment.
(defn ^:private use-dep-graph? [db]
  (or (settings/get db [:experimental :dep-graph-queries] false)
      (= "true" (System/getenv "CLOJURE_LSP_USE_DEP_GRAPH"))))

(defn ^:private deprecated-buckets-external? [buckets]
  (some-> buckets first val first :external?))

(defn ^:private deprecated-internal-analysis [{:keys [analysis]}]
  (medley/remove-vals deprecated-buckets-external? analysis))

(defn internal-analysis [{:keys [analysis] :as db}]
  (if (use-dep-graph? db)
    (medley/filter-keys
      #(dep-graph/file-internal? db %)
      analysis)
    (deprecated-internal-analysis db)))

(defn external-analysis [{:keys [analysis] :as db}]
  (if (use-dep-graph? db)
    (medley/remove-keys
      #(dep-graph/file-internal? db %)
      analysis)
    (medley/filter-vals deprecated-buckets-external? analysis)))

(defn uris-to-filenames [db uris]
  (map #(dep-graph/uri-to-filename db %) uris))

(defn ^:private uris-analysis [{:keys [analysis] :as db} uris]
  (select-keys analysis (uris-to-filenames db uris)))

(defn ns-analysis [{:keys [analysis] :as db} namespace]
  (if (use-dep-graph? db)
    (uris-analysis db (dep-graph/ns-uris db namespace))
    analysis))

(defn ns-dependents-analysis [{:keys [analysis] :as db} namespace]
  (if (use-dep-graph? db)
    (uris-analysis db (dep-graph/ns-dependents-uris db namespace))
    analysis))

(defn ns-and-dependents-analysis [{:keys [analysis] :as db} namespace]
  (if (use-dep-graph? db)
    (uris-analysis db (dep-graph/ns-and-dependents-uris db namespace))
    analysis))

(defn ns-dependencies-analysis [{:keys [analysis] :as db} namespace]
  (if (use-dep-graph? db)
    (uris-analysis db (dep-graph/ns-dependencies-uris db namespace))
    analysis))

(defn nses-analysis [{:keys [analysis] :as db} namespaces]
  (if (use-dep-graph? db)
    (uris-analysis db (dep-graph/nses-uris db namespaces))
    analysis))

(defn nses-and-dependents-analysis [{:keys [analysis] :as db} namespaces]
  (if (use-dep-graph? db)
    (uris-analysis db (dep-graph/nses-and-dependents-uris db namespaces))
    analysis))

(defn uri-dependents-analysis [{:keys [analysis documents] :as db} uri]
  (if (use-dep-graph? db)
    (transduce (map #(ns-dependents-analysis db %))
               merge
               {}
               (get-in documents [uri :namespaces]))
    analysis))

(defn uri-dependencies-analysis [{:keys [analysis documents] :as db} uri]
  (if (use-dep-graph? db)
    (transduce (map #(ns-dependencies-analysis db %))
               merge
               {}
               (get-in documents [uri :namespaces]))
    analysis))

(defn db-with-analysis [db f & args]
  (assoc db :analysis (apply f db args)))

(defn db-with-internal-analysis [db]
  (db-with-analysis db internal-analysis))

(defn db-with-ns-analysis [db namespace]
  (db-with-analysis db ns-analysis namespace))

;;;; Miscelaneous helpers that may belong in clojure-lsp.dep-graph

;; When using the dep-graph, these helpers don't need the analysis, though they
;; do on the non-dep-graph path. Perhaps they should be moved to
;; clojure-lsp.dep-graph when use-dep-graph? is removed.

(def ^:private as-alias-elems-xf ;; works for dep-graph only
  (mapcat (fn [[namespace {:keys [aliases]}]]
            (keep (fn [alias]
                    (when alias
                      {:to namespace
                       :alias alias}))
                  (dep-graph/ms-distinct aliases)))))

(def ^:private deprecated-as-alias-elems-xf ; works for :namespace-alias only
  (map #(select-keys % [:to :alias])))

(def ^:private xf-analysis->by-bucket (map val))
(defn ^:private xf-by-bucket->bucket-elems [bucket-name]
  (mapcat bucket-name))
(defn ^:private xf-by-bucket->buckets-elems [bucket-names]
  (mapcat (fn [buckets]
            (mapcat buckets bucket-names))))
(defn ^:private xf-analysis->bucket-elems [bucket-name]
  (comp xf-analysis->by-bucket
        (xf-by-bucket->bucket-elems bucket-name)))
(defn ^:private xf-analysis->buckets-elems [& bucket-names]
  (comp xf-analysis->by-bucket
        (xf-by-bucket->buckets-elems bucket-names)))

(def xf-analysis->java-class-definitions (xf-analysis->bucket-elems :java-class-definitions))
(def xf-analysis->keyword-definitions (xf-analysis->bucket-elems :keyword-definitions))
(def xf-analysis->keyword-usages (xf-analysis->bucket-elems :keyword-usages))
(def xf-analysis->keywords (xf-analysis->buckets-elems :keyword-definitions :keyword-usages))
(def xf-analysis->namespace-alias (xf-analysis->bucket-elems :namespace-alias))
(def xf-analysis->namespace-definitions (xf-analysis->bucket-elems :namespace-definitions))
(def xf-analysis->namespace-usages (xf-analysis->bucket-elems :namespace-usages))
(def xf-analysis->protocol-impls (xf-analysis->bucket-elems :protocol-impls))
(def xf-analysis->var-definitions (xf-analysis->bucket-elems :var-definitions))
(def xf-analysis->var-usages (xf-analysis->bucket-elems :var-usages))
(def xf-analysis->vars (xf-analysis->buckets-elems :var-definitions :var-usages))

(defn ns-aliases [{:keys [dep-graph] :as db}]
  (if (use-dep-graph? db)
    (into #{}
          (comp
            dep-graph/some-dependents-internal-xf
            as-alias-elems-xf)
          dep-graph)
    (into #{}
          (comp
            xf-analysis->namespace-alias
            deprecated-as-alias-elems-xf)
          (deprecated-internal-analysis db))))

(defn ns-aliases-for-langs [{:keys [dep-graph] :as db} langs]
  (if (use-dep-graph? db)
    (into #{}
          (comp
            dep-graph/some-dependents-internal-xf
            (filter (fn [[_namespace {:keys [dependents-langs]}]]
                      (dep-graph/ms-overlaps-set? dependents-langs langs)))
            as-alias-elems-xf)
          dep-graph)
    (into #{}
          (comp
            xf-analysis->namespace-alias
            (filter :alias)
            (filter (fn [element]
                      (some langs (elem-langs element))))
            deprecated-as-alias-elems-xf)
          (deprecated-internal-analysis db))))

(defn ns-names-for-langs [{:keys [analysis documents] :as db} langs]
  (if (use-dep-graph? db)
    (into #{}
          (mapcat (fn [doc]
                    (when (some langs (:langs doc))
                      (:namespaces doc))))
          (vals documents))
    (into #{}
          (comp
            xf-analysis->namespace-definitions
            (filter (fn [element]
                      (some langs (elem-langs element))))
            (map :name))
          analysis)))

(defn ns-names-for-uri [{:keys [documents] :as db} uri filename]
  (if (use-dep-graph? db)
    (vec (get-in documents [uri :namespaces]))
    (mapv :name (get-in db [:analysis filename :namespace-definitions]))))

(defn ns-names [{:keys [analysis dep-graph] :as db}]
  (if (use-dep-graph? db)
    (set (keys dep-graph))
    (into #{}
          (comp
            xf-analysis->namespace-definitions
            (map :name))
          analysis)))

(defn internal-ns-names [{:keys [documents] :as db}]
  (if (use-dep-graph? db)
    (into #{}
          (comp
            dep-graph/internal-xf
            (mapcat (comp :namespaces val)))
          documents)
    (into #{}
          (comp
            xf-analysis->namespace-definitions
            (map :name))
          (deprecated-internal-analysis db))))

(defn nses-some-internal-uri [db namespaces]
  (if (use-dep-graph? db)
    ;; TODO: this is a very specific return value, but has to be this way to
    ;; match the non-dep-graph version. When use-dep-graph? is removed, it'd be
    ;; better to refactor internal-api/nses->ns+uri to use
    ;; dep-graph/ns-internal-uris directly, perhaps changing it to return all
    ;; uris for a given namespace.
    (into {}
          (keep (fn [namespace]
                  (when-first [uri (dep-graph/ns-internal-uris db namespace)]
                    [namespace uri])))
          namespaces)
    ;; Performance sensitive: Gather uris in one pass, instead of (count
    ;; namespaces) passes.
    (medley/map-vals
      #(shared/filename->uri % db)
      (into {}
            (comp
              xf-analysis->namespace-definitions
              (filter #(contains? (set namespaces) (:name %)))
              (map (juxt :name :filename)))
            (deprecated-internal-analysis db)))))

;;;; Filter elements in analysis

(defn ^:private safe-equal?
  "Fast equals for string and symbols."
  [a b]
  (if (instance? clojure.lang.Symbol a)
    (.equals ^clojure.lang.Symbol a b)
    (.equals ^String a b)))

;; Borrowed from https://github.com/cgrand/xforms
;; Copyright © 2015-2016 Christophe Grand
;; Distributed under the Eclipse Public License version 1.0

(defn ^:private rf-last
  "Reducing function that returns the last value."
  ([] nil)
  ([x] x)
  ([_ x] x))

(defn ^:private rf-some
  "Reducing function that returns the first logical true value."
  ([] nil)
  ([x] x)
  ([_ x] (when x (reduced x))))

;; End cgrand/xforms

(defn ^:private find-first [xf coll] (transduce xf rf-some coll))
(defn ^:private find-last  [xf coll] (transduce xf rf-last coll))

(defn ^:private find-last-order-by-project-analysis [xf db]
  (or (find-last xf (internal-analysis db))
      (find-last xf (external-analysis db))))

(defn var-definition-names [{:keys [defined-by name]}]
  (case defined-by
    (clojure.core/defrecord
     cljs.core/defrecord)
    , #{name (symbol (str "->" name)) (symbol (str "map->" name))}
    (clojure.core/deftype
     cljs.core/deftype)
    , #{name (symbol (str "->" name))}
    , #{name}))

(def kw-signature (juxt :ns :name))
(def var-usage-signature (juxt :to :name))
(defn var-definition-signatures [var-def]
  (into #{}
        (map (fn [var-name]
               [(:ns var-def) var-name]))
        (var-definition-names var-def)))

(defn xf-same-ns
  ([ns] (xf-same-ns ns :ns))
  ([ns get-ns]
   (cond
     (identical? :clj-kondo/unknown-namespace ns)
     , (filter #(identical? :clj-kondo/unknown-namespace (get-ns %)))
     ns
     , (filter #(safe-equal? ns (get-ns %)))
     :else
     , (remove get-ns))))

(defn xf-same-name
  ([name]
   (xf-same-name name :name))
  ([name get-name]
   (filter #(safe-equal? name (get-name %)))))

(defn xf-same-fqn
  ([ns name]
   (comp (xf-same-name name)
         (xf-same-ns ns)))
  ([ns name get-ns]
   (comp (xf-same-name name)
         (xf-same-ns ns get-ns)))
  ([ns name get-ns get-name]
   (comp (xf-same-name name get-name)
         (xf-same-ns ns get-ns))))

(defn xf-same-lang
  [element]
  (let [desired-langs (elem-langs element)]
    (filter (fn [candidate]
              (some (elem-langs candidate) desired-langs)))))

(defn ^:private var-usage-from-own-definition? [usage]
  (and (:from-var usage)
       (= (:from-var usage) (:name usage))
       (= (:from usage) (:to usage))))

(defn find-local-usages-under-form
  [db filename {:keys [row col end-row end-col]}]
  (let [{:keys [locals local-usages]} (get-in db [:analysis filename])
        form-scope {:name-row row
                    :name-col col
                    :name-end-row end-row
                    :name-end-col end-col}
        ;; find locals definitions whose scope includes the form
        local-def-ids (into #{}
                            (comp
                              (filter (fn [local-def]
                                        (shared/inside? form-scope local-def)))
                              (map :id))
                            locals)]
    ;; find locals usages inside the form, whose def is outside the form
    (->> local-usages
         (filter (fn [local-usage]
                   (and (contains? local-def-ids (:id local-usage))
                        (shared/inside? local-usage form-scope))))
         (medley/distinct-by :id))))

(defn find-var-usages-under-form
  [db filename line column end-line end-column]
  (let [local-usages (get-in db [:analysis filename :var-usages])]
    (filter (fn [element]
              (shared/inside? element
                              {:name-row line
                               :name-col column
                               :name-end-row end-line
                               :name-end-col end-column}))
            local-usages)))

(defmulti find-definition
  (fn [_db element]
    (:bucket element)))

(defmethod find-definition :namespace-alias
  [db {:keys [to] :as namespace-alias}]
  (find-definition db (assoc namespace-alias
                             :bucket :namespace-usages
                             :name to)))

(defmethod find-definition :namespace-usages
  [db namespace-usage]
  (find-last-order-by-project-analysis
    (comp xf-analysis->namespace-definitions
          (xf-same-name (:name namespace-usage))
          (xf-same-lang namespace-usage))
    (db-with-ns-analysis db (:name namespace-usage))))

(defmethod find-definition :var-usages
  [db var-usage]
  (or
    (find-last-order-by-project-analysis
      (comp xf-analysis->var-definitions
            (xf-same-fqn (:to var-usage) (:name var-usage))
            (xf-same-lang var-usage))
      (db-with-ns-analysis db (:to var-usage)))
    (when (contains? (elem-langs var-usage) :cljs)
      ;; maybe loaded by :require-macros, in which case, def will be in a clj file.
      (let [definition (find-definition db (assoc var-usage :lang :clj))]
        (when (:macro definition)
          definition)))))

(defmethod find-definition :local-usages
  [db {:keys [id filename] :as _local-usage}]
  (find-first (filter #(= (:id %) id))
              (get-in db [:analysis filename :locals])))

(defmethod find-definition :keyword-usages
  [db keyword-usage]
  (or (find-last-order-by-project-analysis
        (comp xf-analysis->keyword-definitions
              (xf-same-fqn (:ns keyword-usage) (:name keyword-usage)))
        db)
      keyword-usage))

(defmethod find-definition :var-definitions
  [db {:keys [defined-by imported-ns] :as var-definition}]
  (if (safe-equal? 'potemkin/import-vars defined-by)
    (find-definition db (assoc var-definition
                               :bucket :var-usages
                               :to imported-ns))
    var-definition))

(defmethod find-definition :protocol-impls
  [db protocol-impl]
  (find-last-order-by-project-analysis
    (comp xf-analysis->var-definitions
          (xf-same-fqn (:protocol-ns protocol-impl) (:method-name protocol-impl))
          (xf-same-lang protocol-impl))
    (db-with-ns-analysis db (:protocol-ns protocol-impl))))

(defmethod find-definition :java-class-usages
  [db java-class-usage]
  (->> (:analysis db)
       (into []
             (comp
               xf-analysis->java-class-definitions
               (filter #(safe-equal? (:class %) (:class java-class-usage)))))
       (sort-by (complement #(string/ends-with? (:filename %) ".java")))
       first))

(defmethod find-definition :default
  [_db element]
  element)

(defmulti find-declaration
  (fn [_db element]
    (:bucket element)))

(defmethod find-declaration :var-usages
  [db {:keys [alias name to filename] :as var-usage}]
  (when-not (identical? :clj-kondo/unknown-namespace to)
    (let [buckets (get-in db [:analysis filename])
          find-last (fn [xf elems]
                      (find-last
                        (comp xf
                              (xf-same-lang var-usage))
                        elems))]
      (if alias
        (find-last (filter #(and (= to (:to %))
                                 (= alias (:alias %))))
                   (:namespace-alias buckets))
        (or (find-last (comp
                         (xf-same-fqn to name :to)
                         (filter :refer))
                       (:var-usages buckets))
            ;; :refer :all
            (find-last (xf-same-name to)
                       (:namespace-usages buckets)))))))

(defmethod find-declaration :default [_ _] nil)

(defmulti find-implementations
  (fn [_db element]
    (:bucket element)))

(defmethod find-implementations :var-definitions
  [db var-definition]
  (if-let [xf (cond
                ;; protocol method definition
                (and (= 'clojure.core/defprotocol (:defined-by var-definition))
                     (:protocol-name var-definition))
                (comp xf-analysis->protocol-impls
                      (xf-same-fqn (:ns var-definition) (:name var-definition)
                                   :protocol-ns :method-name))

                ;; protocol name definition
                (= 'clojure.core/defprotocol (:defined-by var-definition))
                (comp xf-analysis->var-usages
                      (xf-same-fqn (:ns var-definition) (:name var-definition) :to))

                ;; defmulti definition
                (= 'clojure.core/defmulti (:defined-by var-definition))
                (comp xf-analysis->var-usages
                      (xf-same-fqn (:ns var-definition) (:name var-definition) :to)
                      (filter :defmethod))

                :else
                nil)]
    (into []
          (comp
            xf
            (xf-same-lang var-definition)
            (medley/distinct-by (juxt :filename :name :row :col)))
          (ns-and-dependents-analysis db (:ns var-definition)))
    []))

(defmethod find-implementations :var-usages
  [db var-usage]
  (if (= (:to var-usage) :clj-kondo/unknown-namespace)
    []
    (let [xf-defmethod (comp (xf-same-fqn (:to var-usage) (:name var-usage) :to)
                             (filter :defmethod))
          xf (if (:defmethod var-usage)
               ;; defmethod declaration
               (comp xf-analysis->var-usages xf-defmethod)
               ;; protocol method usage or defmethod usage
               (comp xf-analysis->by-bucket
                     (mapcat (fn [{:keys [protocol-impls var-usages]}]
                               (concat (into []
                                             (xf-same-fqn (:to var-usage) (:name var-usage)
                                                          :protocol-ns :method-name)
                                             protocol-impls)
                                       (into [] xf-defmethod var-usages))))))]
      (into []
            (comp
              xf
              (xf-same-lang var-usage)
              (medley/distinct-by (juxt :filename :name :row :col)))
            (ns-and-dependents-analysis db (:to var-usage))))))

(defmethod find-implementations :default [_ _] [])

(defmulti find-references
  (fn [_db element _include-declaration?]
    (case (:bucket element)
      (:locals :local-usages) :local
      (:keyword-definitions :keyword-usages) :keywords
      (:bucket element))))

(defmethod find-references :namespace-definitions
  [db {:keys [name] :as namespace-definition} include-declaration?]
  (concat
    (when include-declaration?
      [namespace-definition])
    (vec
      (concat
        (into []
              (comp
                xf-analysis->namespace-usages
                (xf-same-name name))
              (ns-and-dependents-analysis db name))
        ;; TODO: do we always need these keywords? If not, probably better to
        ;; split this into a fast version that uses ns-and-dependents-analysis,
        ;; and a slow version that adds these keywords.
        (into []
              (comp
                xf-analysis->keywords
                (xf-same-ns name)
                (remove #(or (:auto-resolved %)
                             (:namespace-from-prefix %))))
              (:analysis db))))))

(defmethod find-references :namespace-usages
  [db namespace-usage include-declaration?]
  (let [namespace-definition (assoc namespace-usage :bucket :namespace-definitions)]
    (find-references db namespace-definition include-declaration?)))

(defmethod find-references :namespace-alias
  [db {:keys [alias filename] :as namespace-alias} include-declaration?]
  (concat
    (when include-declaration?
      [namespace-alias])
    (let [{:keys [var-usages keyword-usages keyword-definitions]} (get-in db [:analysis filename])]
      (into []
            (comp
              (filter #(= (:alias %) alias))
              (medley/distinct-by (juxt :filename :name :name-row :name-col)))
            (concat keyword-definitions keyword-usages var-usages)))))

(defmethod find-references :var-usages
  [db var-usage include-declaration?]
  (if (= (:to var-usage) :clj-kondo/unknown-namespace)
    [var-usage]
    (let [var-definition {:ns (:to var-usage)
                          :name (:name var-usage)
                          :bucket :var-definitions}]
      (find-references db var-definition include-declaration?))))

(defmethod find-references :var-definitions
  [db var-definition include-declaration?]
  (let [names (var-definition-names var-definition)]
    (into []
          (comp
            (if include-declaration? xf-analysis->vars xf-analysis->var-usages)
            (filter #(contains? names (:name %)))
            (filter #(safe-equal? (:ns var-definition) (or (:ns %) (:to %))))
            (filter #(or include-declaration?
                         (not (var-usage-from-own-definition? %))))
            (medley/distinct-by (juxt :filename :name :row :col)))
          (ns-and-dependents-analysis db (:ns var-definition)))))

(defmethod find-references :keywords
  [db {:keys [ns name] :as _keyword} include-declaration?]
  (into []
        (comp
          (if include-declaration? xf-analysis->keywords xf-analysis->keyword-usages)
          (xf-same-fqn ns name)
          (medley/distinct-by (juxt :filename :name :row :col)))
        (internal-analysis db)))

(defmethod find-references :local
  [db {:keys [id name filename] :as element} include-declaration?]
  (if (or id name)
    (let [{:keys [locals local-usages]} (get-in db [:analysis filename])]
      (filter #(= (:id %) id)
              (concat (when include-declaration? locals)
                      local-usages)))
    [element]))

(defmethod find-references :protocol-impls
  [db {:keys [method-name protocol-ns] :as element} include-declaration?]
  (concat
    (when include-declaration?
      [element])
    (into []
          (comp
            xf-analysis->var-usages
            (xf-same-fqn protocol-ns method-name :to)
            (medley/distinct-by (juxt :filename :name :row :col)))
          (ns-and-dependents-analysis db protocol-ns))))

(defmethod find-references :default
  [_db element _]
  [element])

(defn ^:private xf-under-cursor [line column]
  (comp (mapcat val)
        (filter
          (fn [{:keys [name-row name-col name-end-row name-end-col]}]
            ;; TODO Probably should use q/inside? instead
            (and (<= name-row line name-end-row)
                 (<= name-col column name-end-col))))))

(defn find-element-under-cursor
  [db filename line column]
  (find-first (xf-under-cursor line column)
              (get-in db [:analysis filename])))

(defn find-all-elements-under-cursor
  [db filename line column]
  (into []
        (xf-under-cursor line column)
        (get-in db [:analysis filename])))

(defn find-definition-from-cursor [db filename line column]
  (try
    (when-let [element (find-element-under-cursor db filename line column)]
      (find-definition db element))
    (catch Throwable e
      (logger/error e "can't find definition"))))

(defn find-declaration-from-cursor [db filename line column]
  (try
    (when-let [element (find-element-under-cursor db filename line column)]
      (find-declaration db element))
    (catch Throwable e
      (logger/error e "can't find declaration"))))

(defn find-implementations-from-cursor [db filename line column]
  (try
    (when-let [element (find-element-under-cursor db filename line column)]
      (find-implementations db element))
    (catch Throwable e
      (logger/error e "can't find implementation"))))

(defn find-references-from-cursor [db filename line column include-declaration?]
  (try
    (when-let [element (find-element-under-cursor db filename line column)]
      (find-references db element include-declaration?))
    (catch Throwable e
      (logger/error e "can't find references"))))

(defn ^:private xf-var-defs [include-private?]
  (comp
    (filter #(or include-private?
                 (not (get % :private))))
    (medley/distinct-by (juxt :ns :name :row :col))
    (remove #(or (and (#{'clojure.core/defrecord
                         'cljs.core/defrecord} (:defined-by %))
                      (or (string/starts-with? (str (:name %)) "->")
                          (string/starts-with? (str (:name %)) "map->")))
                 (and (#{'clojure.core/deftype
                         'cljs.core/deftype} (:defined-by %))
                      (string/starts-with? (str (:name %)) "->"))))))

(defn find-var-definitions [db filename include-private?]
  (into []
        (xf-var-defs include-private?)
        (get-in db [:analysis filename :var-definitions])))

(defn find-all-var-definitions [db]
  (into []
        (comp
          xf-analysis->var-definitions
          (xf-var-defs false))
        (:analysis db)))

(def ^:private xf-defmethods
  (comp (filter :defmethod)
        (medley/distinct-by (juxt :to :name :name-row :name-col))))

(defn find-defmethods [db filename]
  (into []
        xf-defmethods
        (get-in db [:analysis filename :var-usages])))

(defn find-internal-definitions
  "All ns definitions, var definitions and defmethods."
  [db]
  (let [analysis (internal-analysis db)]
    (concat (into []
                  (xf-analysis->buckets-elems :namespace-definitions :var-definitions)
                  analysis)
            (into []
                  (comp xf-analysis->var-usages
                        xf-defmethods)
                  analysis))))

(defn find-keyword-definitions [db filename]
  (into []
        (medley/distinct-by (juxt :ns :name :row :col))
        (get-in db [:analysis filename :keyword-definitions])))

(defn find-all-keyword-definitions [db]
  (into []
        (comp
          xf-analysis->keyword-definitions
          (medley/distinct-by (juxt :ns :name :row :col)))
        (:analysis db)))

(defn find-local-by-destructured-keyword [db filename keyword-element]
  (find-first (filter #(and (= (:name-row %) (:name-row keyword-element))
                            (= (:name-col %) (:name-col keyword-element))
                            (= (:name-end-row %) (:name-end-row keyword-element))
                            (= (:name-end-col %) (:name-end-col keyword-element))))
              (get-in db [:analysis filename :locals])))

(defn find-unused-aliases [db filename]
  (let [local-var-usages (get-in db [:analysis filename :var-usages])]
    (into #{}
          (comp
            (filter (comp #(identical? :unused-namespace %) :type))
            (remove (fn [finding]
                      (some #(and (not (:refer %))
                                  (safe-equal? (:ns finding) (:to %)))
                            local-var-usages)))
            (map :ns))
          (get-in db [:findings filename]))))

(defn find-unused-refers [db filename]
  (let [local-var-usages (get-in db [:analysis filename :var-usages])]
    (into #{}
          (comp
            (filter (comp #(identical? :unused-referred-var %) :type))
            (remove (fn [finding]
                      (> (->> local-var-usages
                              (filter #(and (safe-equal? (:refer finding) (:name %))
                                            (safe-equal? (:ns finding) (:to %))))
                              (medley/distinct-by (juxt :name :to :row :col :end-row :end-col))
                              count)
                         1)))
            (map #(symbol (-> % :ns str) (-> % :refer str))))
          (get-in db [:findings filename]))))

(defn find-unused-imports [db filename]
  (let [{:keys [var-usages java-class-usages]} (get-in db [:analysis filename])]
    (into #{}
          (comp
            (filter (comp #(identical? :unused-import %) :type))
            (remove (fn [finding]
                      (or
                        (some #(safe-equal? (str (:class finding))
                                            (str (:to %) "." (:name %)))
                              var-usages)
                        (some #(and (safe-equal? (str (:class finding))
                                                 (:class %))
                                    (not (:import %)))
                              java-class-usages))))
            (map :class))
          (get-in db [:findings filename]))))

(defn find-duplicate-requires [db filename]
  (into #{}
        (comp
          (filter (comp #(identical? :duplicate-require %) :type))
          (map :duplicate-ns))
        (get-in db [:findings filename])))

(defn find-namespace-definitions [db filename]
  (vec (get-in db [:analysis filename :namespace-definitions])))

(defn find-namespace-definition-by-filename [db filename]
  (first (find-namespace-definitions db filename)))

(defn find-namespace-usage-by-alias [db filename alias]
  (find-last (filter #(= alias (:alias %)))
             (get-in db [:analysis filename :namespace-usages])))

(defn find-element-for-rename [db from-ns from-name]
  (let [xf
        (if from-name
          (comp
            xf-analysis->var-definitions
            (xf-same-fqn from-ns from-name))
          (comp
            xf-analysis->namespace-definitions
            (xf-same-name from-ns)))]
    (find-last xf (internal-analysis (db-with-ns-analysis db from-ns)))))

(def default-public-vars-defined-by-to-exclude
  '#{clojure.test/deftest
     cljs.test/deftest
     state-flow.cljtest/defflow
     potemkin/import-vars})

(def default-public-vars-name-to-exclude
  '#{-main})

(defn exclude-public-definition? [kondo-config definition]
  (let [excluded-syms (get-in kondo-config [:linters :clojure-lsp/unused-public-var :exclude] #{})
        excluded-defined-by-syms (get-in kondo-config [:linters :clojure-lsp/unused-public-var :exclude-when-defined-by] #{})
        excluded-full-qualified-vars (set (filter qualified-ident? excluded-syms))
        excluded-ns-or-var (set (filter simple-ident? excluded-syms))
        keyword-definition? (identical? :keyword-definitions (:bucket definition))
        fqsn (symbol (-> definition :ns str) (-> definition :name str))]
    (or (contains? (set/union default-public-vars-defined-by-to-exclude excluded-defined-by-syms)
                   (if keyword-definition?
                     (:reg definition)
                     (:defined-by definition)))
        (contains? (set/union excluded-ns-or-var default-public-vars-name-to-exclude)
                   (if keyword-definition?
                     ;; FIXME: this creates a qualified symbol, but the set is
                     ;; all unqualified symbols, and so this check will always
                     ;; be false for keywords. What should it be? Needs a test.
                     (symbol (str (:ns definition)) (:name definition))
                     (:name definition)))
        (contains? (set excluded-ns-or-var) (:ns definition))
        (-> excluded-full-qualified-vars
            set
            (contains? fqsn)))))

(defn xf-all-var-usages-to-namespaces [namespaces]
  (comp
    xf-analysis->var-usages
    (filter #(contains? namespaces (:to %)))
    (remove var-usage-from-own-definition?)))

(def xf-all-keyword-usages xf-analysis->keyword-usages)
