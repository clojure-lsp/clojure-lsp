(ns clojure-lsp.queries
  (:refer-clojure :exclude [ns-aliases])
  (:require
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [clojure.string :as string]
   [lsp4clj.protocols.logger :as logger]
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

(defn ns-aliases [{:keys [dep-graph] :as db}]
  (if (use-dep-graph? db)
    (into #{}
          (comp
            dep-graph/some-dependents-internal-xf
            as-alias-elems-xf)
          dep-graph)
    (into #{}
          (comp
            (mapcat (comp :namespace-alias val))
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
            (mapcat (comp :namespace-alias val))
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
            (mapcat (comp :namespace-definitions val))
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
            (mapcat (comp :namespace-definitions val))
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
            (mapcat (comp :namespace-definitions val))
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
              (mapcat (comp :namespace-definitions val))
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

(defn rf-last
  "Reducing function that returns the last value."
  ([] nil)
  ([x] x)
  ([_ x] x))

(defn rf-some
  "Reducing function that returns the first logical true value."
  ([] nil)
  ([x] x)
  ([_ x] (when x (reduced x))))

;; End cgrand/xforms

(defn ^:private find-last-order-by-project-analysis [bucket pred? db]
  (or (transduce (comp
                   (mapcat (comp bucket val))
                   (filter pred?))
                 rf-last
                 (internal-analysis db))
      (transduce (comp
                   (mapcat (comp bucket val))
                   (filter pred?))
                 rf-last
                 (external-analysis db))))

(defn ^:private match-file-lang
  [check-element match-element]
  (some (elem-langs match-element) (elem-langs check-element)))

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
  (->> (var-definition-names var-def)
       (map (fn [var-name]
              [(:ns var-def) var-name]))
       (into #{})))

(defn ns-equal?-fn [ns]
  (cond
    (identical? :clj-kondo/unknown-namespace ns) #(identical? :clj-kondo/unknown-namespace %)
    ns #(safe-equal? ns %)
    :else #(not %)))

(defn keyword-signature-equal?-fn [ns name]
  (let [ns-equal? (ns-equal?-fn ns)]
    #(and (safe-equal? name (:name %))
          (ns-equal? (:ns %)))))

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
  [db {:keys [to] :as element}]
  (find-last-order-by-project-analysis
    :namespace-definitions
    #(and (= (:name %) to)
          (match-file-lang % element))
    (db-with-ns-analysis db to)))

(defmethod find-definition :namespace-usages
  [db element]
  (find-last-order-by-project-analysis
    :namespace-definitions
    #(and (= (:name %) (:name element))
          (match-file-lang % element))
    (db-with-ns-analysis db (:name element))))

(defmethod find-definition :var-usages
  [db element]
  (letfn [(defines-elem? [candidate]
            (and (= (:name element) (:name candidate))
                 (= (:to element) (:ns candidate))
                 (not= 'clojure.core/declare (:defined-by candidate))))]
    (or
      (find-last-order-by-project-analysis
        :var-definitions
        #(and (defines-elem? %)
              (match-file-lang % element))
        (db-with-ns-analysis db (:to element)))
      (when (contains? (elem-langs element) :cljs)
        ;; maybe loaded by :require-macros, in which case, def will be in a clj file.
        (find-last-order-by-project-analysis
          :var-definitions
          #(and (defines-elem? %)
                (:macro %)
                (match-file-lang % (assoc element :lang :clj)))
          (db-with-ns-analysis db (:to element)))))))

(defmethod find-definition :local-usages
  [db {:keys [id filename] :as _element}]
  (transduce (filter #(= (:id %) id))
             rf-some
             (get-in db [:analysis filename :locals])))

(defmethod find-definition :keyword-usages
  [db element]
  (or (find-last-order-by-project-analysis
        :keyword-definitions
        (keyword-signature-equal?-fn (:ns element) (:name element))
        db)
      element))

(defmethod find-definition :var-definitions
  [db element]
  (if (= 'potemkin/import-vars (:defined-by element))
    ;; FIXME: this is buggy... it goes to **any** definition with the same name,
    ;; not specifically the one from the imported ns. See
    ;; https://github.com/clojure-lsp/clojure-lsp/issues/1020
    ;; To fix, use :imported-ns and :imported-var, and treat this like a
    ;; var-usage Don't forget to switch from db to (db-with-ns-analysis db
    ;; (:imported-ns element))
    (find-last-order-by-project-analysis
      :var-definitions
      #(and (= (:name %) (:name element))
            (not= 'potemkin/import-vars (:defined-by %))
            (match-file-lang % element))
      db)
    element))

(defmethod find-definition :protocol-impls
  [db element]
  (find-last-order-by-project-analysis
    :var-definitions
    #(and (= (:name %) (:method-name element))
          (= (:ns %) (:protocol-ns element))
          (match-file-lang % element))
    (db-with-ns-analysis db (:protocol-ns element))))

(defmethod find-definition :java-class-usages
  [db element]
  (->> (:analysis db)
       (into []
             (comp
               (mapcat (comp :java-class-definitions val))
               (filter #(safe-equal? (:class %) (:class element)))))
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
                      (transduce
                        (comp xf
                              (filter #(match-file-lang % var-usage)))
                        rf-last
                        elems))]
      (if alias
        (find-last (filter #(and (= to (:to %))
                                 (= alias (:alias %))))
                   (:namespace-alias buckets))
        (or (find-last (filter #(and (:refer %)
                                     (= to (:to %))
                                     (= name (:name %))))
                       (:var-usages buckets))
            ;; :refer :all
            (find-last (filter #(= to (:name %)))
                       (:namespace-usages buckets)))))))

(defmethod find-declaration :default [_ _] nil)

(defmulti find-implementations
  (fn [_db element]
    (:bucket element)))

(defmethod find-implementations :var-definitions
  [db element]
  (if-let [[bucket xf] (cond
                         ;; protocol method definition
                         (and (= 'clojure.core/defprotocol (:defined-by element))
                              (:protocol-name element))
                         [:protocol-impls
                          (filter #(and (safe-equal? (:ns element) (:protocol-ns %))
                                        (safe-equal? (:name element) (:method-name %))))]

                         ;; protocol name definition
                         (= 'clojure.core/defprotocol (:defined-by element))
                         [:var-usages
                          (filter #(and (safe-equal? (:ns element) (:to %))
                                        (safe-equal? (:name element) (:name %))))]

                         ;; defmulti definition
                         (= 'clojure.core/defmulti (:defined-by element))
                         [:var-usages
                          (filter #(and (:defmethod %)
                                        (safe-equal? (:ns element) (:to %))
                                        (safe-equal? (:name element) (:name %))))]

                         :else
                         nil)]
    (into []
          (comp
            (mapcat (comp bucket val))
            xf
            (filter #(match-file-lang % element))
            (medley/distinct-by (juxt :filename :name :row :col)))
          (ns-and-dependents-analysis db (:ns element)))
    []))

(defmethod find-implementations :var-usages
  [db element]
  (if (= (:to element) :clj-kondo/unknown-namespace)
    []
    (let [xf (if (:defmethod element)
               ;; defmethod declaration
               (comp (mapcat (comp :var-usages val))
                     (filter #(and (:defmethod %)
                                   (safe-equal? (:to element) (:to %))
                                   (safe-equal? (:name element) (:name %)))))
               ;; protocol method usage or defmethod usage
               (comp (map val)
                     (mapcat (fn [{:keys [protocol-impls var-usages]}]
                               (concat (filter #(and (safe-equal? (:to element) (:protocol-ns %))
                                                     (safe-equal? (:name element) (:method-name %)))
                                               protocol-impls)
                                       (filter #(and (:defmethod %)
                                                     (safe-equal? (:to element) (:to %))
                                                     (safe-equal? (:name element) (:name %)))
                                               var-usages))))))]
      (into []
            (comp
              xf
              (filter #(match-file-lang % element))
              (medley/distinct-by (juxt :filename :name :row :col)))
            (ns-and-dependents-analysis db (:to element))))))

(defmethod find-implementations :default [_ _] [])

(defmulti find-references
  (fn [_db element _include-declaration?]
    (case (:bucket element)
      (:locals :local-usages) :local
      (:keyword-definitions :keyword-usages) :keywords
      (:bucket element))))

(defmethod find-references :namespace-definitions
  [db {:keys [name] :as element} include-declaration?]
  (concat
    (when include-declaration?
      [element])
    (vec
      (concat
        (into []
              (comp
                (mapcat (comp :namespace-usages val))
                (filter #(= (:name %) name)))
              (ns-and-dependents-analysis db name))
        ;; TODO: do we always need these keywords? If not, probably better to
        ;; split this into a fast version that uses ns-and-dependents-analysis,
        ;; and a slow version that adds these keywords.
        (into []
              (comp
                (map val)
                (mapcat (fn [{:keys [keyword-usages keyword-definitions]}]
                          (concat keyword-definitions keyword-usages)))
                (filter
                  #(and (= (:ns %) name)
                        (not (:auto-resolved %))
                        (not (:namespace-from-prefix %)))))
              (:analysis db))))))

(defmethod find-references :namespace-usages
  [db element include-declaration?]
  (let [namespace-definition (assoc element :bucket :namespace-definitions)]
    (find-references db namespace-definition include-declaration?)))

(defmethod find-references :namespace-alias
  [db {:keys [alias filename] :as element} include-declaration?]
  (concat
    (when include-declaration?
      [element])
    (let [{:keys [var-usages keyword-usages keyword-definitions]} (get-in db [:analysis filename])]
      (into []
            (comp
              (filter #(= (:alias %) alias))
              (medley/distinct-by (juxt :filename :name :name-row :name-col)))
            (concat keyword-definitions keyword-usages var-usages)))))

(defmethod find-references :var-usages
  [db element include-declaration?]
  (if (= (:to element) :clj-kondo/unknown-namespace)
    [element]
    (let [var-definition {:ns (:to element)
                          :name (:name element)
                          :bucket :var-definitions}]
      (find-references db var-definition include-declaration?))))

(defmethod find-references :var-definitions
  [db element include-declaration?]
  (let [names (var-definition-names element)]
    (into []
          (comp
            (map val)
            (mapcat (fn [{:keys [var-definitions var-usages]}]
                      (cond->> (vec var-usages)
                        include-declaration? (into (vec var-definitions)))))
            (filter #(contains? names (:name %)))
            (filter #(safe-equal? (:ns element) (or (:ns %) (:to %))))
            (filter #(or include-declaration?
                         (not (var-usage-from-own-definition? %))))
            (medley/distinct-by (juxt :filename :name :row :col)))
          (ns-and-dependents-analysis db (:ns element)))))

(defmethod find-references :keywords
  [db {:keys [ns name] :as _element} include-declaration?]
  (into []
        (comp
          (map val)
          (mapcat (fn [{:keys [keyword-usages keyword-definitions]}]
                    (cond->> (vec keyword-usages)
                      include-declaration? (into (vec keyword-definitions)))))
          (filter (keyword-signature-equal?-fn ns name))
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
            (mapcat (comp :var-usages val))
            (filter #(safe-equal? method-name (:name %)))
            (filter #(safe-equal? protocol-ns (:to %)))
            (medley/distinct-by (juxt :filename :name :row :col)))
          (ns-and-dependents-analysis db protocol-ns))))

(defmethod find-references :default
  [_db element _]
  [element])

(defn find-element-under-cursor
  [db filename line column]
  (transduce (comp (mapcat val)
                   (filter
                     (fn [{:keys [name-row name-col name-end-row name-end-col]}]
                       ;; TODO Probably should use q/inside? instead
                       (and (<= name-row line name-end-row)
                            (<= name-col column name-end-col)))))
             rf-some
             (get-in db [:analysis filename])))

(defn find-all-elements-under-cursor
  [db filename line column]
  (filter (fn [{:keys [name-row name-col name-end-row name-end-col]}]
            ;; TODO Probably should use q/inside? instead
            (and (<= name-row line name-end-row)
                 (<= name-col column name-end-col)))
          (mapcat val (get-in db [:analysis filename]))))

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
          (mapcat (comp :var-definitions val))
          (xf-var-defs false))
        (:analysis db)))

(defn find-keyword-definitions [db filename]
  (into []
        (medley/distinct-by (juxt :ns :name :row :col))
        (get-in db [:analysis filename :keyword-definitions])))

(defn find-all-keyword-definitions [db]
  (into []
        (comp
          (mapcat (comp :keyword-definitions val))
          (medley/distinct-by (juxt :ns :name :row :col)))
        (:analysis db)))

(defn find-local-by-destructured-keyword [db filename keyword-element]
  (transduce (filter #(and (= (:name-row %) (:name-row keyword-element))
                           (= (:name-col %) (:name-col keyword-element))
                           (= (:name-end-row %) (:name-end-row keyword-element))
                           (= (:name-end-col %) (:name-end-col keyword-element))))
             rf-some
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
  (->> (get-in db [:analysis filename :namespace-usages])
       (filter #(= alias (:alias %)))
       last))

(defn find-element-for-rename [db from-ns from-name]
  (let [xf
        (if from-name
          (comp
            (mapcat (comp :var-definitions val))
            (filter
              #(and (= from-ns (:ns %))
                    (= from-name (:name %)))))
          (comp
            (mapcat (comp :namespace-definitions val))
            (filter #(= from-ns (:name %)))))]
    (transduce xf rf-last (internal-analysis (db-with-ns-analysis db from-ns)))))

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
    (mapcat (comp :var-usages val))
    (filter #(contains? namespaces (:to %)))
    (remove var-usage-from-own-definition?)))

(def xf-all-keyword-usages
  (mapcat (comp :keyword-usages val)))
