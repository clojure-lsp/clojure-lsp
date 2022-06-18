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
  (settings/get db [:experimental :dep-graph-queries] false))

(def ^:private deprecated-filter-project-analysis-xf ;; works for analysis only
  (remove (comp :external? first val)))

(defn internal-analysis [{:keys [analysis] :as db}]
  (if (use-dep-graph? db)
    (select-keys analysis (dep-graph/internal-files db))
    (medley/remove-vals
      (fn [elems]
        (:external? (first elems)))
      analysis)))

(defn external-analysis [{:keys [analysis] :as db}]
  (if (use-dep-graph? db)
    (select-keys analysis (dep-graph/external-files db))
    (medley/filter-vals
      (fn [elems]
        (:external? (first elems)))
      analysis)))

(defn ns-analysis [{:keys [analysis] :as db} namespace]
  (if (use-dep-graph? db)
    (select-keys analysis (dep-graph/ns-files db namespace))
    analysis))

(defn ns-dependents-analysis [{:keys [analysis] :as db} namespace]
  (if (use-dep-graph? db)
    (select-keys analysis (dep-graph/ns-dependents-files db namespace))
    analysis))

(defn ns-and-dependents-analysis [{:keys [analysis] :as db} namespace]
  (if (use-dep-graph? db)
    (select-keys analysis (dep-graph/ns-and-dependents-files db namespace))
    analysis))

(defn ns-dependencies-analysis [{:keys [analysis] :as db} namespace]
  (if (use-dep-graph? db)
    (select-keys analysis (dep-graph/ns-dependencies-files db namespace))
    analysis))

(defn nses-analysis [{:keys [analysis] :as db} namespaces]
  (if (use-dep-graph? db)
    (select-keys analysis (dep-graph/nses-files db namespaces))
    analysis))

(defn nses-and-dependents-analysis [{:keys [analysis] :as db} namespaces]
  (if (use-dep-graph? db)
    (select-keys analysis (dep-graph/nses-and-dependents-files db namespaces))
    analysis))

(defn file-dependents-analysis [{:keys [analysis file-meta] :as db} filename]
  (if (use-dep-graph? db)
    (transduce (map #(ns-dependents-analysis db %))
               merge
               {}
               (get-in file-meta [filename :namespaces]))
    analysis))

(defn file-dependencies-analysis [{:keys [analysis file-meta] :as db} filename]
  (if (use-dep-graph? db)
    (transduce (map #(ns-dependencies-analysis db %))
               merge
               {}
               (get-in file-meta [filename :namespaces]))
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

(def ^:private deprecated-as-alias-elems-xf ; works for :namespace-aliases only
  (map #(select-keys % [:to :alias])))

(defn ns-aliases [{:keys [analysis dep-graph] :as db}]
  (if (use-dep-graph? db)
    (into #{}
          (comp
            dep-graph/from-internal-xf
            as-alias-elems-xf)
          dep-graph)
    (into #{}
          (comp
            deprecated-filter-project-analysis-xf
            (mapcat val)
            (filter #(identical? :namespace-alias (:bucket %)))
            deprecated-as-alias-elems-xf)
          analysis)))

(defn ns-aliases-for-langs [{:keys [analysis dep-graph] :as db} langs]
  (if (use-dep-graph? db)
    (into #{}
          (comp
            dep-graph/from-internal-xf
            (filter (fn [[_namespace {:keys [from-langs]}]]
                      (dep-graph/ms-overlaps-set? from-langs langs)))
            as-alias-elems-xf)
          dep-graph)
    (into #{}
          (comp
            deprecated-filter-project-analysis-xf
            (mapcat val)
            (filter #(identical? :namespace-alias (:bucket %)))
            (filter :alias)
            (filter (fn [element]
                      (some langs (elem-langs element))))
            deprecated-as-alias-elems-xf)
          analysis)))

(defn ns-names-for-langs [{:keys [analysis file-meta] :as db} langs]
  (if (use-dep-graph? db)
    (into #{}
          (mapcat (fn [file-meta]
                    (when (some langs (:langs file-meta))
                      (:namespaces file-meta))))
          (vals file-meta))
    (into #{}
          (comp
            (mapcat val)
            (filter #(identical? :namespace-definitions (:bucket %)))
            (filter (fn [element]
                      (some langs (elem-langs element))))
            (map :name))
          analysis)))

(defn ns-names-for-file [{:keys [file-meta] :as db} filename]
  (if (use-dep-graph? db)
    (vec (get-in file-meta [filename :namespaces]))
    (into []
          (comp
            (filter #(identical? :namespace-definitions (:bucket %)))
            (map :name))
          (get-in db [:analysis filename]))))

(defn ns-names [{:keys [analysis dep-graph] :as db}]
  (if (use-dep-graph? db)
    (set (keys dep-graph))
    (into #{}
          (comp
            (mapcat val)
            (filter #(identical? :namespace-definitions (:bucket %)))
            (map :name))
          analysis)))

(defn internal-ns-names [{:keys [analysis file-meta] :as db}]
  (if (use-dep-graph? db)
    (into #{}
          (comp
            dep-graph/internal-xf
            (mapcat (comp :namespaces val)))
          file-meta)
    (into #{}
          (comp
            deprecated-filter-project-analysis-xf
            (mapcat val)
            (filter #(identical? :namespace-definitions (:bucket %)))
            (map :name))
          analysis)))

(defn nses-some-internal-filename [db namespaces]
  (if (use-dep-graph? db)
    ;; TODO: this is a very specific return value, but has to be this way to
    ;; match the non-dep-graph version. When use-dep-graph? is removed, it'd be
    ;; better to refactor internal-api/nses->ns+uri to use
    ;; dep-graph/ns-internal-files directly, perhaps changing it to return all
    ;; uris for a given namespace.
    (into {}
          (keep (fn [namespace]
                  (when-first [file (dep-graph/ns-internal-files db namespace)]
                    [namespace file])))
          namespaces)
    ;; Performance sensitive: Gather filenames in one pass, instead of (count
    ;; namespaces) passes.
    (into {}
          (comp
            deprecated-filter-project-analysis-xf
            (mapcat val)
            (filter #(and (identical? :namespace-definitions (:bucket %))
                          (contains? (set namespaces) (:name %))))
            (map (juxt :name :filename)))
          (:analysis db))))

;;;; Filter elements in analysis

(defn ^:private safe-equal?
  "Fast equals for string and symbols."
  [a b]
  (if (instance? clojure.lang.Symbol a)
    (.equals ^clojure.lang.Symbol a b)
    (.equals ^String a b)))

(defn ^:private find-first [pred coll]
  (reduce
    (fn [_ i]
      (when (pred i)
        (reduced i)))
    nil
    coll))

(defn ^:private find-last-order-by-project-analysis [pred? db]
  (or (peek (into []
                  (comp
                    (mapcat val)
                    (filter pred?))
                  (internal-analysis db)))
      (peek (into []
                  (comp
                    (mapcat val)
                    (filter pred?))
                  (external-analysis db)))))

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

(defn ^:private var-usage-from-own-definition? [usage]
  (and (:from-var usage)
       (= (:from-var usage) (:name usage))
       (= (:from usage) (:to usage))))

(defn find-local-usages-under-form
  [db filename {:keys [row col end-row end-col]}]
  (let [local-elements (get-in db [:analysis filename])
        form-scope {:name-row row
                    :name-col col
                    :name-end-row end-row
                    :name-end-col end-col}
        ;; find locals definitions whose scope includes the form
        local-def-ids (into #{}
                            (comp
                              (filter (fn [{:keys [bucket] :as local-def}]
                                        (and (identical? :locals bucket)
                                             (shared/inside? form-scope local-def))))
                              (map :id))
                            local-elements)]
    ;; find locals usages inside the form, whose def is outside the form
    (->> local-elements
         (filter (fn [local-usage]
                   (and (identical? :local-usages (:bucket local-usage))
                        (contains? local-def-ids (:id local-usage))
                        (shared/inside? local-usage form-scope))))
         (medley/distinct-by :id))))

(defn find-var-usages-under-form
  [db filename line column end-line end-column]
  (let [local-elements (get-in db [:analysis filename])]
    (filter (fn [{:keys [bucket] :as element}]
              (and (= :var-usages bucket)
                   (shared/inside? element
                                   {:name-row line
                                    :name-col column
                                    :name-end-row end-line
                                    :name-end-col end-column})))
            local-elements)))

(defmulti find-definition
  (fn [_db element]
    (:bucket element)))

(defmethod find-definition :namespace-alias
  [db {:keys [to] :as element}]
  (find-last-order-by-project-analysis
    #(and (identical? :namespace-definitions (:bucket %))
          (= (:name %) to)
          (match-file-lang % element))
    (db-with-ns-analysis db to)))

(defmethod find-definition :namespace-usages
  [db element]
  (find-last-order-by-project-analysis
    #(and (identical? :namespace-definitions (:bucket %))
          (= (:name %) (:name element))
          (match-file-lang % element))
    (db-with-ns-analysis db (:name element))))

(defmethod find-definition :var-usages
  [db element]
  (letfn [(defines-elem? [candidate]
            (and (identical? :var-definitions (:bucket candidate))
                 (= (:name element) (:name candidate))
                 (= (:to element) (:ns candidate))
                 (not= 'clojure.core/declare (:defined-by candidate))))]
    (or
      (find-last-order-by-project-analysis
        #(and (defines-elem? %)
              (match-file-lang % element))
        (db-with-ns-analysis db (:to element)))
      (when (contains? (elem-langs element) :cljs)
        ;; maybe loaded by :require-macros, in which case, def will be in a clj file.
        (find-last-order-by-project-analysis
          #(and (defines-elem? %)
                (:macro %)
                (match-file-lang % (assoc element :lang :clj)))
          (db-with-ns-analysis db (:to element)))))))

(defmethod find-definition :local-usages
  [db {:keys [id filename] :as _element}]
  (find-first #(and (= :locals (:bucket %)) (= (:id %) id))
              (get-in db [:analysis filename])))

(defmethod find-definition :keywords
  [db element]
  (or (when (:ns element)
        (find-last-order-by-project-analysis
          #(and (identical? :keywords (:bucket %))
                (= (:name %) (:name element))
                (:reg %)
                (= (:ns %) (:ns element)))
          db))
      element))

(defmethod find-definition :var-definitions
  [db element]
  (if (= 'potemkin/import-vars (:defined-by element))
    ;; FIXME: this is buggy... it goes to **any** definition with the same name,
    ;; not specifically the one from the imported ns.
    ;; TODO: use :imported-ns and :imported-var, and treat this like a var-usage
    ;; Don't forget to switch from db to (db-with-ns-analysis db
    ;; (:imported-ns element))
    (find-last-order-by-project-analysis
      #(and (identical? :var-definitions (:bucket %))
            (= (:name %) (:name element))
            (not= 'potemkin/import-vars (:defined-by %))
            (match-file-lang % element))
      db)
    element))

(defmethod find-definition :protocol-impls
  [db element]
  (find-last-order-by-project-analysis
    #(and (identical? :var-definitions (:bucket %))
          (= (:name %) (:method-name element))
          (= (:ns %) (:protocol-ns element))
          (match-file-lang % element))
    (db-with-ns-analysis db (:protocol-ns element))))

(defmethod find-definition :java-class-usages
  [db element]
  (->> (:analysis db)
       (into []
             (comp
               (mapcat val)
               (filter #(and (identical? :java-class-definitions (:bucket %))
                             (safe-equal? (:class %) (:class element))))))
       (sort-by (complement #(string/ends-with? (:filename %) ".java")))
       first))

(defmethod find-definition :default
  [_db element]
  element)

(defmulti find-declaration
  (fn [_db element]
    (:bucket element)))

(defmethod find-declaration :var-usages
  [db {:keys [alias name to filename] :as element}]
  (when-not (identical? :clj-kondo/unknown-namespace to)
    (peek (into []
                (comp (filter
                        (if alias
                          #(and (identical? :namespace-alias (:bucket %))
                                (= to (:to %))
                                (= alias (:alias %)))
                          #(if (:refer %)
                             (and (identical? :var-usages (:bucket %))
                                  (= to (:to %))
                                  (= name (:name %)))
                             ;; :refer :all
                             (and (identical? :namespace-usages (:bucket %))
                                  (= to (:name %))))))
                      (filter #(match-file-lang % element)))
                (get-in db [:analysis filename])))))

(defmethod find-declaration :default [_ _] nil)

(defmulti find-implementations
  (fn [_db element]
    (:bucket element)))

(defmethod find-implementations :var-definitions
  [db element]
  (into []
        (comp
          (mapcat val)
          (cond
            ;; protocol method definition
            (and (= 'clojure.core/defprotocol (:defined-by element))
                 (:protocol-name element))
            (filter #(and (identical? :protocol-impls (:bucket %))
                          (safe-equal? (:ns element) (:protocol-ns %))
                          (safe-equal? (:name element) (:method-name %))))

            ;; protocol name definition
            (= 'clojure.core/defprotocol (:defined-by element))
            (filter #(and (identical? :var-usages (:bucket %))
                          (safe-equal? (:ns element) (:to %))
                          (safe-equal? (:name element) (:name %))))

            ;; defmulti definition
            (= 'clojure.core/defmulti (:defined-by element))
            (filter #(and (identical? :var-usages (:bucket %))
                          (:defmethod %)
                          (safe-equal? (:ns element) (:to %))
                          (safe-equal? (:name element) (:name %))))

            :else
            (constantly false))
          (filter #(match-file-lang % element))
          (medley/distinct-by (juxt :filename :name :row :col)))
        (ns-and-dependents-analysis db (:ns element))))

(defmethod find-implementations :var-usages
  [db element]
  (if (= (:to element) :clj-kondo/unknown-namespace)
    []
    (into []
          (comp
            (mapcat val)
            (cond
              ;; defmethod declaration
              (:defmethod element)
              (filter #(and (identical? :var-usages (:bucket %))
                            (:defmethod %)
                            (safe-equal? (:to element) (:to %))
                            (safe-equal? (:name element) (:name %))))

              ;; protocol method usage or defmethod usage
              :else
              (filter #(or (and (identical? :protocol-impls (:bucket %))
                                (safe-equal? (:to element) (:protocol-ns %))
                                (safe-equal? (:name element) (:method-name %)))
                           (and (identical? :var-usages (:bucket %))
                                (:defmethod %)
                                (safe-equal? (:to element) (:to %))
                                (safe-equal? (:name element) (:name %))))))
            (filter #(match-file-lang % element))
            (medley/distinct-by (juxt :filename :name :row :col)))
          (ns-and-dependents-analysis db (:to element)))))

(defmethod find-implementations :default [_ _] [])

(defmulti find-references
  (fn [_db element _include-declaration?]
    (case (:bucket element)
      :locals :local
      :local-usages :local
      (:bucket element))))

(defmethod find-references :namespace-definitions
  [db {:keys [name] :as element} include-declaration?]
  (concat
    (when include-declaration?
      [element])
    (into []
          (comp
            (mapcat val)
            (filter #(or (and (identical? :namespace-usages (:bucket %))
                              (= (:name %) name))
                         ;; TODO: do we always need these keywords? If not, a
                         ;; faster version of this could be made that uses
                         ;; (dep-graph/ns-and-dependents-analysis db name)
                         (and (identical? :keywords (:bucket %))
                              (= (:ns %) name)
                              (not (:auto-resolved %))
                              (not (:namespace-from-prefix %))))))
          (:analysis db))))

(defmethod find-references :namespace-usages
  [db element include-declaration?]
  (let [namespace-definition (assoc element :bucket :namespace-definitions)]
    (find-references db namespace-definition include-declaration?)))

(defmethod find-references :namespace-alias
  [db {:keys [alias filename] :as element} include-declaration?]
  (concat
    (when include-declaration?
      [element])
    (into []
          (comp
            (filter #(or (and (= :var-usages (:bucket %))
                              (= (:alias %) alias))
                         (and (= :keywords (:bucket %))
                              (= (:alias %) alias))))
            (medley/distinct-by (juxt :filename :name :name-row :name-col)))
          (get-in db [:analysis filename]))))

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
  (let [names (var-definition-names element)
        exclude-declaration? (not include-declaration?)]
    (into []
          (comp
            (mapcat val)
            (remove #(identical? :keywords (:bucket %)))
            (filter #(contains? names (:name %)))
            (filter #(safe-equal? (:ns element) (or (:ns %) (:to %))))
            (remove #(and exclude-declaration?
                          (or
                            (identical? :var-definitions (:bucket %))
                            (var-usage-from-own-definition? %))))
            (medley/distinct-by (juxt :filename :name :row :col)))
          (ns-and-dependents-analysis db (:ns element)))))

(defmethod find-references :keywords
  [db {:keys [ns name] :as _element} include-declaration?]
  (into []
        (comp
          (mapcat val)
          (filter #(identical? :keywords (:bucket %)))
          (filter #(safe-equal? name (:name %)))
          (filter (cond
                    (identical? :clj-kondo/unknown-namespace ns) #(identical? :clj-kondo/unknown-namespace (:ns %))
                    ns #(safe-equal? ns (:ns %))
                    :else #(not (:ns %))))
          (filter #(or include-declaration?
                       (not (:reg %))))
          (medley/distinct-by (juxt :filename :name :row :col)))
        (internal-analysis db)))

(defmethod find-references :local
  [db {:keys [id name filename] :as element} include-declaration?]
  (if (or id name)
    (filter #(and (= (:id %) id)
                  (or include-declaration?
                      (not (identical? :locals (:bucket %)))))
            (get-in db [:analysis filename]))
    [element]))

(defmethod find-references :protocol-impls
  [db {:keys [method-name protocol-ns] :as element} include-declaration?]
  (concat
    (when include-declaration?
      [element])
    (into []
          (comp
            (mapcat val)
            (filter #(identical? :var-usages (:bucket %)))
            (filter #(safe-equal? method-name (:name %)))
            (filter #(safe-equal? protocol-ns (:to %)))
            (medley/distinct-by (juxt :filename :name :row :col)))
          (ns-and-dependents-analysis db protocol-ns))))

(defmethod find-references :default
  [_db element _]
  [element])

(defn find-element-under-cursor
  [db filename line column]
  (find-first (fn [{:keys [name-row name-col name-end-row name-end-col]}]
                ;; TODO Probably should use q/inside? instead
                (and (<= name-row line name-end-row)
                     (<= name-col column name-end-col)))
              (get-in db [:analysis filename])))

(defn find-all-elements-under-cursor
  [db filename line column]
  (filter (fn [{:keys [name-row name-col name-end-row name-end-col]}]
                ;; TODO Probably should use q/inside? instead
            (and (<= name-row line name-end-row)
                 (<= name-col column name-end-col)))
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
    (filter #(and (identical? :var-definitions (:bucket %))
                  (or include-private?
                      (not (get % :private)))))
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
        (get-in db [:analysis filename])))

(defn find-all-var-definitions [db]
  (into []
        (comp
          (mapcat val)
          (xf-var-defs false))
        (:analysis db)))

(defn find-keyword-definitions [db filename]
  (into []
        (comp
          (filter #(and (identical? :keywords (:bucket %))
                        (:reg %)))
          (medley/distinct-by (juxt :ns :name :row :col)))
        (get-in db [:analysis filename])))

(defn find-all-keyword-definitions [db]
  (into []
        (comp
          (mapcat val)
          (filter #(and (identical? :keywords (:bucket %))
                        (:reg %)))
          (medley/distinct-by (juxt :ns :name :row :col)))
        (:analysis db)))

(defn find-local-by-destructured-keyword [db filename keyword-element]
  (->> (get-in db [:analysis filename])
       (filter #(and (identical? :locals (:bucket %))
                     (= (:name-row %) (:name-row keyword-element))
                     (= (:name-col %) (:name-col keyword-element))
                     (= (:name-end-row %) (:name-end-row keyword-element))
                     (= (:name-end-col %) (:name-end-col keyword-element))))
       first))

(defn find-unused-aliases [db filename]
  (let [local-elements (get-in db [:analysis filename])]
    (into #{}
          (comp
            (filter (comp #(identical? :unused-namespace %) :type))
            (remove (fn [finding]
                      (some #(and (identical? :var-usages (:bucket %))
                                  (not (:refer %))
                                  (safe-equal? (:ns finding) (:to %)))
                            local-elements)))
            (map :ns))
          (get-in db [:findings filename]))))

(defn find-unused-refers [db filename]
  (let [local-elements (get-in db [:analysis filename])]
    (into #{}
          (comp
            (filter (comp #(identical? :unused-referred-var %) :type))
            (remove (fn [finding]
                      (> (->> local-elements
                              (filter #(and (identical? :var-usages (:bucket %))
                                            (safe-equal? (:refer finding) (:name %))
                                            (safe-equal? (:ns finding) (:to %))))
                              (medley/distinct-by (juxt :name :to :row :col :end-row :end-col))
                              count)
                         1)))
            (map #(symbol (-> % :ns str) (-> % :refer str))))
          (get-in db [:findings filename]))))

(defn find-unused-imports [db filename]
  (let [local-elements (get-in db [:analysis filename])]
    (into #{}
          (comp
            (filter (comp #(identical? :unused-import %) :type))
            (remove (fn [finding]
                      (some #(or (and (identical? :var-usages (:bucket %))
                                      (safe-equal? (str (:class finding))
                                                   (str (:to %) "." (:name %))))
                                 (and (identical? :java-class-usages (:bucket %))
                                      (safe-equal? (str (:class finding))
                                                   (:class %))
                                      (not (:import %))))
                            local-elements)))
            (map :class))
          (get-in db [:findings filename]))))

(defn find-duplicate-requires [db filename]
  (into #{}
        (comp
          (filter (comp #(identical? :duplicate-require %) :type))
          (map :duplicate-ns))
        (get-in db [:findings filename])))

(defn find-namespace-definitions [db filename]
  (into []
        (filter #(identical? :namespace-definitions (:bucket %)))
        (get-in db [:analysis filename])))

(defn find-namespace-definition-by-filename [db filename]
  (first (find-namespace-definitions db filename)))

(defn find-namespace-usage-by-alias [db filename alias]
  (->> (get-in db [:analysis filename])
       (filter #(and (identical? :namespace-usages (:bucket %))
                     (= alias (:alias %))))
       last))

(defn find-element-for-rename [db from-ns from-name]
  (find-last-order-by-project-analysis
    (if from-name
      #(and (identical? :var-definitions (:bucket %))
            (= from-ns (:ns %))
            (= from-name (:name %)))
      #(and (identical? :namespace-definitions (:bucket %))
            (= from-ns (:name %))))
    (db-with-internal-analysis db)))

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
        keyword? (boolean (:reg definition))
        fqsn (symbol (-> definition :ns str) (-> definition :name str))]
    (or (contains? (set/union default-public-vars-defined-by-to-exclude excluded-defined-by-syms)
                   (if keyword?
                     (:reg definition)
                     (:defined-by definition)))
        (contains? (set/union excluded-ns-or-var default-public-vars-name-to-exclude)
                   (if keyword?
                     (symbol (str (:ns definition)) (:name definition))
                     (:name definition)))
        (contains? (set excluded-ns-or-var) (:ns definition))
        (-> excluded-full-qualified-vars
            set
            (contains? fqsn)))))

(defn xf-all-var-usages-to-namespaces [namespaces]
  (comp
    (mapcat val)
    (filter #(identical? :var-usages (:bucket %)))
    (filter #(contains? namespaces (:to %)))
    (remove var-usage-from-own-definition?)))

(def xf-all-keyword-usages
  (comp
    (mapcat val)
    (filter #(identical? :keywords (:bucket %)))
    (remove :reg)))
