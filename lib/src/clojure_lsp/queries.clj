(ns clojure-lsp.queries
  (:require
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [clojure.string :as string]
   [medley.core :as medley]))

(set! *warn-on-reflection* true)

(defn elem-langs [element]
  (or (some-> element :lang list set)
      (shared/uri->available-langs (:uri element))))

(defn defined-bys [element]
  (set [(:defined-by->lint-as element)
        (:defined-by element)]))

;;;; Filter analysis, using dep-graph

(defn external-analysis [{:keys [analysis] :as db}]
  (medley/remove-keys #(dep-graph/uri-internal? db %) analysis))

(defn internal-analysis [{:keys [analysis] :as db}]
  (medley/filter-keys #(dep-graph/uri-internal? db %) analysis))

(defn internal-plus-local-analysis [db uri]
  (assoc (internal-analysis db) uri (get-in db [:analysis uri])))

(defn ^:private uris-analysis [{:keys [analysis]} uris]
  (select-keys analysis uris))

(defn ns-analysis [db namespace]
  (uris-analysis db (dep-graph/ns-uris db namespace)))

(defn ns-dependents-analysis [db namespace]
  (uris-analysis db (dep-graph/ns-dependents-uris db namespace)))

(defn ns-and-dependents-analysis [db namespace]
  (uris-analysis db (dep-graph/ns-and-dependents-uris db namespace)))

(defn ns-dependencies-analysis [db namespace]
  (uris-analysis db (dep-graph/ns-dependencies-uris db namespace)))

(defn nses-analysis [db namespaces]
  (uris-analysis db (dep-graph/nses-uris db namespaces)))

(defn nses-and-dependents-analysis [db namespaces]
  (uris-analysis db (dep-graph/nses-and-dependents-uris db namespaces)))

(defn uri-dependents-analysis [{:keys [documents] :as db} uri]
  (transduce (map #(ns-dependents-analysis db %))
             merge
             {}
             (get-in documents [uri :namespaces])))

(defn uri-dependencies-analysis [{:keys [documents] :as db} uri]
  (transduce (map #(ns-dependencies-analysis db %))
             merge
             {}
             (get-in documents [uri :namespaces])))

(defn edn-analysis [{:keys [analysis]}]
  (into {}
        (filter (fn [[k _]] (string/ends-with? k ".edn")))
        analysis))

(defn db-with-analysis [db f & args]
  (assoc db :analysis (apply f db args)))

(defn db-with-internal-analysis [db]
  (db-with-analysis db internal-analysis))

(defn db-with-ns-analysis [db namespace]
  (db-with-analysis db ns-analysis namespace))

;;;; Filter elements in analysis

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
(defn ^:private xf-analysis->scoped-path-analysis [path-uri]
  (filter #(string/starts-with? (:uri %) path-uri)))

(def xf-analysis->java-class-definitions (xf-analysis->bucket-elems :java-class-definitions))
(def xf-analysis->java-class-usages (xf-analysis->bucket-elems :java-class-usages))
(def xf-analysis->java-member-definitions (xf-analysis->bucket-elems :java-member-definitions))
(def xf-analysis->keyword-definitions (xf-analysis->bucket-elems :keyword-definitions))
(def xf-analysis->keyword-usages (xf-analysis->bucket-elems :keyword-usages))
(def xf-analysis->keywords (xf-analysis->buckets-elems :keyword-definitions :keyword-usages))
(def xf-analysis->namespace-definitions (xf-analysis->bucket-elems :namespace-definitions))
(def xf-analysis->namespace-usages (xf-analysis->bucket-elems :namespace-usages))
(def xf-analysis->protocol-impls (xf-analysis->bucket-elems :protocol-impls))
(def xf-analysis->var-definitions (xf-analysis->bucket-elems :var-definitions))
(def xf-analysis->var-usages (xf-analysis->bucket-elems :var-usages))
(def xf-analysis->var-usages-and-symbols (xf-analysis->buckets-elems :var-usages :symbols))
(def xf-analysis->symbols (xf-analysis->bucket-elems :symbols))
(def xf-analysis->vars (xf-analysis->buckets-elems :var-definitions :var-usages :symbols))

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

(defn ^:private var-definition-names [{:keys [name] :as element}]
  (let [defined-bys (defined-bys element)]
    (cond
      (some '#{clojure.core/defrecord
               cljs.core/defrecord} defined-bys)
      #{name (symbol (str "->" name)) (symbol (str "map->" name))}

      (some '#{clojure.core/deftype
               cljs.core/deftype} defined-bys)
      #{name (symbol (str "->" name))}

      :else
      #{name})))

(def kw-signature (juxt :ns :name))
(def var-usage-signature (juxt :to :name))
(def symbol-signature (juxt #(-> % :symbol namespace symbol) :name))
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

(defn ^:private xf-under-form [{:keys [row col end-row end-col]}]
  (let [form-scope {:name-row row
                    :name-col col
                    :name-end-row end-row
                    :name-end-col end-col}]
    (filter (fn [local-usage]
              (shared/inside? local-usage form-scope)))))

(defn find-local-usages-under-form
  [db uri form-scope]
  (into [] (xf-under-form form-scope) (get-in db [:analysis uri :local-usages])))

(defn find-locals-under-form
  [db uri form-scope]
  (into [] (xf-under-form form-scope) (get-in db [:analysis uri :locals])))

(defn find-var-usages-under-form
  [db uri form-scope]
  (into [] (xf-under-form form-scope) (get-in db [:analysis uri :var-usages])))

(defn find-locals-defined-outside-form
  [db uri {:keys [row col end-row end-col]}]
  (let [form-scope {:name-row row
                    :name-col col
                    :name-end-row end-row
                    :name-end-col end-col}]
    (filter (fn [local-def]
              (shared/inside? form-scope local-def))
            (get-in db [:analysis uri :locals]))))

(defn find-local-usages-defined-outside-form
  [db uri form-scope]
  (let [local-def-ids (into #{}
                            (map :id)
                            (find-locals-defined-outside-form db uri form-scope))]
    (->> (find-local-usages-under-form db uri form-scope)
         (filter (fn [local-usage]
                   (contains? local-def-ids (:id local-usage))))
         (medley/distinct-by :id))))

(defn find-namespace-definitions [db uri]
  (vec (get-in db [:analysis uri :namespace-definitions])))

(defn find-namespace-definition-by-uri [db uri]
  (first (find-namespace-definitions db uri)))

(defn find-namespace-usage-by-alias [db uri alias]
  (find-last (filter #(= alias (:alias %)))
             (get-in db [:analysis uri :namespace-usages])))

(defn find-namespace-alias-by-alias [db uri alias]
  (find-last (filter #(= alias (:alias %)))
             (get-in db [:analysis uri :namespace-alias])))

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
          definition)))
    ;; Fallback to navigate from clojure to clojurescript vars, see #1403
    (when-not (:fallbacking? var-usage)
      (find-definition db (assoc var-usage :lang :cljs :fallbacking? true)))
    ;; If alias exists but not var, go to the ns definition
    (when-let [alias (:alias var-usage)]
      (find-definition db (find-namespace-usage-by-alias db (:uri var-usage) alias)))))

(defmethod find-definition :symbols
  [db quoted-symbol]
  (if (:to quoted-symbol)
    (find-definition db (assoc quoted-symbol :bucket :var-usages))
    (let [sym (:symbol quoted-symbol)
          lang (:lang quoted-symbol)
          lang (if (= :edn lang)
                 ;; when referring to qualified-symbols in edn, pretend it's
                 ;; referenced from JVM Clojure
                 :clj
                 lang)]
      (find-definition db
                       (assoc quoted-symbol
                              :bucket :var-usages
                              ;; infer namespace from symbol
                              :to (symbol (namespace sym))
                              :lang lang)))))

(defmethod find-definition :local-usages
  [db {:keys [id uri] :as _local-usage}]
  (find-first (filter #(= (:id %) id))
              (get-in db [:analysis uri :locals])))

(defmethod find-definition :keyword-usages
  [db keyword-usage]
  (or (find-last-order-by-project-analysis
        (comp xf-analysis->keyword-definitions
              (xf-same-fqn (:ns keyword-usage) (:name keyword-usage)))
        db)
      keyword-usage))

(defmethod find-definition :var-definitions
  [db {:keys [imported-ns defined-by name ns] :as var-definition}]
  (let [actual-definition (delay (find-last-order-by-project-analysis
                                   (comp xf-analysis->var-definitions
                                         (xf-same-fqn ns name)
                                         (filter #(not= 'clojure.core/declare (:defined-by %)))
                                         (xf-same-lang var-definition))
                                   (db-with-ns-analysis db ns)))]
    (cond
      ;; Handle potemkin/import-vars
      (some #(safe-equal? 'potemkin/import-vars %) (defined-bys var-definition))
      (find-definition db (assoc var-definition
                                 :bucket :var-usages
                                 :to imported-ns))

      ;; Handle navigating from declare to actual definition
      (and (= defined-by 'clojure.core/declare)
           @actual-definition)
      @actual-definition

      ;; Default case, return the definition as-is
      :else var-definition)))

(defmethod find-definition :protocol-impls
  [db protocol-impl]
  (find-last-order-by-project-analysis
    (comp xf-analysis->var-definitions
          (xf-same-fqn (:protocol-ns protocol-impl) (:method-name protocol-impl))
          (xf-same-lang protocol-impl))
    (db-with-ns-analysis db (:protocol-ns protocol-impl))))

(defmethod find-definition :java-class-usages
  [db java-class-usage]
  (let [full-class-name (:class java-class-usage)
        method-name (:method-name java-class-usage)]
    (or (when method-name
          (->> (:analysis db)
               (into []
                     (comp
                       xf-analysis->java-member-definitions
                       (filter #(and (safe-equal? full-class-name (:class %))
                                     (safe-equal? method-name (:name %))))))
               first))
        (->> (:analysis db)
             (into []
                   (comp
                     xf-analysis->java-class-definitions
                     (filter #(safe-equal? full-class-name (:class %)))))
             (sort-by (complement #(string/ends-with? (:uri %) ".java")))
             first)
        ;; maybe class was defined by defrecord
        (let [split (string/split full-class-name #"\.")
              ns (symbol (string/replace (string/join "." (drop-last split)) "_" "-"))
              name (symbol (last split))]
          (find-definition db (assoc java-class-usage
                                     :bucket :var-usages
                                     :to ns
                                     :name name))))))

(defmethod find-definition :default
  [_db element]
  element)

(defmulti find-declaration
  (fn [_db element]
    (:bucket element)))

(defmethod find-declaration :var-usages
  [db {:keys [alias name to uri] :as var-usage}]
  (when-not (identical? :clj-kondo/unknown-namespace to)
    (let [buckets (get-in db [:analysis uri])
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
                (and (some '#{clojure.core/defprotocol
                              clojure.core/definterface} (defined-bys var-definition))
                     (:protocol-name var-definition))
                (comp xf-analysis->protocol-impls
                      (xf-same-fqn (:ns var-definition) (:name var-definition)
                                   :protocol-ns :method-name))

                ;; protocol name definition
                (some '#{clojure.core/defprotocol
                         clojure.core/definterface} (defined-bys var-definition))
                (comp xf-analysis->var-usages
                      (xf-same-fqn (:ns var-definition) (:name var-definition) :to))

                ;; defmulti definition
                (some #(safe-equal? 'clojure.core/defmulti %) (defined-bys var-definition))
                (comp xf-analysis->var-usages
                      (xf-same-fqn (:ns var-definition) (:name var-definition) :to)
                      (filter :defmethod))

                :else
                nil)]
    (into []
          (comp
            xf
            (xf-same-lang var-definition)
            (medley/distinct-by (juxt :uri :name :row :col)))
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
              (medley/distinct-by (juxt :uri :name :row :col)))
            (ns-and-dependents-analysis db (:to var-usage))))))

(defmethod find-implementations :default [_ _] [])

(defmulti find-references
  (fn [_db element _include-definition?]
    (case (:bucket element)
      (:locals :local-usages) :local
      (:keyword-definitions :keyword-usages) :keywords
      (:bucket element))))

(defmethod find-references :namespace-definitions
  [db {:keys [name] :as namespace-definition} include-definition?]
  (concat
    (when include-definition?
      [namespace-definition])
    (vec
      (concat
        (into []
              (comp
                xf-analysis->namespace-usages
                (xf-same-name name)
                (medley/distinct-by (juxt :uri :name :row :col)))
              (ns-and-dependents-analysis db name))
        ;; TODO: do we always need these keywords? If not, probably better to
        ;; split this into a fast version that uses ns-and-dependents-analysis,
        ;; and a slow version that adds these keywords.
        (into []
              (comp
                xf-analysis->keywords
                (xf-same-ns name)
                (medley/distinct-by (juxt :uri :name :row :col))
                (remove #(or (:auto-resolved %)
                             (:namespace-from-prefix %))))
              (:analysis db))))))

(defmethod find-references :namespace-usages
  [db namespace-usage include-definition?]
  (let [namespace-definition (assoc namespace-usage :bucket :namespace-definitions)]
    (find-references db namespace-definition include-definition?)))

(defmethod find-references :namespace-alias
  [db {:keys [alias uri] :as namespace-alias} include-definition?]
  (concat
    (when include-definition?
      [namespace-alias])
    (let [{:keys [var-usages keyword-usages keyword-definitions]} (get-in db [:analysis uri])]
      (into []
            (comp
              (filter #(= (:alias %) alias))
              (medley/distinct-by (juxt :uri :name :name-row :name-col)))
            (concat keyword-definitions keyword-usages var-usages)))))

(defmethod find-references :var-usages
  [db var-usage include-definition?]
  (if (= (:to var-usage) :clj-kondo/unknown-namespace)
    [var-usage]
    (let [var-definition {:ns (:to var-usage)
                          :name (:name var-usage)
                          :bucket :var-definitions}]
      (find-references db var-definition include-definition?))))

(defmethod find-references :var-definitions
  [db var-definition include-definition?]
  (let [names (var-definition-names var-definition)]
    (into []
          (comp
            (if include-definition? xf-analysis->vars xf-analysis->var-usages-and-symbols)
            (filter #(contains? names (:name %)))

            (filter #(safe-equal? (:ns var-definition) (or (:ns %)
                                                           (:to %)
                                                           (some-> (:symbol %) namespace symbol))))
            (filter #(or include-definition?
                         (not (var-usage-from-own-definition? %))))
            (medley/distinct-by (juxt :uri :name :row :col)))
          (merge (ns-and-dependents-analysis db (:ns var-definition))
                 (edn-analysis db)))))

(defmethod find-references :keywords
  [db {:keys [ns name uri] :as keyword-element} include-definition?]
  (let [analysis (if (contains? (elem-langs keyword-element) :edn)
                   (internal-plus-local-analysis db uri)
                   (internal-analysis db))]
    (into []
          (comp
            (if include-definition? xf-analysis->keywords xf-analysis->keyword-usages)
            (xf-same-fqn ns name)
            (medley/distinct-by (juxt :uri :name :row :col)))
          analysis)))

(defmethod find-references :local
  [db {:keys [id name uri] :as element} include-definition?]
  (if (or id name)
    (let [{:keys [locals local-usages]} (get-in db [:analysis uri])]
      (filter #(= (:id %) id)
              (concat (when include-definition? locals)
                      local-usages)))
    [element]))

(defmethod find-references :protocol-impls
  [db {:keys [method-name protocol-ns] :as element} include-definition?]
  (concat
    (when include-definition?
      [element])
    (into []
          (comp
            xf-analysis->var-usages
            (xf-same-fqn protocol-ns method-name :to)
            (medley/distinct-by (juxt :uri :name :row :col)))
          (ns-and-dependents-analysis db protocol-ns))))

(defmethod find-references :symbols
  [db quoted-symbol include-definition?]
  (let [analysis (if (contains? (elem-langs quoted-symbol) :edn)
                   (internal-plus-local-analysis db (:uri quoted-symbol))
                   (internal-analysis db))
        lang (:lang quoted-symbol)
        lang (if (= :edn lang)
               ;; when referring to qualified-symbols in edn, pretend it's
               ;; referenced from JVM Clojure
               :clj
               lang)
        name (:name quoted-symbol)
        original-to (:to quoted-symbol)
        to (or original-to
               (symbol (namespace (:symbol quoted-symbol))))]
    (concat
      (when include-definition?
        [quoted-symbol])

      ;; symbols references
      (into []
            (comp
              xf-analysis->symbols
              (xf-same-fqn original-to name)
              (medley/distinct-by (juxt :uri :name :row :col)))
            analysis)

      ;; var-usages references
      (find-references db (assoc quoted-symbol
                                 :bucket :var-usages
                                 :lang lang
                                 :to to) include-definition?))))

(defmethod find-references :default
  [_db element _]
  [element])

(defn ^:private xf-under-cursor [row col]
  (filter (fn [{:keys [name-row name-col name-end-row name-end-col]}]
            (and (<= name-row row name-end-row)
                 (<= name-col col name-end-col)))))

(defn find-element-under-cursor
  [db uri row col]
  (find-first (comp (mapcat val)
                    (xf-under-cursor row col))
              (get-in db [:analysis uri])))

(defn find-all-elements-under-cursor
  [db uri row col]
  (into []
        (comp (mapcat val)
              (xf-under-cursor row col))
        (get-in db [:analysis uri])))

(defn find-local-under-cursor
  [db uri line column]
  (find-first (xf-under-cursor line column)
              (get-in db [:analysis uri :locals])))

(defn find-definition-from-cursor [db uri row col]
  (try
    (when-let [element (find-element-under-cursor db uri row col)]
      (find-definition db element))
    (catch Throwable e
      (logger/error e "can't find definition"))))

(defn find-declaration-from-cursor [db uri row col]
  (try
    (when-let [element (find-element-under-cursor db uri row col)]
      (find-declaration db element))
    (catch Throwable e
      (logger/error e "can't find declaration"))))

(defn find-implementations-from-cursor [db uri row col]
  (try
    (when-let [element (find-element-under-cursor db uri row col)]
      (find-implementations db element))
    (catch Throwable e
      (logger/error e "can't find implementation"))))

(defn find-references-from-cursor [db uri row col include-definition?]
  (try
    (when-let [element (find-element-under-cursor db uri row col)]
      (find-references db element include-definition?))
    (catch Throwable e
      (logger/error e "can't find references"))))

(defn ^:private xf-var-defs [include-private?]
  (comp
    (filter #(or include-private?
                 (not (get % :private))))
    (medley/distinct-by (juxt :ns :name :row :col))
    (remove #(or (and (some #{'clojure.core/defrecord
                              'cljs.core/defrecord} (defined-bys %))
                      (or (string/starts-with? (str (:name %)) "->")
                          (string/starts-with? (str (:name %)) "map->")))
                 (and (some #{'clojure.core/deftype
                              'cljs.core/deftype} (defined-bys %))
                      (string/starts-with? (str (:name %)) "->"))))))

(defn find-var-definitions [db uri include-private?]
  (into []
        (xf-var-defs include-private?)
        (get-in db [:analysis uri :var-definitions])))

(defn find-all-var-definitions [db]
  (into []
        (comp
          xf-analysis->var-definitions
          (xf-var-defs false))
        (:analysis db)))

(defn find-all-path-namespace-definitions [db path-uri]
  (into []
        (comp
          xf-analysis->namespace-definitions
          (xf-analysis->scoped-path-analysis path-uri))
        (:analysis db)))

(defn find-all-path-java-class-definitions [db path-uri]
  (into []
        (comp
          xf-analysis->java-class-definitions
          (xf-analysis->scoped-path-analysis path-uri))
        (:analysis db)))

(defn find-keyword-definitions [db uri]
  (into []
        (medley/distinct-by (juxt :ns :name :row :col))
        (get-in db [:analysis uri :keyword-definitions])))

(defn find-all-keyword-definitions [db]
  (into []
        (comp
          xf-analysis->keyword-definitions
          (medley/distinct-by (juxt :ns :name :row :col)))
        (:analysis db)))

(def ^:private xf-defmethods
  (comp (filter :defmethod)
        (medley/distinct-by (juxt :to :name :name-row :name-col))))

(defn find-defmethods [db uri]
  (into []
        xf-defmethods
        (get-in db [:analysis uri :var-usages])))

(defn find-element-definitions
  [db uri]
  (concat []
          (some-> (find-namespace-definition-by-uri db uri) vector)
          (find-var-definitions db uri true)
          (find-keyword-definitions db uri)
          (find-defmethods db uri)))

(defn find-all-element-definitions
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

(defn find-local-by-destructured-keyword [db uri keyword-element]
  (find-first (filter #(and (= (:name-row %) (:name-row keyword-element))
                            (= (:name-col %) (:name-col keyword-element))
                            (= (:name-end-row %) (:name-end-row keyword-element))
                            (= (:name-end-col %) (:name-end-col keyword-element))))
              (get-in db [:analysis uri :locals])))

(defn find-unused-aliases [db uri]
  (let [local-var-usages (get-in db [:analysis uri :var-usages])]
    (into #{}
          (comp
            (filter (comp #(identical? :unused-namespace %) :type))
            (remove (fn [finding]
                      (some #(and (not (:refer %))
                                  (safe-equal? (:ns finding) (:to %)))
                            local-var-usages)))
            (map :ns))
          (get-in db [:findings uri]))))

(defn find-unused-refers [db uri]
  (let [local-var-usages (get-in db [:analysis uri :var-usages])]
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
          (get-in db [:findings uri]))))

(defn find-unused-imports [db uri]
  (let [{:keys [var-usages java-class-usages]} (get-in db [:analysis uri])]
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
          (get-in db [:findings uri]))))

(defn find-duplicate-requires [db uri]
  (into #{}
        (comp
          (filter (comp #(identical? :duplicate-require %) :type))
          (map :duplicate-ns))
        (get-in db [:findings uri])))

(defn find-element-from-sym [db from-ns from-name]
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
        fqsn (symbol (-> definition :ns str) (-> definition :name str))
        default-excludes (set/union default-public-vars-defined-by-to-exclude excluded-defined-by-syms)]
    (or (if keyword-definition?
          (contains? default-excludes (:reg definition))
          (some default-excludes (defined-bys definition)))
        ;; definterface methods
        (and (some #{'clojure.core/definterface} (defined-bys definition))
             (:protocol-name definition))
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

(defn xf-all-var-usages-and-symbols-to-namespaces [namespaces]
  (comp
    xf-analysis->var-usages-and-symbols
    (filter #(contains? namespaces (or (:to %) (some-> % :symbol namespace symbol))))
    (remove var-usage-from-own-definition?)))

(defn find-local-var-usages-to-namespace [db uri namespace]
  (into []
        (filter #(= namespace (:to %)))
        (get-in db [:analysis uri :var-usages])))

(defn find-keyword-usages-by-keyword [db uri kwd-ns kwd-name]
  (into []
        (xf-same-fqn kwd-ns kwd-name)
        (get-in db [:analysis uri :keyword-usages])))

(defn xf-all-java-definitions-by-class-name [class-name]
  (comp
    xf-analysis->java-class-definitions
    (filter (fn [element]
              (= class-name (last (string/split (:class element) #"\.")))))))

(defn find-all-java-class-usages-on-imports-by-class [db class-name]
  (into []
        (comp
          xf-analysis->java-class-usages
          (filter #(and (:import %)
                        (safe-equal? class-name (:class %)))))
        (:analysis db)))

(defn find-all-project-namespace-definitions [db namespace]
  (into []
        (comp
          xf-analysis->namespace-definitions
          (xf-same-name namespace)
          (medley/distinct-by :uri))
        (internal-analysis (db-with-ns-analysis db namespace))))

(defn analysis-summary [db]
  (let [summary-fn (fn [analysis]
                     (reduce-kv
                       (fn [acc k v]
                         (assoc acc k (count v)))
                       {}
                       (->> analysis
                            vals
                            (reduce (fn [acc analysis]
                                      (merge-with into acc analysis)) {}))))]
    {:internal (summary-fn (internal-analysis db))
     :external (summary-fn (external-analysis db))}))

(def ^:private defines-interface?
  '#{clojure.core/defprotocol cljs.core/defprotocol
     clojure.core/definterface cljs.core/definterface
     clojure.core/defmulti cljs.core/defmulti})

(def ^:private defines-class?
  '#{clojure.core/defrecord cljs.core/defrecord
     clojure.core/deftype cljs.core/deftype})

(defn element->symbol-kind [{:keys [bucket] :as el}]
  (case bucket
    (:namespace-usages :namespace-definitions) :namespace
    :var-definitions (cond
                       (or (:fixed-arities el) (:varargs-min-arity el) (:macro el))
                       #_=> :function
                       (some->> el defined-bys (some defines-interface?))
                       #_=> :interface
                       (some->> el defined-bys (some defines-class?))
                       #_=> :class
                       :else
                       #_=> :variable)
    :var-usages (if (:defmethod el)
                  :function
                  :variable)
    :keyword-definitions :function
    :keyword-usages :field
    :null))
