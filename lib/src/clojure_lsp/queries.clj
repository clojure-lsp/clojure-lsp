(ns clojure-lsp.queries
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [clojure.string :as string]
   [lsp4clj.protocols.logger :as logger]
   [medley.core :as medley]))

(set! *warn-on-reflection* true)

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

(defn ^:private project-analysis [{:keys [analysis]}]
  (medley/remove-vals
    (fn [elems]
      (:external? (first elems)))
    analysis))

(defn db-with-project-analysis [db]
  (assoc db :analysis (project-analysis db)))

(def filter-project-analysis-xf
  "Filter only project elements from analysis.
  Checking if the first value is `external?`, we infer all elements
  for that filename are too."
  (remove (comp :external? first val)))

(def filter-external-analysis-xf
  "Filter only external elements from analysis.
  Checking if the first value is not `external?`, we infer all elements
  for that filename are not too."
  (filter (comp :external? first val)))

(defn ^:private find-last-order-by-project-analysis [pred? analysis]
  (or (peek (into []
                  (comp
                    filter-project-analysis-xf
                    (mapcat val)
                    (filter pred?))
                  analysis))
      (peek (into []
                  (comp
                    filter-external-analysis-xf
                    (mapcat val)
                    (filter pred?))
                  analysis))))

(defn elem-langs [element]
  (or (some-> element :lang list set)
      (shared/uri->available-langs (:filename element))))

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
  (let [local-analysis (get-in db [:analysis filename])
        form-scope {:name-row row
                    :name-col col
                    :name-end-row end-row
                    :name-end-col end-col}]
    (->> local-analysis
         ;; 1. find locals defined above form
         (filter (fn [{:keys [bucket] :as local-def}]
                   (and (= :locals bucket)
                        (shared/inside? form-scope local-def))))
         ;; 2. find first usage inside of form
         (keep (fn [local-def]
                 ;; This is a nested-loop over local analysis. If it becomes a
                 ;; performance bottleneck, replace nested-loop with hashjoin on
                 ;; id.
                 (find-first (fn [local-usage]
                               (and (= :local-usages (:bucket local-usage))
                                    (= (:id local-def) (:id local-usage))
                                    (shared/inside? local-usage form-scope)))
                             local-analysis))))))

(defn find-var-usages-under-form
  [db filename line column end-line end-column]
  (let [local-analysis (get-in db [:analysis filename])]
    (filter (fn [{:keys [bucket] :as element}]
              (and (= :var-usages bucket)
                   (shared/inside? element
                                   {:name-row line
                                    :name-col column
                                    :name-end-row end-line
                                    :name-end-col end-column})))
            local-analysis)))

(defmulti find-definition
  (fn [_db element]
    (:bucket element)))

(defmethod find-definition :namespace-alias
  [db element]
  (find-last-order-by-project-analysis
    #(and (identical? :namespace-definitions (:bucket %))
          (= (:name %) (:to element))
          (match-file-lang % element))
    (:analysis db)))

(defmethod find-definition :namespace-usages
  [db element]
  (find-last-order-by-project-analysis
    #(and (identical? :namespace-definitions (:bucket %))
          (= (:name %) (:name element))
          (match-file-lang % element))
    (:analysis db)))

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
        (:analysis db))
      (when (contains? (elem-langs element) :cljs)
        ;; maybe loaded by :require-macros, in which case, def will be in a clj file.
        (find-last-order-by-project-analysis
          #(and (defines-elem? %)
                (:macro %)
                (match-file-lang % (assoc element :lang :clj)))
          (:analysis db))))))

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
          (:analysis db)))
      element))

(defmethod find-definition :var-definitions
  [db element]
  (if (= 'potemkin/import-vars (:defined-by element))
    (find-last-order-by-project-analysis
      #(and (identical? :var-definitions (:bucket %))
            (= (:name %) (:name element))
            (not= 'potemkin/import-vars (:defined-by %))
            (match-file-lang % element))
      (:analysis db))
    element))

(defmethod find-definition :protocol-impls
  [db element]
  (find-last-order-by-project-analysis
    #(and (identical? :var-definitions (:bucket %))
          (= (:name %) (:method-name element))
          (= (:ns %) (:protocol-ns element))
          (match-file-lang % element))
    (:analysis db)))

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
        (:analysis db)))

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
          (:analysis db))))

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
          (:analysis db))))

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
        (project-analysis db)))

(defmethod find-references :local
  [db {:keys [id name filename] :as element} include-declaration?]
  (if (or id name)
    (filter #(and (= (:id %) id)
                  (or include-declaration?
                      (not (identical? :locals (:bucket %)))))
            (get-in db [:analysis filename]))
    [element]))

(defmethod find-references :protocol-impls
  [db element include-declaration?]
  (concat
    (when include-declaration?
      [element])
    (into []
          (comp
            (mapcat val)
            (filter #(identical? :var-usages (:bucket %)))
            (filter #(safe-equal? (:method-name element) (:name %)))
            (filter #(safe-equal? (:protocol-ns element) (:to %)))
            (medley/distinct-by (juxt :filename :name :row :col)))
          (:analysis db))))

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

(def ^:private find-all-ns-definitions-xf
  (comp
    (mapcat val)
    (filter #(identical? :namespace-definitions (:bucket %)))))

(def find-all-ns-definition-names-xf
  (comp
    find-all-ns-definitions-xf
    (map :name)))

;; TODO remove it, use only transducer one?
(defn find-all-ns-definition-names [db]
  (into #{}
        find-all-ns-definition-names-xf
        (:analysis db)))

(defn find-all-ns-definitions-for-langs
  [db langs]
  (into #{}
        (comp
          find-all-ns-definitions-xf
          (filter (fn [element]
                    (some langs (elem-langs element)))))
        (:analysis db)))

(defn find-all-aliases-for-langs
  [db langs]
  (into #{}
        (comp
          (mapcat val)
          (filter #(identical? :namespace-alias (:bucket %)))
          (filter :alias)
          (filter (fn [element]
                    (some langs (elem-langs element)))))
        (project-analysis db)))

(defn find-unused-aliases [db filename]
  (let [local-analysis (get-in db [:analysis filename])]
    (into #{}
          (comp
            (filter (comp #(identical? :unused-namespace %) :type))
            (remove (fn [finding]
                      (some #(and (identical? :var-usages (:bucket %))
                                  (not (:refer %))
                                  (safe-equal? (:ns finding) (:to %)))
                            local-analysis)))
            (map :ns))
          (get-in db [:findings filename]))))

(defn find-unused-refers [db filename]
  (let [local-analysis (get-in db [:analysis filename])]
    (into #{}
          (comp
            (filter (comp #(identical? :unused-referred-var %) :type))
            (remove (fn [finding]
                      (> (->> local-analysis
                              (filter #(and (identical? :var-usages (:bucket %))
                                            (safe-equal? (:refer finding) (:name %))
                                            (safe-equal? (:ns finding) (:to %))))
                              (medley/distinct-by (juxt :name :to :row :col :end-row :end-col))
                              count)
                         1)))
            (map #(symbol (-> % :ns str) (-> % :refer str))))
          (get-in db [:findings filename]))))

(defn find-unused-imports [db filename]
  (let [local-analysis (get-in db [:analysis filename])]
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
                            local-analysis)))
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
    (project-analysis db)))

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
