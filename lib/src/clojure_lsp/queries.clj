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

(defn find-first [pred coll]
  (reduce
    (fn [_ i]
      (when (pred i)
        (reduced i)))
    nil
    coll))

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
    clojure.core/defrecord
    , #{name (symbol (str "->" name)) (symbol (str "map->" name))}
    clojure.core/deftype
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
  [analysis filename {:keys [row col end-row end-col]}]
  (let [local-analysis (get analysis filename)
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
  [analysis filename line column end-line end-column]
  (let [local-analysis (get analysis filename)]
    (filter (fn [{:keys [bucket] :as element}]
              (and (= :var-usages bucket)
                   (shared/inside? element
                                   {:name-row line
                                    :name-col column
                                    :name-end-row end-line
                                    :name-end-col end-column})))
            local-analysis)))

(defmulti find-definition
  (fn [_analysis element]
    (:bucket element)))

(defmethod find-definition :namespace-alias
  [analysis element]
  (find-last-order-by-project-analysis
    #(and (identical? :namespace-definitions (:bucket %))
          (= (:name %) (:to element))
          (match-file-lang % element))
    analysis))

(defmethod find-definition :namespace-usages
  [analysis element]
  (find-last-order-by-project-analysis
    #(and (identical? :namespace-definitions (:bucket %))
          (= (:name %) (:name element))
          (match-file-lang % element))
    analysis))

(defmethod find-definition :var-usages
  [analysis element]
  (letfn [(defines-elem? [candidate]
            (and (identical? :var-definitions (:bucket candidate))
                 (= (:name element) (:name candidate))
                 (= (:to element) (:ns candidate))
                 (not= 'clojure.core/declare (:defined-by candidate))))]
    (or
      (find-last-order-by-project-analysis
        #(and (defines-elem? %)
              (match-file-lang % element))
        analysis)
      (when (contains? (elem-langs element) :cljs)
        ;; maybe loaded by :require-macros, in which case, def will be in a clj file.
        (find-last-order-by-project-analysis
          #(and (defines-elem? %)
                (:macro %)
                (match-file-lang % (assoc element :lang :clj)))
          analysis)))))

(defmethod find-definition :local-usages
  [analysis {:keys [id filename] :as _element}]
  (find-first #(and (= :locals (:bucket %)) (= (:id %) id))
              (get analysis filename)))

(defmethod find-definition :keywords
  [analysis element]
  (or (when (:ns element)
        (find-last-order-by-project-analysis
          #(and (identical? :keywords (:bucket %))
                (= (:name %) (:name element))
                (:reg %)
                (= (:ns %) (:ns element)))
          analysis))
      element))

(defmethod find-definition :var-definitions
  [analysis element]
  (if (= 'potemkin/import-vars (:defined-by element))
    (find-last-order-by-project-analysis
      #(and (identical? :var-definitions (:bucket %))
            (= (:name %) (:name element))
            (not= 'potemkin/import-vars (:defined-by %))
            (match-file-lang % element))
      analysis)
    element))

(defmethod find-definition :protocol-impls
  [analysis element]
  (find-last-order-by-project-analysis
    #(and (identical? :var-definitions (:bucket %))
          (= (:name %) (:method-name element))
          (= (:ns %) (:protocol-ns element))
          (match-file-lang % element))
    analysis))

(defmethod find-definition :java-class-usages
  [analysis element]
  (->> analysis
       (into []
             (comp
               (mapcat val)
               (filter #(and (identical? :java-class-definitions (:bucket %))
                             (safe-equal? (:class %) (:class element))))))
       (sort-by (complement #(string/ends-with? (:filename %) ".java")))
       first))

(defmethod find-definition :default
  [_analysis element]
  element)

(defmulti find-declaration
  (fn [_analysis element]
    (:bucket element)))

(defmethod find-declaration :var-usages
  [analysis {:keys [alias name to filename] :as element}]
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
                (get analysis filename)))))

(defmethod find-declaration :default [_ _] nil)

(defmulti find-implementations
  (fn [_analysis element]
    (:bucket element)))

(defmethod find-implementations :var-definitions
  [analysis element]
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
        analysis))

(defmethod find-implementations :var-usages
  [analysis element]
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
          analysis)))

(defmethod find-implementations :default [_ _] [])

(defmulti find-references
  (fn [_analysis element _include-declaration?]
    (case (:bucket element)
      :locals :local
      :local-usages :local
      (:bucket element))))

(defmethod find-references :namespace-definitions
  [analysis {:keys [name] :as element} include-declaration?]
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
          analysis)))

(defmethod find-references :namespace-usages
  [analysis {:keys [name] :as element} include-declaration?]
  (concat
    (when include-declaration?
      [element])
    (into []
          (comp
            (mapcat val)
            (filter #(or (and (identical? :namespace-definitions (:bucket %))
                              (= (:name %) name))
                         (and (identical? :keywords (:bucket %))
                              (= (:ns %) name)
                              (not (:auto-resolved %))
                              (not (:namespace-from-prefix %))))))
          analysis)))

(defmethod find-references :namespace-alias
  [analysis {:keys [alias filename] :as element} include-declaration?]
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
          (get analysis filename))))

(defmethod find-references :var-usages
  [analysis element include-declaration?]
  (if (= (:to element) :clj-kondo/unknown-namespace)
    [element]
    (let [var-definition {:ns (:to element)
                          :name (:name element)
                          :bucket :var-definitions}]
      (find-references analysis var-definition include-declaration?))))

(defmethod find-references :var-definitions
  [analysis element include-declaration?]
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
          analysis)))

(defmethod find-references :keywords
  [analysis {:keys [ns name] :as _element} include-declaration?]
  (into []
        (comp
          filter-project-analysis-xf
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
        analysis))

(defmethod find-references :local
  [analysis {:keys [id name filename] :as element} include-declaration?]
  (if (or id name)
    (filter #(and (= (:id %) id)
                  (or include-declaration?
                      (not (identical? :locals (:bucket %)))))
            (get analysis filename))
    [element]))

(defmethod find-references :protocol-impls
  [analysis element include-declaration?]
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
          analysis)))

(defmethod find-references :default
  [_analysis element _]
  [element])

(defn find-element-under-cursor
  [analysis filename line column]
  (find-first (fn [{:keys [name-row name-col name-end-row name-end-col]}]
                ;; TODO Probably should use q/inside? instead
                (and (<= name-row line name-end-row)
                     (<= name-col column name-end-col)))
              (get analysis filename)))

(defn find-all-elements-under-cursor
  [analysis filename line column]
  (filter (fn [{:keys [name-row name-col name-end-row name-end-col]}]
                ;; TODO Probably should use q/inside? instead
            (and (<= name-row line name-end-row)
                 (<= name-col column name-end-col)))
          (get analysis filename)))

(defn find-definition-from-cursor [analysis filename line column]
  (try
    (when-let [element (find-element-under-cursor analysis filename line column)]
      (find-definition analysis element))
    (catch Throwable e
      (logger/error e "can't find definition"))))

(defn find-declaration-from-cursor [analysis filename line column]
  (try
    (when-let [element (find-element-under-cursor analysis filename line column)]
      (find-declaration analysis element))
    (catch Throwable e
      (logger/error e "can't find declaration"))))

(defn find-implementations-from-cursor [analysis filename line column]
  (try
    (when-let [element (find-element-under-cursor analysis filename line column)]
      (find-implementations analysis element))
    (catch Throwable e
      (logger/error e "can't find implementation"))))

(defn find-references-from-cursor [analysis filename line column include-declaration?]
  (try
    (when-let [element (find-element-under-cursor analysis filename line column)]
      (find-references analysis element include-declaration?))
    (catch Throwable e
      (logger/error e "can't find references"))))

(defn xf-var-defs [include-private?]
  (comp
    (filter #(and (identical? :var-definitions (:bucket %))
                  (or include-private?
                      (not (get % :private)))))
    (medley/distinct-by (juxt :ns :name :row :col))
    (remove #(or (and (safe-equal? 'clojure.core/defrecord (:defined-by %))
                      (or (string/starts-with? (str (:name %)) "->")
                          (string/starts-with? (str (:name %)) "map->")))
                 (and (safe-equal? 'clojure.core/deftype (:defined-by %))
                      (string/starts-with? (str (:name %)) "->"))))))

(defn find-var-definitions [analysis filename include-private?]
  (into []
        (xf-var-defs include-private?)
        (get analysis filename)))

(defn find-all-var-definitions [analysis]
  (into []
        (comp
          (mapcat val)
          (xf-var-defs false))
        analysis))

(defn find-keyword-definitions [analysis filename]
  (into []
        (comp
          (filter #(and (identical? :keywords (:bucket %))
                        (:reg %)))
          (medley/distinct-by (juxt :ns :name :row :col)))
        (get analysis filename)))

(defn find-all-keyword-definitions [analysis]
  (into []
        (comp
          (mapcat val)
          (filter #(and (identical? :keywords (:bucket %))
                        (:reg %)))
          (medley/distinct-by (juxt :ns :name :row :col)))
        analysis))

(defn find-local-by-destructured-keyword [analysis filename keyword-element]
  (->> (get analysis filename)
       (filter #(and (identical? :locals (:bucket %))
                     (= (:name-row %) (:name-row keyword-element))
                     (= (:name-col %) (:name-col keyword-element))
                     (= (:name-end-row %) (:name-end-row keyword-element))
                     (= (:name-end-col %) (:name-end-col keyword-element))))
       first))

(defn find-all-ns-definition-names-xf []
  (comp
    (mapcat val)
    (filter #(identical? :namespace-definitions (:bucket %)))
    (map :name)))

;; TODO remove it, use only transducer one?
(defn find-all-ns-definition-names [analysis]
  (into #{}
        (find-all-ns-definition-names-xf)
        analysis))

(defn find-all-aliases
  [analysis uri db]
  (let [langs (shared/uri->available-langs uri)]
    (into #{}
          (comp
            filter-project-analysis-xf
            (mapcat val)
            (filter #(identical? :namespace-alias (:bucket %)))
            (filter :alias)
            (filter (fn [element]
                      (seq (set/intersection (-> element
                                                 :filename
                                                 (shared/filename->uri db)
                                                 shared/uri->available-langs)
                                             langs)))))
          analysis)))

(defn find-unused-aliases [analysis findings filename]
  (let [local-analysis (get analysis filename)]
    (into #{}
          (comp
            (filter (comp #(identical? :unused-namespace %) :type))
            (remove (fn [finding]
                      (some #(and (identical? :var-usages (:bucket %))
                                  (not (:refer %))
                                  (safe-equal? (:ns finding) (:to %)))
                            local-analysis)))
            (map :ns))
          (get findings filename))))

(defn find-unused-refers [analysis findings filename]
  (let [local-analysis (get analysis filename)]
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
          (get findings filename))))

(defn find-unused-imports [analysis findings filename]
  (let [local-analysis (get analysis filename)]
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
          (get findings filename))))

(defn find-duplicate-requires [findings filename]
  (into #{}
        (comp
          (filter (comp #(identical? :duplicate-require %) :type))
          (map :duplicate-ns))
        (get findings filename)))

(defn find-namespace-definitions [analysis filename]
  (into []
        (filter #(identical? :namespace-definitions (:bucket %)))
        (get analysis filename)))

(defn find-namespace-definition-by-namespace [analysis namespace]
  (find-last-order-by-project-analysis
    #(and (identical? :namespace-definitions (:bucket %))
          (= (:name %) namespace))
    analysis))

(defn find-namespace-definition-by-filename [analysis filename]
  (peek (find-namespace-definitions analysis filename)))

(defn find-namespace-usage-by-alias [analysis filename alias]
  (->> (get analysis filename)
       (filter #(and (identical? :namespace-usages (:bucket %))
                     (= alias (:alias %))))
       last))

(defn find-element-by-full-name [analysis name ns]
  (find-last-order-by-project-analysis
    #(and (identical? :var-definitions (:bucket %))
          (= ns (:ns %))
          (= name (:name %)))
    analysis))

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
