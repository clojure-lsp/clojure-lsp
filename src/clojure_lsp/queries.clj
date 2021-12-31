(ns clojure-lsp.queries
  (:require
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [clojure.string :as string]
   [medley.core :as medley]
   [taoensso.timbre :as log]))

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

(defn- find-last [pred coll]
  (find-first pred (reverse coll)))

(defn ^:private remove-keys [pred m]
  (apply dissoc m (filter pred (keys m))))

(defn filter-project-analysis [analysis db]
  (let [source-paths (settings/get db [:source-paths])]
    (->> analysis
         (remove-keys #(shared/external-filename? % source-paths)))))

(defn filter-external-analysis [analysis db]
  (let [source-paths (settings/get db [:source-paths])]
    (->> analysis
         (remove-keys (complement #(shared/external-filename? % source-paths))))))

(defn ^:private find-last-order-by-project-analysis [pred? analysis db]
  (or (find-last pred? (mapcat val (filter-project-analysis analysis db)))
      (find-last pred? (mapcat val (filter-external-analysis analysis db)))))

(defn ^:private match-file-lang
  [check-element match-element]
  (let [match-file-lang (or (some-> match-element :lang list set)
                            (shared/uri->available-langs (:filename match-element)))
        check-file-lang (or (some-> check-element :lang list set)
                            (shared/uri->available-langs (:filename check-element)))]
    (seq (set/intersection match-file-lang check-file-lang))))

(defn ^:private defrecord-names-for [{:keys [name]}]
  #{name
    (symbol (str "->" name))
    (symbol (str "map->" name))})

(defn ^:private deftype-names-for [{:keys [name]}]
  #{name (symbol (str "->" name))})

(defn find-local-usages-under-form
  [analysis filename line column end-line end-column]
  (let [local-analysis (get analysis filename)]
    (->> local-analysis
         (filter (fn [{:keys [bucket] :as element}]
                   (and (= :locals bucket)
                        (shared/inside? {:name-row line :name-col column} element))))
         (map (fn [local]
                (find-first #(and (= :local-usages (:bucket %))
                                  (= (:id local) (:id %))
                                  (shared/inside? %
                                                  {:name-row line
                                                   :name-col column
                                                   :name-end-row end-line
                                                   :name-end-col end-column}))
                            local-analysis)))
         (remove nil?))))

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
  (fn [_analysis element _db]
    (:bucket element)))

(defmethod find-definition :namespace-usages
  [analysis element db]
  (find-last-order-by-project-analysis
    #(and (= (:bucket %) :namespace-definitions)
          (= (:name %) (:name element))
          (match-file-lang % element))
    analysis
    db))

(defmethod find-definition :var-usages
  [analysis element db]
  (find-last-order-by-project-analysis
    #(and (= (:bucket %) :var-definitions)
          (= (:name %) (:name element))
          (not (= (:defined-by %) 'clojure.core/declare))
          (= (:ns %) (:to element))
          (match-file-lang % element))
    analysis
    db))

(defmethod find-definition :local-usages
  [analysis {:keys [id filename] :as _element} _db]
  (find-first #(and (= :locals (:bucket %)) (= (:id %) id))
              (get analysis filename)))

(defmethod find-definition :keywords
  [analysis element db]
  (or (when (:ns element)
        (find-last-order-by-project-analysis
          #(and (= (:bucket %) :keywords)
                (= (:name %) (:name element))
                (:reg %)
                (= (:ns %) (:ns element)))
          analysis
          db))
      element))

(defmethod find-definition :var-definitions
  [analysis element db]
  (if (= 'potemkin/import-vars (:defined-by element))
    (find-last-order-by-project-analysis
      #(and (= (:bucket %) :var-definitions)
            (= (:name %) (:name element))
            (not= 'potemkin/import-vars (:defined-by %))
            (match-file-lang % element))
      analysis
      db)
    element))

(defmethod find-definition :default
  [_analysis element _db]
  element)

(defmulti find-references
  (fn [_analysis element _include-declaration? _db]
    (case (:bucket element)
      :locals :local
      :local-usages :local
      (:bucket element))))

(defmethod find-references :namespace-definitions
  [analysis {:keys [name] :as element} include-declaration? _db]
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
  [analysis {:keys [name] :as element} include-declaration? _db]
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
  [analysis {:keys [alias filename] :as element} include-declaration? _db]
  (concat
    (when include-declaration?
      [element])
    (->> (get analysis filename)
         (filter #(or (and (= :var-usages (:bucket %))
                           (= (:alias %) alias))
                      (and (= :keywords (:bucket %))
                           (= (:alias %) alias))))
         (medley/distinct-by (juxt :filename :name :name-row :name-col)))))

(defmethod find-references :var-usages
  [analysis element include-declaration? _db]
  (if (= (:to element) :clj-kondo/unknown-namespace)
    [element]
    (into []
          (comp
            (mapcat val)
            (filter #(not (identical? :keywords (:bucket %))))
            (filter #(safe-equal? (:name element) (:name %)))
            (filter #(safe-equal? (:to element) (or (:ns %) (:to %))))
            (filter #(or include-declaration?
                         (and (or (not (:from-var %))
                                  (not= (:from-var %) (:name element))
                                  (not= (:from %) (:to element)))
                              (not (identical? :var-definitions (:bucket %))))))
            (medley/distinct-by (juxt :filename :name :row :col)))
          analysis)))

(defmethod find-references :var-definitions
  [analysis element include-declaration? _db]
  (let [defrecord? (= 'clojure.core/defrecord (:defined-by element))
        deftype? (= 'clojure.core/deftype (:defined-by element))
        names (cond
                defrecord? (defrecord-names-for element)
                deftype? (deftype-names-for element))]
    (into []
          (comp
            (mapcat val)
            (filter #(not (identical? :keywords (:bucket %))))
            (filter #(or (safe-equal? (:name element) (:name %))
                         (and (or defrecord? deftype?)
                              (contains? names (:name %)))))
            (filter #(safe-equal? (:ns element) (or (:ns %) (:to %))))
            (filter #(or include-declaration?
                         (and (or (not (identical? :var-definitions (:bucket %)))
                                  (= (:defined-by %) 'clojure.core/declare))
                              (or (not (:from-var %))
                                  (not= (:from-var %) (:name element))
                                  (not= (:from %) (:ns element))))))
            (medley/distinct-by (juxt :filename :name :row :col)))
          analysis)))

(defmethod find-references :keywords
  [analysis {:keys [ns name] :as _element} include-declaration? db]
  (let [project-analysis (filter-project-analysis analysis db)]
    (into []
          (if ns
            (comp
              (mapcat val)
              (filter #(identical? :keywords (:bucket %)))
              (filter #(safe-equal? name (:name %)))
              (filter #(safe-equal? ns (:ns %)))
              (filter #(or include-declaration?
                           (not (:reg %))))
              (medley/distinct-by (juxt :filename :name :row :col)))
            (comp
              (mapcat val)
              (filter #(identical? :keywords (:bucket %)))
              (filter #(safe-equal? name (:name %)))
              (filter #(not (:ns %)))
              (medley/distinct-by (juxt :filename :name :row :col))))
          project-analysis)))

(defmethod find-references :local
  [analysis {:keys [id filename] :as _element} include-declaration? _db]
  (filter #(and (= (:id %) id)
                (or include-declaration?
                    (not (identical? :locals (:bucket %)))))
          (get analysis filename)))

(defmethod find-references :default
  [_analysis element _ _]
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

(defn find-definition-from-cursor [analysis filename line column db]
  (try
    (when-let [element (find-element-under-cursor analysis filename line column)]
      (find-definition analysis element db))
    (catch Throwable e
      (log/error e "can't find definition"))))

(defn find-references-from-cursor [analysis filename line column include-declaration? db]
  (try
    (when-let [element (find-element-under-cursor analysis filename line column)]
      (find-references analysis element include-declaration? db))
    (catch Throwable e
      (log/error e "can't find references"))))

(defn find-var-definitions [analysis filename include-private?]
  (->> (get analysis filename)
       (filter #(and (identical? :var-definitions (:bucket %))
                     (or include-private?
                         (not (get % :private)))))
       (medley/distinct-by (juxt :ns :name :row :col))
       (remove #(or (and (= 'clojure.core/defrecord (:defined-by %))
                         (or (string/starts-with? (str (:name %)) "->")
                             (string/starts-with? (str (:name %)) "map->")))
                    (and (= 'clojure.core/deftype (:defined-by %))
                         (string/starts-with? (str (:name %)) "->"))))))

(defn find-all-var-definitions [analysis]
  (into []
        (comp
          (mapcat val)
          (filter #(and (identical? :var-definitions (:bucket %))
                        (not (get % :private))))
          (medley/distinct-by (juxt :ns :name :row :col))
          (remove #(or (and (= 'clojure.core/defrecord (:defined-by %))
                            (or (string/starts-with? (str (:name %)) "->")
                                (string/starts-with? (str (:name %)) "map->")))
                       (and (= 'clojure.core/deftype (:defined-by %))
                            (string/starts-with? (str (:name %)) "->")))))
        analysis))

(defn find-keyword-definitions [analysis filename]
  (->> (get analysis filename)
       (filter #(and (identical? :keywords (:bucket %))
                     (:reg %)))
       (medley/distinct-by (juxt :ns :name :row :col))))

(defn find-local-by-destructured-keyword [analysis filename keyword-element]
  (->> (get analysis filename)
       (filter #(and (identical? :locals (:bucket %))
                     (= (:name-row %) (:name-row keyword-element))
                     (= (:name-col %) (:name-col keyword-element))
                     (= (:name-end-row %) (:name-end-row keyword-element))
                     (= (:name-end-col %) (:name-end-col keyword-element))))
       first))

(defn find-all-ns-definition-names [analysis]
  (into #{}
        (comp
          (mapcat val)
          (filter #(identical? :namespace-definitions (:bucket %)))
          (map :name))
        analysis))

(defn find-all-aliases [analysis]
  (into #{}
        (comp
          (mapcat val)
          (filter #(identical? :namespace-alias (:bucket %)))
          (filter :alias))
        analysis))

(defn find-unused-aliases [analysis findings filename]
  (let [local-analysis (get analysis filename)]
    (->> (get findings filename)
         (filter (comp #(= % :unused-namespace) :type))
         (remove (fn [finding]
                   (some #(and (= :var-usages (:bucket %))
                               (not (:refer %))
                               (= (:ns finding) (:to %)))
                         local-analysis)))
         (map :ns)
         set)))

(defn find-unused-refers [analysis findings filename]
  (let [local-analysis (get analysis filename)]
    (->> (get findings filename)
         (filter (comp #(= % :unused-referred-var) :type))
         (remove (fn [finding]
                   (> (->> local-analysis
                           (filter #(and (= :var-usages (:bucket %))
                                         (= (:refer finding) (:name %))
                                         (= (:ns finding) (:to %))))
                           (medley/distinct-by (juxt :name :to :row :col :end-row :end-col))
                           count)
                      1)))
         (map #(symbol (-> % :ns str) (-> % :refer str)))
         set)))

(defn find-unused-imports [analysis findings filename]
  (let [local-analysis (get analysis filename)]
    (->> (get findings filename)
         (filter (comp #(= % :unused-import) :type))
         (remove (fn [finding]
                   (some #(and (= :var-usages (:bucket %))
                               (= (str (:class finding)) (str (:to %) "." (:name %))))
                         local-analysis)))
         (map :class)
         set)))

(defn find-duplicate-requires [findings filename]
  (->> (get findings filename)
       (filter (comp #(identical? :duplicate-require %) :type))
       (map :duplicate-ns)
       set))

(defn find-namespace-definitions [analysis filename]
  (into []
        (comp
          (mapcat val)
          (filter #(= (:filename %) filename))
          (filter #(identical? :namespace-definitions (:bucket %))))
        analysis))

(defn find-namespace-definition-by-namespace [analysis namespace db]
  (find-last-order-by-project-analysis
    #(and (identical? :namespace-definitions (:bucket %))
          (= (:name %) namespace))
    analysis
    db))

(defn find-namespace-definition-by-filename [analysis filename db]
  (find-last-order-by-project-analysis
    #(and (identical? :namespace-definitions (:bucket %))
          (= (:filename %) filename))
    analysis
    db))

(defn find-namespace-usage-by-alias [analysis filename alias]
  (->> (get analysis filename)
       (filter #(and (identical? :namespace-usages (:bucket %))
                     (= alias (:alias %))))
       last))

(defn find-element-by-full-name [analysis name ns db]
  (find-last-order-by-project-analysis
    #(and (identical? :var-definitions (:bucket %))
          (= ns (:ns %))
          (= name (:name %)))
    analysis
    db))
