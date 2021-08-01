(ns clojure-lsp.queries
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [clojure.string :as string]
   [medley.core :as medley]
   [taoensso.timbre :as log]))

(defn ^:private safe-equal?
  "Fast equals for string and symbols."
  [a b]
  (if (instance? clojure.lang.Symbol a)
    (.equals ^clojure.lang.Symbol a b)
    (.equals ^String a b)))

(defn inside?
  [start-l start-c
   check-l check-c
   end-l end-c]
  (and (or (< start-l check-l)
           (and (= start-l check-l)
                (<= start-c check-c)))
       (or (< check-l end-l)
           (and (= check-l end-l)
                (<= check-c end-c)))))

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

(defn filter-project-analysis [analysis]
  (->> analysis
       (remove-keys #(not (string/starts-with? (-> % name shared/filename->uri) "file://")))))

(defn filter-external-analysis [analysis]
  (->> analysis
       (remove-keys #(string/starts-with? (-> % name shared/filename->uri) "file://"))))

(defn ^:private find-last-order-by-project-analysis [pred? analysis]
  (or (find-last pred? (mapcat val (filter-project-analysis analysis)))
      (find-last pred? (mapcat val (filter-external-analysis analysis)))))

(defn ^:private match-file-lang
  [check-element match-element]
  (let [match-file-lang (shared/uri->available-langs (:filename match-element))
        check-file-lang (shared/uri->available-langs (:filename check-element))]
    (seq (set/intersection match-file-lang check-file-lang))))

(defn ^:private defrecord-names-for [{:keys [name]}]
  #{name
    (symbol (str "->" name))
    (symbol (str "map->" name))})

(defn find-local-usages-under-form
  [analysis filename line column end-line end-column]
  (let [local-analysis (get analysis filename)]
    (->>  (filter (fn [{:keys [name-row name-col name-end-row name-end-col scope-end-row scope-end-col bucket]}]
                    (and (= :locals bucket)
                         (inside?
                           name-row name-col
                           line column
                           (or scope-end-row name-end-row)
                           (or scope-end-col name-end-col))))
                  local-analysis)
          (map (fn [local]
                 (find-first #(and (= :local-usages (:bucket %))
                                   (= (:id local) (:id %))
                                   (inside?
                                     line column
                                     (:name-row %) (:name-col %)
                                     end-line end-column))
                             local-analysis)))
          (remove nil?))))

(defn find-var-usages-under-form
  [analysis filename line column end-line end-column]
  (let [local-analysis (get analysis filename)]
    (filter (fn [{:keys [name-row name-col bucket]}]
              (and (= :var-usages bucket)
                   (inside?
                     line column
                     name-row name-col
                     end-line end-column)))
            local-analysis)))

(defmulti find-definition
  (fn [_analysis element]
    (:bucket element)))

(defmethod find-definition :namespace-usages
  [analysis element]
  (find-last-order-by-project-analysis
    #(and (= (:bucket %) :namespace-definitions)
          (= (:name %) (:name element))
          (match-file-lang % element))
    analysis))

(defmethod find-definition :var-usages
  [analysis element]
  (find-last-order-by-project-analysis
    #(and (= (:bucket %) :var-definitions)
          (= (:name %) (:name element))
          (not (= (:defined-by %) 'clojure.core/declare))
          (= (:ns %) (:to element))
          (match-file-lang % element))
    analysis))

(defmethod find-definition :local-usages
  [analysis {:keys [id filename] :as _element}]
  (find-first #(and (= :locals (:bucket %)) (= (:id %) id))
              (get analysis filename)))

(defmethod find-definition :keywords
  [analysis element]
  (when (:ns element)
    (find-last-order-by-project-analysis
      #(and (= (:bucket %) :keywords)
            (= (:name %) (:name element))
            (:reg %)
            (= (:ns %) (:ns element)))
      analysis)))

(defmethod find-definition :default
  [_analysis element]
  element)

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
            (filter #(= (:name %) name))
            (filter #(identical? :namespace-usages (:bucket %))))
          analysis)))

(defmethod find-references :namespace-alias
  [analysis {:keys [alias filename] :as element} include-declaration?]
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
  [analysis element include-declaration?]
  (if (= (:to element) :clj-kondo/unknown-namespace)
    [element]
    (into []
          (comp
            (mapcat val)
            (filter #(not (identical? :keywords (:bucket %))))
            (filter #(safe-equal? (:name element) (:name %)))
            (filter #(safe-equal? (:to element) (or (:ns %) (:to %))))
            (filter #(or include-declaration?
                         (not (identical? :var-definitions (:bucket %)))))
            (medley/distinct-by (juxt :filename :name :row :col)))
          analysis)))

(defmethod find-references :var-definitions
  [analysis element include-declaration?]
  (let [defrecord? (= 'clojure.core/defrecord (:defined-by element))
        names (when defrecord? (defrecord-names-for element))]
    (into []
          (comp
            (mapcat val)
            (filter #(not (identical? :keywords (:bucket %))))
            (filter #(or (safe-equal? (:name element) (:name %))
                         (and defrecord?
                              (contains? names (:name %)))))
            (filter #(safe-equal? (:ns element) (or (:ns %) (:to %))))
            (filter #(or include-declaration?
                         (not (identical? :var-definitions (:bucket %)))
                         (= (:defined-by %) 'clojure.core/declare)))
            (medley/distinct-by (juxt :filename :name :row :col)))
          analysis)))

(defmethod find-references :keywords
  [analysis {:keys [ns name] :as _element} include-declaration?]
  (let [project-analysis (filter-project-analysis analysis)]
    (into []
          (if ns
            (comp
              (mapcat val)
              (filter #(identical? :keywords (:bucket %)))
              (filter #(safe-equal? name (:name %)))
              (filter #(safe-equal? ns (:ns %)))
              (filter #(not (:keys-destructuring %)))
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
  [analysis {:keys [id filename] :as _element} include-declaration?]
  (filter #(and (= (:id %) id)
                (or include-declaration?
                    (not (identical? :locals (:bucket %)))))
          (get analysis filename)))

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

(defn find-definition-from-cursor [analysis filename line column]
  (try
    (when-let [element (find-element-under-cursor analysis filename line column)]
      (find-definition analysis element))
    (catch Throwable e
      (log/error e "can't find definition"))))

(defn find-references-from-cursor [analysis filename line column include-declaration?]
  (try
    (when-let [element (find-element-under-cursor analysis filename line column)]
      (find-references analysis element include-declaration?))
    (catch Throwable e
      (log/error e "can't find references"))))

(defn find-var-definitions [analysis filename include-private?]
  (->> (get analysis filename)
       (filter #(and (identical? :var-definitions (:bucket %))
                     (or include-private?
                         (not (get % :private)))))
       (medley/distinct-by (juxt :ns :name :row :col))))

(defn find-all-var-definitions [analysis]
  (into []
        (comp
          (mapcat val)
          (filter #(and (identical? :var-definitions (:bucket %))
                        (not (get % :private)))))
        analysis))

(defn find-keyword-definitions [analysis filename]
  (->> (get analysis filename)
       (filter #(and (identical? :keywords (:bucket %))
                     (:reg %)))
       (medley/distinct-by (juxt :ns :name :row :col))))

(defn find-all-ns-definitions [analysis]
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

(defn find-unused-aliases [findings filename]
  (->> (get findings filename)
       (filter (comp #(= % :unused-namespace) :type))
       (map :ns)
       set))

(defn find-unused-refers [findings filename]
  (->> (get findings filename)
       (filter (comp #(= % :unused-referred-var) :type))
       (map #(symbol (-> % :ns str) (-> % :refer str)))
       set))

(defn find-unused-imports [findings filename]
  (->> (get findings filename)
       (filter (comp #(= % :unused-import) :type))
       (map :class)
       set))

(defn find-namespace-definition-by-namespace [analysis namespace]
  (find-last-order-by-project-analysis
    #(and (= (:bucket %) :namespace-definitions)
          (= (:name %) namespace))
    analysis))

(defn find-element-by-full-name [analysis name ns]
  (find-last-order-by-project-analysis
    #(and (= :var-definitions (:bucket %))
          (= ns (:ns %))
          (= name (:name %)))
    analysis))
