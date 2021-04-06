(ns clojure-lsp.queries
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [clojure.string :as string]
   [medley.core :as medley]
   [taoensso.timbre :as log]))

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

(defn ^:private remove-keys [pred m]
  (apply dissoc m (filter pred (keys m))))

(defn filter-project-analysis [analysis]
  (->> analysis
       (remove-keys #(not (string/starts-with? (-> % name shared/filename->uri) "file://")))))

(defn filter-external-analysis [analysis]
  (->> analysis
       (remove-keys #(string/starts-with? (-> % name shared/filename->uri) "file://"))))

(defn ^:private find-first-order-by-project-analysis [pred? analysis]
  (or (find-first pred? (mapcat val (filter-project-analysis analysis)))
      (find-first pred? (mapcat val (filter-external-analysis analysis)))))

(defn ^:private match-file-lang
  [check-element match-element]
  (let [match-file-lang (shared/uri->available-langs (:filename match-element))
        check-file-lang (shared/uri->available-langs (:filename check-element))]
    (seq (set/intersection match-file-lang check-file-lang))))

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
  (find-first-order-by-project-analysis
    #(and (= (:bucket %) :namespace-definitions)
          (= (:name %) (:name element))
          (match-file-lang % element))
    analysis))

(defmethod find-definition :var-usages
  [analysis element]
  (find-first-order-by-project-analysis
    #(and (= (:bucket %) :var-definitions)
          (= (:name %) (:name element))
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
    (find-first-order-by-project-analysis
      #(and (= (:bucket %) :keywords)
            (= (:name %) (:name element))
            (:def %)
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
    (filter #(and (= (:name %) name)
                  (= :namespace-usages (:bucket %)))
            (mapcat val analysis))))

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
    (->> (mapcat val analysis)
         (filter #(and (= (:name %) (:name element))
                       (= (or (:ns %) (:to %)) (:to element))
                       (not= :keywords (:bucket %))
                       (or include-declaration?
                         (not= :var-definitions (:bucket %)))))
         (medley/distinct-by (juxt :filename :name :row :col)))))

(defmethod find-references :var-definitions
  [analysis element include-declaration?]
  (->> (mapcat val analysis)
       (filter #(and (= (:name %) (:name element))
                     (= (or (:ns %) (:to %))
                        (:ns element))
                     (not= :keywords (:bucket %))
                     (or include-declaration?
                         (not= :var-definitions (:bucket %)))))
       (medley/distinct-by (juxt :filename :name :row :col))))

(defmethod find-references :keywords
  [analysis {:keys [ns name] :as _element} include-declaration?]
  (->> (let [project-analysis (filter-project-analysis analysis)]
         (if ns
           (filter #(and (= :keywords (:bucket %))
                         (= (:name %) name)
                         (= (:ns %) ns)
                         (not (:keys-destructuring %))
                         (or include-declaration?
                           (not (:def %))))
                   (mapcat val project-analysis))
           (filter #(and (= :keywords (:bucket %))
                         (= (:name %) name)
                         (not (:ns %)))
                   (mapcat val project-analysis))))
       (medley/distinct-by (juxt :filename :name :row :col))))

(defmethod find-references :local
  [analysis {:keys [id filename] :as _element} include-declaration?]
  (filter #(and (= (:id %) id)
                (or include-declaration? (not= :locals (:bucket %))))
          (get analysis filename)))

(defmethod find-references :default
  [_analysis element _]
  [element])

(defn find-element-under-cursor
  [analysis filename line column]
  (let [local-analysis (get analysis filename)]
    (find-first (fn [{:keys [name-row name-col name-end-row name-end-col] :as _v}]
                  ;; TODO Probably should use q/inside? instead
                  (and (<= name-row line name-end-row)
                       (<= name-col column name-end-col)))
                local-analysis)))

(defn find-definition-from-cursor [analysis filename line column]
  (try
    (let [element (find-element-under-cursor analysis filename line column)]
      (when element
        (find-definition analysis element)))
    (catch Throwable e
      (log/error e "can't find definition"))))

(defn find-references-from-cursor [analysis filename line column include-declaration?]
  (try
    (let [element (find-element-under-cursor analysis filename line column)]
      (when element
        (find-references analysis element include-declaration?)))
    (catch Throwable e
      (log/error e "can't find references"))))

(defn find-vars [analysis filename include-private?]
  (->> (get analysis filename)
       (filter #(and (= (:bucket %) :var-definitions)
                     (or include-private?
                       (not (get % :private)))))
       (medley/distinct-by (juxt :ns :name :row :col))))

(defn find-all-ns-definitions [analysis]
  (->> (mapcat val analysis)
       (filter (comp #(= % :namespace-definitions) :bucket))
       (map :name)
       set))

(defn find-all-aliases [analysis]
  (filter #(and (= (:bucket %) :namespace-alias)
                (:alias %))
          (mapcat val analysis)))

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
