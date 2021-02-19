(ns clojure-lsp.feature.completion
  (:require
    [clojure-lsp.clojure-core :as cc]
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.hover :as f.hover]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.queries :as q]
    [clojure-lsp.refactor.edit :as edit]
    [clojure-lsp.refactor.transform :as r.transform]
    [clojure-lsp.shared :as shared]
    [clojure.string :as string]
    [rewrite-clj.zip :as z]
    [taoensso.timbre :as log]))

(defn ^:private keyword-element->str [{:keys [alias ns] :as element}]
  (cond-> ":"
    alias
    (str ":")

    (or alias ns)
    (str (or alias ns) "/")

    :always
    (str (:name element))))

(defn ^:private matches-cursor? [cursor-value s]
  (when (and s cursor-value)
    (let [cursor-value-str (if (symbol? cursor-value)
                             (name cursor-value)
                             (str cursor-value))]
      (when (string/starts-with? s cursor-value-str)
        s))))

(defn ^:private supports-cljs? [uri]
  (#{:cljc :cljs} (shared/uri->file-type uri)))

(defn ^:private supports-clj-core? [uri]
  (#{:cljc :clj} (shared/uri->file-type uri)))

(defn ^:private element->completion-item-kind [{:keys [bucket fixed-arities]}]
  (cond
    (#{:namespace-definitions
       :namespace-usages
       :namespace-alias} bucket)
    :module

    (#{:keywords} bucket)
    :keyword

    (and (#{:var-definitions} bucket)
         fixed-arities)
    :function

    (#{:var-definitions :var-usages} bucket)
    :variable

    (#{:locals :localusages} bucket)
    :value

    :else
    :text))

(defn ^:private element->label [{:keys [alias bucket] :as element} cursor-alias]
  (cond
    (= :keywords bucket)
    (keyword-element->str element)

    (#{:namespace-alias :namespace-usages} bucket)
    (some-> alias name)

    cursor-alias
    (str cursor-alias "/" (-> element :name name))

    :else
    (-> element :name name)))

(defn ^:private valid-element-completion-item?
  [matches-fn
   cursor-uri
   {cursor-from :from cursor-bucket :bucket :as cursor-element}
   {:keys [bucket to ns filename lang name alias] :as element}]
  (let [supported-file-types #{:cljc (shared/uri->file-type cursor-uri)}]
    (cond
      (#{:var-usages :local-usages :namespace-usages} bucket)
      false

      (and (= bucket :locals)
           (not= filename (shared/uri->filename cursor-uri)))
      false

      (and (= bucket :var-definitions)
           (= cursor-bucket :var-usages)
           (not= ns cursor-from))
      false

      (= :clj-kondo/unknown-namespace to)
      false

      (and lang
           (not (supported-file-types lang)))
      false

      (and (= bucket :keywords)
           (= filename (:filename cursor-element))
           (parser/same-range? cursor-element element)) ;; is the same keyword
      false

      (and (= bucket :keywords)
           (or (matches-fn (keyword-element->str element))
               (and ns (matches-fn (str ns)))
               (and alias (matches-fn (str alias)))))
      true

      (or (and name (matches-fn name))
          (and alias (matches-fn alias)))
      true)))

(defn ^:private element->completion-item
  [{:keys [deprecated ns bucket arglist-strs] :as element} cursor-alias]
  (let [kind (element->completion-item-kind element)
        definition? (contains? #{:namespace-definitions :var-definitions} bucket)
        detail (when (not definition?)
                 (cond
                   (= :namespace-alias bucket)
                   (some-> element :to name)

                   :else
                   (string/join
                     "\n"
                     (cond-> []
                       ns (conj (str (name ns) "/" (name (:name element))))
                       arglist-strs (conj (string/join " " arglist-strs))))))]
    (cond-> {:label (element->label element cursor-alias)}
      deprecated (assoc :tags [1])
      kind (assoc :kind kind)
      detail (assoc :detail detail)
      definition? (assoc :documentation (f.hover/hover-documentation element))
      )))

(defn ^:private with-element-items [matches-fn cursor-uri cursor-element elements]
  (->> elements
       (filter (partial valid-element-completion-item? matches-fn cursor-uri cursor-element))
       (map #(element->completion-item % nil))))

(defn ^:private with-ns-definition-elements [matches-fn other-ns-elements]
  (->> other-ns-elements
       (filter #(and (= :namespace-definitions (:bucket %))
                     (matches-fn (:name %))))
       (map #(element->completion-item % nil))))

(defn ^:private with-elements-from-alias [cursor-loc cursor-alias cursor-value matches-fn analysis]
  (when-let [aliases (some->> analysis
                              (q/filter-project-analysis)
                              (mapcat val)
                              (filter #(and (= (:bucket %) :namespace-alias))))]
    (let [alias-namespaces (->> aliases
                                (filter #(= (-> % :alias str) cursor-alias))
                                (map :to)
                                seq
                                set)]
      (concat
        (when (simple-ident? cursor-value)
          (->> aliases
               (filterv (fn [element]
                         (or
                           (matches-fn (:alias element))
                           (matches-fn (:to element)))))
               (mapv
                 (fn [element]
                   (let [require-edit (some-> cursor-loc
                                              (r.transform/add-known-libspec (symbol (str (:alias element)))
                                                                             (symbol (str (:to element))))
                                              (r.transform/result))]
                     (cond-> (element->completion-item element nil)
                       (seq require-edit) (assoc :additional-text-edits (mapv #(update % :range shared/->range) require-edit))))))))
        (->> analysis
             (mapcat val)
             (keep
               #(when (and (= (:bucket %) :var-definitions)
                           (not (:private %))
                           (contains? alias-namespaces (:ns %))
                           (or (simple-ident? cursor-value) (matches-fn (:name %))))
                  [(:ns %) (element->completion-item % cursor-alias)]))
             set
             (map
               (fn [[element-ns completion-item]]
                 (let [require-edit (some-> cursor-loc
                                            (r.transform/add-known-libspec (symbol cursor-alias) element-ns)
                                            (r.transform/result))]
                   (cond-> completion-item
                     (seq require-edit) (assoc :additional-text-edits (mapv #(update % :range shared/->range) require-edit)))))))))))

(defn ^:private with-clojure-core-items [matches-fn]
  (->> cc/core-syms
       (filter (comp matches-fn str))
       (map (fn [sym] {:label (str sym)
                       :detail (str "clojure.core/" sym)}))))

(defn ^:private with-clojurescript-items [matches-fn]
  (->> cc/cljs-syms
       (filter (comp matches-fn str))
       (map (fn [sym] {:label (str sym)
                       :detail (str "cljs.core/" sym)}))))

(defn ^:private with-java-items [matches-fn]
  (concat
    (->> cc/java-lang-syms
         (filter (comp matches-fn str))
         (map (fn [sym] {:label (str sym)
                         :detail (str "java.lang." sym)})))
    (->> cc/java-util-syms
         (filter (comp matches-fn str))
         (map (fn [sym] {:label (str sym)
                         :detail (str "java.util." sym)})))))

(defn completion [uri row col]
  (let [filename (shared/uri->filename uri)
        {:keys [text]} (get-in @db/db [:documents uri])
        analysis (get @db/db :analysis)
        current-ns-elements (get analysis filename)
        other-ns-elements (->> (dissoc analysis filename)
                               q/filter-project-analysis
                               (mapcat val))
        external-ns-elements (->> (dissoc analysis filename)
                                  q/filter-external-analysis
                                  (mapcat val))
        cursor-loc (parser/safe-loc-at-pos text row col)
        cursor-element (loop [try-column col]
                         (if-let [usage (q/find-element-under-cursor analysis filename row col)]
                           usage
                           (when (pos? try-column)
                             (recur (dec try-column)))))
        cursor-value (if cursor-loc
                       (z/sexpr cursor-loc)
                       (:name cursor-element))
        matches-fn (partial matches-cursor? cursor-value)
        inside-require? (edit/inside-require? cursor-loc)
        simple-cursor? (simple-ident? cursor-value)
        cursor-value-or-ns (if (qualified-ident? cursor-value)
                             (namespace cursor-value)
                             (some-> cursor-value name))]
    (if inside-require?
      (->> (with-ns-definition-elements matches-fn (concat other-ns-elements external-ns-elements))
           (into #{})
           (sort-by :label)
           not-empty)
      (cond-> #{}
        cursor-value-or-ns
        (into (with-elements-from-alias cursor-loc cursor-value-or-ns cursor-value matches-fn analysis))

        simple-cursor?
        (->
          (into (with-element-items matches-fn uri cursor-element current-ns-elements))
          (into (with-clojure-core-items matches-fn)))

        (and simple-cursor?
             (supports-cljs? uri))
        (into (with-clojurescript-items matches-fn))

        (and simple-cursor?
             (supports-clj-core? uri))
        (into (with-java-items matches-fn))

        :always
        (->>
          (sort-by :label)
          not-empty)))))
