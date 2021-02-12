(ns clojure-lsp.feature.completion
  (:require
    [clojure-lsp.clojure-core :as cc]
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.hover :as f.hover]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.queries :as q]
    [clojure-lsp.shared :as shared]
    [clojure.string :as string]
    [rewrite-clj.zip :as z]
    [taoensso.timbre :as log]
    [clojure-lsp.refactor.edit :as edit]))

(defn ^:private remove-keys [pred m]
  (apply dissoc m (filter pred (keys m))))

(defn ^:private keyword-element->str [{:keys [name alias ns]}]
  (let [kw (cond
             alias
             (keyword (str alias) name)

             ns
             (keyword (str ns) name)

             :else
             (keyword name))]
    (if alias
      (str ":" kw)
      (str kw))))

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

(defn ^:private element->completion-item
  [{:keys [deprecated ns bucket arglist-strs] :as element}
   alias]
  (let [kind (element->completion-item-kind element)
        detail (or (when arglist-strs (string/join " " arglist-strs))
                   (some-> ns name))
        definition? (#{:namespace-definitions :var-definitions} bucket)]
    (-> {:label (element->label element alias)}
        (cond-> detail (assoc :detail detail)
                deprecated (assoc :tags [1])
                kind (assoc :kind kind)
                definition? (assoc :documentation (f.hover/hover-documentation element))))))

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
               (and ns
                    (matches-fn (str ns)))
               (and alias
                    (matches-fn (str alias)))))
      true

      (or (and name (matches-fn name))
          (and alias (matches-fn alias)))
      true)))

(defn ^:private with-element-items [elements matches-fn cursor-uri cursor-element]
  (->> elements
       (filter (partial valid-element-completion-item? matches-fn cursor-uri cursor-element))
       (map #(element->completion-item % nil))
       (sort-by :label)))

(defn ^:private with-ns-definition-elements [matches-fn other-ns-elements]
  (->> other-ns-elements
       (filter #(and (= :namespace-definitions (:bucket %))
                     (matches-fn (:name %))))
       (map #(element->completion-item % nil))
       (sort-by :label)))


(defn ^:private with-elements-from-alias [alias matches-fn ns-elements other-elements]
  (when-let [alias-ns (some->> ns-elements
                               (q/find-first #(and (= (:bucket %) :namespace-usages)
                                                   (= (-> % :alias str) alias)))
                               :name)]
    (->> other-elements
         (filter #(and (= (:bucket %) :var-definitions)
                       (= (:ns %) alias-ns)
                       (matches-fn (:name %))))
         (map #(element->completion-item % alias))
         (sort-by :label))))

(defn ^:private with-clojure-core-items [matches-fn]
  (->> cc/core-syms
       (filter (comp matches-fn str))
       (map (fn [sym] {:label (str sym)
                       :detail "clojure.core"}))
       (sort-by :label)))

(defn ^:private with-clojurescript-items [matches-fn]
  (->> cc/cljs-syms
       (filter (comp matches-fn str))
       (map (fn [sym] {:label (str sym)
                       :detail "cljs.core"}))
       (sort-by :label)))

(defn ^:private with-java-items [matches-fn]
  (concat
    (->> cc/java-lang-syms
         (filter (comp matches-fn str))
         (map (fn [sym] {:label  (str sym)
                         :detail "java.lang"}))
         (sort-by :label))
    (->> cc/java-util-syms
         (filter (comp matches-fn str))
         (map (fn [sym] {:label  (str sym)
                         :detail "java.util"}))
         (sort-by :label))))

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
        cursor-loc     (parser/safe-loc-at-pos text row col)
        cursor-element (loop [try-column col]
                         (if-let [usage (q/find-element-under-cursor analysis filename row col)]
                           usage
                           (when (pos? try-column)
                             (recur (dec try-column)))))
        cursor-value (if cursor-loc
                       (z/sexpr cursor-loc)
                       (:name cursor-element))
        matches-fn (partial matches-cursor? cursor-value)
        cursor-alias (when (some-> cursor-loc z/sexpr symbol?)
                       (some-> cursor-loc z/sexpr namespace))
        inside-require? (edit/inside-require? cursor-loc)]
    (if inside-require?
      (with-ns-definition-elements matches-fn (concat other-ns-elements external-ns-elements))
      (cond-> []

        cursor-alias
        (concat (with-elements-from-alias cursor-alias matches-fn current-ns-elements (concat other-ns-elements
                                                                                              external-ns-elements)))

        (not cursor-alias)
        (concat (with-element-items current-ns-elements matches-fn uri cursor-element)
                (with-element-items other-ns-elements matches-fn uri cursor-element)
                (with-clojure-core-items matches-fn))

        (and (not cursor-alias)
             (supports-cljs? uri))
        (concat (with-clojurescript-items matches-fn))

        (and (not cursor-alias)
             (supports-clj-core? uri))
        (concat (with-java-items matches-fn))))))
