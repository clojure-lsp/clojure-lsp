(ns clojure-lsp.feature.completion
  (:require
   [clojure-lsp.common-symbols :as common-sym]
   [clojure-lsp.feature.completion-snippet :as f.completion-snippet]
   [clojure-lsp.feature.hover :as f.hover]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [clojure.walk :as walk]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(def priority-kw->number
  {:simple-cursor 1
   :keyword 2
   :refer 3
   :required-alias 4
   :unrequired-alias 5
   :ns-definition 6
   :clojure-core 7
   :clojurescript-core 8
   :java 9
   :snippet 10})

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

(defn ^:private element->completion-item-kind [{:keys [bucket arglist-strs macro]}]
  (cond
    (#{:namespace-definitions
       :namespace-usages} bucket)
    :module

    (#{:namespace-alias} bucket)
    :property

    (#{:keywords} bucket)
    :keyword

    (and (#{:var-definitions} bucket)
         (or arglist-strs macro))
    :function

    (#{:var-definitions :var-usages :locals} bucket)
    :variable

    (#{:local-usages} bucket)
    :value

    :else
    :reference))

(defn ^:private resolve-item-kind [name ns analysis]
  (->> (mapcat val analysis)
       (filter #(and (= name (:name %))
                     (= ns (:ns %))
                     (= :var-definitions (:bucket %))))
       first
       element->completion-item-kind))

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
   row
   col
   {:keys [bucket to ns filename lang name alias] :as element}]
  (let [supported-file-types (shared/uri->available-langs cursor-uri)]
    (cond
      (#{:var-usages :local-usages :namespace-usages} bucket)
      false

      (and (identical? :locals bucket)
           (or (not= filename (shared/uri->filename cursor-uri))
               (not (shared/inside? (or cursor-element
                                        {:name-row row
                                         :name-col col})
                                    element))))
      false

      (and (identical? :var-definitions bucket)
           (identical? :var-usages cursor-bucket)
           (not= ns cursor-from))
      false

      (identical? :clj-kondo/unknown-namespace to)
      false

      (and lang
           (not (supported-file-types lang)))
      false

      (and (identical? :keywords bucket)
           (= filename (:filename cursor-element))
           (parser/same-range? cursor-element element)) ;; is the same keyword
      false

      (and (identical? :keywords bucket)
           (or (matches-fn (keyword-element->str element))
               (and ns (matches-fn (str ns)))
               (and alias (matches-fn (str alias)))))
      true

      (or (and name (matches-fn name))
          (and alias (matches-fn alias)))
      true)))

(defn ^:private generic-priority->specific-priority
  [element priority]
  (cond
    (and (identical? :simple-cursor priority)
         (identical? :keywords (:bucket element)))
    :keyword

    :else
    priority))

(defn ^:private element->completion-item
  [{:keys [deprecated ns bucket arglist-strs] :as element} cursor-alias priority]
  (let [kind (element->completion-item-kind element)
        definition? (contains? #{:namespace-definitions :var-definitions} bucket)
        detail (when (not definition?)
                 (cond
                   (identical? :namespace-alias bucket)
                   (some-> element :to name)

                   :else
                   (string/join
                     "\n"
                     (cond-> []
                       ns (conj (str (name ns) "/" (name (:name element))))
                       arglist-strs (conj (string/join " " arglist-strs))))))]
    (cond-> {:label (element->label element cursor-alias)
             :priority (generic-priority->specific-priority element priority)
             :data (walk/stringify-keys {:name (-> element :name str)
                                         :filename (:filename element)
                                         :name-row (:name-row element)
                                         :name-col (:name-col element)})}
      deprecated (assoc :tags [1])
      kind (assoc :kind kind)
      detail (assoc :detail detail))))

(defn ^:private with-element-items [matches-fn cursor-uri cursor-element elements row col]
  (->> elements
       (filter (partial valid-element-completion-item? matches-fn cursor-uri cursor-element row col))
       (map #(element->completion-item % nil :simple-cursor))))

(defn ^:private with-ns-definition-elements [matches-fn other-ns-elements]
  (->> other-ns-elements
       (filter #(and (= :namespace-definitions (:bucket %))
                     (matches-fn (:name %))))
       (map #(element->completion-item % nil :ns-definition))))

(defn ^:private with-refer-elements [matches-fn cursor-loc other-ns-elements]
  (let [refer-ns (z/sexpr (edit/find-refer-ns cursor-loc))]
    (->> other-ns-elements
         (filter #(and (identical? :var-definitions (:bucket %))
                       (= refer-ns (:ns %))
                       (matches-fn (:name %))))
         (map #(element->completion-item % nil :refer)))))

(defn ^:private with-elements-from-alias [cursor-loc cursor-alias cursor-value matches-fn db]
  (when-let [aliases (some->> (q/filter-project-analysis (:analysis @db) db)
                              (mapcat val)
                              (filter #(identical? :namespace-alias (:bucket %))))]
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
                                              (r.transform/add-known-alias (symbol (str (:alias element)))
                                                                           (symbol (str (:to element)))
                                                                           db)
                                              r.transform/result)]
                     (cond-> (element->completion-item element nil :required-alias)
                       (seq require-edit) (assoc :additional-text-edits (mapv #(update % :range shared/->range) require-edit))))))))
        (->> (:analysis @db)
             (mapcat val)
             (keep
               #(when (and (identical? :var-definitions (:bucket %))
                           (not (:private %))
                           (contains? alias-namespaces (:ns %))
                           (or (simple-ident? cursor-value) (matches-fn (:name %))))
                  [(:ns %) (element->completion-item % cursor-alias :unrequired-alias)]))
             set
             (map
               (fn [[element-ns completion-item]]
                 (let [require-edit (some-> cursor-loc
                                            (r.transform/add-known-alias (symbol cursor-alias) element-ns db)
                                            r.transform/result)]
                   (cond-> completion-item
                     (seq require-edit) (assoc :additional-text-edits (mapv #(update % :range shared/->range) require-edit)))))))))))

(defn ^:private with-elements-from-full-ns [full-ns analysis]
  (->> (mapcat val analysis)
       (filter #(and (identical? :var-definitions (:bucket %))
                     (= (:ns %) (symbol full-ns))
                     (not (:private %))))
       (mapv #(element->completion-item % full-ns :ns-definition))))

(defn ^:private with-clojure-core-items [matches-fn analysis]
  (->> common-sym/core-syms
       (filter (comp matches-fn str))
       (map (fn [sym] {:label (str sym)
                       :kind (resolve-item-kind sym 'clojure.core analysis)
                       :data (walk/stringify-keys {:filename "/clojure.core.clj"
                                                   :name (str sym)
                                                   :ns "clojure.core"})
                       :detail (str "clojure.core/" sym)
                       :priority :clojure-core}))))

(defn ^:private with-clojurescript-items [matches-fn analysis]
  (->> common-sym/cljs-syms
       (filter (comp matches-fn str))
       (map (fn [sym] {:label (str sym)
                       :kind (resolve-item-kind sym 'cljs.core analysis)
                       :data (walk/stringify-keys {:filename "/cljs.core.cljs"
                                                   :name (str sym)
                                                   :ns "cljs.core"})
                       :detail (str "cljs.core/" sym)
                       :priority :clojurescript-core}))))

(defn ^:private with-java-items [matches-fn]
  (concat
    (->> common-sym/java-lang-syms
         (filter (comp matches-fn str))
         (map (fn [sym] {:label (str sym)
                         :kind :class
                         :detail (str "java.lang." sym)
                         :priority :java})))
    (->> common-sym/java-util-syms
         (filter (comp matches-fn str))
         (map (fn [sym] {:label (str sym)
                         :kind :class
                         :detail (str "java.util." sym)
                         :priority :java})))))

(defn ^:private with-snippets [cursor-loc text row col settings]
  (let [next-loc (parser/safe-loc-at-pos text row (inc col))]
    (->> (concat
           (f.completion-snippet/known-snippets settings)
           (f.completion-snippet/build-additional-snippets cursor-loc next-loc settings))
         (map #(assoc %
                      :kind :snippet
                      :priority :snippet
                      :insert-text-format :snippet)))))

(defn- sort-completion-results [results]
  (sort-by (juxt #(get priority-kw->number (:priority %) 0) :label :detail) results))

(defn completion [uri row col db]
  (let [filename (shared/uri->filename uri)
        {:keys [text]} (get-in @db [:documents uri])
        settings (settings/all db)
        analysis (:analysis @db)
        current-ns-elements (get analysis filename)
        support-snippets? (get-in @db [:client-capabilities :text-document :completion :completion-item :snippet-support] false)
        other-ns-elements (->> (q/filter-project-analysis (dissoc analysis filename) db)
                               (mapcat val))
        external-ns-elements (->> (q/filter-external-analysis (dissoc analysis filename) db)
                                  (mapcat val))
        cursor-loc (when-let [loc (parser/safe-loc-at-pos text row col)]
                     (when (or (not (-> loc z/node meta))
                               (= row (-> loc z/node meta :row)))
                       loc))
        cursor-element (loop [try-column col]
                         (if-let [usage (q/find-element-under-cursor analysis filename row col)]
                           usage
                           (when (pos? try-column)
                             (recur (dec try-column)))))
        cursor-value (if (= :vector (z/tag cursor-loc))
                       ""
                       (if cursor-loc
                         (z/sexpr cursor-loc)
                         ""))
        keyword-value? (keyword? cursor-value)
        matches-fn (partial matches-cursor? cursor-value)
        inside-require? (edit/inside-require? cursor-loc)
        inside-refer? (edit/inside-refer? cursor-loc)
        simple-cursor? (or (simple-ident? cursor-value)
                           (string/blank? (str cursor-value)))
        cursor-value-or-ns (if (qualified-ident? cursor-value)
                             (namespace cursor-value)
                             (if (or (symbol? cursor-value)
                                     keyword-value?)
                               (name cursor-value)
                               (str cursor-value)))
        cursor-full-ns? (when cursor-value-or-ns
                          (contains? (q/find-all-ns-definition-names analysis) (symbol cursor-value-or-ns)))]
    (cond
      inside-refer?
      (->> (with-refer-elements matches-fn cursor-loc (concat other-ns-elements external-ns-elements))
           (into #{})
           set
           sort-completion-results
           not-empty)

      inside-require?
      (->> (with-ns-definition-elements matches-fn (concat other-ns-elements external-ns-elements))
           (into #{})
           set
           sort-completion-results
           (map #(dissoc % :priority))
           not-empty)

      :else
      (cond-> #{}
        cursor-full-ns?
        (into (with-elements-from-full-ns cursor-value-or-ns analysis))

        (and cursor-value-or-ns
             (not keyword-value?))
        (into (with-elements-from-alias cursor-loc cursor-value-or-ns cursor-value matches-fn db))

        simple-cursor?
        (-> (into (with-element-items matches-fn uri cursor-element current-ns-elements row col))
            (into (with-clojure-core-items matches-fn analysis)))

        (and simple-cursor?
             (supports-cljs? uri))
        (into (with-clojurescript-items matches-fn analysis))

        (and simple-cursor?
             (supports-clj-core? uri))
        (into (with-java-items matches-fn))

        support-snippets?
        (into (with-snippets cursor-loc text row col settings))

        :always
        (->> set
             sort-completion-results
             (map #(dissoc % :priority))
             not-empty)))))

(defn ^:private resolve-item-by-ns
  [{{:keys [name ns filename]} :data :as item} db]
  (let [analysis (:analysis @db)
        definition (q/find-definition analysis {:filename filename
                                                :name (symbol name)
                                                :to (symbol ns)
                                                :bucket :var-usages} db)]
    (if definition
      (-> item
          (assoc :documentation (f.hover/hover-documentation definition db))
          (dissoc :data))
      item)))

(defn ^:private resolve-item-by-definition
  [{{:keys [name filename name-row name-col]} :data :as item} db]
  (let [local-analysis (get-in @db [:analysis filename])
        definition (q/find-first #(and (identical? :var-definitions (:bucket %))
                                       (= name (str (:name %)))
                                       (= name-row (:name-row %))
                                       (= name-col (:name-col %))) local-analysis)]
    (if definition
      (-> item
          (assoc :documentation (f.hover/hover-documentation definition db))
          (dissoc :data))
      item)))

(defn resolve-item [{{:keys [ns]} :data :as item} db]
  (if (:data item)
    (if ns
      (resolve-item-by-ns item db)
      (resolve-item-by-definition item db))
    item))
