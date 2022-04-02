(ns clojure-lsp.feature.completion
  (:require
   [clojure-lsp.common-symbols :as common-sym]
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
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
   [medley.core :as medley]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(def completion-kind-enum
  {:text 1 :method 2 :function 3 :constructor 4 :field 5 :variable 6 :class 7 :interface 8 :module 9
   :property 10 :unit 11 :value 12 :enum 13 :keyword 14 :snippet 15 :color 16 :file 17 :reference 18
   :folder 19 :enummember 20 :constant 21 :struct 22 :event 23 :operator 24 :typeparameter 25})

(def priority-kw->number
  {:simple-cursor 1
   :alias-keyword 2
   :keyword 3
   :refer 4
   :required-alias 5
   :unrequired-alias 6
   :ns-definition 7
   :clojure-core 8
   :clojurescript-core 9
   :java 10
   :snippet 11})

(defn ^:private keyword-element->str [{:keys [alias ns] :as element} cursor-alias priority]
  (let [alias (or alias
                  (when (= :alias-keyword priority)
                    cursor-alias))]
    (cond-> ":"
      alias
      (str ":")

      (or alias ns)
      (str (or alias ns) "/")

      :always
      (str (:name element)))))

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

(defn ^:private var-defs-in-ns-named [analysis ns-str name-syms]
  (let [ns (symbol ns-str)
        name-set (set name-syms)]
    (into []
          (comp (mapcat val)
                (filter #(= ns (:ns %)))
                ;; clojure.core is 98% var-definitions, so faster to filter by
                ;; name-set before filtering for var defs.
                (filter #(contains? name-set (:name %)))
                (q/xf-var-defs false))
          analysis)))

(defn ^:private element->label [{:keys [alias bucket] :as element} cursor-alias priority]
  (cond
    (= :keywords bucket)
    (keyword-element->str element cursor-alias priority)

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
           (or (matches-fn (keyword-element->str element nil nil))
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
    (cond-> {:label (element->label element cursor-alias priority)
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
  (when-let [aliases (seq (into []
                                (comp
                                  (q/filter-project-analysis-xf @db)
                                  (mapcat val)
                                  (filter #(identical? :namespace-alias (:bucket %))))
                                (:analysis @db)))]
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
                                              (f.add-missing-libspec/add-known-alias (symbol (str (:alias element)))
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
                                            (f.add-missing-libspec/add-known-alias (symbol cursor-alias) element-ns db)
                                            r.transform/result)]
                   (cond-> completion-item
                     (seq require-edit) (assoc :additional-text-edits (mapv #(update % :range shared/->range) require-edit)))))))))))

(defn ^:private with-elements-from-full-ns [full-ns analysis]
  (->> (mapcat val analysis)
       (filter #(and (identical? :var-definitions (:bucket %))
                     (= (:ns %) (symbol full-ns))
                     (not (:private %))))
       (mapv #(element->completion-item % full-ns :ns-definition))))

(defn ^:private with-elements-from-aliased-keyword
  [cursor-loc cursor-element analysis filename elements]
  (let [alias (or (:alias cursor-element)
                  (-> cursor-loc z/sexpr namespace (subs 1)))
        ns (or (:ns cursor-element)
               (->> (get analysis filename)
                    (filter #(and (identical? :namespace-usages (:bucket %))
                                  (= alias (str (:alias %)))))
                    first
                    :name))
        name (-> cursor-loc z/sexpr name)]
    (->> elements
         (filter #(and (identical? :keywords (:bucket %))
                       (:reg %)
                       (= ns (:ns %))
                       (or (not cursor-loc)
                           (string/starts-with? (:name %) name))))
         (mapv #(element->completion-item % alias :alias-keyword)))))

(defn ^:private with-core-items [matches-fn analysis {:keys [filename ns-name symbols priority]}]
  (let [matches (filter (comp matches-fn str) symbols)
        elem-by-name (->> (var-defs-in-ns-named analysis ns-name matches)
                          (medley/index-by :name))]
    (map (fn [sym] {:label (str sym)
                    :kind (element->completion-item-kind (elem-by-name sym))
                    :data {"filename" filename
                           "name" (str sym)
                           "ns" ns-name}
                    :detail (str ns-name "/" sym)
                    :priority priority})
         matches)))

(defn ^:private with-clojure-core-items [matches-fn analysis]
  (with-core-items matches-fn analysis {:filename "/clojure.core.clj"
                                        :ns-name "clojure.core"
                                        :symbols common-sym/core-syms
                                        :priority :clojure-core}))

(defn ^:private with-clojurescript-items [matches-fn analysis]
  (with-core-items matches-fn analysis {:filename "/cljs.core.cljs"
                                        :ns-name "cljs.core"
                                        :symbols common-sym/cljs-syms
                                        :priority :clojurescript-core}))

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

(defn ^:private merging-snippets [items cursor-loc next-loc matches-fn settings]
  (let [snippet-items-by-label (->> (concat
                                      (f.completion-snippet/known-snippets settings)
                                      (f.completion-snippet/build-additional-snippets cursor-loc next-loc settings))
                                    (map #(assoc %
                                                 :kind :snippet
                                                 :priority :snippet
                                                 :insert-text-format :snippet))
                                    (filter (comp matches-fn :label))
                                    (reduce #(assoc %1 (:label %2) %2) {}))
        items-by-label (reduce #(assoc %1 (:label %2) %2) {} items)]
    (concat
      (map (fn [item]
             (if-let [snippet-item (get snippet-items-by-label (:label item))]
               (let [data (shared/assoc-some (:data item)
                                             "snippet-kind" (get completion-kind-enum (:kind item)))]
                 (shared/assoc-some snippet-item :data data))
               item))
           items)
      (remove #(get items-by-label (:label %))
              (vals snippet-items-by-label)))))

(defn ^:private sorting-and-distincting-items [items]
  (->> items
       (medley/distinct-by (juxt :label :kind :detail))
       (sort-by (juxt #(get priority-kw->number (:priority %) 0) :label :detail))
       (map #(dissoc % :priority))
       not-empty))

(defn completion [uri row col db]
  (let [root-zloc (parser/safe-zloc-of-file @db uri)
        ;; (dec col) because we're completing what's behind the cursor
        cursor-loc (when-let [loc (some-> root-zloc (parser/to-pos row (dec col)))]
                     (when (or (not (-> loc z/node meta))
                               (= row (-> loc z/node meta :row)))
                       loc))
        next-loc (some-> root-zloc (parser/to-pos row col))]
    ;; When not on a symbol or keyword, we want to return all valid completions
    ;; (almost 1000 in an empty file), even though it's expensive to compute.
    ;; The one exception is in comments. Some editors (nvim + coc.nvim) request
    ;; completions in comments. Since rewrite-clj parses the entire comment
    ;; section, not individual words within the comment, we don't have an
    ;; individual symbol, and so would return all valid completions. This
    ;; changes an occasional expensive computation into a frequent expensive
    ;; computation. To avoid this expense, we abort on comments.
    (if (= :comment (some-> cursor-loc z/tag))
      []
      (let [filename (shared/uri->filename uri)
            settings (settings/all @db)
            analysis (:analysis @db)
            current-ns-elements (get analysis filename)
            support-snippets? (get-in @db [:client-capabilities :text-document :completion :completion-item :snippet-support] false)
            other-ns-elements (into []
                                    (comp
                                      (q/filter-project-analysis-xf @db)
                                      (mapcat val))
                                    (dissoc analysis filename))
            external-ns-elements (into []
                                       (comp
                                         (q/filter-external-analysis-xf  db)
                                         (mapcat val))
                                       (dissoc analysis filename))
            cursor-element (q/find-element-under-cursor analysis filename row col)
            cursor-value (if (= :vector (z/tag cursor-loc))
                           ""
                           (if (z/sexpr-able? cursor-loc)
                             (z/sexpr cursor-loc)
                             ""))
            keyword-value? (keyword? cursor-value)
            aliased-keyword-value? (when (and keyword-value?
                                              (qualified-keyword? cursor-value))
                                     (or (string/starts-with? (namespace cursor-value) ":")
                                         (and (string/starts-with? (namespace cursor-value) "??_")
                                              (string/ends-with? (namespace cursor-value) "_??"))))
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
                              (contains? (q/find-all-ns-definition-names analysis) (symbol cursor-value-or-ns)))
            items (cond
                    inside-refer?
                    (with-refer-elements matches-fn cursor-loc (concat other-ns-elements external-ns-elements))

                    inside-require?
                    (cond-> (with-ns-definition-elements matches-fn (concat other-ns-elements external-ns-elements))
                      (and support-snippets?
                           simple-cursor?)
                      (merging-snippets cursor-loc next-loc matches-fn settings))

                    aliased-keyword-value?
                    (with-elements-from-aliased-keyword cursor-loc cursor-element analysis filename (concat other-ns-elements external-ns-elements))

                    :else
                    (cond-> []
                      cursor-full-ns?
                      (into (with-elements-from-full-ns cursor-value-or-ns analysis))

                      (and cursor-value-or-ns
                           (not keyword-value?))
                      (into (with-elements-from-alias cursor-loc cursor-value-or-ns cursor-value matches-fn db))

                      (or simple-cursor?
                          keyword-value?)
                      (-> (into (with-element-items matches-fn uri cursor-element current-ns-elements row col))
                          (into (with-clojure-core-items matches-fn analysis)))

                      (and simple-cursor?
                           (supports-cljs? uri))
                      (into (with-clojurescript-items matches-fn analysis))

                      (and simple-cursor?
                           (supports-clj-core? uri))
                      (into (with-java-items matches-fn))

                      (and support-snippets?
                           simple-cursor?)
                      (merging-snippets cursor-loc next-loc matches-fn settings)))]
        (sorting-and-distincting-items items)))))

(defn ^:private resolve-item-by-ns
  [{{:keys [name ns filename]} :data :as item} {:keys [db] :as components}]
  (let [analysis (:analysis @db)
        definition (q/find-definition analysis {:filename filename
                                                :name (symbol name)
                                                :to (symbol ns)
                                                :bucket :var-usages} db)]
    (if definition
      (-> item
          (assoc :documentation (f.hover/hover-documentation definition components)))
      item)))

(defn ^:private resolve-item-by-definition
  [{{:keys [name filename name-row name-col]} :data :as item} {:keys [db] :as components}]
  (let [local-analysis (get-in @db [:analysis filename])
        definition (q/find-first #(and (identical? :var-definitions (:bucket %))
                                       (= name (str (:name %)))
                                       (= name-row (:name-row %))
                                       (= name-col (:name-col %))) local-analysis)]
    (if definition
      (-> item
          (assoc :documentation (f.hover/hover-documentation definition components)))
      item)))

(defn resolve-item [{{:keys [ns]} :data :as item} components]
  (let [item (shared/assoc-some item
                                :insert-text-format (:insertTextFormat item)
                                :text-edit (:textEdit item)
                                :filter-text (:filterText item)
                                :insert-text (:insertText item))]
    (if (:data item)
      (if ns
        (resolve-item-by-ns item components)
        (resolve-item-by-definition item components))
      item)))
