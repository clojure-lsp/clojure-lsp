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
   [medley.core :as medley]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(def completion-kind-enum
  {:text 1 :method 2 :function 3 :constructor 4 :field 5 :variable 6 :class 7 :interface 8 :module 9
   :property 10 :unit 11 :value 12 :enum 13 :keyword 14 :snippet 15 :color 16 :file 17 :reference 18
   :folder 19 :enummember 20 :constant 21 :struct 22 :event 23 :operator 24 :typeparameter 25})

(def priority-kw->number
  {:kw-arg 1
   :simple-cursor 2
   :alias-keyword 3
   :keyword 4
   :refer 5
   :required-alias 6
   :unrequired-alias 7
   :ns-definition 8
   :clojure-core 9
   :clojurescript-core 10
   :java 11
   :snippet 12})

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

    (#{:keyword-usages :keyword-definitions} bucket)
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

(defn ^:private element->label [{:keys [alias bucket] :as element} cursor-alias priority]
  (cond
    (#{:keyword-usages :keyword-definitions} bucket)
    (keyword-element->str element cursor-alias priority)

    (#{:namespace-alias :namespace-usages} bucket)
    (some-> alias name)

    cursor-alias
    (str cursor-alias "/" (-> element :name name))

    :else
    (-> element :name name)))

(defn ^:private generic-priority->specific-priority
  [element priority]
  (cond
    (and (identical? :simple-cursor priority)
         (contains? #{:keyword-usages :keyword-definitions} (:bucket element)))
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
                   (some->> element :to name (str "alias to: "))

                   :else
                   (string/join
                     "\n"
                     (cond-> []
                       ns (conj (str (name ns) "/" (name (:name element))))
                       arglist-strs (conj (string/join " " arglist-strs))))))]
    (cond-> {:label (element->label element cursor-alias priority)
             :priority (generic-priority->specific-priority element priority)
             :data {"name" (-> element :name str)
                    "filename" (:filename element)
                    "name-row" (:name-row element)
                    "name-col" (:name-col element)}}
      deprecated (assoc :tags [1])
      kind (assoc :kind kind)
      detail (assoc :detail detail))))

(defn ^:private name-matches-xf [matches-fn]
  (filter (fn [{:keys [name]}]
            (and name (matches-fn name)))))

(defmulti ^:private bucket-elems-xf
  (fn [bucket _matches-fn _cursor-element]
    (if (contains? #{:keyword-usages :keyword-definitions} bucket)
      :keywords
      bucket)))

;; TODO: this completes to the namespace the cursor is already in. Why would you
;; want that?
(defmethod bucket-elems-xf :namespace-definitions
  [_bucket matches-fn _cursor-element]
  (name-matches-xf matches-fn))

(defmethod bucket-elems-xf :var-definitions
  [_bucket matches-fn {cursor-from :from cursor-bucket :bucket}]
  (let [on-var-usage? (identical? :var-usages cursor-bucket)]
    (comp
      (remove #(and on-var-usage? (not= (:ns %) cursor-from)))
      (name-matches-xf matches-fn))))

(defmethod bucket-elems-xf :keywords
  [_bucket matches-fn cursor-element]
  (comp
    (remove #(parser/same-range? cursor-element %)) ;; is the same keyword
    (filter (fn [{:keys [ns alias name] :as element}]
              (or (matches-fn (keyword-element->str element nil nil))
                  (and ns (matches-fn (str ns)))
                  (and alias (matches-fn (str alias)))
                  (and name (matches-fn name)))))))

(defmethod bucket-elems-xf :locals
  [_bucket matches-fn cursor-element]
  (comp
    ;; only locals whose scope includes the cursor
    (filter #(shared/inside? cursor-element %))
    (name-matches-xf matches-fn)))

(defn ^:private with-local-items [matches-fn cursor-uri cursor-element local-buckets row col]
  (let [cursor-langs (shared/uri->available-langs cursor-uri)
        cursor-element (or cursor-element {:name-row row, :name-col col})]
    (into []
          (comp
            (mapcat (fn [bucket]
                      (into []
                            (comp (bucket-elems-xf bucket matches-fn cursor-element)
                                  (filter #(or (not (:lang %))
                                               (contains? cursor-langs (:lang %)))))
                            (get local-buckets bucket))))
            (map #(element->completion-item % nil :simple-cursor)))
          [:namespace-definitions :var-definitions :keyword-definitions :keyword-usages :locals])))

(defn ^:private with-definition-kws-args-element-items
  [matches-fn {:keys [arglist-kws name-row name-col filename]}]
  (->> (flatten arglist-kws)
       (map (fn [kw]
              {:name (str kw)
               :name-row name-row
               :name-col name-col
               :filename filename
               :bucket :keyword-usages}))
       (filter #(matches-fn (keyword-element->str % nil nil)))
       (mapv #(element->completion-item % nil :kw-arg))))

(defn ^:private with-ns-definition-elements [matches-fn non-local-db]
  (into []
        (comp
          (mapcat (comp :namespace-definitions val))
          (filter #(matches-fn (:name %)))
          (map #(element->completion-item % nil :ns-definition)))
        (:analysis non-local-db)))

(defn ^:private with-refer-elements [matches-fn cursor-loc non-local-db]
  (let [refer-ns (z/sexpr (edit/find-refer-ns cursor-loc))]
    (into []
          (comp
            (mapcat (comp :var-definitions val))
            (filter #(and (= refer-ns (:ns %))
                          (matches-fn (:name %))))
            (map #(element->completion-item % nil :refer)))
          (q/ns-analysis non-local-db refer-ns))))

(defn ^:private with-elements-from-alias [cursor-loc cursor-alias cursor-value local-buckets matches-fn db]
  (when-let [aliases (or (->> local-buckets
                              :namespace-alias
                              (filter #(= (-> % :alias str) cursor-alias))
                              seq)
                         (->> (q/ns-aliases db)
                              seq))]
    (concat
      (when (simple-ident? cursor-value)
        ;; When the cursor exactly matches an alias in the current namespace,
        ;; suggest that. Otherwise, suggest other namespaces, matching either on
        ;; their name or how they're aliased elsewhere.
        (into []
              (comp
                (filter (fn [element]
                          (or
                            (matches-fn (:alias element))
                            (matches-fn (:to element)))))
                (map (fn [element]
                       [(some-> element :alias name)
                        (some-> element :to name)]))
                (distinct)
                (map (fn [[element-alias element-to]]
                       (let [match-alias? (matches-fn element-alias)
                             label (if match-alias?
                                     element-alias
                                     element-to)
                             detail (if match-alias?
                                      (str "alias to: " element-to)
                                      (str ":as " element-alias))
                             require-edit (some-> cursor-loc
                                                  (f.add-missing-libspec/add-known-alias (symbol (str element-alias))
                                                                                         (symbol (str element-to))
                                                                                         db)
                                                  r.transform/result)]
                         (cond-> {:label label
                                  :priority :required-alias
                                  :kind :property
                                  :detail detail}
                           (seq require-edit) (assoc :additional-text-edits (mapv #(update % :range shared/->range) require-edit)))))))
              aliases))
      ;; When the cursor exactly equals (or when the namespace part of the
      ;; cursor equals) one or more aliases, suggest var definitions from those
      ;; aliases' namespaces.
      (let [alias-namespaces (->> aliases
                                  (filter #(= (-> % :alias str) cursor-alias))
                                  (map :to)
                                  seq
                                  set)]
        (into []
              (comp
                (mapcat (comp :var-definitions val))
                (keep
                  #(when (and (not (:private %))
                              (contains? alias-namespaces (:ns %))
                              (or (simple-ident? cursor-value) (matches-fn (:name %))))
                     [(:ns %) (element->completion-item % cursor-alias :unrequired-alias)]))
                (distinct)
                (map
                  (fn [[element-ns completion-item]]
                    (let [require-edit (some-> cursor-loc
                                               (f.add-missing-libspec/add-known-alias (symbol cursor-alias) element-ns db)
                                               r.transform/result)]
                      (cond-> completion-item
                        (seq require-edit) (assoc :additional-text-edits (mapv #(update % :range shared/->range) require-edit)))))))
              (q/nses-analysis db alias-namespaces))))))

(defn ^:private with-elements-from-full-ns [db full-ns]
  (into []
        (comp
          (mapcat (comp :var-definitions val))
          (filter #(and (= (:ns %) (symbol full-ns))
                        (not (:private %))))
          (map #(element->completion-item % full-ns :ns-definition)))
        (q/ns-analysis db (symbol full-ns))))

(defn ^:private with-elements-from-aliased-keyword
  [cursor-loc cursor-element local-buckets non-local-db]
  (let [alias (or (:alias cursor-element)
                  (-> cursor-loc z/sexpr namespace (subs 1)))
        ns (or (:ns cursor-element)
               (->> (:namespace-usages local-buckets)
                    (filter #(= alias (str (:alias %))))
                    first
                    :name))
        name (-> cursor-loc z/sexpr name)]
    (into []
          (comp
            (mapcat (comp :keyword-definitions val))
            (filter #(and (= ns (:ns %))
                          (or (not cursor-loc)
                              (string/starts-with? (:name %) name))))
            (map #(element->completion-item % alias :alias-keyword)))
          (:analysis non-local-db))))

(defn ^:private with-core-items [matches-fn {:keys [filename ns-name symbols priority]}]
  (keep (fn [{:keys [name kind]}]
          (let [sym-name (str name)]
            (when (matches-fn sym-name)
              {:label sym-name
               :kind kind
               :data {"filename" filename
                      "name" sym-name
                      "ns" ns-name}
               :detail (str ns-name "/" sym-name)
               :priority priority})))
        symbols))

(defn ^:private with-clojure-core-items [matches-fn]
  (with-core-items matches-fn {:filename "/clojure.core.clj"
                               :ns-name "clojure.core"
                               :symbols common-sym/clj-syms
                               :priority :clojure-core}))

(defn ^:private with-clojurescript-items [matches-fn]
  (with-core-items matches-fn {:filename "/cljs.core.cljs"
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

(defn ^:private remove-first-and-last-char [s]
  (-> (string/join "" (drop-last s))
      (subs 1)))

(defn ^:private merging-snippets [items cursor-loc next-loc function-call? matches-fn settings]
  (let [snippet-items (map (fn [snippet]
                             (if (:function-call? snippet)
                               (update snippet :insert-text remove-first-and-last-char)
                               snippet))
                           (f.completion-snippet/known-snippets function-call? settings))
        snippet-items-by-label (->> (concat
                                      snippet-items
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
  (let [root-zloc (parser/safe-zloc-of-file db uri)
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
            settings (settings/all db)
            non-local-db (update db :analysis dissoc filename)
            local-buckets (get-in db [:analysis filename])
            support-snippets? (get-in db [:client-capabilities :text-document :completion :completion-item :snippet-support] false)
            cursor-element (q/find-element-under-cursor db filename row col)
            cursor-value (if (= :vector (z/tag cursor-loc))
                           ""
                           (if (z/sexpr-able? cursor-loc)
                             (z/sexpr cursor-loc)
                             ""))
            cursor-op (some-> cursor-loc edit/find-op)
            function-call? (= (str cursor-value) (some-> cursor-op z/string))
            keyword-value? (keyword? cursor-value)
            aliased-keyword-value? (when (and keyword-value?
                                              (qualified-keyword? cursor-value))
                                     (or (string/starts-with? (namespace cursor-value) ":")
                                         (and (string/starts-with? (namespace cursor-value) "??_")
                                              (string/ends-with? (namespace cursor-value) "_??"))))
            matches-fn (partial matches-cursor? cursor-value)
            {caller-usage-row :row caller-usage-col :col} (some-> cursor-op z/node meta)
            caller-var-definition (when (and caller-usage-row caller-usage-col)
                                    (q/find-definition-from-cursor db filename caller-usage-row caller-usage-col))
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
                              (contains? (q/ns-names db) (symbol cursor-value-or-ns)))
            items (cond
                    inside-refer?
                    (with-refer-elements matches-fn cursor-loc non-local-db)

                    inside-require?
                    (cond-> (with-ns-definition-elements matches-fn non-local-db)
                      (and support-snippets?
                           simple-cursor?)
                      (merging-snippets cursor-loc next-loc function-call? matches-fn settings))

                    aliased-keyword-value?
                    (with-elements-from-aliased-keyword cursor-loc cursor-element local-buckets non-local-db)

                    :else
                    (cond-> []
                      cursor-full-ns?
                      (into (with-elements-from-full-ns db cursor-value-or-ns))

                      (and cursor-value-or-ns
                           (not keyword-value?))
                      (into (with-elements-from-alias cursor-loc cursor-value-or-ns cursor-value local-buckets matches-fn db))

                      (or simple-cursor?
                          keyword-value?)
                      (-> (into (with-local-items matches-fn uri cursor-element local-buckets row col))
                          (into (with-clojure-core-items matches-fn)))

                      (:arglist-kws caller-var-definition)
                      (into (with-definition-kws-args-element-items matches-fn caller-var-definition))

                      (and simple-cursor?
                           (supports-cljs? uri))
                      (into (with-clojurescript-items matches-fn))

                      (and simple-cursor?
                           (supports-clj-core? uri))
                      (into (with-java-items matches-fn))

                      (and support-snippets?
                           simple-cursor?)
                      (merging-snippets cursor-loc next-loc function-call? matches-fn settings)))]
        (sorting-and-distincting-items items)))))

(defn ^:private resolve-item-by-ns
  [{{:keys [name ns filename]} :data :as item} db*]
  (let [db @db*
        definition (q/find-definition db {:filename filename
                                          :name (symbol name)
                                          :to (symbol ns)
                                          :bucket :var-usages})]
    (cond-> item
      definition (assoc :documentation (f.hover/hover-documentation definition db*)))))

(defn ^:private resolve-item-by-definition
  [{{:keys [name filename name-row name-col]} :data :as item} db*]
  (let [db @db*
        element (q/find-element-under-cursor db filename name-row name-col)
        definition (when (and (identical? :var-definitions (:bucket element))
                              (= (symbol name) (:name element)))
                     element)]
    (cond-> item
      definition (assoc :documentation (f.hover/hover-documentation definition db*)))))

(defn resolve-item [{{:keys [ns]} :data :as item} db*]
  (let [item (shared/assoc-some item
                                :insert-text-format (:insertTextFormat item)
                                :text-edit (:textEdit item)
                                :filter-text (:filterText item)
                                :insert-text (:insertText item))]
    (if (:data item)
      (if ns
        (resolve-item-by-ns item db*)
        (resolve-item-by-definition item db*))
      item)))
