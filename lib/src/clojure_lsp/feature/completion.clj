(ns clojure-lsp.feature.completion
  (:require
   [clojure-lsp.common-symbols :as common-sym]
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.feature.completion-snippet :as f.completion-snippet]
   [clojure-lsp.feature.format]
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

(def priority-order
  [:kw-arg
   :locals
   :simple-cursor
   :alias-keyword
   :keyword
   :refer
   :required-alias
   :unrequired-alias
   :ns-definition
   :clojure-core
   :clojurescript-core
   :java-usages
   :java-class-definitions
   :java-member-definitions
   :snippet])

(def priority-kw->number
  (reduce (fn [m priority]
            (assoc m priority (inc (or (.indexOf ^clojure.lang.PersistentVector priority-order priority)
                                       0)))) {} priority-order))

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

    (#{:var-definitions :locals} bucket)
    :variable

    (#{:var-usages} bucket)
    :reference

    (#{:local-usages} bucket)
    :value

    (#{:java-class-usages} bucket)
    :class

    :else
    :reference))

(defn ^:private java-element->class-name [element]
  ;; TODO maybe move to a common place or make all kondo elements have a
  ;; :name field
  (last (string/split (:class element) #"\.")))

(defn ^:private java-member-flags->kind [{:keys [flags]}]
  (cond
    (and (:final flags)
         (:field flags)
         ;; TODO improve on clj-kondo
         (not (:method flags))) :constant
    (:method flags) :method
    (:field flags) :field
    :else :method))

(defn ^:private element->label [{:keys [alias bucket] :as element} cursor-alias priority]
  (cond
    (#{:keyword-usages :keyword-definitions} bucket)
    (keyword-element->str element cursor-alias priority)

    (#{:namespace-alias} bucket)
    (some-> alias name)

    (#{:namespace-usages} bucket)
    (some-> element :name name)

    (#{:java-class-usages} bucket)
    (java-element->class-name element)

    cursor-alias
    (str cursor-alias "/" (-> element :name name))

    :else
    (-> element :name name)))

(defn add-unresolved [completion-item unresolved-type args]
  (update-in completion-item [:data "unresolved"] (fnil conj [])
             [unresolved-type (shared/preserve-kebab-case args)]))

(defn ^:private completion-item-with-documentation
  [completion-item element var-name db* docs-config]
  (let [definition (when (and element
                              (identical? :var-definitions (:bucket element))
                              (= var-name (str (:name element))))
                     element)]
    (cond-> completion-item
      definition (assoc :documentation (f.hover/hover-documentation definition db* docs-config)))))

(defn ^:private completion-item-with-alias-edit
  [completion-item cursor-loc alias-to-add ns-to-add db]
  (let [edits (some-> cursor-loc
                      (f.add-missing-libspec/add-known-alias alias-to-add ns-to-add db)
                      r.transform/result)]
    (cond-> completion-item
      (seq edits) (assoc :additional-text-edits (mapv #(update % :range shared/->range)
                                                      edits)))))

(defn ^:private completion-item-with-unresolved-documentation
  [completion-item args resolve-support]
  (if (contains? (:properties resolve-support) "documentation")
    ;; client supports postponing documentation lookup
    (add-unresolved completion-item "documentation" args)
    ;; clients that don't support postponing documentation don't get
    ;; documentation at all
    completion-item))

(defn ^:private completion-item-with-unresolved-alias-edit
  [completion-item cursor-loc alias-to-add ns-to-add db uri resolve-support]
  (if (contains? (:properties resolve-support) "additionalTextEdits")
    ;; client supports postponing the expensive edit calculation
    (add-unresolved completion-item
                    "alias"
                    {:ns-to-add    (name ns-to-add)
                     :alias-to-add (name alias-to-add)
                     :uri          uri})
    ;; client can't postpone the edit calculation, so do it now, even though it's expensive
    (completion-item-with-alias-edit completion-item cursor-loc alias-to-add ns-to-add db)))

(defn ^:private generic-priority->specific-priority
  [element priority]
  (cond
    (and (identical? :simple-cursor priority)
         (contains? #{:keyword-usages :keyword-definitions} (:bucket element)))
    :keyword

    (and (identical? :simple-cursor priority)
         (contains? #{:locals} (:bucket element)))
    :locals

    (identical? :java-class-usages (:bucket element))
    :java-usages

    :else
    priority))

(defn ^:private element->completion-item
  [{:keys [deprecated ns bucket arglist-strs] :as element} cursor-alias priority resolve-support]
  (let [kind (element->completion-item-kind element)
        definition? (contains? #{:namespace-definitions :var-definitions} bucket)
        detail (when (not definition?)
                 (cond
                   (identical? :namespace-alias bucket)
                   (some->> element :to name (str "alias to: "))

                   (identical? :var-usages bucket)
                   ;; it's always a refer
                   (some->> element :to name (str "refer to: "))

                   (identical? :java-class-usages bucket)
                   (:class element)

                   :else
                   (string/join
                     "\n"
                     (cond-> []
                       ns (conj (str (name ns) "/" (name (:name element))))
                       arglist-strs (conj (string/join " " arglist-strs))))))]
    (cond-> {:label (element->label element cursor-alias priority)
             :priority (generic-priority->specific-priority element priority)}
      deprecated (assoc :tags [1])
      kind (assoc :kind kind)
      detail (assoc :detail detail)
      :always (completion-item-with-unresolved-documentation
                {:name (-> element :name str)
                 :uri (:uri element)
                 :name-row (:name-row element)
                 :name-col (:name-col element)}
                resolve-support))))

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

(defmethod bucket-elems-xf :namespace-usages
  [_bucket matches-fn _cursor-element]
  (name-matches-xf matches-fn))

(defmethod bucket-elems-xf :var-definitions
  [_bucket matches-fn {cursor-from :from cursor-bucket :bucket}]
  (let [on-var-usage? (identical? :var-usages cursor-bucket)]
    (comp
      (remove #(and on-var-usage? (not= (:ns %) cursor-from)))
      (name-matches-xf matches-fn))))

(defmethod bucket-elems-xf :var-usages
  [_bucket matches-fn _cursor-element]
  (comp
    (filter :refer)
    (name-matches-xf matches-fn)))

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

(defmethod bucket-elems-xf :java-class-usages
  [_bucket matches-fn _cursor-element]
  (comp
    (filter :import)
    (filter #(matches-fn (java-element->class-name %)))))

(defn ^:private with-local-items [matches-fn cursor-uri cursor-element local-buckets row col resolve-support]
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
            (map #(element->completion-item % nil :simple-cursor resolve-support)))
          [:namespace-definitions
           :namespace-usages
           :var-definitions
           :var-usages
           :keyword-definitions
           :keyword-usages
           :locals
           :java-class-usages])))

(defn ^:private with-definition-kws-args-element-items
  [matches-fn {:keys [arglist-kws name-row name-col uri]} resolve-support]
  (->> (flatten arglist-kws)
       (map (fn [kw]
              {:name (str kw)
               :name-row name-row
               :name-col name-col
               :uri uri
               :bucket :keyword-usages}))
       (filter #(matches-fn (keyword-element->str % nil nil)))
       (mapv #(element->completion-item % nil :kw-arg resolve-support))))

(defn ^:private with-ns-definition-elements [matches-fn non-local-db resolve-support]
  (into []
        (comp
          q/xf-analysis->namespace-definitions
          (filter #(matches-fn (:name %)))
          (map #(element->completion-item % nil :ns-definition resolve-support)))
        (:analysis non-local-db)))

(defn ^:private with-refer-elements [matches-fn cursor-loc non-local-db resolve-support]
  (let [refer-ns (z/sexpr (edit/find-refer-ns cursor-loc))]
    (into []
          (comp
            q/xf-analysis->var-definitions
            (filter #(and (= refer-ns (:ns %))
                          (matches-fn (:name %))))
            (map #(element->completion-item % nil :refer resolve-support)))
          (q/ns-analysis non-local-db refer-ns))))

(defn ^:private with-elements-from-alias
  [cursor-loc cursor-alias cursor-value local-buckets matches-fn db uri resolve-support]
  (when-let [aliases (or (->> local-buckets
                              :namespace-alias
                              (filter #(= (-> % :alias str) cursor-alias))
                              seq)
                         (->> (dep-graph/ns-aliases db)
                              seq))]
    (->> (concat
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
                            (let [match-alias? (matches-fn element-alias)]
                              {:item         {:label    (if match-alias?
                                                          element-alias
                                                          element-to)
                                              :priority :required-alias
                                              :kind     :property
                                              :detail   (if match-alias?
                                                          (str "alias to: " element-to)
                                                          (str ":as " element-alias))}
                               :alias-to-add (symbol (str element-alias))
                               :ns-to-add    (symbol (str element-to))}))))
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
                     q/xf-analysis->var-definitions
                     (keep
                       #(when (and (not (:private %))
                                   (contains? alias-namespaces (:ns %))
                                   (or (simple-ident? cursor-value) (matches-fn (:name %))))
                          [(:ns %) (element->completion-item % cursor-alias :unrequired-alias resolve-support)]))
                     (distinct)
                     (map
                       (fn [[element-ns completion-item]]
                         {:item         completion-item
                          :alias-to-add (symbol cursor-alias)
                          :ns-to-add    element-ns})))
                   (q/nses-analysis db alias-namespaces))))
         (map (fn [{:keys [item alias-to-add ns-to-add]}]
                (completion-item-with-unresolved-alias-edit item cursor-loc alias-to-add ns-to-add db uri resolve-support))))))

(defn ^:private with-elements-from-full-ns [db full-ns resolve-support]
  (into []
        (comp
          q/xf-analysis->var-definitions
          (filter #(and (= (:ns %) (symbol full-ns))
                        (not (:private %))))
          (map #(element->completion-item % full-ns :ns-definition resolve-support)))
        (q/ns-analysis db (symbol full-ns))))

(defn ^:private with-elements-from-aliased-keyword
  [cursor-loc cursor-element local-buckets non-local-db resolve-support]
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
            q/xf-analysis->keyword-definitions
            (filter #(and (= ns (:ns %))
                          (or (not cursor-loc)
                              (string/starts-with? (:name %) name))))
            (map #(element->completion-item % alias :alias-keyword resolve-support)))
          (:analysis non-local-db))))

(defn ^:private with-core-items [matches-fn {:keys [uri ns-name symbols priority]} resolve-support]
  (keep (fn [{:keys [name kind]}]
          (let [sym-name (str name)]
            (when (matches-fn sym-name)
              (-> {:label    sym-name
                   :kind     kind
                   :detail   (str ns-name "/" sym-name)
                   :priority priority}
                  (completion-item-with-unresolved-documentation
                    {:uri  uri
                     :name sym-name
                     :ns   ns-name}
                    resolve-support)))))
        symbols))

(defn ^:private with-clojure-core-items [matches-fn resolve-support]
  (with-core-items matches-fn
                   {:uri "file:///clojure.core.clj"
                    :ns-name "clojure.core"
                    :symbols common-sym/clj-syms
                    :priority :clojure-core}
                   resolve-support))

(defn ^:private with-clojurescript-items [matches-fn resolve-support]
  (with-core-items matches-fn
                   {:uri "file:///cljs.core.cljs"
                    :ns-name "cljs.core"
                    :symbols common-sym/cljs-syms
                    :priority :clojurescript-core}
                   resolve-support))

(defn ^:private with-java-definition-items [matches-fn cursor-value db]
  ;; For performance reasons, we have thousands of class definitions usually
  ;; we only consider it if user typed anything and is auto completing
  (when (seq (str cursor-value))
    (flatten
      (into []
            (comp
              q/xf-analysis->java-class-definitions
              (keep (fn [{:keys [class] :as e}]
                      (let [class-name* (delay (java-element->class-name e))]
                        (cond-> []

                          (matches-fn class)
                          (conj {:label (str class)
                                 :kind :class
                                 :priority :java-class-definitions})

                          (and (string/starts-with? class "java.lang")
                               (matches-fn @class-name*))
                          (conj {:label @class-name*
                                 :detail class
                                 :kind :class
                                 :priority :java-class-definitions}))))))
            (:analysis db)))))

(defn ^:private with-java-static-member-definition-items [matches-fn class-element cursor-value db]
  (let [full-package? (string/includes? cursor-value ".")
        matches-member-fn (if full-package?
                            (fn [class name] (string/starts-with? (str class "/" name) (str cursor-value)))
                            (fn [_class name] (matches-fn name)))]
    (into []
          (comp
            q/xf-analysis->java-member-definitions
            (filter (fn [{:keys [class name]}]
                      (and (.equals ^String class (:class class-element))
                           (matches-member-fn class name))))
            (map (fn [member]
                   (let [package-and-class (string/split (:class member) #"\.")
                         package-name (string/join "." (pop package-and-class))
                         class-name (last package-and-class)]
                     {:label (if full-package?
                               (str (:class member) "/" (:name member))
                               (str class-name "/" (:name member)))
                      :kind (java-member-flags->kind member)
                      :detail package-name
                      :priority :java-member-definitions}))))
          (:analysis db))))

(defn ^:private remove-first-and-last-char [s]
  (-> (string/join "" (drop-last s))
      (subs 1)))

(defn ^:private merging-snippets [items cursor-loc next-loc function-call? matches-fn settings]
  (let [snippet-items (map (fn [snippet]
                             (if (:function-call snippet)
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
       (mapv #(dissoc % :priority))
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
      (let [settings (settings/all db)
            client-completion-item-caps (get-in db [:client-capabilities :text-document :completion :completion-item])
            resolve-support (-> (get client-completion-item-caps :resolve-support {})
                                (update :properties set))
            support-snippets? (get client-completion-item-caps :snippet-support false)
            non-local-db (update db :analysis dissoc uri)
            local-buckets (get-in db [:analysis uri])
            cursor-element (q/find-element-under-cursor db uri row col)
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
                                    (q/find-definition-from-cursor db uri caller-usage-row caller-usage-col))
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
                              (contains? (dep-graph/ns-names db) (symbol cursor-value-or-ns)))
            class-element (when (identical? :java-class-usages (:bucket cursor-element))
                            cursor-element)
            items (cond
                    inside-refer?
                    (with-refer-elements matches-fn cursor-loc non-local-db resolve-support)

                    inside-require?
                    (cond-> (with-ns-definition-elements matches-fn non-local-db resolve-support)
                      (and support-snippets?
                           simple-cursor?)
                      (merging-snippets cursor-loc next-loc function-call? matches-fn settings))

                    aliased-keyword-value?
                    (with-elements-from-aliased-keyword cursor-loc cursor-element local-buckets non-local-db resolve-support)

                    :else
                    (cond-> []
                      cursor-full-ns?
                      (into (with-elements-from-full-ns db cursor-value-or-ns resolve-support))

                      (and cursor-value-or-ns
                           (not keyword-value?))
                      (into (with-elements-from-alias cursor-loc cursor-value-or-ns cursor-value local-buckets matches-fn db uri resolve-support))

                      (or simple-cursor?
                          keyword-value?)
                      (-> (into (with-local-items matches-fn uri cursor-element local-buckets row col resolve-support))
                          (into (with-clojure-core-items matches-fn resolve-support)))

                      (:arglist-kws caller-var-definition)
                      (into (with-definition-kws-args-element-items matches-fn caller-var-definition resolve-support))

                      (and simple-cursor?
                           (supports-cljs? uri))
                      (into (with-clojurescript-items matches-fn resolve-support))

                      (and simple-cursor?
                           (supports-clj-core? uri))
                      (into (with-java-definition-items matches-fn cursor-value db))

                      (and class-element
                           (supports-clj-core? uri))
                      (into (with-java-static-member-definition-items matches-fn class-element cursor-value db))

                      (and support-snippets?
                           simple-cursor?)
                      (merging-snippets cursor-loc next-loc function-call? matches-fn settings)))]
        (->> items
             sorting-and-distincting-items
             ;; Limit the returned items for better performance.
             ;; If user needs more items one should be more specific in the completion query.
             (take 600))))))

;;;; Resolve Completion Item (completionItem/resolve)

(defn ^:private find-element-by-ns [{:keys [name ns uri]} db]
  (q/find-definition db {:uri uri
                         :name (symbol name)
                         :to (symbol ns)
                         :bucket :var-usages}))

(defn ^:private find-element-by-position [{:keys [uri name-row name-col]} db]
  (q/find-element-under-cursor db uri name-row name-col))

(defmulti ^:private resolve-unresolved (fn [unresolved-type _item _other-unresolved _args]
                                         unresolved-type))

(defmethod resolve-unresolved "documentation" [_ item other-unresolved {:keys [ns db db*] :as args}]
  (if-let [element (if ns
                     (find-element-by-ns args db)
                     (find-element-by-position args db))]
    (completion-item-with-documentation item element (:name args) db* {:additional-text-edits? (->> other-unresolved
                                                                                                    (map first)
                                                                                                    (some #{"alias"}))})
    item))

(defmethod resolve-unresolved "alias" [_ item _other-unresolved {:keys [uri alias-to-add ns-to-add db]}]
  (if-let [zloc (parser/safe-zloc-of-file db uri)]
    (completion-item-with-alias-edit item zloc (symbol alias-to-add) (symbol ns-to-add) db)
    item))

(defn resolve-item [{:keys [data] :as item} db*]
  (let [db @db*
        item (dissoc item :data)]
    (if-let [unresolved (some-> data :unresolved seq)]
      (reduce (fn [item [unresolved-type args]]
                (resolve-unresolved unresolved-type
                                    item
                                    unresolved
                                    (assoc args :db db :db* db*)))
              item
              unresolved)
      item)))
