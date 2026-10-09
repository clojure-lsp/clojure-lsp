(ns clojure-lsp.feature.inlay-hints
  (:require
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared :refer [fast=]]
   [clojure.string :as string]
   [edamame.core :as edamame]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(def ^:private skipped-namespaces
  #{'clojure.core 'cljs.core
    'clj-kondo/unknown-namespace
    :clj-kondo/unknown-namespace})

(def ^:private skipped-return-types #{"void" "java.lang.Void"})

(def ^:private java-lang-classes
  #{"Appendable" "Boolean" "Byte" "Character" "CharSequence" "Class" "Comparable"
    "Double" "Enum" "Exception" "Float" "Integer" "Iterable" "Long" "Math"
    "Number" "Object" "Runnable" "Short" "String" "StringBuffer" "StringBuilder"
    "System" "Thread" "Throwable" "Void"})

(defn ^:private child-nodes [node]
  (remove n/whitespace-or-comment? (n/children node)))

(defn ^:private token-value [node]
  (when (and node (contains? #{:token :multi-line} (n/tag node)))
    (try
      (n/sexpr node)
      (catch Exception _e
        nil))))

(defn ^:private token-symbol [node]
  (let [value (token-value node)]
    (when (symbol? value)
      value)))

(defn ^:private node-position [node row-key col-key]
  (let [metadata (meta node)
        row (get metadata row-key)
        col (get metadata col-key)]
    (when (and row col)
      (shared/row-col->position row col))))

(defn ^:private parameter-hint [node label]
  (when-let [position (node-position node :row :col)]
    {:position position
     :label label
     :kind :parameter}))

(defn ^:private type-hint [position label]
  (when (and position label)
    {:position position
     :label (str ": " label)
     :kind :type
     :padding-left true}))

(defn ^:private matching-arglist [arglist-strs argument-count]
  (let [matches (->> arglist-strs
                     (keep (fn [arglist-str]
                             (try
                               (let [params (vec (edamame/parse-string arglist-str {:auto-resolve #(symbol (str ":" %))}))
                                     rest-index (first (keep-indexed (fn [index parameter]
                                                                       (when (fast= '& parameter)
                                                                         index))
                                                                     params))
                                     descriptor (if rest-index
                                                  {:fixed-params (subvec params 0 rest-index)
                                                   :rest-param (get params (inc rest-index))}
                                                  {:fixed-params params})]
                                 (assoc descriptor :arglist-str arglist-str))
                               (catch Exception _e
                                 nil))))
                     (filter (fn [{:keys [fixed-params rest-param]}]
                               (if rest-param
                                 (>= argument-count (count fixed-params))
                                 (= argument-count (count fixed-params))))))]
    (when (= 1 (count matches))
      (first matches))))

(defn ^:private ignored-symbol? [parameter]
  (or (fast= '_ parameter)
      (string/starts-with? (name parameter) "_")))

(defn ^:private map-parameter-label [parameter]
  (let [parts (keep (fn [[label values]]
                      (when (seq values)
                        (str label " " (pr-str (vec values)))))
                    [[":keys" (:keys parameter)]
                     [":strs" (:strs parameter)]
                     [":syms" (:syms parameter)]])]
    (cond
      (seq parts) (str "{" (string/join " " parts) "}")
      (and (symbol? (:as parameter))
           (not (ignored-symbol? (:as parameter))))
      (str (:as parameter)))))

(defn ^:private parameter-label [parameter]
  (cond
    (symbol? parameter)
    (when-not (ignored-symbol? parameter)
      (str parameter ":"))

    (map? parameter)
    (when-let [label (map-parameter-label parameter)]
      (str label ":"))

    (and (vector? parameter) (seq parameter))
    (str parameter ":")))

(defn ^:private argument-matches-parameter? [argument-node parameter]
  (when (and (symbol? parameter) (n/symbol-node? argument-node))
    (let [argument (n/sexpr argument-node)]
      (and (symbol? argument)
           (fast= (name parameter) (name argument))))))

(defn ^:private var-parameter-hints [arglist argument-nodes]
  (keep-indexed
    (fn [index argument-node]
      (let [{:keys [fixed-params rest-param]} arglist
            fixed-count (count fixed-params)
            parameter (cond
                        (< index fixed-count) (nth fixed-params index)
                        (nil? rest-param) nil
                        (symbol? rest-param) rest-param
                        (= index fixed-count) rest-param)
            label (parameter-label parameter)]
        (when (and label
                   (not (argument-matches-parameter? argument-node parameter)))
          (parameter-hint argument-node label))))
    argument-nodes))

(defn ^:private arity-hint [usage definition arglist]
  (when (and arglist
             (:name-end-row usage)
             (:name-end-col usage)
             (> (count (remove nil? (:arglist-strs definition))) 1))
    (type-hint (shared/row-col->position (:name-end-row usage) (:name-end-col usage))
               (:arglist-str arglist))))

(defn ^:private source-qualified? [usage]
  (or (:alias usage)
      (:refer usage)
      (:full-qualified-symbol? usage)
      (:unresolved? usage)
      (contains? skipped-namespaces (:to usage))
      (= (:to usage) (:from usage))))

(defn ^:private namespace-hint [usage]
  (when (and (:to usage)
             (:name-end-row usage)
             (:name-end-col usage)
             (not (source-qualified? usage)))
    (type-hint (shared/row-col->position (:name-end-row usage) (:name-end-col usage))
               (str (:to usage)))))

(defn ^:private var-usage->hints [root-zloc db usage]
  (let [call-hints (when-let [function-loc (when-let [located (some-> root-zloc
                                                                      (parser/to-pos (:name-row usage) (:name-col usage))
                                                                      edit/find-function-usage-name-loc)]
                                             (let [{:keys [row col]} (meta (z/node located))]
                                               (when (and (= row (:name-row usage))
                                                          (= col (:name-col usage)))
                                                 located)))]
                     (let [argument-nodes (->> function-loc
                                               z/up
                                               z/node
                                               child-nodes
                                               (drop 1))
                           definition (q/find-definition db usage)]
                       (when (and definition (not (:macro definition)))
                         (when-let [arglist (matching-arglist (:arglist-strs definition)
                                                              (count argument-nodes))]
                           (concat (var-parameter-hints arglist argument-nodes)
                                   [(arity-hint usage definition arglist)])))))]
    (concat call-hints [(namespace-hint usage)])))

(defn ^:private simple-class-name [^String class-name]
  (let [array? (string/ends-with? class-name "[]")
        base (if array? (subs class-name 0 (- (count class-name) 2)) class-name)
        dot (.lastIndexOf base ".")]
    (cond-> (if (neg? dot) base (subs base (inc dot)))
      array? (str "[]"))))

(defn ^:private resolve-class-symbol [imported sym]
  (when (symbol? sym)
    (let [sym-name (name sym)]
      (cond
        (namespace sym) (str (namespace sym) "." sym-name)
        (contains? imported sym-name) (get imported sym-name)
        (contains? java-lang-classes sym-name) (str "java.lang." sym-name)))))

(defn ^:private meta-class [node imported]
  (when (= :meta (some-> node n/tag))
    (let [tag (token-value (first (child-nodes node)))]
      (cond
        (symbol? tag) (resolve-class-symbol imported tag)
        (string? tag) tag
        (and (map? tag) (symbol? (:tag tag))) (resolve-class-symbol imported (:tag tag))
        (and (map? tag) (string? (:tag tag))) (:tag tag)))))

(defn ^:private binding-class [node root-zloc db uri imported]
  (let [{:keys [row col]} (meta node)
        element (when (and row col) (q/find-element-under-cursor db uri row col))]
    (when (contains? #{:locals :local-usages} (:bucket element))
      (let [definition (q/find-definition db element)
            binding-zloc (parser/to-pos root-zloc (:name-row definition) (:name-col definition))]
        (when binding-zloc
          (when-let [class-name (meta-class (some-> binding-zloc z/up z/node) imported)]
            [class-name :binding]))))))

(defn ^:private literal-class [node]
  (let [value (token-value node)]
    (when-let [class-name (cond
                            (string? value) "java.lang.String"
                            (boolean? value) "java.lang.Boolean")]
      [class-name :literal])))

(defn ^:private constructor-class [node imported]
  (when (= :list (some-> node n/tag))
    (when-let [head (token-symbol (first (child-nodes node)))]
      (let [head-name (str head)]
        (when (string/ends-with? head-name ".")
          (when-let [class-name (resolve-class-symbol imported (symbol (subs head-name 0 (dec (count head-name)))))]
            [class-name :constructor]))))))

(defn ^:private receiver-class [node root-zloc db uri imported]
  (when node
    (or (when (= :meta (n/tag node))
          (if-let [class-name (meta-class node imported)]
            [class-name :meta]
            (receiver-class (second (child-nodes node)) root-zloc db uri imported)))
        (literal-class node)
        (constructor-class node imported)
        (binding-class node root-zloc db uri imported))))

(defn ^:private dot-invocation [zloc]
  (when-let [parent (z/up zloc)]
    (or (when (= :list (some-> parent z/tag))
          (let [call-node (z/node parent)
                children (vec (child-nodes call-node))
                head-name (some-> (token-symbol (first children)) name)]
            (cond
              (and head-name
                   (string/starts-with? head-name ".")
                   (not (contains? #{"." ".."} head-name)))
              {:receiver (second children)
               :arguments (vec (drop 2 children))
               :call-node call-node}

              (= "." head-name)
              (let [method (nth children 2 nil)]
                {:receiver (second children)
                 :arguments (if (= :list (some-> method n/tag))
                              (vec (rest (child-nodes method)))
                              (vec (drop 3 children)))
                 :call-node call-node}))))
        (dot-invocation parent))))

(defn ^:private java-call-arguments [symbol-zloc]
  (let [parent (z/up symbol-zloc)]
    (when (= :list (some-> parent z/tag))
      (let [children (vec (child-nodes (z/node parent)))
            symbol-node (z/node symbol-zloc)
            first-child (first children)]
        (when (and symbol-node
                   first-child
                   (= (:row (meta symbol-node)) (:row (meta first-child)))
                   (= (:col (meta symbol-node)) (:col (meta first-child))))
          {:arguments (vec (rest children))
           :call-node (z/node parent)})))))

(defn ^:private unique-arity-member [members argument-count]
  (let [matches (filterv (fn [member]
                           (and (or (:method (:flags member))
                                    (:parameter-types member)
                                    (:parameters member)
                                    (fast= "<init>" (:name member)))
                                (= argument-count (count (or (:parameter-types member)
                                                             (:parameters member)
                                                             [])))))
                         members)]
    (when (= 1 (count matches))
      (first matches))))

(defn ^:private prefer-flag [members flag present?]
  (let [matches (filterv #(= present? (boolean (flag (:flags %)))) members)]
    (if (seq matches) matches members)))

(defn ^:private members-named [index class-name method-name]
  (filterv #(fast= method-name (:name %)) (get index class-name)))

(defn ^:private member-type-label [member]
  (let [type-name (or (:return-type member) (:type member))]
    (when (and type-name (not (contains? skipped-return-types type-name)))
      (simple-class-name type-name))))

(defn ^:private java-parameter-label [parameter-text parameter-type]
  (let [tokens (when parameter-text (string/split parameter-text #"\s+"))
        parameter-name (when (and tokens (> (count tokens) 1))
                         (peek tokens))]
    (or parameter-name
        (some-> parameter-type simple-class-name)
        (some-> parameter-text simple-class-name))))

(defn ^:private java-parameter-hints [member arguments]
  (keep-indexed
    (fn [index argument]
      (let [label (java-parameter-label (get (:parameters member) index)
                                        (get (:parameter-types member) index))
            parameter (when (and label (re-matches #"[A-Za-z_*][A-Za-z0-9_*]*" label))
                        (symbol label))]
        (when (and label
                   (not (argument-matches-parameter? argument parameter)))
          (parameter-hint argument (str label ":")))))
    arguments))

(defn ^:private usage-symbol-zloc [root-zloc usage]
  (when (and (:name-row usage) (:name-col usage))
    (parser/to-pos root-zloc (:name-row usage) (:name-col usage))))

(defn ^:private java-candidates [member-index usage symbol-zloc call]
  (cond
    (and call
         (let [sym (token-symbol (some-> symbol-zloc z/node))]
           (and sym (string/ends-with? (name sym) "."))))
    (let [members (get member-index (:class usage))
          inits (filterv #(fast= "<init>" (:name %)) members)]
      (if (seq inits)
        inits
        (filterv #(fast= (some-> (:class usage) simple-class-name) (:name %)) members)))

    (:method-name usage)
    (prefer-flag (members-named member-index (:class usage) (:method-name usage))
                 :static
                 true)

    :else []))

(defn ^:private selected-java-member [candidates call]
  (if call
    (unique-arity-member candidates (count (:arguments call)))
    (or (let [fields (filterv (fn [{:keys [flags]}]
                                (and (:field flags)
                                     (not (:method flags))))
                              candidates)]
          (when (= 1 (count fields))
            (first fields)))
        (unique-arity-member candidates 0))))

(defn ^:private java-usage->hints [root-zloc member-index usage]
  (when (and (:class usage) (not (:import usage)))
    (when-let [symbol-zloc (usage-symbol-zloc root-zloc usage)]
      (let [call (java-call-arguments symbol-zloc)
            member (selected-java-member (java-candidates member-index usage symbol-zloc call) call)
            type-node (if call (:call-node call) (z/node symbol-zloc))]
        (concat
          (when (and call member)
            (java-parameter-hints member (:arguments call)))
          [(type-hint (node-position type-node :end-row :end-col)
                      (member-type-label member))])))))

(defn ^:private instance-usage->hints [root-zloc db uri imported member-index usage]
  (when-let [method-zloc (usage-symbol-zloc root-zloc usage)]
    (when-let [{:keys [receiver arguments call-node]} (dot-invocation method-zloc)]
      (let [[class-name source] (receiver-class receiver root-zloc db uri imported)
            member (unique-arity-member
                     (prefer-flag (members-named member-index class-name (:method-name usage))
                                  :static
                                  false)
                     (count arguments))]
        (concat
          (when member
            (java-parameter-hints member arguments))
          [(when (= :binding source)
             (type-hint (node-position receiver :end-row :end-col)
                        (simple-class-name class-name)))]
          [(type-hint (node-position call-node :end-row :end-col)
                      (member-type-label member))])))))

(defn hints [uri range {:keys [db*] :as components}]
  (when-let [root-zloc (some-> (f.file-management/force-get-document-text uri components)
                               parser/safe-zloc-of-string)]
    (let [db @db*
          analysis (get-in db [:analysis uri])
          imported (into {}
                         (keep (fn [{:keys [import class]}]
                                 (when (and import class)
                                   [(simple-class-name class) class])))
                         (:java-class-usages analysis))
          class-names (into (set (keep :class (:java-class-usages analysis)))
                            (keep (fn [usage]
                                    (when-let [method-zloc (usage-symbol-zloc root-zloc usage)]
                                      (when-let [{:keys [receiver]} (dot-invocation method-zloc)]
                                        (first (receiver-class receiver root-zloc db uri imported))))))
                            (:instance-invocations analysis))
          member-index (if (empty? class-names)
                         {}
                         (reduce
                           (fn [index {:keys [class] :as member}]
                             (if (contains? class-names class)
                               (update index class (fnil conj []) member)
                               index))
                           {}
                           (into [] (mapcat :java-member-definitions) (vals (:analysis db)))))]
      (->> (concat (mapcat #(var-usage->hints root-zloc db %) (:var-usages analysis))
                   (mapcat #(java-usage->hints root-zloc member-index %) (:java-class-usages analysis))
                   (mapcat #(instance-usage->hints root-zloc db uri imported member-index %)
                           (:instance-invocations analysis)))
           (remove nil?)
           (filter (fn [hint]
                     (let [position [(:line (:position hint)) (:character (:position hint))]
                           {:keys [start end]} range]
                       (and (not (neg? (compare position [(:line start) (:character start)])))
                            (neg? (compare position [(:line end) (:character end)]))))))
           distinct
           vec))))
