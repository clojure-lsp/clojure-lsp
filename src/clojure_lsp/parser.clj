(ns clojure-lsp.parser
  (:require
    [clojure-lsp.clojure-core :as cc]
    [clojure-lsp.db :as db]
    [clojure-lsp.refactor.edit :as edit]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]
    [rewrite-clj.zip.find :as zf]
    [rewrite-clj.zip.move :as zm]
    [rewrite-clj.zip.subedit :as zsub])
  (:import
    (rewrite_clj.node.meta MetaNode)))

(declare find-usages*)
(declare parse-destructuring)

(def core-refers
  (->> cc/core-syms
       (mapv (juxt identity (constantly 'clojure.core)))
       (into {})))

(def lang-imports
  (->> cc/java-lang-syms
       (mapv (juxt identity #(symbol (str "java.lang." (name %)))))
       (into {})))

(def default-env
  {:ns 'user
   :requires #{'clojure.core}
   :refer-all-syms {}
   :refers {}
   :imports {}
   :aliases {}
   :local-classes #{}
   :publics #{}
   :locals #{}
   :usages []
   :ignored? false})

(defmacro zspy [loc]
  `(do
     (log/warn '~loc (pr-str (z/sexpr ~loc)))
     ~loc))

(defn z-next-sexpr [loc]
  (z/find-next loc z/next #(not (n/printable-only? (z/node %)))))

(defn z-right-sexpr [loc]
  (z/find-next loc z/right #(not (n/printable-only? (z/node %)))))

(defn ident-split [ident-str]
  (let [ident-conformed (some-> ident-str (string/replace ":" ""))
        prefix (string/replace ident-str #"^(::?)?.*" "$1")
        idx (string/index-of ident-conformed "/")]
    (if (and idx (not= idx (dec (count ident-conformed))))
      (into [prefix] (string/split ident-conformed #"/" 2))
      [prefix nil ident-conformed])))

(defn skip-meta [ident-node]
  (loop [result ident-node]
    (if (instance? MetaNode result)
      (->> result
           (n/children)
           (remove n/printable-only?)
           (second)
           recur)
      result)))

(defn qualify-ident [ident-node {:keys [aliases local-classes imports locals publics refers requires refer-all-syms file-type] :as context} scoped declaration?]
  (when (ident? (n/sexpr ident-node))
    (let [ident (n/sexpr ident-node)
          ident-str (n/string (skip-meta ident-node))
          [prefix ident-ns-str ident-name] (ident-split ident-str)
          ident-ns (some-> ident-ns-str symbol)
          alias-ns (get aliases ident-ns-str)
          ns-sym (:ns context)
          declared (when (and ns-sym (or (get locals ident-name) (get publics ident-name)))
                              (symbol (name ns-sym) ident-name))
          local-classes' (->> local-classes
                              (map (juxt identity #(symbol (str (name ns-sym) "." %))))
                              (into {}))
          java-classes (merge local-classes' imports lang-imports)
          java-sym (get java-classes (symbol (string/replace ident-name #"\.$" "")))
          refered (get refers ident-name)
          required-ns (get requires ident-ns)
          ctr (if (symbol? ident) symbol keyword)]
      (assoc
        (if-not ident-ns
          (cond
            java-sym {:sym java-sym :tags #{:norename}}
            declaration? {:sym (ctr (name (or ns-sym 'user)) ident-name)}
            (and (keyword? ident) (= prefix "::")) {:sym (ctr (name ns-sym) ident-name)}
            (keyword? ident) {:sym ident}
            (contains? scoped ident) {:sym (ctr (name (get-in scoped [ident :ns])) ident-name) :tags #{:scoped}}
            declared {:sym declared}
            refered {:sym refered}
            (contains? refer-all-syms ident) {:sym (get refer-all-syms ident)}
            (contains? core-refers ident) {:sym (ctr (name (get core-refers ident)) ident-name) :tags #{:norename}}
            (string/starts-with? ident-name ".") {:sym ident :tags #{:method :norename}}
            :else {:sym (ctr (name (gensym)) ident-name) :tags #{:unknown}})
          (cond
            (and alias-ns
                 (or
                   (and (keyword? ident) (= prefix "::"))
                   (symbol? ident)))
            {:sym (ctr (name alias-ns) ident-name)}

            (and (keyword? ident) (= prefix "::"))
            {:sym (ctr (name (gensym)) ident-name) :tags #{:unknown} :unknown-ns ident-ns}

            (keyword? ident)
            {:sym ident}

            required-ns
            {:sym ident}

            (contains? imports ident-ns)
            {:sym (symbol (name (get imports ident-ns)) ident-name) :tags #{:method :norename}}

            (contains? lang-imports ident-ns)
            {:sym (symbol (name (get lang-imports ident-ns)) ident-name) :tags #{:method :norename}}

            (and (= :cljs file-type) (= 'js ident-ns))
            {:sym ident :tags #{:method :norename}}

            :else
            {:sym (ctr (name (gensym)) ident-name) :tags #{:unknown} :unknown-ns ident-ns}))
        :str ident-str))))

(defn add-reference [context scoped node extra]
  (let [{:keys [row end-row col end-col]} (meta node)
        sexpr (n/sexpr node)
        scope-bounds (get-in scoped [sexpr :bounds])
        ctx @context
        declaration? (and (get-in extra [:tags :declare])
                          (or (get-in extra [:tags :local])
                              (get-in extra [:tags :public])))
        ident-info (qualify-ident node ctx scoped declaration?)
        new-usage (cond-> {:row row
                           :end-row end-row
                           :col col
                           :end-col end-col
                           :file-type (:file-type @context)}
                    :always (merge ident-info)
                    (seq extra) (merge extra)
                    (and (:ignored? ctx) declaration?) (-> (update :sym (fn [s] (symbol (str "__lsp_comment_" (gensym)) (name s))))
                                                           (update :tags disj :local :public))
                    (seq scope-bounds) (assoc :scope-bounds scope-bounds))]
    (vswap! context update :usages conj new-usage)
    new-usage))

(defn destructure-map [map-loc scope-bounds context scoped]
  (loop [key-loc (z/down (zsub/subzip map-loc))
         scoped scoped]
    (if (and key-loc (not (z/end? key-loc)))
      (let [key-sexpr (z/sexpr key-loc)
            val-loc (z-right-sexpr key-loc)]
        (cond
          (#{:keys :strs :syms} key-sexpr)
          (recur (edit/skip-over val-loc)
                 (loop [child-loc (z/down val-loc)
                        scoped scoped]
                   (if child-loc
                     (let [sexpr (z/sexpr child-loc)
                           scoped-ns (gensym)
                           new-scoped (assoc scoped (symbol (name sexpr)) {:ns scoped-ns :bounds scope-bounds})]
                       (add-reference context scoped (z/node child-loc) {:tags #{:declare :param}
                                                                         :scope-bounds scope-bounds
                                                                         :sym (symbol (name scoped-ns)
                                                                                      (name sexpr))})
                       (if (nil? (z-right-sexpr child-loc))
                         new-scoped
                         (recur (z-right-sexpr child-loc) new-scoped)))
                     scoped)))

          (= :as key-sexpr)
          (let [val-node (z/node val-loc)
                sexpr (n/sexpr val-node)
                scoped-ns (gensym)
                new-scoped (assoc scoped sexpr {:ns scoped-ns :bounds scope-bounds})]
            (add-reference context scoped (z/node val-loc) {:tags #{:declare :param}
                                                            :scope-bounds scope-bounds
                                                            :sym (symbol (name scoped-ns)
                                                                         (name sexpr))})
            (recur (z-right-sexpr val-loc) new-scoped))

          (keyword? key-sexpr)
          (recur (edit/skip-over val-loc) scoped)

          (not= '& key-sexpr)
          (recur (z-right-sexpr val-loc) (parse-destructuring key-loc scope-bounds context scoped))

          :else
          (recur (edit/skip-over val-loc) scoped)))
      scoped)))

(defn handle-rest
  "Crawl each form from `loc` to the end of the parent-form
  `(fn [x 1] | (a) (b) (c))`
  With cursor at `|` find references for `(a)`, `(b)`, and `(c)`"
  [loc context scoped]
  (loop [sub-loc loc]
    (when sub-loc
      (find-usages* (zsub/subzip sub-loc) context scoped)
      (recur (zm/right sub-loc)))))

(defn parse-destructuring [param-loc scope-bounds context scoped]
  (if param-loc
    (loop [param-loc (zsub/subzip param-loc)
           scoped scoped]
      ;; MUTATION Updates scoped AND adds declared param references
      (if (and param-loc (not (z/end? param-loc)))
        (let [node (z/node param-loc)
              sexpr (n/sexpr node)]
          (cond
            (= :meta (z/tag param-loc))
            (let [meta-loc (z-next-sexpr param-loc)]
              (handle-rest (zsub/subzip meta-loc) context scoped)
              (recur (z-right-sexpr meta-loc) scoped))

            (and (symbol? sexpr) (not= '& sexpr))
            (let [scoped-ns (gensym)
                  new-scoped (assoc scoped sexpr {:ns scoped-ns :bounds scope-bounds})]
              (add-reference context new-scoped node
                             {:tags #{:declare :param}
                              :scope-bounds scope-bounds
                              :sym (symbol (name scoped-ns)
                                           (name sexpr))})
              (recur (z-next-sexpr param-loc) new-scoped))

            (vector? sexpr)
            (recur (z-next-sexpr param-loc) scoped)

            (map? sexpr)
            (recur (edit/skip-over param-loc) (destructure-map param-loc scope-bounds context scoped))

            :else
            (recur (edit/skip-over param-loc) scoped)))
        scoped))
    scoped))

(defn end-bounds [loc]
  (select-keys (meta (z/node loc)) [:end-row :end-col]))

(defn parse-bindings [bindings-loc context end-scope-bounds scoped]
  (try
    (loop [binding-loc (zm/down (zsub/subzip bindings-loc))
           scoped scoped]
      (let [not-done? (and binding-loc (not (z/end? binding-loc)))]
        ;; MUTATION Updates scoped AND adds declared param references AND adds references in binding vals)
        (cond
          (and not-done? (= :uneval (z/tag binding-loc)))
          (recur (edit/skip-over binding-loc) scoped)

          not-done?
          (let [right-side-loc (z-right-sexpr binding-loc)
                binding-sexpr (z/sexpr binding-loc)]
            ;; Maybe for/doseq needs to use different bindings
            (cond
              (= :let binding-sexpr)
              (let [new-scoped (parse-bindings right-side-loc context end-scope-bounds scoped)]
                (recur (edit/skip-over right-side-loc) new-scoped))

              (#{:when :while} binding-sexpr)
              (do
                (handle-rest (zsub/subzip right-side-loc) context scoped)
                (recur (edit/skip-over right-side-loc) scoped))

              :else
              (let [{:keys [end-row end-col]} (meta (z/node (or (z-right-sexpr right-side-loc) (z/up right-side-loc) bindings-loc)))
                      scope-bounds (assoc end-scope-bounds :row end-row :col end-col)
                      new-scoped (parse-destructuring binding-loc scope-bounds context scoped)]
                  (when right-side-loc (handle-rest (zsub/subzip right-side-loc) context scoped))
                  (recur (edit/skip-over right-side-loc) (if right-side-loc new-scoped scoped)))))

          :else
          scoped)))
    (catch Throwable e
      (log/warn "bindings" (.getMessage e) (z/sexpr bindings-loc) (z/sexpr (z/up bindings-loc)))
      (throw e))))

(defn parse-params [params-loc context scoped]
  (try
    (let [{:keys [row col]} (meta (z/node params-loc))
          {:keys [end-row end-col]} (meta (z/node (z/up params-loc)))
          scope-bounds {:row row :col col :end-row end-row :end-col end-col}
          single? (not= :vector (z/tag params-loc))]
      (loop [param-loc (cond-> params-loc
                         (not single?) (z/down))
             scoped scoped]
        (if param-loc
          (let [new-scoped (parse-destructuring param-loc scope-bounds context scoped)]
            (if (or single? (nil? (z-right-sexpr param-loc)))
              new-scoped
              (recur (z-right-sexpr param-loc) new-scoped)))
          scoped)))
    (catch Exception e
      (log/warn "params" (.getMessage e) (z/sexpr params-loc))
      (throw e))))

(defn handle-ignored
  [rest-loc context scoped]
  (let [curr-ignored? (:ignored? @context false)]
    (vswap! context assoc :ignored? true)
    (handle-rest rest-loc context scoped)
    (vswap! context assoc :ignored? curr-ignored?)))

(defn handle-comment
  [op-loc _loc context scoped]
  (handle-ignored (z-right-sexpr op-loc) context scoped))

(defn handle-quote
  [op-loc _loc context scoped]
  (let [current-quoted (:quoting? @context)]
    (vswap! context assoc :quoting? true)
    (handle-rest (z-right-sexpr op-loc) context scoped)
    (vswap! context assoc :quoting? current-quoted)))

(defn add-libspec [libtype context scoped entry-loc prefix-ns]
  (let [entry-ns-loc (z/down entry-loc)
        required-ns (z/sexpr entry-ns-loc)
        full-ns (if prefix-ns
                  (symbol (str (name prefix-ns) "." (name required-ns)))
                  required-ns)
        alias-loc (some-> entry-ns-loc (z/find-value :as) (z-right-sexpr))
        refer-loc (some-> entry-ns-loc (z/find-value :refer) (z-right-sexpr))
        refer-all? (when refer-loc (= (z/sexpr refer-loc) :all))]
    (when (= libtype :require)
      (vswap! context update :requires conj full-ns)
      (when refer-all?
        (vswap! context update :refer-all-syms merge
                (->> (for [[_ usages] (:file-envs @db/db)
                           :when (->> usages
                                      (filter (comp #(set/subset? #{:ns :public} %) :tags))
                                      (filter (comp #{full-ns} :sym))
                                      (seq))
                           {:keys [sym tags]} usages
                           :when (set/subset? #{:public :declare} tags)]
                       [(symbol (name sym)) sym])
                     (into {})))))
    (add-reference context scoped (z/node entry-ns-loc)
                   (cond-> {:tags #{libtype} :sym full-ns}
                     alias-loc (assoc :alias (z/sexpr alias-loc))))
    (when alias-loc
      (vswap! context update :aliases assoc (z/string alias-loc) full-ns)
      (add-reference context scoped (z/node alias-loc)
                     {:tags #{:alias :declare}
                      :ns full-ns
                      :sym (z/sexpr alias-loc)}))
    (when (and refer-loc (not refer-all?))
      (doseq [refer-node (remove n/printable-only? (n/children (z/node refer-loc)))
              :let [refered (symbol (name full-ns) (n/string refer-node))
                    referee (n/string refer-node)]]
        (vswap! context update :refers assoc referee refered)
        (add-reference context scoped refer-node
                       {:tags #{:refer :declare}
                        :sym refered})))))

(defn add-libspecs [libtype context scoped entry-loc prefix-ns]
  (let [libspec? (fn [sexpr] (or (vector? sexpr) (list? sexpr)))
        prefix? (fn [sexpr] (or (symbol? (second sexpr)) (libspec? (second sexpr))))]
    (loop [entry-loc entry-loc]
      (when (and entry-loc (not (-> entry-loc z/node n/printable-only?)))
        (let [sexpr (z/sexpr entry-loc)]
          (cond
            (symbol? sexpr)
            (let [full-ns (if prefix-ns
                            (symbol (str (name prefix-ns) "." (name sexpr)))
                            sexpr)
                  class-sym (when (= libtype :import)
                              (-> full-ns
                                  name
                                  (string/split #"\.")
                                  last
                                  symbol))]
              (when class-sym
                (vswap! context update :imports assoc class-sym full-ns full-ns full-ns))

              (when (= libtype :require)
                (vswap! context update :requires conj full-ns))
              (add-reference context scoped (z/node entry-loc)
                             {:tags #{libtype} :sym full-ns}))
            (and (libspec? sexpr) (prefix? sexpr))
            (add-libspecs libtype context scoped (z-right-sexpr (z/down entry-loc)) (z/sexpr (z/down entry-loc)))

            (libspec? sexpr)
            (add-libspec libtype context scoped entry-loc prefix-ns)))
        (recur (z-right-sexpr entry-loc))))))

(defn handle-ns
  [_op-loc loc context scoped]
  (let [name-loc (z-right-sexpr (z/down (zsub/subzip loc)))
        first-list-loc (z/find-tag name-loc z-right-sexpr :list)
        require-loc (z/find first-list-loc z-right-sexpr (comp #{:require} z/sexpr z/down))
        require-macros-loc (z/find first-list-loc z-right-sexpr (comp #{:require-macros} z/sexpr z/down))
        import-loc (z/find first-list-loc z-right-sexpr (comp #{:import} z/sexpr z/down))]
    (vswap! context assoc :ns (z/sexpr name-loc))
    (add-reference context scoped (z/node name-loc) {:tags #{:declare :public :ns} :kind :module :sym (z/sexpr name-loc)})
    (add-libspecs :require context scoped (some-> require-macros-loc z/down z-right-sexpr) nil)
    (add-libspecs :require context scoped (some-> require-loc z/down z-right-sexpr) nil)
    (add-libspecs :import context scoped (some-> import-loc z/down z-right-sexpr) nil)))

(defn handle-let
  [op-loc loc context scoped]
  (let [bindings-loc (zf/find-tag op-loc :vector)
        scoped (parse-bindings bindings-loc context (end-bounds loc) scoped)]
    (handle-rest (z-right-sexpr bindings-loc) context scoped)))

(defn handle-if-let
  [op-loc _loc context scoped]
  (let [bindings-loc (zf/find-tag op-loc :vector)
        if-loc (z-right-sexpr bindings-loc)
        if-scoped (parse-bindings bindings-loc context (end-bounds if-loc) scoped)]
    (handle-rest if-loc context if-scoped)
    (handle-rest (z-right-sexpr if-loc) context scoped)))

(defn- local? [op-loc]
  (let [op (z/sexpr op-loc)
        op-name (name op)
        name-sexpr (z/sexpr (z-right-sexpr op-loc))]
    (or (and (symbol? name-sexpr) (:private (meta name-sexpr)))
        (not (string/starts-with? op-name "def"))
        (string/ends-with? op-name "-"))))

(defn handle-def
  [op-loc _loc context scoped]
  (let [name-loc (z-right-sexpr op-loc)
        def-sym (z/node name-loc)
        def-meta  (meta (z/sexpr name-loc))
        op-local? (local? op-loc)]
    (if op-local?
      (vswap! context update :locals conj (name (n/sexpr def-sym)))
      (vswap! context update :publics conj (name (n/sexpr def-sym))))
    (add-reference context scoped def-sym {:tags (if op-local?
                                                   #{:declare :local}
                                                   #{:declare :public})
                                           :doc (:doc def-meta)
                                           :signatures (:arglists def-meta)})
    (handle-rest (z-right-sexpr name-loc)
                 context scoped)))

(defn- function-signatures [params-loc]
  (if (= :list (z/tag params-loc))
    (loop [list-loc params-loc
           params []]
      (let [params (conj params (z/string (z/down list-loc)))]
        (if-let [next-list (z/find-next-tag list-loc :list)]
          (recur next-list params)
          params)))
    [(z/string params-loc)]))

(defn- single-params-and-body [params-loc context scoped]
  (let [body-loc (z-right-sexpr params-loc)]
    (->> (parse-params params-loc context scoped)
         (handle-rest body-loc context))))

(defn- function-params-and-bodies [params-loc context scoped]
  (if (= :list (z/tag params-loc))
    (loop [list-loc params-loc]
      (single-params-and-body (z/down list-loc) context scoped)
      (when-let [next-list (z/find-next-tag list-loc :list)]
        (recur next-list)))
    (single-params-and-body params-loc context scoped)))

(def check (fn [pred x] (when (pred x) x)))

(defn handle-function
  [op-loc _loc context scoped name-tags]
  (let [op-local? (local? op-loc)
        op-fn? (= "fn" (name (z/sexpr op-loc)))
        name-loc (z-right-sexpr op-loc)
        params-loc (z/find op-loc (fn [loc] (#{:vector :list} (z/tag loc))))]
    (when (symbol? (z/sexpr name-loc))
      (cond
        op-fn? nil
        op-local? (vswap! context update :locals conj (name (z/sexpr name-loc)))
        :else (vswap! context update :publics conj (name (z/sexpr name-loc))))

      (add-reference context scoped (z/node name-loc)
                     {:tags (cond
                              op-fn? name-tags
                              op-local? (conj name-tags :local)
                              :else (conj name-tags :public))
                      :kind :function
                      :doc (check string? (z/sexpr (z-right-sexpr name-loc)))
                      :signatures (function-signatures params-loc)}))
    (function-params-and-bodies params-loc context scoped)))

(defn handle-fn
  [op-loc loc context scoped]
  (handle-function op-loc loc context scoped #{:declare}))

(defn handle-defn
  [op-loc loc context scoped]
  (handle-function op-loc loc context scoped #{:declare}))

(defn handle-defmacro
  [op-loc loc context scoped]
  (handle-function op-loc loc context scoped #{:declare}))

(defn handle-fn-spec [method-loc context scoped tags]
  (let [name-loc (z/down method-loc)
        params-loc (z-right-sexpr name-loc)]
    (add-reference context scoped (z/node name-loc) {:tags tags})
    (let [new-scoped (parse-params params-loc context scoped)]
      (handle-rest (z-right-sexpr params-loc) context new-scoped))))

(defn handle-type-methods
  [proto-loc context scoped]
  (loop [prev-loc proto-loc
         method-loc (z-right-sexpr proto-loc)]
    (if (and method-loc (= :list (z/tag method-loc)))
      (do
        (handle-fn-spec method-loc context scoped #{:method :norename})
        (recur method-loc (z-right-sexpr method-loc)))
      prev-loc)))

(defn handle-deftype
  [op-loc _loc context scoped]
  (let [type-loc (z-right-sexpr op-loc)
        fields-loc (z-right-sexpr type-loc)]
    (vswap! context update :local-classes conj (z/sexpr type-loc))
    (add-reference context scoped (z/node type-loc) {:tags #{:declare :public}
                                                     :kind :class
                                                     :signatures [(z/sexpr fields-loc)]})

    (when (= "defrecord" (name (z/sexpr op-loc)))
      (let [type-name (name (z/sexpr type-loc))
            mapper-name (str "map->" type-name)
            mapper (with-meta
                     (z/node (z/replace type-loc (symbol mapper-name)))
                     (meta (z/node type-loc)))
            construct-name (str "->" type-name)
            constructor (with-meta
                          (z/node (z/replace type-loc (symbol construct-name)))
                          (meta (z/node type-loc)))]
        (vswap! context update :publics conj mapper-name)
        (add-reference context scoped mapper {:tags #{:declare :public :factory :norename}
                                              :kind :function})
        (vswap! context update :publics conj construct-name)
        (add-reference context scoped constructor {:tags #{:declare :public :factory :norename}
                                                   :kind :function})))
    (let [field-scope (parse-params fields-loc context scoped)]
      (loop [proto-loc (z-right-sexpr fields-loc)]
        (when proto-loc
          (add-reference context field-scope (z/node proto-loc) {})
          (let [next-loc (z-right-sexpr proto-loc)]
            (cond
              (= (z/tag next-loc) :token)
              (recur next-loc)

              (= (z/tag next-loc) :list)
              (some-> proto-loc
                      (handle-type-methods context field-scope)
                      (z-right-sexpr)
                      (recur)))))))))

(defn handle-dispatch-macro
  [loc context scoped]
  (vswap! context assoc :in-fn-literal? true)
  (->>
    (loop [sub-loc (z-next-sexpr (zsub/subzip loc))
           scoped scoped]
      (if (and sub-loc (not (z/end? sub-loc)))
        (let [sexpr (z/sexpr sub-loc)]
          (if (and (symbol? sexpr)
                   (re-find #"^%(:?\d+|&)?$" (name sexpr)))
            (recur (z-next-sexpr sub-loc) (assoc scoped sexpr {:ns (gensym) :bounds (meta (z/node sub-loc))}))
            (recur (z-next-sexpr sub-loc) scoped)))
        scoped))
    (handle-rest loc context))
  (vswap! context dissoc :in-fn-literal?))

(comment
  '[
    clojure.core/dotimes
    clojure.core/letfn
    clojure.core/with-local-vars
    ])

(def ^:dynamic *sexpr-handlers*
  {'clojure.core/ns handle-ns
   'clojure.core/defn handle-defn
   'clojure.core/defn- handle-defn
   'clojure.core/fn handle-fn
   'clojure.core/defmulti handle-def
   'clojure.core/deftype handle-deftype
   'clojure.core/defrecord handle-deftype
   'clojure.core/def handle-def
   'clojure.core/defonce handle-def
   'clojure.core/defmacro handle-defmacro
   'clojure.core/let handle-let
   'clojure.core/when-let handle-let
   'clojure.core/when-some handle-let
   'clojure.core/when-first handle-let
   'clojure.core/if-let handle-if-let
   'clojure.core/if-some handle-if-let
   'clojure.core/with-open handle-let
   'clojure.core/loop handle-let
   'clojure.core/for handle-let
   'clojure.core/doseq handle-let
   'clojure.core/comment handle-comment
   'clojure.core/quote handle-quote})

(def default-macro-defs
  {'clojure.core.match/match [:element {:element [:params :bound-element] :repeat true}]
   'clojure.core/as-> [:element :param :bound-elements]
   'clojure.core/catch [:element :param :bound-elements]
   'clojure.core/declare [{:element :declaration :repeat true}]
   'clojure.core/defmethod [:element :element :function-params-and-bodies]
   'clojure.core/proxy [:element :element {:element :fn-spec :repeat true :tags #{:method :norename}}]
   'clojure.test/are [:params :bound-element :elements]
   'clojure.test/deftest [{:element :declaration :tags #{:unused}} :elements]
   'compojure.core/ANY [:element :param :bound-elements]
   'compojure.core/DELETE [:element :param :bound-elements]
   'compojure.core/GET [:element :param :bound-elements]
   'compojure.core/PATCH [:element :param :bound-elements]
   'compojure.core/POST [:element :param :bound-elements]
   'compojure.core/PUT [:element :param :bound-elements]
   'compojure.core/context [:element :param :bound-elements]
   'compojure.core/defroutes [:declaration :elements]
   'korma.core/defentity [:declaration :elements]
   'net.cgrand.enlive-html/deftemplate [:declaration :element :params :bound-elements]
   'outpace.config/defconfig [:declaration :element]
   'outpace.config/defconfig! [:declaration :element]
   're-frame.core/reg-event-db [{:element :declaration :signature [:next :next :next :right]} :element]
   're-frame.core/reg-event-fx [{:element :declaration :signature [:next :next :next :right]} :element]
   're-frame.core/reg-sub [{:element :declaration :signature [:rightmost :next :next :next :right]} :element]
   'schema.macros/try-catchall [{:element :bound-elements :sub-forms {'catch [:param :bound-elements]}}]
   'slingshot.slingshot/try+ [{:element :bound-elements :sub-forms {'else [:elements]}}]})

(defn- macro-signature-loc [signature-dirs element-loc]
  (when signature-dirs
    (loop [curr-loc (zsub/subzip (z-right-sexpr element-loc))
           [dir & dirs] signature-dirs]
      (let [next-loc (case dir
                       :next (z-next-sexpr curr-loc)
                       :right (z-right-sexpr curr-loc)
                       :rightmost (z/rightmost curr-loc)
                       curr-loc)]
        (if (seq dirs)
          (recur next-loc dirs)
          next-loc)))))

(defn- macro-declaration [{:keys [signature kind tags doc? attr-map?]} element-loc context scoped]
  (let [name-sexpr (z/sexpr element-loc)]
    (when (or (symbol? name-sexpr) (keyword? name-sexpr))
      (let [signature-loc (macro-signature-loc signature element-loc)
            signatures (when signature-loc (function-signatures signature-loc))
            doc (when-let [doc-loc (and doc? (z/find-next element-loc z/right (comp #{:token :vector} z/tag)))]
                  (when (= :token (z/tag doc-loc))
                    (z/sexpr doc-loc)))
            attr-map (when-let [attr-map-loc (and attr-map? (z/find-next element-loc z/right (comp #{:map :vector} z/tag)))]
                  (when (= :map (z/tag attr-map-loc))
                    (z/sexpr attr-map-loc)))
            name-meta (merge (meta (z/sexpr element-loc)) attr-map)
            dec-meta (cond-> name-meta
                       doc (assoc :doc doc)
                       (seq signatures) (assoc :arglists signatures))
            tags' (set tags)
            op-local? (contains? tags' :local)]
        (cond
          op-local? (vswap! context update :locals conj (name name-sexpr))
          :else (vswap! context update :publics conj (name name-sexpr)))
        (add-reference context scoped (z/node element-loc)
                       (cond->
                         {:tags (set/union
                                  (if op-local?
                                    #{:declare :local}
                                    #{:declare :public})
                                  tags')}
                         (:doc dec-meta) (assoc :doc (:doc dec-meta))
                         (:arglists dec-meta) (assoc :signatures (:arglists dec-meta))
                         kind (assoc :kind kind)))))))

(defn- add-macro-sub-forms [element-loc context scope-bounds bound-scope sub-forms]
  (let [k->qualified (into {} (map (comp (juxt identity #(symbol (str (gensym)) (name %))) key) sub-forms))
        new-scope (reduce (fn [accum qualified-k]
                            (assoc accum (symbol (name qualified-k)) {:ns (symbol (namespace qualified-k))
                                                      :bounds scope-bounds}))
                          bound-scope
                          (vals k->qualified))]
    (doseq [[k macro-def] sub-forms
            :let [qualified-k (k->qualified k)]]
      (vswap! context assoc-in [:macro-defs qualified-k] macro-def))
    [new-scope (vals k->qualified)]))

(defn parse-macro-def
  [op-loc loc context scoped macro-def]
  (loop [[element' & elements] macro-def
         repeat-idx' 0
         element-loc (z-right-sexpr op-loc)
         bound-scope' scoped]
    (let [map-element (and (map? element') (:element element'))
          repeat-idx (cond-> repeat-idx'
                       (vector? map-element) (rem (count map-element)))
          element (cond
                    (vector? map-element)
                    (nth map-element repeat-idx)

                    (map? element')
                    (:element element')

                    :else
                    element')
          element-info (when (map? element')
                         element')
          sexpr (z/sexpr element-loc)
          repeat? (case (:repeat element-info)
                    :symbol (symbol? sexpr)
                    :list (list? sexpr)
                    true true
                    nil)
          process? (case (:pred element-info)
                     :string (string? sexpr)
                     :map (map? sexpr)
                     nil true)
          scope-bounds (merge (meta (z/node element-loc)) (end-bounds loc))
          [bound-scope macro-sub-forms] (if (not-empty (:sub-forms element-info))
                                           (add-macro-sub-forms element-loc context scope-bounds bound-scope' (:sub-forms element-info))
                                           [bound-scope' nil])]
      (when process?
        (case element
          :bound-elements
          (handle-rest element-loc context bound-scope)
          :elements
          (handle-rest element-loc context scoped)
          :bound-element
          (find-usages* (zsub/subzip element-loc) context bound-scope)
          :element
          (find-usages* (zsub/subzip element-loc) context scoped)
          :function-params-and-bodies
          (function-params-and-bodies element-loc context scoped)
          :declaration
          (macro-declaration element-info element-loc context scoped)
          :fn-spec
          (handle-fn-spec element-loc context scoped (:tags element-info))
          nil))
      (when macro-sub-forms
        (vswap! context update :macro-defs #(apply dissoc % macro-sub-forms)))
      (when-let [next-element-loc (and (or repeat? (seq elements)) (z-right-sexpr element-loc))]
        (recur (cond-> elements
                 repeat? (conj element'))
               (if repeat? (inc repeat-idx) 0)
               (if process? next-element-loc element-loc)
               (cond
                 (and (= :bindings element) (= :vector (z/tag element-loc)))
                 (parse-bindings element-loc context (end-bounds loc) scoped)

                 (= :params element)
                 (parse-params element-loc context scoped)

                 (= :param element)
                 (parse-destructuring element-loc
                                      scope-bounds
                                      context scoped)

                 :else
                 scoped))))))

(defn handle-sexpr [loc context scoped]
  (try
    (let [op-loc (some-> loc (zm/down))]
      (cond
        (and op-loc (symbol? (z/sexpr op-loc)))
        (let [usage (add-reference context scoped (z/node op-loc) {})
              handler (get *sexpr-handlers* (:sym usage))
              macro-def (get (:macro-defs @context) (:sym usage))]
          (cond
            (and macro-def (not (:quoting? @context)))
            (parse-macro-def op-loc loc context scoped macro-def)

            (and handler (not (:quoting? @context)))
            (handler op-loc loc context scoped)

            :else
            (handle-rest (zm/right op-loc) context scoped)))
        op-loc
        (handle-rest op-loc context scoped)))
    (catch Throwable e
      (log/warn #_e "Cannot parse" (:uri @context) "\n" (.getMessage e) "\n" (z/string loc)))))

(defn- find-usages* [loc context scoped]
  (loop [loc loc
         scoped scoped]
    (if (or (not loc) (zm/end? loc))
      (:usages @context)

      (let [tag (z/tag loc)]
        (cond
          (#{:syntax-quote :quote} tag)
          (let [current-quoted (:quoting? @context)]
            (vswap! context assoc :quoting? true)
            (handle-sexpr (z/next loc) context scoped)
            (vswap! context assoc :quoting? current-quoted)
            (recur (edit/skip-over loc) scoped))

          (= :uneval tag)
          (do
            (handle-ignored (z/next loc) context scoped)
            (recur (edit/skip-over loc) scoped))

          (or (= :list tag)
              (and (= :fn tag) (:in-fn-literal? @context)))
          (do
            (handle-sexpr loc context scoped)
            (recur (edit/skip-over loc) scoped))

          (and (= :fn tag) (not (:in-fn-literal? @context)))
          (do
            (handle-dispatch-macro loc context scoped)
            (recur (edit/skip-over loc) scoped))

          (and (= :token tag) (ident? (z/sexpr loc)))
          (do
            (add-reference context scoped (z/node loc) {})
            (recur (edit/skip-over loc) scoped))

          (= :reader-macro tag)
          (recur (edit/skip-over loc) scoped)

          :else
          (recur (zm/next loc) scoped))))))

(defn process-reader-macro [loc file-type]
  (loop [loc loc]
    (if (and (= :reader-macro (z/tag loc))
             (contains? #{"?@" "?"} (z/string (z/down loc))))
      (if-let [file-type-loc (or (-> loc
                                     z/down
                                     z/right
                                     z/down
                                     (z/find-value z/right file-type))
                                 (-> loc
                                     z/down
                                     z/right
                                     z/down
                                     (z/find-value z/right :default)))]
        (let [file-type-expr (z/node (z/right file-type-loc))
              splice? (= "?@" (z/string (z/down loc)))]
          (recur (cond-> (z/replace loc file-type-expr)
                   splice? (z/splice))))
        (recur (z/next (z/remove loc))))
      (if (and loc (z/next loc) (not (zm/end? loc)))
        (recur (z/next loc))
        (vary-meta (z/skip z/prev #(z/prev %) loc) assoc ::zm/end? false)))))

(defn find-usages
  ([code file-type client-macro-defs]
   (find-usages nil code file-type client-macro-defs))
  ([uri code file-type client-macro-defs]
   (when-let [code-loc (try (-> code
                                (string/replace #"(\w)/(\s|$)" "$1 $2")
                                (z/of-string)
                                (zm/up))
                            (catch Throwable e
                              (log/warn "Cannot read" uri (.getMessage e))))]
     (let [macro-defs (merge default-macro-defs client-macro-defs)]
       (if (= :cljc file-type)
         (into (-> code-loc
                   (process-reader-macro :clj)
                   (find-usages* (volatile! (assoc default-env :file-type :clj :macro-defs macro-defs :uri uri)) {}))
               (-> code-loc
                   (process-reader-macro :cljs)
                   (find-usages* (volatile! (assoc default-env :file-type :cljs :macro-defs macro-defs :uri uri)) {})))
         (-> code-loc
             (find-usages* (volatile! (assoc default-env :file-type file-type :macro-defs macro-defs :uri uri)) {})))))))

;; From rewrite-cljs
(defn in-range? [{:keys [row col end-row end-col] :as _form-pos}
                 {r :row c :col er :end-row ec :end-col :as _selection-pos}]
  (and (>= r row)
       (<= er end-row)
       (if (= r row) (>= c col) true)
       (if (= er end-row) (< ec end-col) true)))

;; From rewrite-cljs
(defn find-forms
  "Find last node (if more than one node) that is in range of pos and
  satisfying the given predicate depth first from initial zipper
  location."
  [zloc p?]
  (->> zloc
       (iterate z-next-sexpr)
       (take-while identity)
       (take-while (complement z/end?))
       (filter p?)))

(defn find-last-by-pos
  [zloc pos]
  (last (find-forms zloc (fn [loc]
                           (in-range?
                            (-> loc z/node meta) pos)))))

(defn find-top-forms-in-range
  [code pos]
  (->> (find-forms (z/of-string code) #(in-range?
                                         pos (-> % z/node meta)))
       (mapv (fn [loc] (z/find loc z/up edit/top?)))
       (distinct)))

(defn loc-at-pos [code row col]
  (-> code
      (z/of-string)
      (find-last-by-pos {:row row :col col :end-row row :end-col col})))

(defn usages-in-form [loc usages]
  (let [form-loc (if (not= :token (z/tag loc))
                   loc
                   (z/up loc))
        form-pos (-> form-loc z/node meta)]
    (filter #(in-range? form-pos %) usages)))

(comment
  (loc-at-pos  "foo" 1 5)
  (in-range? {:row 23, :col 1, :end-row 23, :end-col 9} {:row 23, :col 7, :end-row 23, :end-col 7})
  (let [code (string/join "\n"
                          ["(ns thinger.foo"
                           "  (:refer-clojure :exclude [update])"
                           "  (:require"
                           "    [thinger [my.bun :as bun]"
                           "             [bung.bong :as bb :refer [bing byng]]]))"
                           "(comment foo)"
                           "(let [x 1] (inc x))"
                           "(def x 1)"
                           "(defn y [y] (y x))"
                           "(inc x)"
                           "(bun/foo)"
                           "(bing)"])]
    (n/string (z/node (z/of-string "::foo")))
    (find-usages code :clj {}))


  (do
    (require '[taoensso.tufte :as tufte :refer (defnp p profiled profile)])

    (->>
      (find-usages (slurp "test/clojure_lsp/parser_test.clj") :clj {})
      (tufte/profiled {})
      (second)
      deref
      :stats
      (map (juxt key (comp #(int (quot % 1000000)) :max val) (comp :n val) (comp #(int (quot % 1000000)) :sum val)))
      clojure.pprint/pprint
      with-out-str
      (spit "x.edn")))

  (let [code (slurp "bad.clj")]
    (find-usages code :clj {})
    #_
    (println (tufte/format-pstats prof)))

  (let [code "(ns foob) (defn ^:private chunk [] :a)"]
    (find-usages code :clj {}))
  (let [code "(def #?@(:clj a :cljs b) 1)"]
    (n/children (z/node (z/of-string code)))
    (find-usages code :cljc {}))

  (do (defmacro x [] (let [y 2] `(let [z# ~y] [z# ~'y]))) (let [y 3]  (x)))
  (do (defmacro x [] 'y)
      (let [y 3] (y)))

  (find-usages "(do #inst \"2019-04-04\")" :clj {})
  (z/sexpr (z/right (z/of-string "#(:a 1)")))

  (find-usages "(deftype JSValue [val])" :clj)
  (z/sexpr (loc-at-pos code 1 2)))
