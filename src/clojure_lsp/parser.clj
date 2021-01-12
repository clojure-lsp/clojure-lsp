(ns clojure-lsp.parser
  (:require
    [clojure-lsp.clojure-core :as cc]
    [clojure-lsp.db :as db]
    [clojure-lsp.refactor.edit :as edit]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [medley.core :as medley]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]
    [rewrite-clj.zip.find :as zf]
    [rewrite-clj.zip.move :as zm]
    [rewrite-clj.zip.subedit :as zsub])
  (:import
    (rewrite_clj.node.meta MetaNode)))

(declare find-usages*)
(declare parse-destructuring)

(def default-env
  {:ns 'user
   :refer-all-syms {}
   :refers {}
   :imports {}
   :aliases {}
   :local-classes #{}
   :publics #{}
   :locals #{}
   :usages []
   :ignored? false})

(def default-macro-defs
  {'clojure.core.match/match [:element {:element [:params :bound-element] :repeat true}]
   'clojure.core.async/go-loop [:params :bound-elements]
   'clojure.core/as-> [:element :param :bound-elements]
   'clojure.core/catch [:element :param :bound-elements]
   'clojure.core/comment [:elements]
   'clojure.core/declare [{:element :declaration :forward? true :repeat true}]
   'clojure.core/defmethod [:element :element :function-params-and-bodies]
   'clojure.core/defprotocol [{:element :declaration :declare-class? true :doc? true} {:element :element :pred :string} {:element :fn-spec :repeat true :tags #{:method :declare}}]
   'clojure.core/proxy [:element :element {:element :fn-spec :repeat true :tags #{:method :norename}}]
   'clojure.core/reify [{:element :class-and-methods}]
   'clojure.core/quote []
   'clojure.test/are [:params :bound-element :elements]
   'clojure.test/deftest [{:element :declaration :tags #{:unused}} :elements]
   'clojure.test/testing [{:element :declaration :tags #{:unused}} :elements]
   'cljs.core.match/match [:element {:element [:params :bound-element] :repeat true}]
   'cljs.core.async/go-loop [:params :bound-elements]
   'cljs.core/as-> [:element :param :bound-elements]
   'cljs.core/catch [:element :param :bound-elements]
   'cljs.core/declare [{:element :declaration :repeat true}]
   'cljs.core/defmethod [:element :element :function-params-and-bodies]
   'cljs.core/defprotocol [{:element :declaration :declare-class? true :doc? true} {:element :element :pred :string} {:element :fn-spec :repeat true :tags #{:method :declare}}]
   'cljs.core/proxy [:element :element {:element :fn-spec :repeat true :tags #{:method :norename}}]
   'cljs.core/reify [{:element :class-and-methods}]
   'cljs.test/are [:params :bound-element :elements]
   'cljs.test/deftest [{:element :declaration :tags #{:unused}} :elements]
   'clojure.spec.alpha/fdef [{:element :declaration :forward? true} :elements]
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
   're-frame.core/reg-sub [{:element :declaration :signature [:rightmost :next :next :next :right]} :element :element]
   'schema.macros/try-catchall [{:element :bound-elements :sub-forms {'catch [:param :bound-elements]}}]
   'slingshot.slingshot/try+ [{:element :bound-elements :sub-forms {'else [:elements]}}]
   'schema.core/defn [{:element :declaration
                       :doc? [{:pred :keyword} {:pred :follows-constant :constant :-}]
                       :attr-map? [{:pred :keyword} {:pred :follows-constant :constant :-} {:pred :string}]
                       :signature-style :typed
                       :signature [{:pred :keyword} {:pred :follows-constant :constant :-} {:pred :string} {:pred :map}]}
                      {:element :element :pred :keyword}
                      {:element :element :pred :follows-constant :constant :-}
                      {:element :element :pred :string}
                      {:element :element :pred :map}
                      {:element :function-params-and-bodies
                       :signature-style :typed}]
   'midje.sweet/fact [{:element :bound-elements :sub-forms {'=> [] '=not=> [] '=deny=> [] '=expands-to=> [] '=future=> [] 'provided [:elements]}}]
   'midje.sweet/facts [{:element :bound-elements :sub-forms {'=> [] '=not=> [] '=deny=> [] '=expands-to=> [] '=future=> [] 'provided [:elements]}}]})

(defmacro zspy [loc]
  `(do
     (log/warn '~loc (pr-str (z/sexpr ~loc)))
     ~loc))

(defn z-next-sexpr [loc]
  (z/find-next loc z/next #(not (n/printable-only? (z/node %)))))

(defn z-right-sexpr [loc]
  (z/find-next loc z/right #(not (n/printable-only? (z/node %)))))

(defn ident-split [ident-str]
  (let [ident-conformed (some-> ident-str (string/replace #"^::?" ""))
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

(defn qualify-ident
  [ident-node
   {:keys [aliases local-classes imports locals publics refers requires refer-all-syms file-type ns] :as _context}
   scoped
   declaration?]
  (when (ident? (n/sexpr ident-node))
    (let [ident (n/sexpr ident-node)
          ident-str (n/string (skip-meta ident-node))
          [prefix ident-ns-str ident-name] (ident-split ident-str)
          ident-ns (some-> ident-ns-str symbol)
          alias-ns (get aliases ident-ns-str)
          declared (when (and ns (or (get locals ident-name) (get publics ident-name)))
                              (symbol (name ns) ident-name))
          local-classes' (->> local-classes
                              (map (juxt identity #(symbol (str (name ns) "." %))))
                              (into {}))
          java-classes (merge local-classes' imports cc/java-lang-imports)
          java-sym (get java-classes (symbol (string/replace ident-name #"\.$" "")))
          refered (get refers ident-name)
          required-ns (get requires ident-ns)
          ctr (if (symbol? ident) symbol keyword)
          core-clj? (and (= :clj file-type) (contains? cc/core-refers ident))
          core-cljs? (and (= :cljs file-type) (contains? cc/cljs-refers ident))
          core-clj-symbol (when core-clj? (ctr (name (get cc/core-refers ident)) ident-name))
          core-cljs-symbol (when core-cljs? (ctr (name (get cc/cljs-refers ident)) ident-name))
          core-macro? (and (or core-clj? core-cljs?)
                           (contains? default-macro-defs (or core-clj-symbol core-cljs-symbol)))
          known-macro? (or core-macro?
                           (and refered (contains? default-macro-defs refered)))]
      (-> (if-not ident-ns
            (cond
              java-sym {:sym java-sym :tags #{:norename :java}}
              declaration? {:sym (ctr (name (or ns 'user)) ident-name) :tags #{:declarion}}
              (and (keyword? ident) (= prefix "::")) {:sym (ctr (name ns) ident-name) :tags #{:keyword :namespaced}}
              (keyword? ident) {:sym ident :tags #{:keyword}}
              (contains? scoped ident) {:sym (ctr (name (get-in scoped [ident :ns])) ident-name) :tags #{:scoped}}
              declared {:sym declared :tags #{:declared}}
              refered {:sym refered :tags #{:refered}}
              (contains? refer-all-syms ident) {:sym (get refer-all-syms ident)}
              core-clj? {:sym core-clj-symbol :tags #{:norename :core}}
              core-cljs? {:sym core-cljs-symbol :tags #{:norename :core}}
              (string/starts-with? ident-name ".") {:sym ident :tags #{:java :method :norename}}
              :else {:sym (ctr (name (gensym)) ident-name) :tags #{:unknown}})
            (cond
              (and alias-ns
                   (or
                     (and (keyword? ident) (= prefix "::"))
                     (symbol? ident)))
              {:sym (ctr (name alias-ns) ident-name) :tags #{:alias-reference}}

              (and (keyword? ident) (= prefix "::"))
              {:sym (ctr (name (gensym)) ident-name) :tags #{:unknown} :unknown-ns ident-ns}

              (keyword? ident)
              {:sym ident :tags #{:keyword}}

              required-ns
              {:sym ident :tags #{:raw-reference}}

              (contains? imports ident-ns)
              {:sym (symbol (name (get imports ident-ns)) ident-name) :tags #{:java :method :norename}}

              (contains? cc/java-lang-imports ident-ns)
              {:sym (symbol (name (get cc/java-lang-imports ident-ns)) ident-name) :tags #{:java :method :norename}}

              (and (= :cljs file-type) (= 'js ident-ns))
              {:sym ident :tags #{:method :norename}}

              :else
              {:sym (ctr (name (gensym)) ident-name) :tags #{:unknown} :unknown-ns ident-ns}))
          (assoc :str ident-str)
          (update :tags #(if known-macro?
                           (set/union % #{:macro})
                           (or % #{})))))))

(defn add-reference [context scoped node extra]
  (let [{:keys [row end-row col end-col]} (meta (skip-meta node))
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

(defn ^:private safe-keyword-namespace [ns key]
  (keyword (namespace ns) (name key)))

(defn destructure-keys [scoped key-loc scope-bounds context val-loc]
  (loop [child-loc (z/down val-loc)
         scoped scoped]
    (if child-loc
      (let [sexpr (z/sexpr child-loc)
            child-node (z/node child-loc)
            key-type (name (z/sexpr key-loc))
            key-ident-info (qualify-ident (z/node key-loc) @context scoped false)
            ident-info (qualify-ident child-node @context scoped false)
            scoped-ns (gensym)
            new-scoped (assoc scoped (symbol (name sexpr)) {:ns scoped-ns :bounds scope-bounds})
            ok-ns? (and (contains? (:tags ident-info) :unknown)
                        (not (:unknown-ns ident-info)))
            key-sym (:sym key-ident-info)]
        (when (#{"keys" "syms"} key-type)
          (add-reference context scoped child-node (cond-> {:sym (cond->> (:sym ident-info)
                                                                   ok-ns?
                                                                   (name)

                                                                   (and (= "keys" key-type) (simple-ident? key-sym))
                                                                   (keyword)

                                                                   (and (= "keys" key-type) (qualified-ident? key-sym))
                                                                   (safe-keyword-namespace key-sym)

                                                                   (and (= "syms" key-type) (simple-ident? key-sym))
                                                                   (symbol)

                                                                   (and (= "syms" key-type) (qualified-ident? key-sym))
                                                                   (symbol (namespace key-sym)))}
                                                     ok-ns? (assoc :tags nil)
                                                     (:unknown-ns key-ident-info) (assoc :unknown-ns (:unknown-ns key-ident-info))
                                                     (contains? (:tags key-ident-info) :unknown) (assoc :tags (:tags key-ident-info)))))
        (add-reference context scoped (z/node child-loc) {:tags #{:declare :param}
                                                          :scope-bounds scope-bounds
                                                          :sym (symbol (name scoped-ns)
                                                                       (name sexpr))})
        (if (nil? (z-right-sexpr child-loc))
          new-scoped
          (recur (z-right-sexpr child-loc) new-scoped)))
      scoped)))

(defn destructure-map [map-loc scope-bounds context scoped]
  (loop [key-loc (z/down (zsub/subzip map-loc))
         scoped scoped]
    (if (and key-loc (not (z/end? key-loc)))
      (let [key-sexpr (z/sexpr key-loc)
            val-loc (z-right-sexpr key-loc)]
        (cond
          (and (keyword? key-sexpr) (#{"keys" "strs" "syms"} (name key-sexpr)))
          (recur (edit/skip-over val-loc)
                 (destructure-keys scoped key-loc scope-bounds context val-loc))

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
          (let [val-sexpr (z/sexpr val-loc)]
            (when (keyword? val-sexpr)
              (add-reference context scoped (z/node val-loc) {}))
            (recur (z-right-sexpr val-loc) (parse-destructuring key-loc scope-bounds context scoped)))

          :else
          (recur (edit/skip-over val-loc) scoped)))
      scoped)))

(declare handle-sexpr)

(defn handle-rest
  "Crawl each form from `loc` to the end of the parent-form
  `(fn [x 1] | (a) (b) (c))`
  With cursor at `|` find references for `(a)`, `(b)`, and `(c)`"
  ([loc context scoped]
   (handle-rest loc context scoped false))
  ([loc context scoped threading?]
   (loop [sub-loc loc]
     (when sub-loc
       (find-usages* (zsub/subzip sub-loc) context scoped threading?)
       (recur (zm/right sub-loc))))))

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

(defn parse-params
  ([params-loc context scoped] (parse-params params-loc context scoped nil))
  ([params-loc context scoped signature-style]
    (try
      (let [{:keys [row col]} (meta (z/node params-loc))
            {:keys [end-row end-col]} (meta (z/node (z/up params-loc)))
            scope-bounds {:row row :col col :end-row end-row :end-col end-col}
            typed? (= signature-style :typed)
            single? (not= :vector (z/tag params-loc))]
        (loop [param-loc (cond-> params-loc
                           (not single?) (z/down))
               scoped scoped]
          (cond
            (and typed? param-loc (= :- (z/sexpr param-loc)))
            (let [type-loc (-> param-loc z-right-sexpr)]
              (handle-rest (zsub/subzip type-loc) context scoped)
              (recur (-> type-loc z-right-sexpr) scoped))

            param-loc
            (let [new-scoped (parse-destructuring param-loc scope-bounds context scoped)]
              (if (or single? (nil? (z-right-sexpr param-loc)))
                new-scoped
                (recur (z-right-sexpr param-loc) new-scoped)))

            :else
            scoped)))
      (catch Exception e
        (log/warn "params" (.getMessage e) (z/sexpr params-loc))
        (throw e)))))

(defn handle-ignored
  [rest-loc context scoped]
  (let [curr-ignored? (:ignored? @context false)]
    (vswap! context assoc :ignored? true)
    (handle-rest rest-loc context scoped)
    (vswap! context assoc :ignored? curr-ignored?)))

(defn handle-comment
  [op-loc _loc context scoped _threading?]
  (handle-ignored (z-right-sexpr op-loc) context scoped))

(defn handle-thread
  [op-loc _loc context scoped threading?]
  (let [first-loc (zm/right op-loc)
        ; Check if the value is threaded in already
        thread-loc (if threading?
                     first-loc
                     (zm/right first-loc))]
      ; Look at the value
      (when (not threading?)
        (find-usages* (zsub/subzip first-loc) context scoped))
      ; Look at the forms threaded through
      (loop [sub-loc thread-loc]
        (when sub-loc
          (find-usages* (zsub/subzip sub-loc) context scoped true)
          (recur (zm/right sub-loc))))))

(defn handle-cond-thread
  [op-loc _loc context scoped threading?]
  (let [first-loc (zm/right op-loc)
        ; Check if the value is threaded in already
        thread-loc (if threading?
                     first-loc
                     (zm/right first-loc))]
      ; Look at the value
      (when (not threading?)
        (find-usages* (zsub/subzip first-loc) context scoped))
      ; Look at the tests and forms threaded through
      (loop [sub-loc thread-loc]
        (when sub-loc
          ; Test
          (some-> sub-loc
                  zsub/subzip
                  (find-usages* context scoped))
          ; Form
          (some-> (zm/right sub-loc)
                  zsub/subzip
                  (find-usages* context scoped true))
          (recur (zm/right (zm/right sub-loc)))))))

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
                       {:tags #{:refer}
                        :ns full-ns
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
  [_op-loc loc context scoped _threading?]
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
  [op-loc loc context scoped _threading?]
  (let [bindings-loc (zf/find-tag op-loc :vector)
        scoped (parse-bindings bindings-loc context (end-bounds loc) scoped)]
    (handle-rest (z/right bindings-loc) context scoped)))

(defn handle-if-let
  [op-loc _loc context scoped _threading?]
  (let [bindings-loc (zf/find-tag op-loc :vector)
        if-loc (z-right-sexpr bindings-loc)
        if-scoped (parse-bindings bindings-loc context (end-bounds if-loc) scoped)]
    (handle-rest (zsub/subzip if-loc) context if-scoped)
    (handle-rest (z-right-sexpr if-loc) context scoped)))

(defn- local? [op-loc]
  (let [op (z/sexpr op-loc)
        op-name (name op)
        name-sexpr (z/sexpr (z-right-sexpr op-loc))]
    (or (and (symbol? name-sexpr) (:private (meta name-sexpr)))
        (not (string/starts-with? op-name "def"))
        (string/ends-with? op-name "-"))))

(defn- arglists-to-signatures
  [arglists]
  (cond
    (= 'quote (nth arglists 0 nil))
    (let [sexprs (eval arglists)]
      {:sexprs (seq sexprs)
       :strings (map str sexprs)})

    (string? (nth arglists 0 nil))
    {:sexprs (map (comp z/sexpr z/of-string) arglists)
     :strings arglists}

    :else
    {:sexprs (seq arglists)
     :strings (map str arglists)}))

(defn handle-def
  [op-loc _loc context scoped _threading?]
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
                                           :signatures (some-> (:arglists def-meta)
                                                               (arglists-to-signatures))})
    (handle-rest (z-right-sexpr name-loc)
                 context scoped)))

(defn- remove-type-annots
  "Remove `:- Type` type annotations from an fn signature sexpr."
  [sexpr]
  (loop [to-add sexpr
         args []]
    (if (seq to-add)
      (let [[arg & xs] to-add]
        (if (= arg :-)
          (recur (drop 1 xs) args)
          (recur xs (conj args arg))))
      args)))

(defn- function-signatures
  ([params-loc] (function-signatures params-loc nil))
  ([params-loc signature-style]
   (let [signature-fn (case signature-style
                        :typed remove-type-annots
                        identity)]
     (if (= :list (z/tag params-loc))
       (loop [list-loc params-loc
              sexprs   []
              strings  []]
         (let [next-loc (z/down list-loc)
               sexprs   (conj sexprs (-> next-loc z/sexpr signature-fn))
               strings  (conj strings (z/string next-loc))]
           (if-let [next-list (z/find-next-tag list-loc :list)]
             (recur next-list sexprs strings)
             {:sexprs sexprs :strings strings})))
       {:sexprs  [(-> params-loc z/sexpr signature-fn)]
        :strings [(z/string params-loc)]}))))

(defn- single-params-and-body [params-loc context scoped signature-style]
  (let [body-loc (z/right params-loc)]
    (->> (parse-params params-loc context scoped signature-style)
         (handle-rest body-loc context))))

(defn- function-params-and-bodies
  ([params-loc context scoped] (function-params-and-bodies params-loc context scoped nil))
  ([params-loc context scoped signature-style]
    (if (= :list (z/tag params-loc))
      (loop [list-loc params-loc]
        (single-params-and-body (z/down list-loc) context scoped signature-style)
        (when-let [next-list (z/find-next-tag list-loc :list)]
          (recur next-list)))
      (single-params-and-body params-loc context scoped signature-style))))

(def check (fn [pred x] (when (pred x) x)))

(defn ^:private params?
  [loc]
  (or
    (#{:vector :list} (z/tag loc))
    ; z/tag could be :meta not :vector if there is a return type annotation
    (vector? (z/sexpr loc))))

(defn ^:private find-function-params
  [op-loc]
  (when-let [raw-params (z/find op-loc params?)]
    (if (= :map (-> raw-params z/left z/tag))
      (edit/wrap-meta raw-params (-> raw-params z/left))
      raw-params)))

(defn ^:private handle-function
  [op-loc _loc context scoped name-tags]
  (let [op-local? (local? op-loc)
        op-fn? (= "fn" (name (z/sexpr op-loc)))
        name-loc (z-right-sexpr op-loc)
        params-loc (find-function-params op-loc)]
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
  [op-loc loc context scoped _threading?]
  (handle-function op-loc loc context scoped #{:declare}))

(defn handle-defn
  [op-loc loc context scoped _threading?]
  (handle-function op-loc loc context scoped #{:declare}))

(defn handle-defmacro
  [op-loc loc context scoped _threading?]
  (handle-function op-loc loc context scoped #{:declare}))

(defn handle-fn-spec [method-loc context scoped tags]
  (let [name-loc (z/down method-loc)
        params-loc (z-right-sexpr name-loc)]
    (when (contains? tags :declare)
      (vswap! context update :publics conj (name (z/sexpr name-loc))))
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

(defn handle-class-and-methods [class-loc context scoped]
  (loop [proto-loc class-loc]
        (when proto-loc
          (add-reference context scoped (z/node proto-loc) {})
          (let [next-loc (z-right-sexpr proto-loc)]
            (cond
              (= (z/tag next-loc) :token)
              (recur next-loc)

              (= (z/tag next-loc) :list)
              (some-> proto-loc
                      (handle-type-methods context scoped)
                      (z-right-sexpr)
                      (recur)))))))

(defn handle-deftype
  [op-loc _loc context scoped _threading?]
  (let [type-loc (z-right-sexpr op-loc)
        fields-loc (z-right-sexpr type-loc)]
    (vswap! context update :local-classes conj (z/sexpr type-loc))
    (add-reference context scoped (z/node type-loc) {:tags #{:declare :public}
                                                     :kind :class
                                                     :signatures {:sexprs [(-> fields-loc z/sexpr)]
                                                                  :strings [(z/string fields-loc)]}})
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
    (let [field-scoped (parse-params fields-loc context scoped)]
      (handle-class-and-methods (z-right-sexpr fields-loc) context field-scoped))))

(defn handle-dispatch-macro
  [loc context scoped]
  (let [sub-loc (zsub/subzip loc)]
    (vswap! context assoc :in-fn-literal? true)
    (->>
      (loop [sub-loc (z-next-sexpr sub-loc)
             scoped scoped]
        (if (and sub-loc (not (z/end? sub-loc)))
          (let [sexpr (z/sexpr sub-loc)]
            (if (and (symbol? sexpr)
                     (re-find #"^%(:?\d+|&)?$" (name sexpr)))
              (recur (z-next-sexpr sub-loc) (assoc scoped sexpr {:ns (gensym) :bounds (meta (z/node sub-loc))}))
              (recur (z-next-sexpr sub-loc) scoped)))
          scoped))
      (handle-rest sub-loc context))
    (vswap! context dissoc :in-fn-literal?)))

(def ^:dynamic *sexpr-handlers*
  (let [handlers {'clojure.core/ns handle-ns
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
                  'clojure.core/-> handle-thread
                  'clojure.core/->> handle-thread
                  'clojure.core/some-> handle-thread
                  'clojure.core/some->> handle-thread
                  'clojure.core/cond-> handle-cond-thread
                  'clojure.core/cond->> handle-cond-thread
                  'clojure.core/doto handle-thread}]

    (merge handlers (medley/map-keys #(symbol "cljs.core" (name %)) handlers))))

(defn- match-pred? [loc {:keys [pred constant]}]
  (let [sexpr (z/sexpr loc)]
    (case pred
      :keyword (keyword? sexpr)
      :string (string? sexpr)
      :map (map? sexpr)
      :symbol (symbol? sexpr)
      :follows-constant (some-> loc z/left z/sexpr #{constant}))))

(defn- macro-dirs-to-loc [dirs element-loc]
  (when dirs
    (loop [curr-loc (z-right-sexpr element-loc)
           [dir & dirs] dirs]
      (let [next-loc (case dir
                       :next (z-next-sexpr curr-loc)
                       :right (z-right-sexpr curr-loc)
                       :rightmost (z/rightmost curr-loc)
                       (if (and (map? dir) (contains? dir :pred) (match-pred? curr-loc dir))
                         (z-right-sexpr curr-loc)
                         curr-loc))]
        (if (seq dirs)
          (recur next-loc dirs)
          next-loc)))))

(defn- macro-declaration [{:keys [signature kind tags forward? doc? attr-map? signature-style declare-class? ignore-arity?]} element-loc context scoped]
  (let [name-sexpr (z/sexpr element-loc)]
    (when (or (symbol? name-sexpr) (keyword? name-sexpr))
      (let [signature-loc (macro-dirs-to-loc signature element-loc)
            signatures (when signature-loc (function-signatures signature-loc signature-style))
            doc-loc (cond
                      (vector? doc?) (macro-dirs-to-loc doc? element-loc)
                      doc? (z/find-next element-loc z/right (fn [loc]
                                                              (cond
                                                                (= :vector (z/tag loc)) loc
                                                                (and (= :token (z/tag loc))
                                                                     (string? (z/sexpr loc))) loc))))
            doc (when (and doc-loc (= :token (z/tag doc-loc)))
                  (z/sexpr doc-loc))
            attr-map-loc (cond
                           (vector? attr-map?) (macro-dirs-to-loc attr-map? element-loc)
                           attr-map? (z/find-next element-loc z/right (comp #{:map :vector} z/tag)))
            attr-map (when (and attr-map-loc (= :map (z/tag attr-map-loc)))
                       (z/sexpr attr-map-loc))
            name-meta (merge (meta (z/sexpr element-loc)) attr-map)
            dec-meta (cond-> name-meta
                       doc (assoc :doc doc)
                       (seq signatures) (assoc :arglists (:strings signatures)))
            tags' (set tags)
            op-local? (contains? tags' :local)
            context-ns (:ns @context)
            [class-sym full-sym] (when declare-class?
                                   [(symbol (name name-sexpr)) (symbol (str context-ns "." (name name-sexpr)))])]
        (cond
          declare-class? (vswap! context update :imports assoc class-sym full-sym full-sym full-sym)
          op-local? (vswap! context update :locals conj (name name-sexpr))
          :else (vswap! context update :publics conj (name name-sexpr)))
        (add-reference context scoped (z/node element-loc)
                       (cond->  {:tags tags'}
                         forward? (update :tags conj :forward)
                         ignore-arity? (update :tags conj :ignore-arity?)
                         (not forward?) (update :tags set/union (if op-local?  #{:declare :local} #{:declare :public}))
                         (:doc dec-meta) (assoc :doc (:doc dec-meta))
                         (:arglists dec-meta) (assoc :signatures (arglists-to-signatures (:arglists dec-meta)))
                         (seq signatures) (assoc :signatures signatures)
                         kind (assoc :kind kind)))))))

(defn- add-macro-sub-forms [context scope-bounds bound-scope sub-forms]
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

(declare parse-macro-def-elements)

(defn parse-match-patterns
  [first-loc match-patterns bound-scope context scoped]
  (loop [loc first-loc
         [pattern macro-defs & other-patterns] match-patterns
         bound-scope' bound-scope]
    (let [pattern-count (count pattern)
          pattern-locs (->> loc
                            (iterate z/right))
          pattern-sexprs (->> pattern-locs
                              (take pattern-count)
                              (keep z/sexpr)
                              vec)
          matches? (and loc
                        (= pattern-count (count pattern-sexprs))
                        (->> (mapv vector pattern-sexprs pattern)
                          (every? (fn [[s p]]
                                    (case p
                                      :symbol (symbol? s)
                                      :keyword (keyword? s)
                                      :any true)))))]
      (cond
        (or (nil? loc) (nil? pattern))
        bound-scope'

        matches?
        (recur
          (nth pattern-locs pattern-count)
          match-patterns
          (merge bound-scope'
                 (parse-macro-def-elements macro-defs
                                           loc
                                           bound-scope'
                                           end-bounds
                                           context
                                           scoped)))

        :else
        (recur loc other-patterns bound-scope')))))

(defn parse-macro-def-elements
  [macro-def element-loc bound-scope end-bounds context scoped]
  (when element-loc
    (loop [[element' & elements] macro-def
           repeat-idx' 0
           element-loc element-loc
           bound-scope' bound-scope]
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
            process? (or (nil? (:pred element-info))
                         (match-pred? element-loc element-info))
            scope-bounds (merge (meta (z/node element-loc)))
            [bound-scope macro-sub-forms] (cond
                                            (not-empty (:sub-forms element-info))
                                            (add-macro-sub-forms context scope-bounds bound-scope' (:sub-forms element-info))

                                            (and process? (= :sub-elements element))
                                            [(parse-match-patterns
                                               (z/down (zsub/subzip element-loc))
                                               (:match-patterns element-info)
                                               bound-scope'
                                               context
                                               scoped)
                                             nil]

                                            :else
                                            [bound-scope' nil])]
        (when process?
          (case element
            :bound-elements
            (handle-rest element-loc context bound-scope)
            :elements
            (handle-rest element-loc context scoped (boolean (:thread-style element-info)))
            :bound-element
            (find-usages* (zsub/subzip element-loc) context bound-scope)
            :element
            (find-usages* (zsub/subzip element-loc) context scoped)
            :function-params-and-bodies
            (function-params-and-bodies element-loc context scoped (:signature-style element-info))
            :declaration
            (macro-declaration element-info element-loc context scoped)
            :fn-spec
            (handle-fn-spec element-loc context scoped (:tags element-info))
            :class-and-methods
            (handle-class-and-methods element-loc context scoped)
            nil))
        (when macro-sub-forms
          (vswap! context update :macro-defs #(apply dissoc % macro-sub-forms)))
        (let [next-bound-scope (cond
                                 (and (= :bindings element) (= :vector (z/tag element-loc)))
                                 (parse-bindings element-loc context end-bounds scoped)

                                 (= :params element)
                                 (parse-params element-loc context scoped (:signature-style element-info))

                                 (= :param element)
                                 (parse-destructuring element-loc scope-bounds context scoped)

                                 :else
                                 bound-scope)]
          (if-let [next-element-loc (and (or repeat? (seq elements)) (z-right-sexpr element-loc))]
            (recur
              (cond-> elements
                repeat? (conj element'))
              (if repeat? (inc repeat-idx) 0)
              (if process? next-element-loc element-loc)
              next-bound-scope)
            next-bound-scope))))))

(defn parse-macro-def
  [op-loc loc context scoped macro-def]
  (parse-macro-def-elements
    macro-def
    (z-right-sexpr op-loc)
    scoped
    (end-bounds loc)
    context
    scoped))

(defn handle-sexpr
  ([loc context scoped] (handle-sexpr loc context scoped false))
  ([loc context scoped threading?]
   (try
     (let [op-loc (some-> loc (zm/down))]
       (cond
         (and op-loc (symbol? (z/sexpr op-loc)))
         (let [argc (->> loc
                         (z/node)
                         (n/child-sexprs)
                         (count))
               usage (add-reference context scoped (z/node op-loc) {:argc (if threading?
                                                                            argc
                                                                            (dec argc))})
               handler (get *sexpr-handlers* (:sym usage))
               macro-def (get (:macro-defs @context) (:sym usage))]
           (cond
             (and macro-def (not (:quoting? @context)))
             (parse-macro-def op-loc loc context scoped macro-def)

             (and handler (not (:quoting? @context)))
             (handler op-loc loc context scoped threading?)

             :else
             (handle-rest (zm/right op-loc) context scoped)))
         op-loc
         (handle-rest op-loc context scoped)))
     (catch Throwable e
       (log/warn #_e "Cannot parse" (:uri @context) "\n" (.getMessage e) "\n" (z/string loc))))))

(defn- find-usages*
  ([loc context scoped] (find-usages* loc context scoped false))
  ([loc context scoped threading?]
    (loop [loc loc
           scoped scoped]
      (if (or (not loc) (zm/end? loc))
        (:usages @context)
        (let [tag (z/tag loc)]
          (cond
            (= :quote tag)
            (recur (edit/skip-over loc) scoped)

            (= :syntax-quote tag)
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
              (handle-sexpr loc context scoped threading?)
              (recur (edit/skip-over loc) scoped))

            (and (= :fn tag) (not (:in-fn-literal? @context)))
            (do
              (handle-dispatch-macro loc context scoped)
              (recur (edit/skip-over loc) scoped))

            (and (= :token tag) (ident? (z/sexpr loc)))
            (do
              (add-reference context scoped (z/node loc) {:argc (when threading? 1)})
              (recur (edit/skip-over loc) scoped))

            (= :reader-macro tag)
            (do
              (handle-rest (-> loc z/down z/right) context scoped)
              (recur (edit/skip-over loc) scoped))

            :else
            (recur (zm/next loc) scoped)))))))

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

(defn ^:private merge-by-file-types
  "Merge usages by file-type to avoid duplicated usages for cljc files."
  [usages]
  (->> usages
       (group-by (juxt :sym :tags :row :col :end-row :end-col))
       (map (fn [[_k v]]
              (->> (map #(assoc % :file-type #{(:file-type %)}) v)
                   (reduce (fn [a b]
                             (assoc b :file-type (set/union (:file-type a)
                                                            (:file-type b))))
                           {})
                   )))
       (sort-by (juxt :row :col :end-row :end-col count))
       vec))

(defn ^:private find-raw-usages
  [code-loc file-type client-macro-defs uri]
  (let [macro-defs (merge default-macro-defs client-macro-defs)
        requires {:clj #{'clojure.core}
                  :cljs #{'cljs.core}}]
    (if (= :cljc file-type)
      (into (-> code-loc
                (process-reader-macro :clj)
                (find-usages* (volatile! (assoc default-env :requires (get requires :clj) :file-type :clj :macro-defs macro-defs :uri uri)) {}))
            (-> code-loc
                (process-reader-macro :cljs)
                (find-usages* (volatile! (assoc default-env :requires (get requires :cljs) :file-type :cljs :macro-defs macro-defs :uri uri)) {})))
      (-> code-loc
          (find-usages* (volatile! (assoc default-env :requires (get requires file-type) :file-type file-type :macro-defs macro-defs :uri uri)) {})))))

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
     (-> (find-raw-usages code-loc file-type client-macro-defs uri)
         merge-by-file-types))))

;; From rewrite-cljs
(defn in-range? [{:keys [row col end-row end-col] :as form-pos}
                 {r :row c :col er :end-row ec :end-col :as selection-pos}]
  (or (nil? form-pos)
      (nil? selection-pos)
      (and (>= r row)
           (<= er end-row)
           (if (= r row) (>= c col) true)
           (if (= er end-row) (< ec end-col) true))))

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
  (->> (find-forms (z/of-string code) #(in-range? pos (-> % z/node meta)))
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
