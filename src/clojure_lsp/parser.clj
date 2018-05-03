(ns clojure-lsp.parser
  (:require
   [clojure-lsp.clojure-core :as cc]
   [clojure-lsp.db :as db]
   [clojure-lsp.refactor.edit :as edit]
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [clojure.walk :as walk]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [rewrite-clj.custom-zipper.core :as cz]
   [rewrite-clj.zip.edit :as ze]
   [rewrite-clj.zip.find :as zf]
   [rewrite-clj.zip.move :as zm]
   [rewrite-clj.zip.walk :as zw]
   [rewrite-clj.zip.subedit :as zsub]))

(declare find-references*)
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
   :refers {}
   :imports {}
   :aliases {}
   :publics #{}
   :usages []})

(defmacro zspy [loc]
  `(do
     (log/warn '~loc (pr-str (z/sexpr ~loc)))
     ~loc))

(defn qualify-ident [ident {:keys [aliases publics refers imports requires ns] :as context} scoped]
  (when (ident? ident)
    (let [ident-ns (some-> (namespace ident) symbol)
          ident-name (name ident)
          constructor (when-let [sym (symbol (string/replace ident-name #"\.$" ""))]
                        (or (get lang-imports sym)
                            (get imports sym)))
          alias->ns (set/map-invert aliases)
          ctr (if (symbol? ident) symbol keyword)]
      (if (simple-ident? ident)
        (cond
          (keyword? ident) {:sym ident}
          (contains? scoped ident) {:sym (ctr (name (get-in scoped [ident :ns])) ident-name)}
          (contains? publics ident) {:sym (ctr (name ns) ident-name)}
          (contains? refers ident) {:sym (ctr (name (get refers ident)) ident-name)}
          (contains? imports ident) {:sym (get imports ident) :tags #{:norename}}
          (contains? core-refers ident) {:sym (ctr (name (get core-refers ident)) ident-name) :tags #{:norename}}
          (contains? lang-imports ident) {:sym (get lang-imports ident) :tags #{:norename}}
          (string/starts-with? ident-name ".") {:sym ident :tags #{:method :norename}}
          constructor {:sym constructor :tags #{:norename}}
          :else {:sym (ctr (name (gensym)) ident-name) :tags #{:unknown}})
        (cond
          (contains? alias->ns ident-ns)
          {:sym (ctr (name (alias->ns ident-ns)) ident-name)}

          (contains? requires ident-ns)
          {:sym ident}

          (contains? imports ident-ns)
          {:sym (symbol (name (get imports ident-ns)) ident-name) :tags #{:method :norename}}

          (contains? lang-imports ident-ns)
          {:sym (symbol (name (get lang-imports ident-ns)) ident-name) :tags #{:method :norename}}

          :else
          {:sym (ctr (name (gensym)) ident-name) :tags #{:unknown}})))))

(defn add-reference [context scoped node extra]
  (vswap! context
          (fn [context]
            (update context :usages
                    (fn [usages]
                      (let [{:keys [row end-row col end-col] :as m} (meta node)
                            sexpr (n/sexpr node)
                            scope-bounds (get-in scoped [sexpr :bounds])
                            ident-info (qualify-ident sexpr context scoped)]
                        (conj usages (cond-> {:sexpr sexpr
                                              :row row
                                              :end-row end-row
                                              :col col
                                              :end-col end-col}
                                       :always (merge ident-info)
                                       (seq extra) (merge extra)
                                       (seq scope-bounds) (assoc :scope-bounds scope-bounds)))))))))

(defn destructure-map [map-loc scope-bounds context scoped]
  (loop [key-loc (z/down (zsub/subzip map-loc))
         scoped scoped]
    (if (and key-loc (not (z/end? key-loc)))
      (let [key-sexpr (z/sexpr key-loc)
            val-loc (z/right key-loc)]
        (cond
          (= :keys key-sexpr)
          (recur (edit/skip-over val-loc)
                 (loop [child-loc (z/down val-loc)
                        scoped scoped]
                   (let [sexpr (z/sexpr child-loc)
                         scoped-ns (gensym)
                         new-scoped (assoc scoped sexpr {:ns scoped-ns :bounds scope-bounds})]
                     (add-reference context scoped (z/node child-loc) {:tags #{:declare :param}
                                                                       :scope-bounds scope-bounds
                                                                       :sym (symbol (name scoped-ns)
                                                                                    (name sexpr))
                                                                       :sexpr sexpr})
                     (if (z/rightmost? child-loc)
                       new-scoped
                       (recur (z/right child-loc) new-scoped)))))

          (= :as key-sexpr)
          (let [val-node (z/node val-loc)
                sexpr (n/sexpr val-node)
                scoped-ns (gensym)
                new-scoped (assoc scoped sexpr {:ns scoped-ns :bounds scope-bounds})]
            (add-reference context scoped (z/node val-loc) {:tags #{:declare :param}
                                                            :scope-bounds scope-bounds
                                                            :sym (symbol (name scoped-ns)
                                                                         (name sexpr))
                                                            :sexpr sexpr})
            (recur (z/right val-loc) new-scoped))

          (keyword? key-sexpr)
          (recur (edit/skip-over val-loc) scoped)

          (not= '& key-sexpr)
          (recur (z/right val-loc) (parse-destructuring key-loc scope-bounds context scoped))

          :else
          (recur (edit/skip-over val-loc) scoped)))
      scoped)))

(comment
  (let [context (volatile! {})]
    #_(do (destructure-map (z/of-string "{:keys [a b]}") {} context {}) (map :sym (:usages @context)))
    (map :sym (:usages (find-references "(let [a (b c d)])")))))

(defn handle-rest
  "Crawl each form from `loc` to the end of the parent-form
  `(fn [x 1] | (a) (b) (c))`
  With cursor at `|` find references for `(a)`, `(b)`, and `(c)`"
  [loc context scoped]
  (loop [sub-loc loc]
    (when sub-loc
      (find-references* (zsub/subzip sub-loc) context scoped)
      (recur (zm/right sub-loc)))))

(defn parse-destructuring [param-loc scope-bounds context scoped]
  (loop [param-loc (zsub/subzip param-loc)
         scoped scoped]
    ;; MUTATION Updates scoped AND adds declared param references
    (if (and param-loc (not (z/end? param-loc)))
      (let [node (z/node param-loc)
            sexpr (n/sexpr node)]
        (cond
          ;; TODO handle map and seq destructing
          (symbol? sexpr)
          (let [scoped-ns (gensym)
                new-scoped (assoc scoped sexpr {:ns scoped-ns :bounds scope-bounds})]
            (add-reference context new-scoped node {:tags #{:declare :param}
                                                    :scope-bounds scope-bounds
                                                    :sym (symbol (name scoped-ns)
                                                                 (name sexpr))
                                                    :sexpr sexpr})
            (recur (z/next param-loc) new-scoped))

          (vector? sexpr)
          (recur (z/next param-loc) scoped)

          (map? sexpr)
          (recur (edit/skip-over param-loc) (destructure-map param-loc scope-bounds context scoped))

          :else
          (recur (edit/skip-over param-loc) scoped)))
      scoped)))

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
          (let [right-side-loc (z/right binding-loc)
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
              (let [{:keys [end-row end-col]} (meta (z/node (or (z/right right-side-loc) (z/up right-side-loc) bindings-loc)))
                    scope-bounds (assoc end-scope-bounds :row end-row :col end-col)
                    new-scoped (parse-destructuring binding-loc scope-bounds context scoped)]
                (handle-rest (zsub/subzip right-side-loc) context scoped)
                (recur (edit/skip-over right-side-loc) new-scoped))))

          :else
          scoped)))
    (catch Throwable e
      (log/warn "bindings" (.getMessage e) (z/sexpr bindings-loc) (z/sexpr (z/up bindings-loc)))
      (throw e))))

(defn parse-params [params-loc context scoped]
  (try
    (let [{:keys [row col]} (meta (z/node params-loc))
          {:keys [end-row end-col]} (meta (z/node (z/up params-loc)))
          scoped-ns (gensym)
          scope-bounds {:row row :col col :end-row end-row :end-col end-col}]
      (loop [param-loc (z/down params-loc)
             scoped scoped]
        (if param-loc
          (let [new-scoped (parse-destructuring param-loc scope-bounds context scoped)]
            (if (z/rightmost? param-loc)
              new-scoped
              (recur (z/right param-loc) new-scoped)))
          scoped)))
    (catch Exception e
      (log/warn "params" (.getMessage e) (z/sexpr params-loc))
      (throw e))))

(defn handle-comment
  [op-loc loc context scoped]
  ;; Ignore contents of comment
  nil)

(defn add-imports [conformed context]
  (when-first [import-clause (filter (comp #{:import} first) (:clauses conformed))]
    (let [all-imports (:classes (second import-clause))
          {classes :class packages :package-list} (group-by first all-imports)
          simple-classes (map second classes)]
      (vswap! context assoc :imports
              (into (zipmap simple-classes simple-classes)
                    (for [[_ {:keys [package classes]}] packages
                          cls classes]
                      [cls (symbol (str (name package) "." (name cls)))]))))))

(defn handle-ns
  [op-loc loc context scoped]
  (let [args (-> loc
                 (z/subedit->
                  ((fn [loc]
                     ;; TODO should be able to put these in :require
                     (if-let [require-macros (z/find-value loc z/next :require-macros)]
                       (-> require-macros z/up z/remove)
                       loc))))
                 (z/sexpr)
                 (rest))
        ns-arg-spec (:args (s/get-spec 'clojure.core/ns))
        conformed (s/conform ns-arg-spec args)
        _ (when (= :clojure.spec.alpha/invalid conformed)
            (throw (ex-info "Could not parse ns" (s/explain-data ns-arg-spec args))))
        libs (:body (second (first (filter (comp #{:require} first) (:clauses conformed)))))
        prefix-symbol (fn [prefix sym]
                        (if prefix
                          (symbol (str (name prefix) "." (name sym)))
                          sym))
        expand-lib-spec (fn [prefix [tag arg]]
                          (if (= :lib tag)
                            {:lib (prefix-symbol prefix arg)}
                            (update arg :lib #(prefix-symbol prefix %))))
        expand-prefix-list (fn [{:keys [prefix libspecs]}]
                             (mapv (partial expand-lib-spec prefix) libspecs))
        libspecs (reduce (fn [libspecs libspec-or-prefix-list]
                           (let [[tag arg] libspec-or-prefix-list]
                             (into libspecs
                                   (cond
                                     (= :libspec tag) [(expand-lib-spec nil arg)]
                                     (= :prefix-list tag) (expand-prefix-list arg)))))
                         []
                         libs)
        lib-publics (fn [lib]
                      (for [[_ {:keys [ns usages]}] (:file-envs @db/db)
                            {:keys [sym tags]} usages
                            :when (and (= ns lib) (:declare tags) (:public tags))]
                        [(symbol (name sym)) lib]))
        require-loc (-> loc
                        (z/down)
                        (z/find z/right (fn [node]
                                          (let [sexpr (z/sexpr node)]
                                            (and (seq? sexpr) (= :require (first sexpr)))))))
        require-node (-> (or require-loc loc)
                         (z/down)
                         (z/rightmost)
                         (z/node)
                         (meta))]
    (add-imports conformed context)
    (doseq [{:keys [lib options]} libspecs]
      (vswap! context (fn [env]
                        (let [{:keys [as refer]} options]
                          (cond-> env
                            :always (update :requires conj lib)
                            as (update :aliases conj [lib as])

                            (and refer (= :all (first refer)))
                            (update :refers into (lib-publics lib))

                            (and refer (= :syms (first refer)))
                            (update :refers into (map vector (second refer) (repeat lib))))))))
    (vswap! context assoc
            :ns (:name conformed)
            :require-pos {:add-require? (not require-loc)
                          :row (:end-row require-node)
                          :col (:end-col require-node)})))

(defn handle-let
  [op-loc loc context scoped]
  (let [bindings-loc (zf/find-tag op-loc :vector)
        scoped (parse-bindings bindings-loc context (end-bounds loc) scoped)]
    (handle-rest (z/right bindings-loc) context scoped)))

(defn handle-if-let
  [op-loc loc context scoped]
  (let [bindings-loc (zf/find-tag op-loc :vector)
        if-loc (z/right bindings-loc)
        if-scoped (parse-bindings bindings-loc context (end-bounds if-loc) scoped)]
    (handle-rest if-loc context if-scoped)
    (handle-rest (z/right if-loc) context scoped)))

(defn handle-def
  [op-loc loc context scoped]
  (let [def-sym (z/node (z/right op-loc))]
    (vswap! context update :publics conj (n/sexpr def-sym))
    (add-reference context scoped def-sym {:tags #{:declare :public}})
    (handle-rest (z/right (z/right op-loc))
                 context scoped)))

(defn handle-function
  [op-loc loc context scoped name-tags]
  (let [name-loc (z/right op-loc)
        multi? (= :list (z/tag (z/find op-loc (fn [loc] (#{:vector :list} (z/tag loc))))))]
    (if (:public name-tags)
      (vswap! context update :publics conj (z/sexpr name-loc)))
    ;; TODO handle multi signatures
    (if (symbol? (z/sexpr name-loc))
      (add-reference context scoped (z/node name-loc)
                     {:tags name-tags
                      :signature (z/string (z/find-tag name-loc z/next :vector))}))
    (if multi?
      (loop [list-loc (z/find-tag op-loc :list)]
        (let [params-loc (z/down list-loc)
              body-loc (z/right params-loc)]
          (->> (parse-params params-loc context scoped)
               (handle-rest body-loc context)))
        (when-let [next-list (z/find-next-tag list-loc :list)]
          (recur next-list)))
      (let [params-loc (z/find-tag op-loc :vector)
            body-loc (z/right params-loc)]
        (->> (parse-params params-loc context scoped)
             (handle-rest body-loc context))))))

(defn handle-defmethod
  [op-loc loc context scoped]
  (handle-function op-loc loc context scoped #{}))

(defn handle-fn
  [op-loc loc context scoped]
  (handle-function op-loc loc context scoped #{:declare}))

(defn handle-defn
  [op-loc loc context scoped]
  (handle-function op-loc loc context scoped #{:declare :public}))

(defn handle-catch
  [op-loc loc context scoped]
  (let [type-loc (z/right op-loc)
        e-loc (z/right type-loc)
        scoped-ns (gensym)
        e-node (z/node e-loc)
        scope-bounds (merge (meta e-node) (end-bounds loc))
        e-sexpr (z/sexpr e-loc)
        new-scoped (assoc scoped e-sexpr {:ns scoped-ns :bounds scope-bounds})]
    (add-reference context scoped (z/node type-loc) {})
    (add-reference context scoped e-node {:tags #{:declare :params}
                                          :scope-bounds scope-bounds
                                          :sym (symbol (name scoped-ns)
                                                       (name e-sexpr))
                                          :sexpr e-sexpr})
    (handle-rest (z/right e-loc) context new-scoped)))

(defn handle-defmacro
  [op-loc loc context scoped]
  (let [defn-loc (z/right op-loc)
        defn-sym (z/node defn-loc)
        multi? (= :list (z/tag (z/find defn-loc (fn [loc]
                                                  (#{:vector :list} (z/tag loc))))))]
    (vswap! context update :publics conj (n/sexpr defn-sym))
    (add-reference context scoped defn-sym {:tags #{:declare :public}})
    (if multi?
      (loop [list-loc (z/find-tag defn-loc :list)]
        (let [params-loc (z/down list-loc)]
          (parse-params params-loc context scoped))
        (when-let [next-list (z/find-next-tag list-loc :list)]
          (recur next-list)))
      (let [params-loc (z/find-tag defn-loc :vector)]
        (parse-params params-loc context scoped)))))

(defn handle-dispatch-macro
  [loc context scoped]
  (->>
   (loop [sub-loc (z/next (zsub/subzip loc))
          scoped scoped]
     (if (and sub-loc (not (z/end? sub-loc)))
       (let [sexpr (z/sexpr sub-loc)]
         (if (and (symbol? sexpr)
                  (re-find #"^%(:?\d+|&)?$" (name sexpr)))
           (recur (z/next sub-loc) (assoc scoped sexpr {:ns (gensym) :bounds (meta (z/node sub-loc))}))
           (recur (z/next sub-loc) scoped)))
       scoped))
   (handle-rest (z/down loc) context)))

(comment
  '[clojure.core/with-open
    clojure.core/dotimes
    clojure.core/letfn
    clojure.core/with-local-vars
    clojure.core/as->])

(def ^:dynamic *sexpr-handlers*
  {'clojure.core/ns handle-ns
   'clojure.core/defn handle-defn
   'clojure.core/defn- handle-defn
   'clojure.core/fn handle-fn
   'clojure.core/declare handle-def
   'clojure.core/defmulti handle-def
   'clojure.core/defmethod handle-defmethod
   'clojure.core/def handle-def
   'clojure.core/defonce handle-def
   'clojure.core/defmacro handle-defmacro
   'clojure.core/let handle-let
   'clojure.core/catch handle-catch
   'clojure.core/when-let handle-let
   'clojure.core/when-some handle-let
   'clojure.core/when-first handle-let
   'clojure.core/if-let handle-if-let
   'clojure.core/if-some handle-if-let
   'clojure.core/with-open handle-let
   'clojure.core/loop handle-let
   'clojure.core/for handle-let
   'clojure.core/doseq handle-let
   'clojure.core/comment handle-comment})

(defn handle-sexpr [loc context scoped]
  (let [op-loc (some-> loc (zm/down))]
    (cond
      (and op-loc (symbol? (z/sexpr op-loc)))
      (let [qualified-op (:sym (qualify-ident (z/sexpr op-loc) @context scoped))
            handler (get *sexpr-handlers* qualified-op)]
        (add-reference context scoped (z/node op-loc) {})
        (if handler
          (handler op-loc loc context scoped)
          (handle-rest (zm/right op-loc) context scoped)))
      op-loc
      (handle-rest op-loc context scoped))))

(defn find-references* [loc context scoped]
  (loop [loc loc
         scoped scoped]
    (if (or (not loc) (zm/end? loc))
      @context

      (let [tag (z/tag loc)]
        (cond
          (#{:quote :uneval :syntax-quote} tag)
          (recur (edit/skip-over loc) scoped)

          (= :list tag)
          (do
            (handle-sexpr loc context scoped)
            (recur (edit/skip-over loc) scoped))

          (= :fn tag)
          (do
            (handle-dispatch-macro loc context scoped)
            (recur (edit/skip-over loc) scoped))

          (and (= :token tag) (symbol? (z/sexpr loc)))
          (do
            (add-reference context scoped (z/node loc) {})
            (recur (edit/skip-over loc) scoped))

          :else
          (recur (zm/next loc) scoped))))))

(defn find-references [code]
  (-> code
      (z/of-string)
      (zm/up)
      (find-references* (volatile! default-env) {})))

;; From rewrite-cljs
(defn in-range? [{:keys [row col end-row end-col]} {r :row c :col er :end-row ec :end-col}]
  (and (>= r row)
       (<= er end-row)
       (if (= r row) (>= col c) true)
       (if (= er end-row) (<= end-col ec) true)))

;; From rewrite-cljs
(defn find-forms-in-range
  "Find last node (if more than one node) that is in range of pos and
  satisfying the given predicate depth first from initial zipper
  location."
  ([zloc pos] (find-forms-in-range zloc pos (constantly true)))
  ([zloc pos p?]
   (->> zloc
        (iterate z/next)
        (take-while identity)
        (take-while (complement z/end?))
        (filter #(and (p? %)
                      (in-range? (-> % z/node meta) pos))))))

(defn find-last-by-pos
  [zloc pos]
  (last (find-forms-in-range zloc pos)))

(defn find-top-forms-in-range
  [zloc pos]
  (->> (find-forms-in-range zloc pos)
       (mapv (fn [loc] (z/find loc z/up edit/top?)))
       (distinct)))

(defn loc-at-pos [code row col]
  (-> code
      (z/of-string)
      (find-last-by-pos {:row row :col col :end-row row :end-col col})))

(comment
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
    #_(find-references code)
    (z/sexpr (loc-at-pos code 1 2))))
