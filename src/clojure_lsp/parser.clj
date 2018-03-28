(ns clojure-lsp.parser
  (:require
    [clojure.walk :as walk]
    [clojure.string :as string]
    [clojure.spec.alpha :as s]
    [clojure.tools.reader :as reader]
    [clojure.tools.reader.reader-types :as types]
    [rewrite-clj.parser :as p]
    [rewrite-clj.zip :as z]
    [rewrite-clj.node :as n]
    [clojure.core.specs.alpha :as specs.alpha]
    [clojure.pprint :as pp]
    [clojure.tools.logging :as log]))
#_(ns-unalias *ns* 'z)

(def default-env
  {:ns 'user
   :requires #{'clojure.core}
   :refers (->> #{'defn 'def 'ns 'let}
                (mapv (juxt identity (constantly 'clojure.core)))
                (into {}))
   :aliases #{}
   :publics #{}
   :scoped #{}})

(defmacro spy [v]
  `(let [v# ~v]
     (pp/pprint v#)
     v#))

(defn zspy [loc]
  (spy (z/sexpr loc))
  loc)

(defn qualify-symbol [sym {:keys [:aliases :scoped :publics :refers :requires :ns] :as env}]
  (when (symbol? sym)
    (let [sym-ns (namespace sym)
          simple? (not sym-ns)
          sym-name (name sym)]
      (if simple?
        (cond
          (contains? scoped sym) sym
          (contains? publics sym) (symbol (name ns) sym-name)
          (contains? refers sym) (symbol (name (get refers sym)) sym-name))
        (cond
          (contains? aliases sym-ns) (symbol (name (get aliases sym-ns)) sym-name)
          (contains? requires sym-ns) sym)))))

(defmulti add-vars*
  (fn [sym args new-env loc]
    sym))

(defmethod add-vars* :default
  [sym args new-env loc])

(defmethod add-vars* 'clojure.core/ns
  [sym args new-env loc]
  (let [conformed (s/conform (:args (s/get-spec 'clojure.core/ns)) args)
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
        require-loc (-> loc
                        (z/down)
                        (z/find z/right (fn [node]
                                          (let [sexpr (log/spy (z/sexpr node))]
                                            (and (seq? sexpr) (= :require (first sexpr)))))))
        require-node (-> (or require-loc loc)
                         (z/down)
                         (z/rightmost)
                         (z/node)
                         log/spy
                         (meta)
                         log/spy)]
    (doseq [{:keys [lib options]} libspecs]
      (vswap! new-env (fn [env]
                        (let [{:keys [as refer]} options]
                          (cond-> env
                            :always (update :requires conj lib)
                            as (update :aliases conj as)
                            (and refer (= :syms (first refer))) (update :refers into (map vector (second refer) (repeat lib))))))))
    (vswap! new-env assoc :ns (:name conformed) :require-pos {:add-require? (not require-loc)
                                                              :line (:end-row require-node)
                                                              :column (:end-col require-node)})))

(defmethod add-vars* 'clojure.core/let
  [sym args new-env loc]
  (s/conform (:args (s/get-spec 'clojure.core/let)) args))

(defmethod add-vars* 'clojure.core/def
  [sym args new-env loc]
  (let [def-sym (first (filter symbol? args))]
    (vswap! new-env (fn [env]
                      (-> env
                          (update :publics conj def-sym))))))

(defmethod add-vars* 'clojure.core/defn
  [sym args new-env loc]
  (let [def-sym (first (filter symbol? args))]
    (vswap! new-env (fn [env]
                      (-> env
                          (update :publics conj def-sym)))))
  (s/conform (:args (s/get-spec sym)) args))

(defn add-vars [new-env loc]
  (let [form (z/child-sexprs loc)
        sym (some-> form
                    first
                    (qualify-symbol @new-env))]
    (add-vars* sym (rest form) new-env loc)))

(defn update-vars [loc env]
  (let [new-env (volatile! env)]
    (z/postwalk
      loc
      (comp #{:list} z/tag)
      (fn [loc]
        (add-vars new-env loc)
        loc))
    @new-env))

(defn check-bounds [node line column]
  (let [{:keys [row end-row col end-col] :as m} (meta node)]
    (cond
      (< line row) :before
      (and (= line row) (< column col)) :before
      (< line end-row) :within
      (and (= end-row line) (>= end-col column)) :within
      :else :after)))

(defn update-scoped
  "
  Is loc a let?
  find position relative to bindings
  add indings before to scoped
   "
  [loc env line column]
  (let [form (z/child-sexprs loc)
        sym (some-> form
                    first
                    (qualify-symbol env))]
    (cond
      (= 'clojure.core/let sym)
      (let [bindings (->> (z/find-tag (z/down loc) :vector)
                          (z/node)
                          (n/children)
                          (remove (comp #{:whitespace :newline} n/tag))
                          (partition-all 2)
                          (filter (fn [binding]
                                    (and (second binding)
                                         (= :after (check-bounds (second binding) line column)))))
                          (mapv (comp n/sexpr first)))
            syms (volatile! [])]
        (walk/postwalk (fn [node]
                         (when (symbol? node)
                           (vswap! syms conj node)))
                       bindings)
        @syms)
      :else
      [])))

(defn find-position [loc env line column]
  (let [node (z/node loc)
        bounds (check-bounds node line column)
        bindings (update-scoped loc env line column)
        env (if (= :after bounds)
              (update-vars loc env)
              (update env :scoped into bindings))]
    (cond
      (and (= :after bounds) (z/rightmost? loc)) [env nil]
      (= :after bounds) (recur (z/right loc) env line column)
      (not (z/seq? loc)) [env node]
      :else (recur (z/down loc) env line column))))

(defn parse [code line column]
  (-> code
      log/spy
      (z/of-string)
      (find-position default-env line column)))

(defn find-publics [code]
  (-> code
      (z/of-string)
      (z/up)
      (update-vars default-env)))

(comment
  (let [code (string/join "\n"
                          ["#_(ns omg.pop) 'foo"
                           "(ns thinger.foo"
                           "  (:refer-clojure :exclude [update])"
                           "  (:require"
                           "    thinger.bleep"
                           "    [thinger.boo :as boo :refer [bang]]"
                           "    [thinger [bun :as bun]"
                           "             [bin :as bin :refer :all]"
                           "             ben"
                           "             [bung.bong :as bb :refer [bing byng]]]))"
                           "(declare foo)"
                           "(def x 1)"
                           "(inc 1 (defn xxx [a b c] foo))"
                           "(let [a b"
                           "      c njnjjmmmd"
                           "      {:keys [e f] {:keys [w v] :as x} :x :as g} h"
                           "      [i j k :as m] n]"
                           "  (inc a b c d))"
                           "(println \"wat\") "
                           "(def yyy :bsrg)"])]
    (find-publics code)

    (parse code
           12 2)
    #_(time
        (-> (z/of-string code)
            (find-position default-env 50 0)
            (spy)))))


