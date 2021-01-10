(ns clojure-lsp.refactor.transform
  (:require
   [clojure-lsp.clojure-core :as cc]
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.references :as f.references]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.refactor.edit :as edit]
   [clojure.set :as set]
   [clojure.string :as string]
   [medley.core :as medley]
   [rewrite-clj.custom-zipper.core :as cz]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [rewrite-clj.zip.subedit :as zsub]))

(defn result [zip-edits]
  (mapv (fn [zip-edit]
          (let [loc (:loc zip-edit)]
            (-> zip-edit
                (assoc :new-text (if loc (z/string loc) ""))
                (dissoc :loc))))
        zip-edits))

(defn cycle-coll
  "Cycles collection between vector, list, map and set"
  [zloc]
  (let [sexpr (z/sexpr zloc)]
    (if (coll? sexpr)
      (let [node (z/node zloc)
            coerce-to-next (fn [sexpr children]
                             (cond
                               (map? sexpr) (n/vector-node children)
                               (vector? sexpr) (n/set-node children)
                               (set? sexpr) (n/list-node children)
                               (list? sexpr) (n/map-node children)))]
        [{:range (meta (z/node zloc))
          :loc (z/replace zloc (coerce-to-next sexpr (n/children node)))}])
      [])))

(defn thread-sym
  [zloc sym top-meta]
  (let [movement (if (= '-> sym) z/right (comp z/rightmost z/right))]
    (if-let [first-loc (-> zloc z/down movement)]
      (let [first-node (z/node first-loc)
            parent-op (z/sexpr (z/left zloc))
            threaded? (= sym parent-op)
            meta-node (cond-> zloc
                        threaded? z/up
                        :always (-> z/node meta))
            first-col (+ (count (str sym)) (:col top-meta))
            result-loc (-> first-loc
                           (z/leftmost)
                           (z/edit->
                             (movement)
                             (z/remove))
                           (z/up)
                           ((fn [loc] (cond-> loc
                                        (edit/single-child? loc)
                                        (-> z/down edit/raise)

                                        threaded?
                                        (-> (z/insert-left first-node)
                                            (z/left)
                                            (cz/insert-right (n/spaces first-col))
                                            (cz/insert-right (n/newlines 1))
                                            z/up)

                                        (not threaded?)
                                        (-> (edit/wrap-around :list)
                                            (z/insert-child (n/spaces first-col))
                                            (z/insert-child (n/newlines 1))
                                            (z/insert-child first-node)
                                            (z/insert-child sym))))))]
        [{:range meta-node
          :loc result-loc}])
      [])))

(defn- can-thread? [zloc]
  (= (z/tag zloc) :list))

(defn thread-first
  [zloc]
  (when (can-thread? zloc)
    (thread-sym zloc '-> (meta (z/node zloc)))))

(defn thread-last
  [zloc]
  (when (can-thread? zloc)
    (thread-sym zloc '->> (meta (z/node zloc)))))

(defn thread-all
  [zloc sym]
  (when (can-thread? zloc)
    (let [top-meta (meta (z/node zloc))
          [{top-range :range} :as result] (thread-sym zloc sym top-meta)]
      (loop [[{:keys [loc]} :as result] result]
        (let [next-loc (z/right (z/down loc))]
          (if (and (can-thread? next-loc) (z/right (z/down next-loc)))
            (recur (thread-sym next-loc sym top-meta))
            (assoc-in result [0 :range] top-range)))))))

(defn thread-first-all
  [zloc]
  (thread-all zloc '->))

(defn thread-last-all
  [zloc]
  (thread-all zloc '->>))

(defn unwind-thread
  [zloc]
  (let [thread-loc (if (= (z/tag zloc) :list)
                     (z/next zloc)
                     zloc)
        thread-sym (#{'-> '->> 'some-> 'some->>} (z/sexpr thread-loc))]
    (when thread-sym
      (let [val-loc (z/right thread-loc)
            target-loc (z/right val-loc)
            extra? (z/right target-loc)
            insert-fn (if (string/ends-with? (name thread-sym) "->")
                        z/insert-right
                        (fn [loc node] (-> loc
                                           (z/rightmost)
                                           (z/insert-right node))))]
        (when (and val-loc target-loc)
          (let [result-loc (-> thread-loc
                               z/up
                               (z/subedit->
                                 z/down
                                 z/right
                                 z/remove
                                 z/right
                                 (cond-> (not= :list (z/tag target-loc)) (edit/wrap-around :list))
                                 (z/down)
                                 (insert-fn (z/node val-loc))
                                 (z/up)
                                 (cond-> (not extra?) (edit/raise))))]
            [{:range (meta (z/node (z/up thread-loc)))
              :loc result-loc}]))))))

(defn unwind-all
  [zloc]
  (loop [current (unwind-thread zloc)
         result nil]
    (if current
      (recur (unwind-thread (:loc (first current))) current)
      result)))

(defn find-within [zloc p?]
  (when (z/find (zsub/subzip zloc) z/next p?)
    (z/find zloc z/next p?)))

(defn replace-in-bind-values [first-bind p? replacement]
  (loop [bind first-bind
         marked? false]
    (let [exists? (some-> bind
                          (z/right)
                          (find-within p?))
          bind' (if exists?
                  (-> bind
                      (edit/mark-position-when :first-occurrence (not marked?))
                      (z/edit->
                        (z/right)
                        (find-within p?)
                        (z/replace replacement)))
                  bind)]
      (if-let [next-loc (z/right (z/right bind'))]
        (recur next-loc (or marked? exists?))
        (edit/back-to-mark-or-nil bind' :first-occurrence)))))

(defn move-to-let
  "Adds form and symbol to a let further up the tree"
  [zloc binding-name]
  (when-let [let-top-loc (some-> zloc
                                 (edit/find-ops-up 'let)
                                 z/up)]
    (let [let-loc (z/down (zsub/subzip let-top-loc))
          bound-string (z/string zloc)
          bound-node (z/node zloc)
          binding-sym (symbol binding-name)
          bindings-loc (z/right let-loc)
          {:keys [col]} (meta (z/node bindings-loc)) ;; indentation of bindings
          first-bind (z/down bindings-loc)
          bindings-pos (replace-in-bind-values
                         first-bind
                         #(= bound-string (z/string %))
                         binding-sym)
          with-binding (if bindings-pos
                         (-> bindings-pos
                             (z/insert-left binding-sym)
                             (cz/insert-left bound-node)
                             (cz/insert-left (n/newlines 1))
                             (cz/insert-left (n/spaces col)))
                         (-> bindings-loc
                             (cond->
                               first-bind (cz/append-child (n/newlines 1))
                               first-bind (cz/append-child (n/spaces col))) ; insert let and binding backwards
                             (z/append-child binding-sym) ; add binding symbol
                             (z/append-child bound-node)
                             (z/down)
                             (z/rightmost)))
          new-let-loc (loop [loc (z/next with-binding)]
                        (cond
                          (z/end? loc) (z/replace let-top-loc (z/root loc))
                          (= (z/string loc) bound-string) (recur (z/next (z/replace loc binding-sym)))
                          :else (recur (z/next loc))))]
      [{:range (meta (z/node (z/up let-loc)))
        :loc new-let-loc}])))

(defn introduce-let
  "Adds a let around the current form."
  [zloc binding-name]
  (let [sym (symbol binding-name)
        {:keys [col]} (meta (z/node zloc))
        loc (-> zloc
                (edit/wrap-around :list) ; wrap with new let list
                (z/insert-child 'let) ; add let
                (cz/append-child (n/newlines 1)) ; add new line after location
                (cz/append-child (n/spaces (inc col)))  ; indent body
                (z/append-child sym) ; add new symbol to body of let
                (z/down) ; enter let list
                (z/right) ; skip 'let
                (edit/wrap-around :vector) ; wrap binding vec around form
                (z/insert-child sym) ; add new symbol as binding
                z/up
                (edit/join-let))]
    [{:range (meta (z/node (or loc zloc)))
      :loc loc}]))

(defn expand-let
  "Expand the scope of the next let up the tree."
  [zloc]
  (let [let-loc (some-> zloc
                        (edit/find-ops-up 'let)
                        z/up)]
    (when let-loc
      (let [bind-node (-> let-loc z/down z/right z/node)
            parent-loc (edit/parent-let? let-loc)]
        (if parent-loc
          [{:range (meta (z/node parent-loc))
            :loc (edit/join-let let-loc)}]
          (let [{:keys [col] :as parent-meta} (meta (z/node (z/up let-loc)))]
            [{:range parent-meta
              :loc (-> let-loc
                       (z/splice) ; splice in let
                       (z/remove) ; remove let
                       (z/next)
                       (z/remove) ; remove binding
                       (z/find z/up #(= (z/tag %) :list)) ; go to parent form container
                       (edit/wrap-around :list) ; wrap with new let list
                       (cz/insert-child (n/spaces col)) ; insert let and bindings backwards
                       (cz/insert-child (n/newlines 1)) ; insert let and bindings backwards
                       (z/insert-child bind-node)
                       (z/insert-child 'let)
                       (edit/join-let))}]))))))

(defn ^:private process-clean-ns
  [ns-loc removed-nodes col keep-at-start? form-type]
  (let [sep (n/whitespace-node (apply str (repeat col " ")))
        single-space (n/whitespace-node " ")
        imports (->> removed-nodes
                        z/node
                        n/children
                        (remove n/printable-only?)
                        (sort-by (comp str n/sexpr))
                        (map-indexed (fn [idx node]
                                       (if (and keep-at-start?
                                                (= idx 0))
                                         [single-space node]
                                         [(n/newlines 1) sep node])))
                        (apply concat)
                        (cons (n/keyword-node form-type)))]
    (if (empty? (z/child-sexprs removed-nodes))
                       (z/subedit-> ns-loc
                                    (z/find-value z/next form-type)
                                    (z/up)
                                    z/remove)
                       (z/subedit-> ns-loc
                                    (z/find-value z/next form-type)
                                    (z/up)
                                    (z/replace (n/list-node imports))))))

(defn ^:private remove-unused-refers
  [node unused-refers]
  (let [node-refers (-> node z/down (z/find-next-value ':refer) z/right z/sexpr set)
        unused-refers-symbol (->> unused-refers (map (comp symbol name)) set)
        removed-refers (set/difference node-refers unused-refers-symbol)]
    (if (empty? removed-refers)
      (z/remove node)
      (-> node
          z/down
          z/rightmost
          (z/replace (n/vector-node (interpose (n/whitespace-node " ")
                                               (vec (sort removed-refers)))))
          z/up))))

(defn ^:private remove-unused-require
  [node unused-aliases unused-refers]
  (cond
    (not (z/vector? node))
    node

    (contains? unused-aliases (-> node z/down z/leftmost z/sexpr))
    (z/remove node)

    (contains? (->> unused-refers (map (comp symbol namespace)) set)
               (-> node z/down z/leftmost z/sexpr))
    (remove-unused-refers node unused-refers)

    :else
    node))

(defn ^:private remove-unused-requires
  [unused-aliases unused-refers nodes]
  (let [single-require? (= 1 (count (z/child-sexprs nodes)))
        first-node      (z/next nodes)
        first-node-ns   (when (and single-require?
                                   (z/vector? first-node))
                          (-> first-node z/down z/leftmost z/sexpr))
        first-node-refers (when (and single-require?
                                     (z/vector? first-node))
                            (->> (-> first-node
                                     z/down
                                     (z/find-next-value ':refer)
                                     z/right
                                     z/sexpr)
                                 (map #(symbol (str first-node-ns) (str %)))
                                 set))
        single-unused?  (when (and single-require? (z/vector? first-node))
                          (or (contains? unused-aliases first-node-ns)
                              (and (seq first-node-refers)
                                   (seq unused-refers)
                                   (set/subset? first-node-refers unused-refers))))]
    (if single-unused?
      (z/remove first-node)
      (edit/map-children nodes #(remove-unused-require % unused-aliases unused-refers)))))

(defn ^:private clean-requires
  [ns-loc usages keep-at-start?]
  (if-let [require-loc (z/find-value (zsub/subzip ns-loc) z/next :require)]
    (let [col (if require-loc
                (if keep-at-start?
                  (-> require-loc z/node meta :end-col)
                  (-> require-loc z/right z/node meta :col dec))
                4)
          unused-aliases (crawler/find-unused-aliases usages)
          unused-refers (crawler/find-unused-refers usages)
          removed-nodes (->> require-loc
                             z/remove
                             (remove-unused-requires unused-aliases unused-refers))]
      (process-clean-ns ns-loc removed-nodes col keep-at-start? :require))
    ns-loc))

(defn ^:private package-import?
  [node]
  (or (z/vector? node)
      (z/list? node)))

(defn ^:private remove-unused-package-import
  [node base-package col unused-imports]
  (cond
    (string/includes? (z/string node) ".")
    node

    (contains? unused-imports
               (symbol (str base-package "." (z/string node))))
    (z/remove node)

    (z/rightmost? node)
    node

    :else
    (z/edit-> node
              (z/insert-right (n/spaces (+ col (count base-package))))
              (z/insert-right (n/newlines 1)))))

(defn ^:private remove-unused-import
  [parent-node col unused-imports]
  (cond
    (package-import? parent-node)
    (let [base-package (-> parent-node z/down z/leftmost z/string)
          removed (edit/map-children parent-node
                                     #(remove-unused-package-import % base-package col unused-imports))]
      (if (= 1 (count (z/child-sexprs removed)))
        (z/remove removed)
        removed))

    (contains? unused-imports (z/sexpr parent-node))
    (z/remove parent-node)

    :else
    parent-node))

(defn ^:private clean-imports
  [ns-loc usages keep-at-start?]
  (if-let [import-loc (z/find-value (zsub/subzip ns-loc) z/next :import)]
    (let [col (if import-loc
                (if keep-at-start?
                  (-> import-loc z/node meta :end-col)
                  (-> import-loc z/right z/node meta :col dec))
                4)
          unused-imports (crawler/find-unused-imports usages)
          removed-nodes (-> import-loc
                             z/remove
                             (edit/map-children #(remove-unused-import % col unused-imports)))]
      (process-clean-ns ns-loc removed-nodes col keep-at-start? :import))
    ns-loc))

(defn clean-ns
  [zloc uri]
  (let [safe-loc (or zloc (z/of-string (get-in @db/db [:documents uri :text])))
        ns-loc (edit/find-namespace safe-loc)]
    (when ns-loc
      (let [keep-at-start? (get-in @db/db [:settings :keep-require-at-start?])
            usages (f.references/safe-find-references uri (slurp uri) false false)
            result-loc (-> ns-loc
                           (clean-requires usages keep-at-start?)
                           (clean-imports usages keep-at-start?))]
        [{:range (meta (z/node result-loc))
          :loc result-loc}]))))

(defn ^:private add-form-to-namespace [zloc form-to-add form-type form-to-check-exists]
  (let [ns-loc (edit/find-namespace zloc)
        ns-zip (zsub/subzip ns-loc)
        cursor-sym (z/sexpr zloc)
        need-to-add? (and (not (z/find-value ns-zip z/next cursor-sym))
                          (not (z/find-value ns-zip z/next form-to-add))
                          (not (z/find-value ns-zip z/next form-to-check-exists)))]
    (when (and form-to-add need-to-add?)
      (let [add-form-type? (not (z/find-value ns-zip z/next form-type))
            form-type-loc (z/find-value (zsub/subzip ns-loc) z/next form-type)
            keep-require-at-start? (get-in @db/db [:settings :keep-require-at-start?])
            col (if form-type-loc
                  (:col (meta (z/node (z/rightmost form-type-loc))))
                  (if keep-require-at-start?
                    2
                    5))
            result-loc (z/subedit-> ns-zip
                                    (cond->
                                        add-form-type? (z/append-child (n/newlines 1))
                                        add-form-type? (z/append-child (n/spaces 2))
                                        add-form-type? (z/append-child (list form-type)))
                                    (z/find-value z/next form-type)
                                    (z/up)
                                    (cond->
                                        (or (not add-form-type?)
                                            (not keep-require-at-start?)) (cz/append-child (n/newlines 1)))
                                    (cz/append-child (n/spaces (dec col)))
                                    (z/append-child form-to-add))]
        [{:range (meta (z/node result-loc))
          :loc result-loc}]))))

(defn add-import-to-namespace [zloc import-name]
  (add-form-to-namespace zloc (symbol import-name) :import import-name))

(defn add-common-import-to-namespace [zloc]
  (let [class-with-dot (z/sexpr zloc)
        class-name (->> class-with-dot str drop-last (string/join "") symbol)]
    (when-let [import-name (or (get cc/java-util-imports class-name)
                               (get cc/java-util-imports class-with-dot))]
      (let [result (add-form-to-namespace zloc (symbol import-name) :import import-name)]
        {:result result
         :code-action-data {:import-name import-name}}))))

(defn add-known-libspec
  [zloc ns-to-add qualified-ns-to-add]
  (when (and qualified-ns-to-add ns-to-add)
    (add-form-to-namespace zloc [qualified-ns-to-add :as ns-to-add] :require ns-to-add)))

(def common-alias->info
  {:string {:alias-str "string" :label "clojure.string" :detail "clojure.string" :alias-ns 'clojure.string}
   :set    {:alias-str "set" :label "clojure.set" :detail "clojure.set" :alias-ns 'clojure.set}
   :walk   {:alias-str "walk" :label "clojure.walk" :detail "clojure.walk" :alias-ns 'clojure.walk}
   :pprint {:alias-str "pprint" :label "clojure.pprint" :detail "clojure.pprint" :alias-ns 'clojure.pprint}
   :async  {:alias-str "async" :label "clojure.core.async" :detail "clojure.core.async" :alias-ns 'clojure.core.async}})

(defn ^:private add-missing-alias-ns [zloc source]
  (let [ns-str-to-add (some-> zloc z/sexpr namespace)
        ns-to-add (some-> ns-str-to-add symbol)
        alias->info (->> (:file-envs @db/db)
                         (mapcat val)
                         (filter (fn [usage]
                                   (or
                                     (set/subset? #{:public :ns} (:tags usage))
                                     (get-in usage [:tags :alias]))))
                         (mapv (fn [{:keys [sym _tags] alias-str :str alias-ns :ns}]
                                 {:alias-str alias-str
                                  :label (name sym)
                                  :detail (if alias-ns
                                            (str alias-ns)
                                            (name sym))
                                  :alias-ns (if alias-ns
                                              alias-ns
                                              sym)}))
                         (distinct)
                         (group-by :alias-str))
        posibilities (or (get alias->info ns-str-to-add)
                         [(get common-alias->info (keyword ns-str-to-add))])
        qualified-ns-to-add (when (= 1 (count posibilities))
                              (-> posibilities first :alias-ns))
        result (add-known-libspec zloc ns-to-add qualified-ns-to-add)]
    (if (= source :code-action)
      {:result result
       :code-action-data {:ns-name qualified-ns-to-add}}
      result)))

(def common-refers->info
  {'deftest      'clojure.test
   'testing      'clojure.test
   'is           'clojure.test
   'are          'clojure.test
   'ANY          'compojure.core
   'DELETE       'compojure.core
   'GET          'compojure.core
   'PATCH        'compojure.core
   'POST         'compojure.core
   'PUT          'compojure.core
   'context      'compojure.core
   'defroutes    'compojure.core
   'defentity    'korma.core
   'reg-event-db 're-frame.core
   'reg-sub      're-frame.core
   'reg-event-fx 're-frame.core
   'fact         'midje.sweet
   'facts        'midje.sweet})

(defn ^:private add-missing-refer [zloc source]
  (when-let [qualified-ns-to-add (get common-refers->info (z/sexpr zloc))]
    (let [refer-to-add (-> zloc z/sexpr symbol)
          ns-loc (edit/find-namespace zloc)
          ns-zip (zsub/subzip ns-loc)
          need-to-add? (not (z/find-value ns-zip z/next refer-to-add))]
      (when need-to-add?
        (let [existing-ns-require (z/find-value ns-zip z/next qualified-ns-to-add)
              add-require? (and (not existing-ns-require)
                                (not (z/find-value ns-zip z/next :require)))
              require-loc (z/find-value (zsub/subzip ns-loc) z/next :require)
              col (if require-loc
                    (-> require-loc z/rightmost z/node meta :col)
                    5)
              result-loc (if existing-ns-require
                           (z/subedit-> ns-loc
                                        (z/find-value z/next qualified-ns-to-add)
                                        (z/find-value z/next ':refer)
                                        z/right
                                        (cz/append-child (n/spaces 1))
                                        (z/append-child (z/sexpr zloc)))
                           (z/subedit-> ns-loc
                                        (cond->
                                            add-require? (z/append-child (n/newlines 1))
                                            add-require? (z/append-child (n/spaces 2))
                                            add-require? (z/append-child (list :require)))
                                        (z/find-value z/next :require)
                                        (z/up)
                                        (cz/append-child (n/newlines 1))
                                        (cz/append-child (n/spaces (dec col)))
                                        (z/append-child [qualified-ns-to-add :refer [refer-to-add]])))
              result [{:range (meta (z/node result-loc))
                       :loc result-loc}]]
          (if (= source :code-action)
            {:result result
             :code-action-data {:ns-name qualified-ns-to-add}}
            result))))))

(defn add-missing-libspec
  [zloc {:keys [source]}]
  (let [ns-str (some-> zloc z/sexpr namespace)]
    (if ns-str
      (add-missing-alias-ns zloc source)
      (add-missing-refer zloc source))))

(defn extract-function
  [zloc fn-name usages]
  (let [expr-loc (if (not= :token (z/tag zloc))
                   zloc
                   (z/up (edit/find-op zloc)))
        expr-node (z/node expr-loc)
        form-loc (edit/to-top expr-loc)
        form-pos (meta (z/node form-loc))
        fn-sym (symbol fn-name)
        {:keys [declared scoped]} (->> usages
                                       (group-by #(condp set/subset? (:tags %)
                                               #{:declare} :declared
                                               #{:scoped} :scoped
                                               nil))
                                       (medley/map-vals #(set (map :sym %))))
        used-syms (mapv (comp symbol name) (set/difference scoped declared))
        expr-edit (-> (z/of-string "")
                      (z/replace `(~fn-sym ~@used-syms)))
        defn-edit (-> (z/of-string "(defn)\n\n")
                      (z/append-child fn-sym)
                      (z/append-child used-syms)
                      (cz/append-child (n/newlines 1))
                      (cz/append-child (n/spaces 2))
                      (z/append-child expr-node))]
    [{:loc defn-edit
      :range (assoc form-pos :end-row (:row form-pos)
                    :end-col (:col form-pos))}
     {:loc (z/of-string "\n\n")
      :range (assoc form-pos :end-row (:row form-pos)
                    :end-col (:col form-pos))}
     {:loc expr-edit
      :range (meta expr-node)}]))

(defn inside-function? [zloc]
  (edit/find-ops-up zloc 'defn 'defn- 'def 'defonce 'defmacro 'defmulti 's/defn 's/def))

(defn cycle-privacy
  [zloc]
  (when-let [oploc (inside-function? zloc)]
    (let [op (z/sexpr oploc)
          switch-defn-? (and (= 'defn op)
                             (not (get-in @db/db [:settings :use-metadata-for-privacy?])))
          switch-defn? (= 'defn- op)
          name-loc (z/right oploc)
          private? (or switch-defn?
                       (-> name-loc z/sexpr meta :private))
          switch (cond
                   switch-defn? 'defn
                   switch-defn-? 'defn-
                   private? (vary-meta (z/sexpr name-loc) dissoc :private)
                   (not private?) (n/meta-node :private (z/node name-loc)))
          source (if (or switch-defn? switch-defn-?)
                   oploc
                   name-loc)]
      [{:loc (z/replace source switch)
        :range (meta (z/node source))}])))

(defn inline-symbol?
  [def-uri definition]
  (let [{:keys [text]} (get-in @db/db [:documents def-uri])
        def-loc        (parser/loc-at-pos text (:row definition) (:col definition))]
    (some-> (edit/find-op def-loc)
            z/sexpr
            #{'let 'def})))

(defn inline-symbol
  [[_ {def-uri :uri definition :usage}] references]
  (let [{:keys [text]} (get-in @db/db [:documents def-uri])
        def-loc (parser/loc-at-pos text (:row definition) (:col definition))
        op (inline-symbol? def-uri definition)]
    (when op
      (let [uses (remove (comp #(contains? % :declare) :tags :usage) references)
            val-loc (z/right def-loc)
            end-pos (if (= op 'def) (meta (z/node (z/up def-loc))) (meta (z/node val-loc)))
            prev-loc (if (= op 'def) (z/left (z/up def-loc)) (z/left def-loc))
            start-pos (if prev-loc
                        (set/rename-keys (meta (z/node prev-loc))
                                         {:end-row :row :end-col :col})
                        (meta (z/node def-loc)))
            def-range {:row (:row start-pos)
                       :col (:col start-pos)
                       :end-row (:end-row end-pos)
                       :end-col (:end-col end-pos)}]
        (reduce
          (fn [accum {:keys [uri usage]}]
            (update accum uri (fnil conj []) {:loc val-loc :range usage}))
          {def-uri [{:loc nil :range def-range}]}
          uses)))))
