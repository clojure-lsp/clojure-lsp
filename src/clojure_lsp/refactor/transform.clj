(ns clojure-lsp.refactor.transform
  (:require
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
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

(defn ^:private remove-unused-require
  [node unused-aliases]
  (if (z/vector? node)
    (let [alias-node (-> node z/down z/leftmost)]
      (if (contains? unused-aliases (z/sexpr alias-node))
        (let [removed-node (-> node z/remove)]
          (if (z/list? removed-node)
            (z/down removed-node)
            (z/up removed-node)))
        node))
    node))

(defn ^:private remove-unused-requires
  [unused-ns nodes]
  (let [single-require? (= 1 (count (z/child-sexprs nodes)))
        first-node      (z/next nodes)
        single-unused?  (when (and single-require? (z/vector? first-node))
                          (contains? unused-ns (-> first-node
                                                   z/down
                                                   z/leftmost
                                                   z/sexpr)))]
    (if single-unused?
      (z/remove first-node)
      (let [removed (z/map #(remove-unused-require % unused-ns) nodes)]
        (if (= :vector (z/tag removed))
          (z/up removed)
          removed)))))

(defn clean-ns
  [zloc uri]
  (let [safe-loc (or zloc (z/of-string (get-in @db/db [:documents uri :text])))
        ns-loc (edit/find-namespace safe-loc)
        require-loc (z/find-value (zsub/subzip ns-loc) z/next :require)
        keep-require-at-start? (get-in @db/db [:settings :keep-require-at-start?])
        col (if require-loc
              (if keep-require-at-start?
                (-> require-loc z/node meta :end-col)
                (-> require-loc z/right z/node meta :col dec))
              4)
        sep (n/whitespace-node (apply str (repeat col " ")))
        single-space (n/whitespace-node " ")
        unused-aliases (crawler/find-unused-aliases uri)
        unused-refers (crawler/find-unused-refers uri)
        removed-nodes (->> require-loc
                           z/remove
                           (remove-unused-requires (set/union unused-aliases unused-refers)))
        requires (->> removed-nodes
                      z/node
                      n/children
                      (remove n/printable-only?)
                      (sort-by (comp str n/sexpr))
                      (map-indexed (fn [idx node]
                                     (if (and keep-require-at-start?
                                              (= idx 0))
                                       [single-space node]
                                       [(n/newlines 1) sep node])))
                      (apply concat)
                      (cons (n/keyword-node :require)))
        result-loc (if (empty? (z/child-sexprs removed-nodes))
                     (z/subedit-> ns-loc
                                  (z/find-value z/next :require)
                                  (z/up)
                                  z/remove)
                     (z/subedit-> ns-loc
                                  (z/find-value z/next :require)
                                  (z/up)
                                  (z/replace (n/list-node requires))))]
    [{:range (meta (z/node result-loc))
      :loc result-loc}]))

(defn add-known-libspec
  [zloc ns-to-add qualified-ns-to-add]
  (let [ns-loc (edit/find-namespace zloc)
        ns-zip (zsub/subzip ns-loc)
        need-to-add? (and (not (z/find-value ns-zip z/next qualified-ns-to-add))
                          (not (z/find-value ns-zip z/next ns-to-add)))]
    (when (and ns-to-add qualified-ns-to-add need-to-add?)
      (let [add-require? (not (z/find-value ns-zip z/next :require))
            require-loc (z/find-value (zsub/subzip ns-loc) z/next :require)
            col (if require-loc
                  (:col (meta (z/node (z/rightmost require-loc))))
                  5)
            result-loc (z/subedit-> ns-loc
                                    (cond->
                                      add-require? (z/append-child (n/newlines 1))
                                      add-require? (z/append-child (n/spaces 2))
                                      add-require? (z/append-child (list :require)))
                                    (z/find-value z/next :require)
                                    (z/up)
                                    (cz/append-child (n/newlines 1))
                                    (cz/append-child (n/spaces (dec col)))
                                    (z/append-child [qualified-ns-to-add :as ns-to-add]))]
        [{:range (meta (z/node result-loc))
          :loc result-loc}]))))

(defn add-missing-libspec
  [zloc]
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
        posibilities (get alias->info ns-str-to-add)
        qualified-ns-to-add (cond
                              (= 1 (count posibilities))
                              (-> posibilities first :alias-ns))]
    (add-known-libspec zloc ns-to-add qualified-ns-to-add)))

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
