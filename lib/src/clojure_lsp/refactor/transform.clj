(ns clojure-lsp.refactor.transform
  (:require
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [lsp4clj.protocols.producer :as producer]
   [medley.core :as medley]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [rewrite-clj.zip.subedit :as zsub]))

(set! *warn-on-reflection* true)

(def common-var-definition-symbols
  '#{defn
     defn-
     def
     defmacro
     defmulti
     defmethod
     defonce
     deftest
     deftype
     defrecord})

(defn result [zip-edits]
  (mapv (fn [zip-edit]
          (let [loc (:loc zip-edit)]
            (-> zip-edit
                (assoc :new-text (if loc (z/string loc) ""))
                (dissoc :loc))))
        zip-edits))

(defn- coll-tag [zloc]
  (get #{:vector :set :list :map} (z/tag zloc)))

(defn find-other-colls [zloc]
  (when-let [tag (coll-tag zloc)]
    (remove #{tag} [:vector :set :list :map])))

(defn change-coll
  "Change collection to specified collection"
  [zloc coll]
  (when (coll-tag zloc)
    (let [node     (z/node zloc)
          children (n/children node)]
      [{:range (meta node)
        :loc   (z/replace zloc (case (keyword coll)
                                 :map    (n/map-node children)
                                 :vector (n/vector-node children)
                                 :set    (n/set-node children)
                                 :list   (n/list-node children)))}])))

(defn cycle-coll
  "Cycles collection between vector, list, map and set"
  [zloc]
  (when-let [tag (coll-tag zloc)]
    (change-coll zloc (case tag
                        :map    :vector
                        :vector :set
                        :set    :list
                        :list   :map))))

(defn ^:private thread-sym
  [zloc sym top-meta db]
  (let [keep-parens-when-threading? (settings/get db [:keep-parens-when-threading?] false)
        movement (if (= '-> sym) z/right (comp z/rightmost z/right))]
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
                                        (and (edit/single-child? loc)
                                             (not keep-parens-when-threading?))
                                        (-> z/down edit/raise)

                                        threaded?
                                        (-> (z/insert-left first-node)
                                            (z/left)
                                            (z/insert-right* (n/spaces first-col))
                                            (z/insert-right* (n/newlines 1))
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

(def thread-invalid-symbols
  (set/union common-var-definition-symbols
             '#{-> ->> ns :require :import deftest testing comment when if}))

(defn can-thread-list? [zloc]
  (let [zloc (z/skip-whitespace z/up zloc)]
    (and (= (z/tag zloc) :list)
         (not (contains? thread-invalid-symbols
                         (some-> zloc z/next z/sexpr))))))

(defn can-thread? [zloc]
  (let [zloc (z/skip-whitespace z/up zloc)]
    (or (can-thread-list? zloc)
        (and (= (z/tag zloc) :token)
             (= (z/tag (z/up zloc)) :list)
             (not (contains? thread-invalid-symbols
                             (some-> zloc z/up z/next z/sexpr)))))))

(defn thread-first
  [zloc db]
  (when (can-thread? zloc)
    (thread-sym zloc '-> (meta (z/node zloc)) db)))

(defn thread-last
  [zloc db]
  (when (can-thread? zloc)
    (thread-sym zloc '->> (meta (z/node zloc)) db)))

(defn thread-all
  [zloc sym db]
  (when (can-thread? zloc)
    (let [zloc (if (= (z/tag zloc) :list) zloc (z/up zloc))
          top-meta (meta (z/node zloc))
          [{top-range :range} :as result] (thread-sym zloc sym top-meta db)]
      (loop [[{:keys [loc]} :as result] result]
        (let [next-loc (z/right (z/down loc))]
          (if (and (can-thread-list? next-loc) (z/right (z/down next-loc)))
            (recur (thread-sym next-loc sym top-meta db))
            (assoc-in result [0 :range] top-range)))))))

(defn thread-first-all
  [zloc db]
  (thread-all zloc '-> db))

(defn thread-last-all
  [zloc db]
  (thread-all zloc '->> db))

(def thread-first-symbols #{'-> 'some->})
(def thread-last-symbols #{'->> 'some->>})

(def thread-symbols (set/union thread-first-symbols
                               thread-last-symbols))

(defn can-unwind-thread? [zloc]
  (let [thread-loc (apply edit/find-ops-up zloc (map str thread-symbols))
        thread-sym (when thread-loc
                     (thread-symbols
                       (z/sexpr thread-loc)))]
    (when thread-sym
      {:thread-loc thread-loc
       :thread-sym thread-sym})))

(defn unwind-thread
  [zloc]
  (when-let [{:keys [thread-loc thread-sym]} (can-unwind-thread? zloc)]
    (let [val-loc (z/right thread-loc)
          target-loc (z/right val-loc)
          extra? (z/right target-loc)
          insert-fn (if (some #(string/ends-with? (name thread-sym) (str %))
                              thread-first-symbols)
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
            :loc result-loc}])))))

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

(defn ^:private widest-scoped-local [zloc uri db]
  (let [z-meta (meta (z/node zloc))
        local-defs (->> (q/find-local-usages-under-form db
                                                        (shared/uri->filename uri)
                                                        z-meta)
                        (map #(q/find-definition db %)))]
    (reduce
      (fn [accum d]
        (if (or (not accum)
                (< (:row accum) (:row d))
                (and (= (:row accum) (:row d))
                     (< (:col accum) (:col d))))
          d
          accum))
      nil
      local-defs)))

(defn ^:private in-scope-of-definition? [loc definition]
  (if (not definition)
    true
    (when loc
      (edit/in-range? (-> definition
                          (set/rename-keys {:scope-end-row :end-row :scope-end-col :end-col})
                          (update :end-col inc))
                      (meta (z/node loc))))))

(defn find-let-form
  "Finds a let-form that would be valid to move zloc to"
  [zloc uri db]
  (let [let-loc (some-> zloc
                        (edit/find-ops-up "let")
                        z/up)]
    (when let-loc
      (let [bindings-loc (z/right (z/down (zsub/subzip let-loc)))
            definition (widest-scoped-local zloc uri db)
            valid? (or
                     (not definition)
                     ;; definition is defined in the let
                     (edit/in-range? (meta (z/node bindings-loc)) definition)
                     ;; definition's encloses the let
                     (in-scope-of-definition? let-loc definition))]
        (when valid?
          let-loc)))))

(defn introduce-let
  "Adds a let around the current form."
  [zloc binding-name]
  (when-let [zloc (or (z/skip-whitespace z/right zloc)
                      (when-not (edit/top? zloc) (z/skip-whitespace z/up zloc)))]
    (let [sym (symbol binding-name)
          {:keys [col]} (meta (z/node zloc))
          loc (-> zloc
                  (edit/wrap-around :list) ; wrap with new let list
                  (z/insert-child 'let) ; add let
                  (z/append-child* (n/newlines 1)) ; add new line after location
                  (z/append-child* (n/spaces (inc col)))  ; indent body
                    ;; TODO we should add proper spaces to whole sym body to match indentation
                  (z/append-child sym) ; add new symbol to body of let
                  (z/down) ; enter let list
                  (z/right) ; skip 'let
                  (edit/wrap-around :vector) ; wrap binding vec around form
                  (z/insert-child sym) ; add new symbol as binding
                  z/up
                  (edit/join-let))]
      [{:range (meta (z/node (or loc zloc)))
        :loc   loc}])))

(defn expand-let
  "Expand the scope of the next let up the tree."
  ([zloc uri db]
   (expand-let zloc true uri db))
  ([zloc expand-to-top? uri db]
   (let [let-loc (some-> zloc
                         (edit/find-ops-up "let")
                         z/up)]
     (when (and let-loc (not (edit/top? let-loc)))
       (let [bind-node (-> let-loc z/down z/right z/node)
             parent-let-loc (edit/parent-let? let-loc)
             parent-loc (some-> let-loc z/up)]
         (if parent-let-loc
           [{:range (meta (z/node parent-let-loc))
             :loc (edit/join-let let-loc)}]
           (when (and (or expand-to-top? (not (edit/top? parent-loc)))
                      (in-scope-of-definition? parent-loc (widest-scoped-local let-loc uri db)))
             (let [{:keys [col] :as parent-meta} (meta (z/node parent-loc))
                   result-loc (-> let-loc
                                  (z/insert-child ::dummy) ; prepend dummy element to let form
                                  (z/splice) ; splice in let
                                  (z/right)
                                  (z/remove) ; remove let
                                  (z/right)
                                  (z/remove) ; remove binding
                                  (z/find z/up #(not= (z/tag %) :token)) ; go to parent form container
                                  (z/edit->
                                    (z/find-value z/next ::dummy)
                                    (z/remove)) ; remove dummy element
                                  (edit/wrap-around :list) ; wrap with new let list
                                  (z/insert-child* (n/spaces col)) ; insert let and bindings backwards
                                  (z/insert-child* (n/newlines 1)) ; insert let and bindings backwards
                                  (z/insert-child bind-node)
                                  (z/insert-child 'let))
                   merge-result-with-parent-let? (edit/parent-let? result-loc)]
               [{:range (if merge-result-with-parent-let?
                          (meta (z/node (z/up (z/up let-loc))))
                          parent-meta)
                 :loc (edit/join-let result-loc)}]))))))))

(defn move-to-let
  "Adds form and symbol to a let further up the tree"
  [zloc uri db binding-name]
  (let [zloc (z/skip-whitespace z/right zloc)]
    (if-let [let-top-loc (find-let-form zloc uri db)]
      (let [let-loc       (z/down (zsub/subzip let-top-loc))
            bound-string  (z/string zloc)
            bound-node    (z/node zloc)
            binding-sym   (symbol binding-name)
            bindings-loc  (z/right let-loc)
            {:keys [col]} (meta (z/node bindings-loc)) ;; indentation of bindings
            first-bind    (z/down bindings-loc)
            bindings-pos  (replace-in-bind-values
                            first-bind
                            #(= bound-string (z/string %))
                            binding-sym)
            with-binding  (if bindings-pos
                            (-> bindings-pos
                                (z/insert-left binding-sym)
                                (z/insert-left* bound-node)
                                (z/insert-left* (n/newlines 1))
                                (z/insert-left* (n/spaces col)))
                            (-> bindings-loc
                                (cond->
                                 first-bind (z/append-child* (n/newlines 1))
                                 first-bind (z/append-child* (n/spaces col))) ; insert let and binding backwards
                                (z/append-child binding-sym) ; add binding symbol
                                (z/append-child bound-node)
                                (z/down)
                                (z/rightmost)))
            new-let-loc   (loop [loc (z/next with-binding)]
                            (cond
                              (z/end? loc)                    (z/replace let-top-loc (z/root loc))
                              (= (z/string loc) bound-string) (recur (z/next (z/replace loc binding-sym)))
                              :else                           (recur (z/next loc))))]
        [{:range (meta (z/node (z/up let-loc)))
          :loc   new-let-loc}])
      ;; There's no existing let to move to, introduce-let and expand until it stops.
      (loop [{:keys [loc] :as current-edit} (first (introduce-let zloc binding-name))
             previous-edit nil]
        (if current-edit
          (recur (first (expand-let loc false uri db)) current-edit)
          (some-> previous-edit vector))))))

(defn new-defn-zloc [fn-name private? params body db]
  (let [root (z/of-string
               (cond
                 (not private?)
                 (format "(defn %s)" fn-name)

                 (settings/get db [:use-metadata-for-privacy?] false)
                 (format "(defn ^:private %s)" fn-name)

                 :else
                 (format "(defn- %s)" fn-name)))]
    (reduce z/append-child*
            (-> root
                (z/append-child* (n/spaces 1))
                (z/append-child* params))
            body)))

(defn prepend-preserving-comment
  "Returns an edit that places `new-loc` before `existing-loc`, keeping any
  comments or whitespace that preceed `existing-loc` close to it."
  [existing-loc new-loc]
  (let [{existing-row :row
         new-col :col} (meta (z/node existing-loc))
        prev-end-row-w-space (some-> (z/find-next existing-loc z/left z/sexpr-able?)
                                     z/node
                                     meta
                                     :end-row
                                     inc)
        new-row (or prev-end-row-w-space existing-row)
        new-range {:row     new-row
                   :col     new-col
                   :end-row new-row
                   :end-col new-col}
        new-edit (-> new-loc
                     z/insert-newline-left
                     z/insert-newline-right
                     z/up)]
    {:loc   new-edit
     :range new-range}))

(defn extract-function
  [zloc uri fn-name db]
  (when-let [zloc (or (z/skip-whitespace z/right zloc)
                      (z/skip-whitespace z/up zloc))]
    (let [;; the expression that will be extracted
          expr-loc (if (= :token (z/tag zloc))
                     (z/up (edit/find-op zloc))
                     zloc)
          ;; the top-level form it will be extracted from
          form-loc (edit/to-top expr-loc)]
      (when (and expr-loc form-loc)
        (let [expr-node (z/node expr-loc)
              expr-meta (meta expr-node)
              {defn-col :col} (meta (z/node form-loc))

              fn-sym (symbol fn-name)
              used-syms (into []
                              (comp (map :name)
                                    (distinct))
                              (q/find-local-usages-under-form db
                                                              (shared/uri->filename uri)
                                                              expr-meta))

              expr-edit (z/of-node (list* fn-sym used-syms))
              private? true
              defn-loc (new-defn-zloc fn-sym private? used-syms
                                      [(n/newlines 1)
                                       (n/spaces (+ defn-col 1))
                                       expr-node]
                                      db)]
          [(prepend-preserving-comment form-loc defn-loc)
           {:loc   expr-edit
            :range expr-meta}])))))

(defn ^:private extract-definition-params [zloc]
  ;; the expression that will be extracted
  (when-let [zloc (or (z/skip-whitespace z/right zloc)
                      (z/skip-whitespace z/up zloc))]
    ;; the top-level form it will be extracted from (possibly the same as zloc)
    (when-let [form-loc (edit/to-top zloc)]
      {:zloc zloc
       :form-loc form-loc})))

(defn can-extract-definition? [zloc]
  (boolean (extract-definition-params zloc)))

(defn extract-definition [zloc def-name]
  (when-let [{:keys [zloc form-loc]} (extract-definition-params zloc)]
    (let [expr-node (z/node zloc)
          expr-meta (meta expr-node)

          def-name (or def-name "new-value")
          expr-edit (z/of-node (symbol def-name))
          def-loc (-> (format "(def ^:private %s\n  )" def-name)
                      z/of-string
                      (z/append-child* expr-node))]
      [(prepend-preserving-comment form-loc def-loc)
       {:loc   expr-edit
        :range expr-meta}])))

(defn ^:private replace-sexprs [zloc replacements]
  (z/prewalk zloc z/sexpr-able?
             (fn [zloc]
               (when-let [replacement (get replacements (z/sexpr zloc))]
                 (z/replace zloc replacement)))))

(defn ^:private outer-fn-form? [zloc]
  (and (= :list (z/tag zloc))
       (some-> zloc z/down z/sexpr (= 'fn))))

(defn ^:private outer-literal-form? [zloc]
  (= :fn (z/tag zloc)))

(defn ^:private convert-fn-to-literal-params [zloc]
  ;; skip non-fns
  (when-let [fn-zloc (if (outer-fn-form? zloc)
                       zloc
                       (some-> zloc (edit/find-ops-up "fn") z/up))]
    ;; skip multi-arity fns
    (when-let [params-vector (-> fn-zloc z/down (z/find-tag z/right :vector))]
      (let [params (z/child-sexprs params-vector)]
        ;; skip fns with destructured params
        (when (every? symbol? params)
          [fn-zloc params])))))

(defn ^:private promote-fn-params [zloc]
  (when-let [zloc (z/find zloc z/up (some-fn outer-literal-form? outer-fn-form?))]
    {:promotion (cond (outer-literal-form? zloc) :literal-to-fn
                      (outer-fn-form? zloc)      :fn-to-defn)
     :zloc      zloc}))

(defn can-demote-fn? [zloc]
  (boolean (convert-fn-to-literal-params zloc)))

(defn can-promote-fn? [zloc]
  (:promotion (promote-fn-params zloc)))

(defn ^:private convert-literal-to-fn [zloc provided-name]
  (let [literal-params (->> (z/down (z/subzip zloc))
                            (iterate z/next)
                            (take-while (complement z/end?))
                            (keep (fn [zloc]
                                    (when (= :token (z/tag zloc))
                                      (when-let [[_ trailing] (re-find #"^%([1-9][0-9]*|&)?$" (z/string zloc))]
                                        (cond
                                          (= "&" trailing) {:key     :varargs
                                                            :unnamed #{(z/sexpr zloc)}
                                                            :named   'args}
                                          (nil? trailing)  {:key     0
                                                            :unnamed #{(z/sexpr zloc)}
                                                            :named   'element}
                                          :else            (let [n (Long/valueOf ^String trailing)]
                                                             {:key     n
                                                              :unnamed #{(z/sexpr zloc)}
                                                              :named   (symbol (str "element" n))}))))))
                            (medley/index-by :key))
        param-0 (get literal-params 0)
        param-1 (get literal-params 1)
        vararg (get literal-params :varargs)
        positioned-params (cond-> (dissoc literal-params 0 :varargs)
                            param-0 (assoc 1
                                           (cond-> (assoc param-0 :key 1)
                                             param-1 (update :unnamed set/union (:unnamed param-1)))))
        fn-params (if (seq positioned-params)
                    (->> (range 1 (inc (apply max (keys positioned-params))))
                         (mapv (fn [pos]
                                 (if-let [param (get positioned-params pos)]
                                   (:named param)
                                   '_))))
                    [])
        fn-params (cond-> fn-params
                    vararg (conj '& (:named vararg)))
        replacement (fn [param]
                      (reduce (fn [result sym]
                                (assoc result sym (:named param)))
                              {}
                              (:unnamed param)))
        replacements (reduce (fn [result param]
                               (merge result (replacement param)))
                             (if vararg
                               (replacement vararg)
                               {})
                             (vals positioned-params))
        interior (n/children (z/node (replace-sexprs zloc replacements)))
        fn-node (n/list-node
                  (concat ['fn (n/spaces 1)]
                          (when provided-name
                            [(symbol provided-name) (n/spaces 1)])
                          [fn-params]
                          (let [first-form (first (filter n/sexpr-able? interior))]
                            (cond
                              (not first-form)
                              , interior
                              ;; remove explicit do
                              (= 'do (n/sexpr first-form))
                              , (let [[before-do [_do & after-do]] (split-with (complement
                                                                                 #(and (n/sexpr-able? %)
                                                                                       (= 'do (n/sexpr %))))
                                                                               interior)]
                                  (concat before-do after-do))
                              ;; add implicit sexpr wrapper
                              :else
                              , (let [[before-sexpr sexpr-and-more] (split-with (complement n/sexpr-able?)
                                                                                interior)]
                                  (concat [(n/spaces 1)]
                                          before-sexpr
                                          [(n/list-node sexpr-and-more)]))))))]
    [{:loc (z/replace zloc fn-node)
      :range (meta (z/node zloc))}]))

(defn ^:private convert-fn-to-literal [zloc params]
  (let [[positioned-params [_ vararg]] (split-with #(not= '& %) params)
        replacements (if (= 1 (count positioned-params))
                       {(first positioned-params) '%}
                       (->> positioned-params
                            (map-indexed (fn [idx param]
                                           [param (symbol (str "%" (inc idx)))]))
                            (into {})))
        replacements (cond-> replacements
                       vararg (assoc vararg '%&))
        interior (-> zloc
                     (replace-sexprs replacements)
                     z/down
                     (z/find-tag z/right :vector)
                     z/right*
                     (->> (iterate z/right*)
                          (take-while (complement z/end?))
                          (drop-while z/whitespace?)
                          (map z/node)))
        literal-node (n/fn-node
                       (if (< 1 (count (filter n/sexpr-able? interior)))
                           ;; add implicit do
                         (into ['do (n/spaces 1)] interior)
                           ;; remove explicit sexpr wrapper
                         (mapcat (fn [node]
                                   (if (n/inner? node) (n/children node) [node]))
                                 interior)))]
    [{:loc   (z/replace zloc literal-node)
      :range (meta (z/node zloc))}]))

(defn ^:private convert-fn-to-defn [zloc uri db provided-name]
  (let [fn-form-meta (meta (z/node zloc))
        space (n/spaces 1)
        ;; We'll walk over the `fn`, converting it to a `defn`. Make the `fn` a
        ;; `defn`, ensure it's private, give it a name, and ensure any locals
        ;; defined outside the `fn` are passed in as args.
        ;; First isolate the `fn` from its context, so it can be inserted at the
        ;; top level later.
        isolated-zloc (z/of-node (z/node zloc))
        ;; Replace `fn` node with `defn` or `defn-`
        metadata-for-privacy? (settings/get db [:use-metadata-for-privacy?] false)
        zloc-on-defn (-> isolated-zloc z/down (z/replace (if metadata-for-privacy? 'defn 'defn-)))
        ;; Add or replace name, possibly adding metadata privacy.
        fn-name-zloc (->> (z/right zloc-on-defn)
                          (iterate z/right)
                          (take-while (complement #(contains? #{:list :vector} (z/tag %))))
                          (filter #(n/symbol-node? (z/node %)))
                          first)
        defn-name (or (some-> provided-name symbol)
                      (some-> fn-name-zloc z/sexpr)
                      'new-function)
        defn-name-with-meta (cond->> defn-name
                              metadata-for-privacy? (n/meta-node (n/keyword-node :private)))
        zloc-on-name (if fn-name-zloc
                       (z/replace fn-name-zloc defn-name-with-meta)
                       (-> zloc-on-defn
                           (z/insert-right defn-name-with-meta)
                           (z/right)))
        ;; Prepend locals to param lists.
        ;; We prepend because it works whether replacing with `partial` or `#()`.
        used-locals (->> (q/find-local-usages-under-form db
                                                         (shared/uri->filename uri)
                                                         fn-form-meta)
                         (map :name))
        add-locals (fn [zloc]
                     ;; Navigate to the params node and prepend all the locals.
                     (reduce (fn [params-zloc used-local]
                               (z/insert-child params-zloc used-local))
                             (z/find-tag zloc z/right :vector)
                             (reverse used-locals)))
        single-arity? (z/find-next-tag zloc-on-name z/right :vector)
        defn-zloc (z/up
                    (if single-arity?
                      (add-locals zloc-on-name)
                      (loop [zloc zloc-on-name]
                        (if-let [next-arity (z/find-next-tag zloc z/right :list)]
                          (recur (-> next-arity z/down add-locals z/up))
                          zloc))))
        ;; The `defn` is ready. Now construct a node that will replace the `fn`.
        replacement-node (cond
                             ;; new-function
                           (not (seq used-locals))
                           defn-name
                             ;; (partial new-function a b)
                           (or (not single-arity?)
                                 ;; don't nest fn literals
                               (z/find-tag zloc z/up :fn))
                           (n/list-node
                             (list* 'partial space defn-name space (interpose space used-locals)))
                             ;; depending on whether function originally had params:
                             ;; #(new-function a b)
                             ;; #(new-function a b %1 %2 %&)
                           :else
                           (n/fn-node
                             (list* defn-name space
                                    (let [orig-params (z/node (z/find-tag (z/down zloc) z/right :vector))
                                          [before-amp amp-and-after] (->> (n/children orig-params)
                                                                          (filter n/sexpr-able?)
                                                                          (split-with #(not= '& (n/sexpr %))))
                                          literal-args (concat
                                                         (map-indexed (fn [idx _]
                                                                        (n/token-node (symbol (str "%" (inc idx)))))
                                                                      before-amp)
                                                         (when (seq amp-and-after)
                                                           ['%&]))]
                                      (->> (concat used-locals literal-args)
                                           (interpose space))))))]
    [(prepend-preserving-comment (edit/to-top zloc) defn-zloc)
     {:loc (z/of-node* replacement-node)
      :range fn-form-meta}]))

(defn demote-fn [zloc]
  ;; TODO: someday, this could inline a defn, so defn -> fn.
  (when-let [[zloc params] (convert-fn-to-literal-params zloc)]
    (convert-fn-to-literal zloc params)))

(defn promote-fn [zloc uri db fn-name]
  (when-let [{:keys [promotion zloc]} (promote-fn-params zloc)]
    (case promotion
      :literal-to-fn (convert-literal-to-fn zloc fn-name)
      :fn-to-defn    (convert-fn-to-defn zloc uri db fn-name))))

(defn find-function-form [zloc]
  (apply edit/find-ops-up zloc (mapv str common-var-definition-symbols)))

(defn cycle-privacy
  [zloc db]
  (when-let [oploc (find-function-form zloc)]
    (let [op (z/sexpr oploc)
          switch-defn-? (and (= 'defn op)
                             (not (settings/get db [:use-metadata-for-privacy?])))
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
  [{:keys [filename name-row name-col bucket]} db]
  (when (or (identical? :locals bucket)
            (identical? :var-definitions bucket))
    (some-> (parser/safe-zloc-of-file db (shared/filename->uri filename db))
            (parser/to-pos name-row name-col)
            edit/find-op
            z/sexpr
            #{'let 'def})))

(defn inline-symbol
  [uri line column db]
  (let [definition (q/find-definition-from-cursor db (shared/uri->filename uri) line column)]
    (when-let [op (inline-symbol? definition db)]
      (let [references (q/find-references-from-cursor db (shared/uri->filename uri) line column false)
            def-uri    (shared/filename->uri (:filename definition) db)
            ;; TODO: use safe-zloc-of-file and handle nils
            def-loc    (some-> (parser/zloc-of-file db def-uri)
                               (parser/to-pos (:name-row definition) (:name-col definition)))
            val-loc    (z/right def-loc)
            end-pos    (if (= op 'def)
                         (meta (z/node (z/up def-loc)))
                         (meta (z/node val-loc)))
            prev-loc   (if (= op 'def)
                         (z/left (z/up def-loc))
                         (z/left def-loc))
            start-pos  (if prev-loc
                         (set/rename-keys (meta (z/node prev-loc))
                                          {:end-row :row :end-col :col})
                         (meta (z/node def-loc)))
            def-range  {:row     (:row start-pos)
                        :col     (:col start-pos)
                        :end-row (:end-row end-pos)
                        :end-col (:end-col end-pos)}]
        {:changes-by-uri
         (reduce
           (fn [accum {:keys [filename] :as element}]
             (update accum
                     (shared/filename->uri filename db)
                     (fnil conj [])
                     {:loc val-loc :range element}))
           {def-uri [{:loc nil :range def-range}]}
           references)}))))

(defn can-create-function? [zloc]
  (and zloc
       (#{:list :token} (z/tag zloc))))

(defn find-public-function-to-create [zloc uri db]
  (when (and zloc
             (identical? :token (z/tag zloc))
             (qualified-symbol? (z/sexpr zloc)))
    (let [z-sexpr (z/sexpr zloc)
          z-name (name z-sexpr)
          z-ns (namespace z-sexpr)
          ;; TODO: shouldn't this also look for unaliased ns-usages?
          ;; See https://github.com/clojure-lsp/clojure-lsp/issues/1023
          ns-usage (q/find-namespace-usage-by-alias db (shared/uri->filename uri) (symbol z-ns))
          ns-def (when ns-usage
                   (q/find-definition db ns-usage))]
      (cond
        ;; namespace exists; add a function in it
        ns-def   (when-not (:external? ns-def)
                   {:ns   (:name ns-def)
                    :name z-name})
        ;; alias exists, but namespace it points to does not; create namespace
        ;; and function
        ns-usage {:new-ns (:name ns-usage)
                  :name   z-name}
        ;; neither namespace nor alias exists; assume alias is full name of new
        ;; namespace; create namespace and function
        :else    {:new-ns z-ns
                  :name   z-name}))))

(defn ^:private create-function-param [node index]
  (if (and node
           (identical? :token (n/tag node))
           (symbol? (n/sexpr node)))
    (let [sexpr (n/sexpr node)]
      (if-let [[_ num]  (re-matches #"^%([\d]*)$" (str sexpr))]
        (symbol (str "element" num))
        sexpr))
    (symbol (str "arg" (inc index)))))

(defn ^:private create-function-for-alias
  [local-zloc ns-or-alias fn-name defn-edit uri db]
  (let [filename (shared/uri->filename uri)
        ;; TODO: shouldn't this also look for unaliased ns-usages?
        ;; See https://github.com/clojure-lsp/clojure-lsp/issues/1023
        ns-usage (q/find-namespace-usage-by-alias db filename (symbol ns-or-alias))
        ns-definition (when ns-usage
                        (q/find-definition db ns-usage))
        source-paths (settings/get db [:source-paths])
        def-uri (cond
                  ns-definition
                  (shared/filename->uri (:filename ns-definition) db)
                  ns-usage
                  (shared/namespace->uri (:name ns-usage) source-paths filename db)
                  :else
                  (shared/namespace->uri ns-or-alias source-paths filename db))
        min-range {:row 1 :end-row 1 :col 1 :end-col 1}
        max-range {:row 999999 :end-row 999999 :col 1 :end-col 1}
        defn-edits [{:loc defn-edit
                     :range max-range}
                    {:loc (z/of-string "\n")
                     :range max-range}]]
    (merge {:show-document-after-edit {:uri def-uri
                                       :take-focus? true}}
           (if ns-definition
             {:changes-by-uri {def-uri defn-edits}}
             {:resource-changes [{:kind "create"
                                  :uri def-uri
                                  :options {:overwrite? false
                                            :ignore-if-exists? true}}]
              :changes-by-uri {uri (f.add-missing-libspec/add-known-alias
                                     local-zloc
                                     (symbol ns-or-alias)
                                     (symbol fn-name)
                                     db)
                               def-uri (into
                                         [{:loc (z/up (z/of-string (format "(ns %s)\n" ns-or-alias)))
                                           :range min-range}]
                                         defn-edits)}}))))

(defn create-function [local-zloc uri db]
  (when (and local-zloc
             (identical? :token (z/tag local-zloc)))
    (let [local-sexpr (z/sexpr local-zloc)
          fn-sexpr (z/sexpr (z/down (z/up local-zloc)))
          calling? (= fn-sexpr local-sexpr)
          parent-sexpr (if calling?
                         (z/sexpr (z/down (z/up (z/up local-zloc))))
                         (z/sexpr (z/down (z/up local-zloc))))
          inside-thread-first? (thread-first-symbols parent-sexpr)
          inside-thread-last? (thread-last-symbols parent-sexpr)
          threadding? (or inside-thread-first? inside-thread-last?)
          fn-call? (and (not calling?) (not threadding?))
          fn-call-with-partial? (and fn-call?
                                     (= 'partial (z/sexpr (z/left local-zloc))))
          ns-or-alias (when (qualified-symbol? local-sexpr) (namespace local-sexpr))
          fn-name local-sexpr
          fn-name (if ns-or-alias (name fn-name) fn-name)
          args (->> (z/up local-zloc)
                    z/node
                    n/children
                    (remove n/whitespace?)
                    (drop 1)
                    vec)
          args (cond
                 fn-call-with-partial? (vec (concat [nil] (rest args)))
                 fn-call? [nil]
                 (and threadding?
                      (not calling?)) [(z/node (z/left local-zloc))]
                 inside-thread-first? (->> args
                                           (cons (z/node (z/left (z/up local-zloc))))
                                           vec)
                 inside-thread-last? (-> args
                                         (conj (z/node (z/left (z/up local-zloc))))
                                         vec)
                 :else args)
          params (->> args
                      (map-indexed (fn [index arg]
                                     (create-function-param arg index)))
                      vec)
          defn-loc (new-defn-zloc fn-name
                                  (not ns-or-alias)
                                  params
                                   ;; empty body
                                  [(n/newlines 1) (n/spaces 2)]
                                  db)]
      (if ns-or-alias
        (create-function-for-alias local-zloc ns-or-alias fn-name defn-loc uri db)
        [(prepend-preserving-comment (edit/to-top local-zloc) defn-loc)]))))

(defn ^:private create-test-for-source-path
  [uri function-name-loc source-path db]
  (let [file-type (shared/uri->file-type uri)
        function-name (z/sexpr function-name-loc)
        namespace (shared/uri->namespace uri db)
        namespace-test (str namespace "-test")
        test-filename (shared/namespace+source-path->filename namespace-test source-path file-type)
        test-uri (shared/filename->uri test-filename db)
        test-namespace-file (io/file test-filename)]
    (if (shared/file-exists? test-namespace-file)
      (let [existing-text (shared/slurp-filename test-uri)
            lines (count (string/split existing-text #"\n"))
            test-text (format "(deftest %s\n  (is (= 1 1)))" (str function-name "-test"))
            test-zloc (z/up (z/of-string (str "\n" test-text)))]
        {:show-document-after-edit {:uri test-uri
                                    :take-focus? true}
         :changes-by-uri
         {test-uri [{:loc test-zloc
                     :range {:row (inc lines) :col 1 :end-row (+ 3 lines) :end-col 1}}]}})
      (let [ns-text (format "(ns %s\n  (:require\n   [%s.test :refer [deftest is]]\n   [%s :as subject]))"
                            namespace-test
                            (if (= :cljs file-type) "cljs" "clojure")
                            namespace)
            test-text (format "(deftest %s\n  (is (= true\n         (subject/foo))))"
                              (str function-name "-test"))
            test-zloc (z/up (z/of-string (str ns-text "\n\n" test-text)))]
        {:show-document-after-edit {:uri test-uri
                                    :take-focus? true}
         :resource-changes [{:kind "create"
                             :uri test-uri
                             :options {:overwrite? false
                                       :ignore-if-exists? true}}]
         :changes-by-uri {test-uri [{:loc test-zloc
                                     :range (-> test-zloc z/node meta)}]}}))))

(defn can-create-test? [zloc uri db]
  (when-let [function-name-loc (edit/find-var-definition-name-loc zloc)]
    (let [source-paths (settings/get db [:source-paths])]
      (when-let [current-source-path (->> source-paths
                                          (filter #(and (string/starts-with? (shared/uri->filename uri) %)
                                                        (not (string/includes? % "test"))))
                                          first)]
        {:source-paths source-paths
         :current-source-path current-source-path
         :function-name-loc function-name-loc}))))

(defn create-test [zloc uri db {:keys [producer]}]
  (when-let [{:keys [source-paths
                     current-source-path
                     function-name-loc]} (can-create-test? zloc uri db)]
    (let [test-source-paths (remove #(= current-source-path %) source-paths)]
      (cond
        (= 1 (count test-source-paths))
        (create-test-for-source-path uri function-name-loc (first test-source-paths) db)

        (< 1 (count test-source-paths))
        (let [actions (mapv #(hash-map :title %) source-paths)
              chosen-source-path (producer/show-message-request producer "Choose a source-path to create the test file" :info actions)]
          (create-test-for-source-path uri function-name-loc chosen-source-path db))

        ;; No source paths besides current one
        :else nil))))

(defn suppress-diagnostic [zloc diagnostic-code]
  (let [form-zloc (or (z/up (edit/find-op zloc))
                      zloc)
        {form-row :row form-col :col :as form-pos} (-> form-zloc z/node meta)
        loc-w-comment (z/edit-> form-zloc
                                (z/insert-left (n/uneval-node (cond-> [(n/map-node [(n/keyword-node :clj-kondo/ignore)
                                                                                    (n/spaces 1)
                                                                                    (n/vector-node [(keyword diagnostic-code)])])
                                                                       (n/newlines 1)]
                                                                (> (dec form-col) 0) (conj (n/spaces (dec form-col)))))))]
    [{:loc loc-w-comment
      :range (assoc form-pos
                    :end-row form-row
                    :end-col form-col)}]))
