(ns clojure-lsp.refactor.transform
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.refactor.edit :as edit]
    [clojure.set :as set]
    [clojure.tools.logging :as log]
    [rewrite-clj.custom-zipper.core :as cz]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]
    [rewrite-clj.zip.subedit :as zsub]))

(defn result [zip-edits]
  (mapv (fn [zip-edit]
          (let [loc (:loc zip-edit)]
            (-> zip-edit
                (assoc :new-text (z/string loc))
                (dissoc :loc))))
        zip-edits))

(defn cycle-coll
  "Cycles collection between vector, list, map and set"
  [zloc _uri]
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

(defmacro zass [loc sexpr]
  `(do
     (assert (= '~sexpr (z/sexpr ~loc)) (pr-str (z/sexpr ~loc)))
     ~loc))

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
                                        (edit/single-child? loc) (-> z/down edit/raise)

                                        threaded? (-> (z/insert-left first-node)
                                                      (z/left)
                                                      (cz/insert-right (n/spaces first-col))
                                                      (cz/insert-right (n/newlines 1))
                                                      z/up)
                                        (not threaded?) (-> (edit/wrap-around :list)
                                                            (z/insert-child (n/spaces first-col))
                                                            (z/insert-child (n/newlines 1))
                                                            (z/insert-child first-node)
                                                            (z/insert-child sym))))))]
        [{:range meta-node
          :loc result-loc}])
      [])))

(comment
  [:a :b]
  (foo (-> (quux (qux x w))
           (bar y)) z)
  (->> (bar w (qux y x))
       (foo z))
  )

(defn- can-thread? [zloc]
  (= (z/tag zloc) :list))

(defn thread-first
  [zloc _uri]
  (when (can-thread? zloc)
    (thread-sym zloc '-> (meta (z/node zloc)))))

(defn thread-last
  [zloc _uri]
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
  [zloc _uri]
  (thread-all zloc '->))

(defn thread-last-all
  [zloc _uri]
  (thread-all zloc '->>))

(defn find-within [zloc p?]
  (when (z/find (zsub/subzip zloc) z/next p?)
    (z/find zloc z/next p?)))

(defn replace-in-bind-values [first-bind p? replacement]
  (loop [bind first-bind
         marked? false]
    (let [exists? (-> bind
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
  [zloc _uri binding-name]
  (if-let [let-top-loc (some-> zloc
                               (edit/find-ops-up 'let)
                               z/up)]
    (let [let-loc (z/down (zsub/subzip let-top-loc))
          bound-sexpr (z/sexpr zloc)
          bound-string (z/string zloc)
          bound-node (z/node zloc)
          binding-sym (symbol binding-name)
          bindings-loc (z/right let-loc)
          {:keys [col]} (meta (z/node bindings-loc)) ;; indentation of bindings
          bindings-pos (replace-in-bind-values
                         (z/down bindings-loc)
                         #(= bound-string (z/string %))
                         binding-sym)
          with-binding (if bindings-pos
                         (-> bindings-pos
                             (z/insert-left binding-sym)
                             (cz/insert-left bound-node)
                             (cz/insert-left (n/newlines 1))
                             (cz/insert-left (n/spaces col)))
                         (-> bindings-loc
                             (cz/append-child (n/newlines 1))
                             (cz/append-child (n/spaces col)) ; insert let and binding backwards
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
        :loc new-let-loc}])
    []))

(defn introduce-let
  "Adds a let around the current form."
  [zloc _uri binding-name]
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
  [zloc _uri]
  (let [let-loc zloc
        bind-node (-> let-loc z/down z/right z/node)]
    (if-let [parent-loc (edit/parent-let? let-loc)]
      [{:range (meta (z/node parent-loc))
        :loc (edit/join-let let-loc)}]
      (let [{:keys [col] :as parent-meta} (meta (z/node (z/up let-loc)))]
        [{:range parent-meta
          :loc (-> let-loc
                   (z/splice) ; splice in let
                   (z/remove) ; remove let
                   (z/next)
                   (z/remove) ; remove binding
                   (z/up) ; go to form container
                   (edit/wrap-around :list) ; wrap with new let list
                   (cz/insert-child (n/spaces col)) ; insert let and bindings backwards
                   (cz/insert-child (n/newlines 1)) ; insert let and bindings backwards
                   (z/insert-child bind-node)
                   (z/insert-child 'let)
                   (edit/join-let))}]))))

(defn clean-ns
  [zloc _uri]
  (let [ns-loc (edit/find-namespace zloc)
        require-loc (z/find-value (zsub/subzip ns-loc) z/next :require)
        col (if require-loc
              (dec (:col (meta (z/node (z/right require-loc)))))
              4)
        sep (n/whitespace-node (apply str (repeat col " ")))
        requires (->> require-loc
                      z/remove
                      z/node
                      n/children
                      (remove n/printable-only?)
                      (sort-by (comp str n/sexpr))
                      (mapcat (fn [node]
                                [(n/newlines 1) sep node]))
                      (cons (n/keyword-node :require)))
        result-loc (z/subedit-> ns-loc
                                (z/find-value z/next :require)
                                (z/up)
                                (z/replace (n/list-node requires)))]
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
  [zloc _uri]
  (let [ns-str-to-add (some-> zloc z/sexpr namespace)
        ns-to-add (some-> ns-str-to-add symbol)
        alias->info (->> (:file-envs @db/db)
                         (mapcat val)
                         (filter (fn [usage]
                                   (or
                                     (set/subset? #{:public :ns} (:tags usage))
                                     (get-in usage [:tags :alias]))))
                         (mapv (fn [{:keys [sym tags] alias-str :str alias-ns :ns :as usage}]
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

(comment
   ; join if let above

  ;; TODO replace bound forms that are being expanded around
   ; join if let above

  (defn extract-def
    [zloc [def-name]]
    (let [def-sexpr (z/sexpr zloc)
          def-node (z/node zloc)
          def-sym (symbol def-name)]
      (-> zloc
          (edit/to-top)
          (edit/mark-position :first-occurrence)
          (edit/replace-all-sexpr def-sexpr def-sym true)
          (edit/find-mark :first-occurrence)
          (zz/insert-left (n/coerce (list 'def def-sym))) ; add declare
          (zz/insert-left (n/newlines 2)) ; add new line after location
          (z/left)
          (zz/append-child (n/newlines 1))
          (z/append-child def-node))))

  (defn add-declaration
    "Adds a declaration for the current symbol above the current top level form"
    [zloc _]
    (let [node (z/sexpr zloc)]
      (if (symbol? node)
        (-> zloc
            (edit/to-top)
            (zz/insert-left (n/coerce (list 'declare node))) ; add declare
            (zz/insert-left (n/newlines 2)) ; add new line after location
            (z/left))
        zloc)))

  (defn cycle-if
    "Cycles between if and if-not form"
    [zloc _]
    (if-let [if-loc (z/find-value zloc z/prev #{'if 'if-not})] ; find first ancestor if
      (-> if-loc
          (z/insert-left (if (= 'if (z/sexpr if-loc)) 'if-not 'if)) ; add inverse if / if-not
          (z/remove) ; remove original if/if-not
          (z/rightmost) ; Go to last child (else form)
          (edit/transpose-with-left)) ; Swap children
      zloc))

  (defn ensure-list
    [zloc]
    (if (z/seq? zloc)
      (z/down zloc)
      (p/wrap-around zloc :list)))

  (defn unwind-thread
    [zloc _]
    (let [oploc (edit/find-op zloc)
          thread-type (z/sexpr oploc)]
      (if (contains? #{'-> '->>} thread-type)
        (let [first-loc (z/right oploc)
              first-node (z/node first-loc)
              move-to-insert-pos (if (= '-> thread-type)
                                   z/leftmost
                                   z/rightmost)]
          (-> first-loc
              (z/right) ; move to form to unwind into
              (edit/remove-left) ; remove threaded form
              (ensure-list) ; make sure we're dealing with a wrapped fn
              (move-to-insert-pos) ; move to pos based on thread type
              (z/insert-right first-node)
              (z/up)
              ((fn [loc]
                 (if (z/rightmost? loc)
                   (p/raise loc)
                   loc)))
              (z/up)))
        zloc)))

  (defn unwind-all
    [zloc _]
    (loop [loc (unwind-thread zloc nil)]
      (let [oploc (edit/find-op loc)
            thread-type (z/sexpr oploc)]
        (if (contains? #{'-> '->>} thread-type)
          (recur (unwind-thread loc nil))
          loc))))

  ;; TODO will insert duplicates
  ;; TODO handle :type and :macro
  (defn add-candidate
    "Add a lib spec to ns form - `missing` is the package or class and `missing-type` is one of `#{:ns :class :type :macro}`"
    [zloc [missing missing-type sym-ns]]
    (-> zloc
        (edit/find-namespace)
        (edit/mark-position :reformat)
        (cond->
         (= missing-type :class)
          (->
           (edit/find-or-create-libspec :import) ; go to import
           (zz/insert-right (n/newlines 1))
           (z/insert-right (symbol missing))) ; add class

          (= missing-type :ns)
          (->
           (edit/find-or-create-libspec :require) ; go to require
           (zz/insert-right (n/newlines 1))
           (z/insert-right [(symbol missing)]) ; add require vec and ns
           (z/right))

          (and sym-ns (= missing-type :ns)) ; if there was a requested ns `str/trim`
          (->
           (z/append-child :as) ; add :as
           (z/append-child (symbol sym-ns)))))) ; as prefix

  (defn replace-ns
    [zloc [new-ns]]
    (-> zloc
        (edit/find-namespace)
        (z/insert-right new-ns)
        (z/remove)
        (edit/find-namespace)))

  (defn cycle-op
    [zloc a-op b-op]
    (if-let [oploc (edit/find-ops-up zloc a-op b-op)]
      (let [thread-type (z/sexpr oploc)]
        (cond
          (= a-op thread-type) (z/replace oploc b-op)
          (= b-op thread-type) (z/replace oploc a-op)
          :else zloc))
      zloc))

  (defn cycle-thread
    [zloc _]
    (cycle-op zloc '-> '->>))

  (defn cycle-privacy
    [zloc _]
    (cycle-op zloc 'defn 'defn-))

  (defn function-from-example
    [zloc _]
    (let [op-loc (edit/find-op zloc)
          example-loc (z/up (edit/find-op zloc))
          child-sexprs (n/child-sexprs (z/node example-loc))
          fn-name (first child-sexprs)
          args (for [[i arg] (map-indexed vector (rest child-sexprs))]
                 (if (symbol? arg)
                   arg
                   (symbol (str "arg" (inc i)))))]
      (-> example-loc
          (edit/to-top)
          (zz/insert-left (n/coerce `(~'defn ~fn-name [~@args]))) ; add declare
          (zz/insert-left (n/newlines 2))))) ; add new line after location

  (defn extract-function
    [zloc [fn-name used-locals]]
    (let [expr-loc (z/up (edit/find-op zloc))
          expr-node (z/node expr-loc)
          expr (z/sexpr expr-loc)
          fn-sym (symbol fn-name)
          used-syms (mapv symbol used-locals)]
      (-> expr-loc
          (z/replace `(~fn-sym ~@used-syms))
          (edit/mark-position :reformat)
          (edit/mark-position :new-cursor)
          (edit/to-top)
          (zz/insert-left (n/coerce (list 'defn fn-sym used-syms)))
          (zz/insert-left (n/newlines 2))
          (z/left)
          (zz/append-child (n/newlines 1))
          (z/append-child expr-node))))

  (defn format-form
    [zloc _]
    (edit/format-form zloc))

  (defn format-all
    [zloc _]
    (edit/format-all zloc)))
