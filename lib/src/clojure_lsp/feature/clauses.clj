(ns clojure-lsp.feature.clauses
  (:require
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn ^:private tag-p [tag-set]
  (comp tag-set z/tag))

;;;; Redefine core helpers to treat uneval as comment. See
;;;; https://github.com/clj-commons/rewrite-clj/issues/70.

;; rewrite-clj calls the #_ reader macro and its contents an "uneval" node. We
;; duplicate several zipper movement operators, changing them to treat uneval
;; nodes as comments. This avoids errors related to counting nodes or
;; affiliating them with their whitespace.

;; TODO: Periodically audit this namespace for mistaken uses of z/right, etc.,
;; or for the introduction of new core helpers that depend on
;; z/whitespace-or-comment? :/

(def whitespace-or-comment-tags
  #{:whitespace :newline :comma :comment :uneval})

(def ^:private whitespace-or-comment?
  "Treat uneval (#_) as comment. Otherwise the same as z/whitespace-or-comment?"
  (tag-p whitespace-or-comment-tags))

(defn ^:private skip-whitespace [f zloc] (z/skip f whitespace-or-comment? zloc))
(defn ^:private skip-whitespace-right [zloc] (skip-whitespace z/right* zloc))
(defn ^:private skip-whitespace-left [zloc] (skip-whitespace z/left* zloc))
(defn z-right [zloc] (some-> zloc z/right* skip-whitespace-right))
(defn z-left [zloc] (some-> zloc z/left* skip-whitespace-left))
(defn ^:private z-down [zloc] (some-> zloc z/down* skip-whitespace-right))
(defn ^:private z-up [zloc] (some-> zloc z/up* skip-whitespace-left))
(defn ^:private z-leftmost? [zloc] (nil? (skip-whitespace-left (z/left* zloc))))

;;;; More helpers

(defn ^:private z-safe-simple-sym [zloc]
  (and zloc
       (n/symbol-node? (z/node zloc))
       (symbol (name (z/sexpr zloc)))))

(defn z-take-while
  "Returns a sequence of locations in the direction of `f` from `zloc` that
  satisfy `p?`."
  [zloc f p?]
  (->> zloc
       (iterate f)
       (take-while identity)
       (take-while (complement z/end?))
       (take-while p?)))

(defn ^:private newline-comment? [n]
  (and (n/comment? n)
       (string/ends-with? (:s n) "\n")))

(defn ^:private z-split-with
  "Like clojure.core/split-with, but for a clj-rewrite zipper. Returns two
  items, a sequence of locations to the z/right* of `zloc` that satisfy `p?`,
  and the first location that doesn't."
  [zloc p?]
  (loop [zloc zloc
         satisfies []]
    (if (p? zloc)
      (recur (z/right* zloc) (conj satisfies zloc))
      [satisfies zloc])))

;;;; Main algorithm

;;;; Identify clauses

;; 1. Start with a sequence of nodes, the children of the parent expression.
;; 2. Split them up into elements (regular nodes plus affiliated comments) and
;;    padding (whitespace) between elements.
;; 3. Separate the rind (the elements and padding that can't be dragged) from
;;    the pulp (the ones that can).
;; 4. Based on the breadth of a clause, group the pulp into clauses, leaving
;;    interstitial padding between clauses.

(defn ^:private divide-parent
  "Returns the contents of `parent-zloc` split up into elements and padding.

  That is, the return value will be a sequence of sequences of nodes, each
  corresponding to an element or to padding.

  For convenience in the rest of the algorithm, the first item will always be
  padding. If there isn't any whitespace before the first element, this will be
  an empty sequence. There may or may not be a final item of padding."
  [parent-zloc]
  (loop [zloc (-> parent-zloc
                  (z/edit* (fn [parent-node]
                             (n/replace-children
                               parent-node
                               (mapcat (fn [node]
                                         (if (newline-comment? node)
                                           (let [{:keys [row col end-row end-col]} (meta node)
                                                 len (count (:s node))]
                                             (assert (= (inc row) end-row) "unexpected multiline comment")
                                             [(with-meta
                                                (update node :s subs 0 (dec len))
                                                {:row row :col col
                                                 :end-row row :end-col (+ col len)})
                                              (with-meta
                                                (n/newline-node "\n")
                                                {:row row, :col (+ col len),
                                                 :end-row end-row, :end-col end-col})])
                                           [node]))
                                       (n/children parent-node)))))
                  z/down*)

         state  :in-padding
         result []]
    (cond
      (nil? zloc) ;; rightmost?*
      ;; everything processed
      result

      (= :in-padding state)
      (let [[padding zloc] (z-split-with zloc (tag-p #{:whitespace :newline :comma}))]
        (recur zloc
               :on-elem
               (conj result (map z/node padding))))

      (= :on-elem state)
      (let [[prefix elem-loc] (z-split-with zloc whitespace-or-comment?)]
        (if-not elem-loc
          ;; We've processed all the elements and this is trailing whitespace.
          (conj result (map z/node prefix))
          (let [;; affiliate elem with (optional) comment following it
                postfix-start (z/right* elem-loc)
                ;; TODO: handle case where comma precedes comment `a 1, ;; one
                ;; comment`. Conceptually, the comma should stay put while the
                ;; pair and comment move.

                ;; seek to the first newline, comma or regular node
                dividing-start  (z/skip z/right* (tag-p #{:whitespace :comment :uneval}) postfix-start)
                padding-start (if dividing-start
                                (->> dividing-start
                                     ;; relinquish that node
                                     z/left*
                                     ;; then give up as much whitespace as possible,
                                     ;; back to the (optional) comment
                                     (z/skip z/left* (tag-p #{:whitespace}))
                                     ;; then forward to the padding
                                     z/right*)
                                postfix-start)
                postfix       (z-take-while postfix-start
                                            z/right*
                                            #(not= padding-start %))]
            (recur padding-start
                   :in-padding
                   (conj result
                         (map z/node (concat prefix [elem-loc] postfix))))))))))

(defn start-point [node]
  (let [{:keys [row col]} (meta node)]
    [row col]))

(defn end-point [node]
  (let [{:keys [end-row end-col]} (meta node)]
    [end-row end-col]))

(defn offset [[first-row first-col] [second-row second-col]]
  ;; See n.protocols/extent
  (let [rows (- second-row first-row)]
    [rows
     (if (zero? rows)
       (- second-col first-col)
       second-col)]))

(defn nodes-offset [nodes]
  (offset (start-point (first nodes)) (end-point (last nodes))))

(defn identify
  "Identifies the clauses described by the clause-spec."
  [{:keys [zloc breadth rind pulp]}]
  (let [zloc (edit/mark-position zloc ::orig)
        parent-zloc (z-up zloc)

        elems+padding (divide-parent parent-zloc)

        [ignore-left _] rind
        [rind-before rst] (split-at (inc (* 2 ignore-left)) elems+padding)
        [pulp-elems+padding rind-after] (split-at (dec (* 2 pulp)) rst)

        clause-width (dec (* 2 breadth))
        clauses+padding
        (->> pulp-elems+padding
             (partition-all (inc clause-width)) ;; clause and, if present, one piece of padding after it
             (map-indexed vector)
             (mapcat (fn [[clause-idx clause-elems+padding]]
                       (let [[clause-nodes padding-nodes]
                             (->> clause-elems+padding
                                  (split-at clause-width)
                                  (map flatten))
                             origin? (some #(edit/node-marked? % ::orig) clause-nodes)]
                         [;; clause
                          {:idx clause-idx
                           :nodes clause-nodes
                           :origin? origin?}
                          ;; padding
                          {:nodes padding-nodes}]))))
        origin-clause (->> clauses+padding (filter :origin?) first)]
    {:rind-before {:nodes (flatten rind-before)}
     :rind-after {:nodes (flatten rind-after)}
     :clauses+padding clauses+padding
     :clause-count (quot pulp breadth)
     :origin-clause origin-clause}))

(defn loc-of-nodes [clause-nodes]
  (z/up (z/of-node (n/forms-node clause-nodes))))

(defn nodes-range [clause-nodes]
  (let [{:keys [row col]} (meta (first clause-nodes))
        {:keys [end-row end-col]} (meta (last clause-nodes))]
    {:row row :col col
     :end-row end-row :end-col end-col}))

;;;; Clause specs
;;
;; Given an expression, describe what constitutes a clause within that
;; expression.
;;
;; For example, for the expression `(case x :foo 1 :bar 2)` the clause spec
;; would be:
;;
;; {:rind    [2 0]
;;  :pulp    4
;;  :breadth 2}
;;
;; This says that in this expresion:
;; * `:rind [2 0]` - two elements at the beginning cannot be dragged: `case x`
;; * `:pulp 4` - four elements in the interior can be dragged: `:foo 1 :bar 2`
;; * `:breadth 2` - pairs of elements should be dragged together as clauses: `:foo 1` and `:bar 2`
;;
;; If a clause spec is nil, movement within the expression is not permitted.

(def ^:private no-rind [0 0])

(def ^:private common-binding-syms
  "Symbols that are typically used to define bindings."
  ;; We aren't concerned with macros like `if-let` that establish one binding
  ;; only. They can be treated like regular breadth-1 vectors.
  '#{binding doseq for let loop with-local-vars with-open with-redefs})

(defn ^:private establishes-bindings?
  "Returns whether a vector node `vector-zloc` establishes bindings, such as in
  `clojure.core/let`.

  This uses a few heurisitics.

  First, there are several clojure.core functions and macros which establish
  bindings, such as `for`, `let`, `loop` and `binding`. It returns true if
  `zloc` is in one of these expressions (even if the expression is actually
  imported from some namespace besides clojure.core TODO: Is it reasonable to
  assume that any function with one of these names establishes bindings, even if
  it isn't in clojure.core?).

  Otherwise, if the `zloc`'s children are pairs of local variable definitions
  and values (according to the clj-kondo analysis), then this also returns true.
  This allows library- and user-defined code to declare that it establishes
  bindings, via `:lint-as`. Originally this was the only heuristic, since it
  works for `let` and `for`. But it fails for `bindings` and other forms that
  don't establish new variables, but merely reference existing variables,
  according to the clj-kondo analysis."
  [vector-zloc uri db]
  (boolean
    (or (when-let [op-zloc (z-left vector-zloc)]
          (and (z-leftmost? op-zloc)
               (contains? common-binding-syms (z-safe-simple-sym op-zloc))))
        (let [child-nodes (filter n/sexpr-able? (n/children (z/node vector-zloc)))]
          (when (even? (count child-nodes))
            (let [enclosed-by? (fn [node]
                                 (let [{:keys [row col end-row end-col]} (meta node)
                                       scope {:name-row     row
                                              :name-col     col
                                              :name-end-row end-row
                                              :name-end-col end-col}]
                                   #(shared/inside? % scope)))
                  elements (->> (get-in db [:analysis (shared/uri->filename uri)])
                                (mapcat val)
                                (filter (enclosed-by? (z/node vector-zloc))))]
              (->> child-nodes
                   (partition 2)
                   (every? (fn [[lvar rvar]]
                             (let [lelems (filter (enclosed-by? lvar) elements)
                                   relems (filter (enclosed-by? rvar) elements)
                                   local? #(identical? :locals (:bucket %))]
                               (and (some local? lelems)
                                    (not-any? local? relems))))))))))))

(defn ^:private vector-clause-spec
  "Returns a partial clause spec for a vector node, `vector-zloc`."
  [vector-zloc uri db]
  {:breadth (if (establishes-bindings? vector-zloc uri db) 2 1)
   :rind no-rind})

(defn ^:private in-threading? [parent-zloc]
  (when-let [up-op-loc (some-> parent-zloc z-up z-down)]
    (contains? '#{-> cond-> some->} (z-safe-simple-sym up-op-loc))))

(defn ^:private count-children [zloc]
  (let [node (z/node zloc)]
    (if (n/inner? node)
      (->> node n/children (filter n/sexpr-able?) count)
      0)))

(defn ^:private list-clause-spec
  "Returns a partial clause spec for a list node, `list-zloc`. In a regular list
  a clause is just one element. Some lists are function calls, and functions
  like case and cond have clauses of arguments that should be dragged together. If
  the list node is such a function call, returns the appropriate clause
  description for it. Returns nil in one special case when we know we're in an
  invalid condp."
  [list-zloc child-count]
  ;; case and condp permit final default expression, which should not be dragged.
  ;; assoc, assoc!, cond->, cond->>, and case sometimes appear inside other threading expressions.
  ;; condp has a variation with ternary expressions.
  (case (some-> list-zloc z-down z-safe-simple-sym)
    cond
    #_=> {:breadth 2, :rind [1 0]}
    (cond-> cond->> assoc assoc!)
    #_=> {:breadth 2, :rind [(if (in-threading? list-zloc) 1 2) 0]}
    case
    #_=> {:breadth 2, :rind (if (in-threading? list-zloc)
                              [1 (if (odd? child-count) 0 1)]
                              [2 (if (even? child-count) 0 1)])}
    condp
    #_=> (let [breadth (if (z/find-next-value (z-down list-zloc) z-right :>>) 3 2)
               ignore-left 3
               ignore-right (mod (- child-count ignore-left) breadth)
               invalid-ternary? (= 2 ignore-right)]
           (when-not invalid-ternary?
             {:breadth breadth, :rind [ignore-left ignore-right]}))
    are
    #_=> (let [param-count (-> list-zloc z-down z-right count-children)]
           (when (< 0 param-count)
             {:breadth param-count, :rind [3 0]}))
    {:breadth 1, :rind no-rind}))

(defn ^:private pulp [[ignore-left ignore-right] child-count]
  (- child-count ignore-left ignore-right))

(defn ^:private target-locs [zloc]
  ;; [v 'q :kw] is parsed as
  ;; [:vector [:token v] [:quote [:token q]] [:keyword :kw]]
  ;; If the cursor is between the quote and `q`, we end up on the :token node
  ;; inside the :quote. As the sole child it isn't draggable. In this and
  ;; similar cases, we move up one node, to the :quote, and drag it within the
  ;; :vector.
  (let [parent-zloc (z/up zloc)] ;; intentionally not z-up, to avoid NPE when on an uneval with no earlier siblings
    (if (and (n/inner? (z/node parent-zloc))
             (not (contains? #{:map :set :vector :forms :list :fn} (z/tag parent-zloc))))
      [parent-zloc (z-up parent-zloc)]
      [zloc parent-zloc])))

(defn clause-spec
  "Returns a clause spec for the `zloc`."
  [zloc uri db]
  (when zloc
    (let [[zloc parent-zloc] (target-locs zloc)
          child-count (count-children parent-zloc)]
      (when-let [{:keys [breadth rind], :as spec}
                 (case (z/tag parent-zloc)
                   :map          {:breadth 2, :rind no-rind}
                   (:set :forms) {:breadth 1, :rind no-rind}
                   :vector       (vector-clause-spec parent-zloc uri db)
                   (:list :fn)   (list-clause-spec parent-zloc child-count)
                   nil)]
        (let [pulp (pulp rind child-count)]
          (when (zero? (mod pulp breadth)) ;; Can the expression be split into clauses?
            (assoc spec
                   :pulp pulp
                   :zloc zloc)))))))
