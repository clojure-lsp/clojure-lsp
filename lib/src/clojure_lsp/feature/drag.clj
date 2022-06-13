(ns clojure-lsp.feature.drag
  "Drag clauses forward or backward.

  {:a 1, :b 2} -> {:b 2, :a 1}

  What constitutes a clause is context dependent. In a map, it will be a
  key/value pair. In a vector it will be a single element, unless the vector
  establishes bindings as in `let`, in which case it will be a pair of elements.
  This code tries to be aware of common functions, data structures and other
  forms, and their conventions for establishing clauses.

  Though this feature was originally called 'move collection entry', it is now
  more generally used to drag any clause forward or backward, even if it isn't
  in an immutable collection."
  (:require
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.node.protocols :as n.protocols]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

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

(def ^:private whitespace-or-comment?
  "Treat uneval (#_) as comment. Otherwise the same as z/whitespace-or-comment?"
  (tag-p #{:whitespace :newline :comma :comment :uneval}))

(defn ^:private skip-whitespace [f zloc] (z/skip f whitespace-or-comment? zloc))
(defn ^:private skip-whitespace-right [zloc] (skip-whitespace z/right* zloc))
(defn ^:private skip-whitespace-left [zloc] (skip-whitespace z/left* zloc))
(defn ^:private z-right [zloc] (some-> zloc z/right* skip-whitespace-right))
(defn ^:private z-left [zloc] (some-> zloc z/left* skip-whitespace-left))
(defn ^:private z-down [zloc] (some-> zloc z/down* skip-whitespace-right))
(defn ^:private z-up [zloc] (some-> zloc z/up* skip-whitespace-left))
(defn ^:private z-leftmost? [zloc] (nil? (skip-whitespace-left (z/left* zloc))))

;;;; More helpers

(defn ^:private z-safe-simple-sym [zloc]
  (and zloc
       (n/symbol-node? (z/node zloc))
       (symbol (name (z/sexpr zloc)))))

(defn ^:private z-take-while
  "Returns a sequence of locations in the direction of `f` from `zloc` that
  satisfy `p?`."
  [zloc f p?]
  (->> zloc
       (iterate f)
       (take-while identity)
       (take-while (complement z/end?))
       (take-while p?)))

(defn ^:private count-siblings-left
  "Count the number of sibling nodes to the left of the child node `zloc`."
  [zloc]
  (-> (z-left zloc)
      (z-take-while z-left identity)
      count))

(defn ^:private count-siblings-right
  "Count the number of sibling nodes to the right of the child node `zloc`."
  [zloc]
  (-> (z-right zloc)
      (z-take-while z-right identity)
      count))

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

;;;; Main algorithm for moving a clause

;; 1. Start with a sequence of nodes, the children of the parent expression.
;; 2. Split them up into elements (regular nodes plus affiliated comments) and padding (whitespace) between elements.
;; 3. Based on the breadth of a clause, group elements and padding into clauses, leaving interstitial padding between them.
;; 4. Figure out which element the cursor is on, or if it was on padding, which element is next.
;; 5. Calculate which clause that corresponds to.
;; 6. Swap that clause with the previous or next clause, depending on the direction of movement, maintaining interstitial padding.

;; A map with two map-entries might be broken down like this:
;;  nodes:            ws ws ws  co ws co n co  ws      n co    ws ws ws  co ws co n co  ws      n co    ws
;;  padding+elems:    padding   element        padding element padding   element        padding element padding
;;  p+e-idx:          0 ------- 1 ------------ 2 ----- 3 ----- 4 ------- 5 ------------ 6 ----- 7 ----- 8 ----
;;  e-idx:            0 ---------------------- 1 ------------- 2 ---------------------- 3 -------------
;;  clause-idx:                 0 ----------------------------           1 ----------------------------

(defn ^:private padding+elems
  "Returns the contents of `parent-zloc` as a a sequence of elements and padding
  between them.

  What's an element? It's a sequence of nodes—a regular child of the parent,
  plus any comments that are attached to it, either before or after.

  What's padding? It's the whitespace between elements.

  For example, the following vector is broken up into two elements and three
  pieces of padding:

  [;; comment before a

   ;; another comment before a
   :a ;; comment after a

   ;; comment before b
   :b ;; comment after b
   ]

  [<><;; comment before a

   ;; another comment before a
   :a ;; comment after a><

   ><;; comment before b
   :b ;; comment after b><
   >]

  The first returned element will always be padding (a possibly empty sequence
  of whitespace nodes). That is, padding will be at the even indices and
  elements at the odd indices. There may or may not be a final chunk of
  padding."
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
         idx    0
         result []]
    (cond
      (nil? zloc) ;; rightmost?*
      ;; everything processed
      result

      (= :in-padding state)
      (let [[padding zloc] (z-split-with zloc (tag-p #{:whitespace :newline :comma}))]
        (recur zloc
               :on-elem
               idx
               (conj result (map z/node padding))))

      (= :on-elem state)
      (let [[prefix elem-loc] (z-split-with zloc whitespace-or-comment?)]
        (if-not elem-loc
          ;; We've processed all the elements and this is trailing whitespace.
          (conj result (map z/node prefix))
          (let [;; affiliate elem with (optional) comment following it
                postfix-start (z/right* elem-loc)
                padding-start (->> postfix-start
                                   ;; seek to the first newline, comma or regular node
                                   ;; TODO: handle case where comma precedes comment
                                   ;; `a 1, ;; one comment`
                                   ;; Conceptually, the comma should stay put while
                                   ;; the pair and comment move.
                                   (z/skip z/right* (tag-p #{:whitespace :comment :uneval}))
                                   ;; relinquish that node
                                   z/left*
                                   ;; then give up as much whitespace as possible,
                                   ;; back to the (optional) comment
                                   (z/skip z/left* (tag-p #{:whitespace}))
                                   ;; then forward to the padding
                                   z/right*)
                postfix       (z-take-while postfix-start
                                            z/right*
                                            #(not= padding-start %))]
            (recur padding-start
                   :in-padding
                   (inc idx)
                   (conj result
                         (map z/node (concat prefix [elem-loc] postfix))))))))))

(defn ^:private marked-elem-index
  "Search for index of an element within `elems` that was previously marked.

  Note that this returns the 'elem index', not the seq index. If the cursor is
  on padding between elements, we assume the intention was to drag the next
  element.
  seq:      padding elem padding elem
  seq-idx:  0 ----- 1 -- 2 ----- 3 --
  elem-idx: 0 ---------- 1 ----------"
  [elems]
  (->> elems
       (keep-indexed (fn [idx nodes]
                       (when (some #(edit/node-marked? % ::orig) nodes)
                         (quot idx 2))))
       first))

(defn ^:private can-swap-clauses?
  "In a few cases, the simple [[probable-valid-movement?]] heuristics return the
  wrong result. This happens:
  A) When on whitespace following the first clause, and `drag-backward` is invoked.
  B) When on whitespace preceding the last clause, and `drag-forward` is invoked.
  Though `probable-valid-movement?` thought it saw enough elements before or
  after the zloc, now that we've more carefully allocated the whitespace to a
  clause, we know that isn't true. The problem manifests as the origin-idx or
  dest-idx being out of bounds."
  [origin-elem-idx dest-elem-idx {:keys [breadth pulp rind]}]
  (let [[lower-bound _] rind
        upper-bound (- (+ lower-bound pulp) breadth)]
    (and (<= lower-bound origin-elem-idx upper-bound)
         (<= lower-bound dest-elem-idx upper-bound))))

(defn ^:private loc-of-clause [clause]
  (z/up (z/of-node (n/forms-node clause))))

(defn ^:private clause-range [clause]
  (let [{:keys [row col]} (meta (first clause))
        {:keys [end-row end-col]} (meta (last clause))]
    {:row row :col col
     :end-row end-row :end-col end-col}))

(defn ^:private edited-nodes [before earlier-clause later-clause after]
  (let [final-trailing (concat earlier-clause after)
        trailing-comment-fix (when (some-> final-trailing last n/comment?)
                               (let [orig-leading (concat before earlier-clause)
                                     col (:col (meta (first orig-leading)))]
                                 [(n/newline-node "\n") (n/spaces (dec col))]))]
    ;; The actual swap. Puts the later clause where the earlier was, and
    ;; vice-versa.
    [{:range (clause-range earlier-clause)
      :loc (loc-of-clause later-clause)}
     {:range (clause-range later-clause)
      :loc (loc-of-clause (concat earlier-clause trailing-comment-fix))}]))

(defn ^:private bottom-position
  "Returns the position where the cursor should be placed after the swap in
  order to end up at the top of the (eventual) bottom clause.

  This is calculated by starting at the `top-position`, and adjusting by adding
  the extent of the `intervening-nodes`. The extent is calculated by re-parsing
  the intervening nodes, counting their rows and columns by looking at their
  string representations. This is probably slow, but it avoids needing to use
  {:track-position? true} in the parser. If someday this project uses
  :track-position?, this code probably should be changed to read the revised
  position of the original zloc."
  [top-position intervening-nodes]
  (let [[bottom-row bottom-col]
        (reduce (fn [pos node]
                  (n.protocols/+extent pos (n.protocols/extent node)))
                [(:row top-position) (:col top-position)]
                intervening-nodes)]
    {:row bottom-row
     :col bottom-col}))

(defn ^:private final-position [dir earlier-clause interstitial later-clause]
  (let [top-position (meta (first earlier-clause))
        {:keys [row col]}
        (case dir
          :backward top-position
          :forward (bottom-position top-position (concat later-clause interstitial)))]
    {:row row :col col
     :end-row row :end-col col}))

(defn ^:private drag-clause
  "Drag a clause of `breadth` elements around `zloc` in direction of `dir`
  ignoring elements in `rind`.

  Returns two pieces of data:
  - The edits that swap the clauses, each with the old range and the new loc.
  - The position where the cursor should be placed after the edits."
  [zloc dir {:keys [breadth rind] :as clause-spec}]
  (let [zloc (edit/mark-position zloc ::orig)
        parent-zloc (z-up zloc)

        elems (padding+elems parent-zloc)

        [ignore-left _] rind
        origin-elem-idx (marked-elem-index elems)
        ;; move back to first element in clause
        origin-elem-idx (- origin-elem-idx
                           (mod (- origin-elem-idx ignore-left) breadth))
        dest-elem-idx ((case dir :backward -, :forward +) origin-elem-idx breadth)]
    (when (can-swap-clauses? origin-elem-idx dest-elem-idx clause-spec)
      (let [earlier-elem-idx  (min origin-elem-idx dest-elem-idx)
            ;; skip padding and elems, including first padding, to get to first element of first clause
            clause-offset (+ 1 (* 2 earlier-elem-idx))
            ;; a clause made up of two elements includes one piece of padding between them
            clause-size (+ breadth (dec breadth))

            [before rst]         (split-at clause-offset elems)
            [earlier-clause rst] (split-at clause-size rst)
            [interstitial rst]   (split-at 1 rst) ;; padding
            [later-clause after] (split-at clause-size rst)

            ;; squash padding and elems into clauses (keeping interstitial padding)
            [before earlier-clause interstitial later-clause after]
            (map flatten [before earlier-clause interstitial later-clause after])]
        [(edited-nodes before earlier-clause later-clause after)
         (final-position dir earlier-clause interstitial later-clause)]))))

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
  [vector-zloc uri {:keys [analysis]}]
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
                  analysis (filter (enclosed-by? (z/node vector-zloc))
                                   (get analysis (shared/uri->filename uri)))]
              (->> child-nodes
                   (partition 2)
                   (every? (fn [[lvar rvar]]
                             (let [lelems (filter (enclosed-by? lvar) analysis)
                                   relems (filter (enclosed-by? rvar) analysis)
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

(defn ^:private clause-spec
  "Returns a clause spec for the `parent-zloc`."
  [parent-zloc uri db]
  (when parent-zloc
    (let [child-count (count-children parent-zloc)
          spec (case (z/tag parent-zloc)
                 :map          {:breadth 2, :rind no-rind}
                 (:set :forms) {:breadth 1, :rind no-rind}
                 :vector       (vector-clause-spec parent-zloc uri db)
                 (:list :fn)   (list-clause-spec parent-zloc child-count)
                 nil)]
      (some-> spec
              (assoc :pulp (pulp (:rind spec) child-count))))))

;;;; Plan
;; Form a plan about how to drag

(defn ^:private probable-valid-movement?
  "Checks whether `zloc` can be dragged in direction `dir`, assuming it is part of
  a clause as described by `clause-spec`.

  This isn't a perfect test. May return true even when the zloc cannot actually
  be dragged. See [[can-swap-clauses?]]."
  [zloc dir {:keys [breadth pulp rind] :as clause-spec}]
  (and clause-spec
       ;; Can the expression be split into clauses?
       (zero? (mod pulp breadth))
       (let [[ignore-left ignore-right] rind
             movable-before (-> zloc count-siblings-left (- ignore-left))
             movable-after (-> zloc count-siblings-right (- ignore-right))]
         (and
           ;; Are we in the pulp?
           (<= 0 (case dir :backward movable-after, :forward movable-before))
           ;; Is there another clause to swap with?
           ;; Can be erroneously true if we are on a comment or whitespace that
           ;; will eventually be allocated to padding or another element.
           (<= breadth (case dir :backward movable-before, :forward movable-after))))))

(defn ^:private target-locs [zloc]
  ;; [v 'q :kw] is parsed as
  ;; [:vector [:token v] [:quote [:token q]] [:keyword :kw]]
  ;; If the cursor is between the quote and `q`, we end up on the :token node
  ;; inside the :quote. As the sole child it isn't draggable. In this and
  ;; similar cases, we move up one node, to the :quote, and drag it within the
  ;; :vector.
  (let [parent-zloc (z-up zloc)]
    (if (and (n/inner? (z/node parent-zloc))
             (not (contains? #{:map :set :vector :forms :list :fn} (z/tag parent-zloc))))
      [parent-zloc (z-up parent-zloc)]
      [zloc parent-zloc])))

(defn ^:private plan [zloc dir uri db]
  (when zloc
    (let [[zloc parent-zloc] (target-locs zloc)
          clause-spec (clause-spec parent-zloc uri db)]
      (when (probable-valid-movement? zloc dir clause-spec)
        [zloc clause-spec]))))

;;;; Public API

;; Drag zloc's clause forward or backward.

(defn ^:private can-drag? [zloc dir uri db] (boolean (plan zloc dir uri db)))
(defn can-drag-backward? [zloc uri db] (can-drag? zloc :backward uri db))
(defn can-drag-forward? [zloc uri db] (can-drag? zloc :forward uri db))

(defn ^:private drag [zloc dir uri db]
  (when-let [[zloc clause-spec] (plan zloc dir uri db)]
    (when-let [[edits cursor-position] (drag-clause zloc dir clause-spec)]
      {:show-document-after-edit {:uri         uri
                                  :take-focus? true
                                  :range       cursor-position}
       :changes-by-uri {uri edits}})))

(defn drag-backward [zloc uri db] (drag zloc :backward uri db))
(defn drag-forward [zloc uri db] (drag zloc :forward uri db))
