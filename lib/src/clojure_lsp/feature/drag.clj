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
   [clojure-lsp.queries :as q]
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
  [(z-take-while zloc z/right* p?)
   (z/skip z/right* p? zloc)])

(defn ^:private z-cursor-position [zloc]
  (meta (z/node zloc)))

;;;; Main algorithm for moving a clause

(defn ^:private seq-elems
  "Returns the contents of `parent-zloc` as a a sequence of elements and padding
  between them.

  What is an element? It's a child of the parent, plus any comments that are
  associated with that child. For example, in the following vector there are two
  elements:

  [;; comment before a
   :a ;; comment after a

   ;; comment before b
   :b ;; comment after b
  ]

  :a and its comments are one element, as are :b and its comments. The
  whitespace between them is considered padding. To be precise, the padding
  includes all whitespace, including newlines, between the :a and :b elements.
  That is, the padding starts at the newline character following ';; comment
  after a' and continues to the space character preceding ';; comment before b'.
  In this case, there is also padding after the :b element: a newline and some
  more whitespace.

  The format of a returned elem is:
  {:type :elem
   :idx  <index of the element within the sequence>
   :locs <sequence of zipper locations whose nodes make up the element>}

  The format of a returned padding is:
  {:type :padding
   :idx  <index of the padding within the sequence>
   :locs <sequence of zipper locations whose nodes make up the padding>}

  The returned elems will always be interposed with padding, with padding at
  the beginning and possibly at the end, even if the padding contains no locs.
  The first padding's and first element's idx will be 0, increasing in step from
  there.

  The children of the original `parent-zloc` could be reconstructed by
  concatenating all the `:locs` together."
  [parent-zloc]
  (loop [zloc (-> parent-zloc
                  (z/edit* (fn [parent-node]
                             (n/replace-children parent-node
                                                 (mapcat (fn [node]
                                                           (if (newline-comment? node)
                                                             [(update node :s subs 0 (dec (count (:s node))))
                                                              (n/newline-node "\n")]
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
               (conj result {:type :padding
                             :idx  idx
                             :locs padding})))

      (= :on-elem state)
      (let [[prefix elem-loc] (z-split-with zloc whitespace-or-comment?)]
        (if-not elem-loc
          ;; We've processed all the elements and this is trailing whitespace.
          (conj result {:type :padding
                        :idx  idx
                        :locs prefix})
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
                         {:type :elem
                          :idx  idx
                          :locs (concat prefix [elem-loc] postfix)}))))))))

(defn ^:private elem-index-by-cursor-position
  "Search for an element within `elems` that was at `cursor-position`."
  [cursor-position elems]
  (->> elems
       (filter (fn [{:keys [locs]}]
                 ;; We compare cursor position, not full loc, because comments
                 ;; have been separated from their newlines by `seq-elems`.
                 (some (comp #{cursor-position} z-cursor-position)
                       locs)))
       first
       ;; If the cursor is on padding between elements, we assume the intention
       ;; was to drag the next element. This padding will have the correct idx.
       :idx))

(defn ^:private split-elems-at-idx [elems split-idx]
  (split-with (fn [{:keys [type idx]}]
                (or (= :padding type)
                    (< idx split-idx)))
              elems))

(defn ^:private edit-parent
  "Put revised elements and padding back into parent."
  [parent-zloc swapped]
  (z/replace parent-zloc
             (n/replace-children (z/node parent-zloc)
                                 (->> swapped
                                      (mapcat :locs)
                                      (map z/node)))))

(defn ^:private fix-trailing-comment
  "After reordering, the last child may now be a comment. We need to add a
  newline because otherwise the closing bracket will be commented out. We align
  the closing bracket indented one space from the opening bracket."
  [parent-zloc]
  (let [last-zloc (-> parent-zloc z-down z/rightmost*)]
    (if (-> last-zloc z/node n/comment?)
      (let [col (-> parent-zloc
                    z-cursor-position
                    :col)]
        (-> last-zloc
            (z/insert-space-right col) ;; uses z/insert-right, so automatically adds one extra space
            z/insert-newline-right
            z-up))
      parent-zloc)))

(defn ^:private can-swap-clauses?
  "In a few cases, the simple [[probable-valid-movement?]] heuristics return the
  wrong result. This happens:
  A) When on whitespace following the first clause, and `drag-backward` is invoked.
  B) When on whitespace preceding the last clause, and `drag-forward` is invoked.
  Though `probable-valid-movement?` thought it saw enough elements before or
  after the zloc, now that we've more carefully allocated the whitespace to a
  clause, we know that isn't true. The problem manifests as the origin-idx or
  dest-idx being out of bounds."
  [origin-idx dest-idx {:keys [breadth pulp rind]}]
  (let [[lower-bound _] rind
        upper-bound (- (+ lower-bound pulp) breadth)]
    (and (<= lower-bound origin-idx upper-bound)
         (<= lower-bound dest-idx upper-bound))))

(defn ^:private bottom-position
  "Returns the position where the cursor should be placed after the swap in
  order to end up at the top of the (eventual) bottom clause.

  This is calculated by starting at the `top-position`, and adjusting by adding
  the extent of the `intervening-elems`. The extent is calculated by re-parsing
  the intervening locs, counting their rows and columns by looking at their
  string representations. This is probably slow, but it avoids needing to use
  {:track-position? true} in the parser. If someday this project uses
  :track-position?, this code probably should be changed to read the revised
  position of the original zloc."
  [top-position intervening-elems]
  (let [[bottom-row bottom-col]
        (->> intervening-elems
             (mapcat :locs)
             (reduce (fn [pos zloc]
                       (n.protocols/+extent pos (n.protocols/extent (z/node zloc))))
                     [(:row top-position) (:col top-position)]))]
    {:row bottom-row
     :col bottom-col}))

(defn ^:private final-position [dir earlier-clause interstitial later-clause]
  (let [top-position (z-cursor-position (first (:locs (first earlier-clause))))]
    (case dir
      :backward top-position
      :forward (bottom-position top-position (concat later-clause interstitial)))))

(defn ^:private drag-clause
  "Drag a clause of `breadth` elements around `zloc` in direction of `dir`
  ignoring elements in `rind`.

  Returns two pieces of data. The first is the edited parent expression with
  the clauses swapped, whose string representation should be sent to the editor.
  The second is the position where the cursor should be placed after the edit."
  [zloc dir {:keys [breadth rind] :as clause-spec}]
  (let [[ignore-left _] rind
        parent-zloc (z-up zloc)

        elems (seq-elems parent-zloc)

        origin-idx (elem-index-by-cursor-position (z-cursor-position zloc) elems)
        ;; move back to first element in clause
        origin-idx (- origin-idx
                      (mod (- origin-idx ignore-left) breadth))
        dest-idx (->> origin-idx
                      (iterate (case dir :backward dec, :forward inc))
                      (drop breadth)
                      first)]
    (when (can-swap-clauses? origin-idx dest-idx clause-spec)
      (let [earlier-idx  (min origin-idx dest-idx)
            [before rst] (split-elems-at-idx elems earlier-idx)

            ;; the clause is the elements _and_ intervening padding that are
            ;; moving together
            clause-size          (+ breadth (dec breadth))
            [earlier-clause rst] (split-at clause-size rst)
            [interstitial rst]   (split-at 1 rst) ;; padding
            [later-clause rst]   (split-at clause-size rst)

            swapped     (concat before
                                later-clause
                                interstitial
                                earlier-clause
                                rst)
            parent-zloc (-> parent-zloc
                            (edit-parent swapped)
                            (fix-trailing-comment))]
        [parent-zloc
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

  Otherwise, if the first child of `zloc` defines a local variable (according to
  the clj-kondo analysis), then this also returns true. This allows library- and
  user-defined code to declare that it establishes bindings, via `:lint-as`.
  Originally this was the only heuristic, since it works for `let` and `for`.
  But it fails for `bindings` and other forms where the clj-kondo analysis
  reports that it merely references existing variables, rather than establishing
  definitions."
  [vector-zloc uri {:keys [analysis]}]
  (boolean
    (or (when-let [outer-zloc (z-left vector-zloc)]
          (and (z-leftmost? outer-zloc)
               (contains? common-binding-syms (z/sexpr outer-zloc))))
        (let [z-pos   (z-cursor-position (z-down vector-zloc))
              z-scope {:name-row     (:row z-pos)
                       :name-col     (:col z-pos)
                       :name-end-row (:end-row z-pos)
                       :name-end-col (:end-col z-pos)}]
          (q/find-first (fn [element]
                          (and (= :locals (:bucket element))
                               (shared/inside? element z-scope)))
                        (get analysis (shared/uri->filename uri)))))))

(defn ^:private vector-clause-spec
  "Returns a partial clause spec for a vector node, `vector-zloc`."
  [vector-zloc uri db]
  {:breadth (if (establishes-bindings? vector-zloc uri db) 2 1)
   :rind no-rind})

(defn ^:private in-threading? [parent-zloc]
  (when-let [up-op-loc (some-> parent-zloc z-up z-down)]
    (when (z/sexpr-able? up-op-loc)
      (contains? '#{-> cond-> some->} (z/sexpr up-op-loc)))))

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
  (case (some-> list-zloc z-down z/sexpr)
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

;;;; Public API

;; Drag zloc's clause forward or backward.

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

(defn ^:private can-drag? [zloc dir uri db]
  (probable-valid-movement? zloc dir (clause-spec (z-up zloc) uri db)))

(defn can-drag-backward? [zloc uri db] (can-drag? zloc :backward uri db))
(defn can-drag-forward? [zloc uri db] (can-drag? zloc :forward uri db))

(defn ^:private drag [zloc dir uri db]
  (let [clause-spec (clause-spec (z-up zloc) uri db)]
    (when (probable-valid-movement? zloc dir clause-spec)
      (when-let [[parent-zloc position] (drag-clause zloc dir clause-spec)]
        {:show-document-after-edit {:uri         uri
                                    :take-focus? true
                                    :range       (assoc position :end-row (:row position) :end-col (:col position))}
         :changes-by-uri {uri
                          [{:range (z-cursor-position parent-zloc)
                            :loc   parent-zloc}]}}))))

(defn drag-backward [zloc uri db] (drag zloc :backward uri db))
(defn drag-forward [zloc uri db] (drag zloc :forward uri db))
