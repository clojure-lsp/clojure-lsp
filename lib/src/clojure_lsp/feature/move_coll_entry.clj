(ns clojure-lsp.feature.move-coll-entry
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

;; clj-rewrite calls the #_ reader macro and its contents an "uneval" node. We
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

(defn ^:private seq-elems
  "Returns the contents of `seq-zloc` as a a sequence of elements and padding
  between them.

  What is an element? It's an item of the parent, plus any comments that are
  associated with that item. For example, in the following vector there are two
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
   :idx  <index of the element within seq-zloc>
   :locs <sequence of zipper locations whose nodes make up the element>}

  The format of a returned padding is:
  {:type :padding
   :idx  <index of the padding within seq-zloc>
   :locs <sequence of zipper locations whose nodes make up the padding>}

  The returned values will always be interposed with padding, with padding at
  the beginning and possibly at the end, even if the padding contains no locs.
  The first padding's and first element's idx will be 0, increasing in step from
  there.

  The children of the original `seq-zloc` could be reconstructed by
  concatenating all the `:locs` together."
  [seq-zloc]
  (loop [zloc (-> seq-zloc
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

(defn ^:private z-cursor-position [zloc]
  (meta (z/node zloc)))

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
       ;; was to move the next element. This padding will have the correct idx.
       :idx))

(defn ^:private split-elems-at-idx [elems split-idx]
  (split-with (fn [{:keys [type idx]}]
                (or (= :padding type)
                    (< idx split-idx)))
              elems))

(defn ^:private edit-collection
  "Put revised entries back into parent."
  [parent-zloc swapped]
  (z/replace parent-zloc
             (n/replace-children (z/node parent-zloc)
                                 (->> swapped
                                      (mapcat :locs)
                                      (map z/node)))))

(defn ^:private fix-trailing-comment
  "After reordering, the last component may now be a comment. We need to add a
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

(defn ^:private can-swap?
  "In a few cases, the simple [[valid-strat?]] heuristics return the wrong results.
  This happens:
  A) When on whitespace following the first group, and `move-up` is invoked.
  B) When on whitespace before the last group, and `move-down` is invoked.
  `valid-strat?` thought it saw enough elements before or after the zloc to
  permit movement, but now that we've more carefully allocated the whitespace to
  a group, we know that isn't true. The problem manifests as the origin-idx or
  dest-idx being out of bounds."
  [origin-idx dest-idx {:keys [breadth pulp rind]}]
  (let [[lower-bound _] rind
        upper-bound (- (+ lower-bound pulp) breadth)]
    (and (<= lower-bound origin-idx upper-bound)
         (<= lower-bound dest-idx upper-bound))))

(defn ^:private move-group-by-strategy
  "Move a group of `breadth` elements around `zloc` in direction of `dir`
  ignoring elements in `rind`.

  Returns three pieces of data. The first is the edited parent expression with
  the groups swapped, whose string representation should be sent to the editor.
  The second and third are used for positioning the cursor: a loc whose position
  marks the top of the top group; and a series of locs, which sit between the
  tops of the top and bottom groups. Their combined extent can be used to
  calculate the top of the bottom group; see [[final-bottom-cursor]]."
  [zloc {:keys [breadth dir rind] :as strategy}]
  (let [[ignore-left _] rind
        parent-zloc (z-up zloc)

        elems (seq-elems parent-zloc)

        origin-idx (elem-index-by-cursor-position (z-cursor-position zloc) elems)
        ;; move back to first element in group
        origin-idx (- origin-idx
                      (mod (- origin-idx ignore-left) breadth))
        dest-idx (->> origin-idx
                      (iterate (case dir :up dec, :down inc))
                      (drop breadth)
                      first)]
    (when (can-swap? origin-idx dest-idx strategy)
      (let [earlier-idx  (min origin-idx dest-idx)
            [before rst] (split-elems-at-idx elems earlier-idx)

            ;; the group is the elements _and_ intervening padding that are
            ;; moving together
            group-size          (+ breadth (dec breadth))
            [earlier-group rst] (split-at group-size rst)
            [interstitial rst]  (split-at 1 rst) ;; padding
            [later-group rst]   (split-at group-size rst)

            swapped     (concat before
                                later-group
                                interstitial
                                earlier-group
                                rst)
            parent-zloc (-> parent-zloc
                            (edit-collection swapped)
                            (fix-trailing-comment))]
        [parent-zloc
         (first (:locs (first earlier-group)))
         (mapcat :locs (concat later-group interstitial))]))))

(def ^:private no-rind [0 0])

(def ^:private common-binding-syms
  "Symbols that are typically used to define bindings."
  ;; We aren't concerned with macros like `if-let` that establish one binding
  ;; only. They can be treated like regular breadth-1 vectors.
  '#{binding doseq for let loop with-local-vars with-open with-redefs})

(defn ^:private establishes-bindings?
  "Returns whether `zloc` (which should be a vector node) establishes bindings,
  such as in `clojure.core/let`.

  This uses a few heurisitics.

  First, there are several clojure.core functions and macros which establish
  bindings, such as `for`, `let`, `loop` and `binding`. It returns true if
  `zloc` is in one of these expressions (even if the expression is actually
  imported from some namespace besides clojure.core (TODO is it reasonable to
  assume that any function with one of these names establishes bindings, even if
  it isn't in clojure.core?)).

  Otherwise, if the first child of `zloc` defines a local variable (according to
  the clj-kondo diagnostics), then this also returns true. This allows library-
  and user-defined code to declare that it establishes bindings, via `:lint-as`.
  Originally this was the only heuristic, since it works for `let` and `for`.
  But it fails for `bindings` and other forms where the clj-kondo analysis
  reports that it merely references existing variables, rather than establishing
  definitions."
  [zloc uri {:keys [analysis]}]
  (boolean
    (or (when-let [outer-zloc (z-left zloc)]
          (and (z-leftmost? outer-zloc)
               (contains? common-binding-syms (z/sexpr outer-zloc))))
        (let [z-pos   (z-cursor-position (z-down zloc))
              z-scope {:name-row     (:row z-pos)
                       :name-col     (:col z-pos)
                       :name-end-row (:end-row z-pos)
                       :name-end-col (:end-col z-pos)}]
          (q/find-first (fn [element]
                          (and (= :locals (:bucket element))
                               (shared/inside? element z-scope)))
                        (get analysis (shared/uri->filename uri)))))))

(defn ^:private vector-strategy
  "Returns the movement strategy for `parent-zloc`, which should be a vector
  node."
  [parent-zloc uri db]
  {:breadth (if (establishes-bindings? parent-zloc uri db) 2 1)
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

(defn ^:private list-strategy
  "Returns a movement strategy for `parent-zloc`, which should be a list node.
  The strategy will include a `:rind` and a `:breadth`. In the base case, the
  strategy will be to move elements one by one. Some functions like case and
  cond have groups of arguments that should be moved together. If the list node
  is such a function call, returns the appropriate movement strategy for it.
  Returns nil in one special case when we know we're in an invalid condp."
  [parent-zloc child-count]
  ;; case and condp permit final default expression, which should not be moved.
  ;; assoc, assoc!, cond->, cond->>, and case sometimes appear inside other threading expressions.
  ;; condp has a variation with ternary expressions.
  (case (some-> parent-zloc z-down z/sexpr)
    cond
    #_=> {:breadth 2, :rind [1 0]}
    (cond-> cond->> assoc assoc!)
    #_=> {:breadth 2, :rind [(if (in-threading? parent-zloc) 1 2) 0]}
    case
    #_=> {:breadth 2, :rind (if (in-threading? parent-zloc)
                              [1 (if (odd? child-count) 0 1)]
                              [2 (if (even? child-count) 0 1)])}
    condp
    #_=> (let [breadth (if (z/find-next-value (z-down parent-zloc) z-right :>>) 3 2)
               ignore-left 3
               ignore-right (mod (- child-count ignore-left) breadth)
               invalid-ternary? (= 2 ignore-right)]
           (when-not invalid-ternary?
             {:breadth breadth, :rind [ignore-left ignore-right]}))
    {:breadth 1, :rind no-rind}))

(defn ^:private movable-sibling-counts
  "Returns the number of movable siblings to the left and right of `zloc`."
  [zloc [ignore-left ignore-right]]
  [(-> zloc count-siblings-left (- ignore-left))
   (-> zloc count-siblings-right (- ignore-right))])

(defn ^:private valid-strat?
  "Checks whether a movement strategy can be applied to `zloc`. This isn't a
  perfect test; see [[can-swap?]]."
  [zloc {:keys [dir breadth pulp rind]}]
  ;; do we have a multiple of the right number of elements?
  (and (zero? (mod pulp breadth))
       ;; are there enough elements before and after this zloc?
       (let [[left right] (movable-sibling-counts zloc rind)]
         (or
          ;; erroneously true if on whitespace following first group
           (and (= :up dir)   (>= left breadth) (>= right 0))
          ;; erroneously true if on whitespace preceding last group
           (and (= :down dir) (>= left 0)       (>= right breadth))))))

(defn ^:private pulp [[ignore-left ignore-right] child-count]
  (- child-count ignore-left ignore-right))

(defn ^:private movement-strategy
  "Returns a movement strategy when the `zloc` can be moved in the direction
  `dir`.

  A movement strategy is three pieces of data.
  - `:breadth`: how many element should be moved together.
  - `:rind`: how many elements at the beginning and end of the containing
     expression can be ignored for the purposes of movement.
  - `:pulp`: how many elements in the interior of the expression can be moved.
  - `:dir` A copy of the `dir`.

  May return a movement strategy even when the zloc cannot actually be moved.
  See [[valid-strat?]] and [[can-swap?]]."
  [dir zloc uri db]
  (let [parent-zloc (z-up zloc)
        child-count (count-children parent-zloc)
        strat (case (some-> parent-zloc z/tag)
                :map    {:breadth 2, :rind no-rind}
                :set    {:breadth 1, :rind no-rind}
                :vector (vector-strategy parent-zloc uri db)
                :list   (list-strategy parent-zloc child-count)
                nil)]
    (when strat
      (let [strat (assoc strat
                         :dir dir
                         :pulp (pulp (:rind strat) child-count))]
        (when (valid-strat? zloc strat)
          strat)))))

(defn can-move-entry-up? [zloc uri db]
  (boolean (movement-strategy :up zloc uri db)))

(defn can-move-entry-down? [zloc uri db]
  (boolean (movement-strategy :down zloc uri db)))

(defn ^:private final-bottom-cursor
  "Returns the position where the cursor should be placed after the swap in
  order to end up at the top of the (eventual) bottom group.

  This is calculated by starting at the beginning of the `top-loc`, and
  adjusting that position by adding the extent of the `intervening-locs`. The
  extent is calculated by re-parsing the intervening-locs, counting their rows
  and columns by looking at their string representations. This is probably slow,
  but it avoids needing to use {:track-position? true} in the parser. If someday
  this project uses :track-position?, this code probably should be changed to
  read the revised position of the original zloc."
  [top-loc intervening-locs]
  (let [top-position (z-cursor-position top-loc)

        [bottom-row bottom-col]
        (reduce (fn [pos zloc]
                  (n.protocols/+extent pos (n.protocols/extent (z/node zloc))))
                [(:row top-position) (:col top-position)]
                intervening-locs)]
    {:row     bottom-row
     :col     bottom-col
     :end-row bottom-row
     :end-col bottom-col}))

(defn ^:private changes [uri parent-loc cursor-position]
  {:show-document-after-edit {:uri         uri
                              :take-focus? true
                              :range       cursor-position}
   :changes-by-uri           {uri
                              [{:range (z-cursor-position parent-loc)
                                :loc   parent-loc}]}})

(defn ^:private movement [dir zloc uri db]
  (when-let [strategy (movement-strategy dir zloc uri db)]
    (move-group-by-strategy zloc strategy)))

(defn move-up [zloc uri db]
  (when-let [[parent-zloc top-loc _] (movement :up zloc uri db)]
    (changes uri parent-zloc (z-cursor-position top-loc))))

(defn move-down [zloc uri db]
  (when-let [[parent-zloc top-loc intervening-locs] (movement :down zloc uri db)]
    (changes uri parent-zloc (final-bottom-cursor top-loc intervening-locs))))
