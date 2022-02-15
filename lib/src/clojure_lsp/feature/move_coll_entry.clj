(ns clojure-lsp.feature.move-coll-entry
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.node.protocols :as n.protocols]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defn- count-siblings-left
  "Count the number of sibling nodes to the left of the child node `zloc`."
  [zloc]
  (->> zloc
       (iterate z/left)
       (take-while (complement z/leftmost?))
       count))

(defn- count-siblings-right
  "Count the number of sibling nodes to the right of the child node `zloc`."
  [zloc]
  (->> zloc
       (iterate z/right)
       (take-while (complement z/rightmost?))
       count))

(defn ^:private newline-comment? [n]
  (and (n/comment? n)
       (string/ends-with? (:s n) "\n")))

(defn ^:private z-take-while
  "Returns a sequence of locations in the direction of `f` from `zloc` that
  satisfy `p?`."
  [zloc f p?]
  (->> zloc
       (iterate f)
       (take-while identity)
       (take-while (complement z/end?))
       (take-while p?)))

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
      (let [[padding zloc] (z-split-with zloc z/whitespace?)]
        (recur zloc
               :on-elem
               idx
               (conj result {:type :padding
                             :idx  idx
                             :locs padding})))

      (= :on-elem state)
      (let [[prefix elem-loc] (z-split-with zloc z/whitespace-or-comment?)]
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
                                   (z/skip z/right* (comp #{:whitespace :comment}
                                                          z/tag))
                                   ;; relinquish that node
                                   z/left*
                                   ;; then give up as much whitespace as possible,
                                   ;; back to the (optional) comment
                                   (z/skip z/left* z/whitespace?)
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
  "Search for an element within `elems` that was at `cursor-position`.

  We compare cursor position, not full loc, because comments have been separated
  from their newlines by `seq-elems`.

  This procedure is complicated by the possiblity that the cursor is on padding
  between elements. If this was the case, we assume the intention was to move
  the next element."
  [cursor-position elems]
  (->> elems
       (filter (fn [{:keys [locs]}]
                 (some (comp #{cursor-position} z-cursor-position)
                       locs)))
       first
       :idx))

(defn ^:private elem-by-index [search-idx elems]
  (->> elems
       (filter (fn [{:keys [type idx]}]
                 (and (= :elem type)
                      (= search-idx idx))))
       first))

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
  newline because otherwise the closing bracket will be commented out. By
  default this aligns the bracket with rightmost element, but this can be
  modified by providing a `movement` from the rightmost."
  ([parent-zloc] (fix-trailing-comment parent-zloc identity))
  ([parent-zloc movement]
   (let [last-zloc (-> parent-zloc z/down z/rightmost*)]
     (if (-> last-zloc z/node n/comment?)
       (let [col (-> parent-zloc
                     z/down
                     z/rightmost
                     movement
                     z-cursor-position
                     :col)]
         (-> last-zloc
             (z/insert-space-right (dec col))
             z/insert-newline-right
             z/up))
       parent-zloc))))

(defn ^:private can-swap?
  "In a few cases, the simple can-move-*? heuristics return the wrong results.
  This happens:
  A) When on a comment following the first element/pair, and `move-up` is
     invoked.
  B) When on a padding line before the last element/pair, and `move-down` is
     invoked.
  Movement should not be allowed in either of these cases. Unfortunately,
  there's no quick way to know we're in this situation until after we've
  affiliated whitespace with elements. But now that we've done that, we can
  detect it. It manifests as the origin-idx or dest-idx being out of bounds, in
  which case either origin-elem or dest-elem will be missing. We bail out now to
  avoid erroneous swaps."
  [origin-idx dest-idx elems]
  (and (elem-by-index origin-idx elems)
       (elem-by-index dest-idx elems)))

(defn ^:private move-pair-zloc
  "Move a pair of elements around `zloc` in direction `dir` considering multiple
  comments cases."
  [zloc dir]
  (let [parent-zloc (z/up zloc)

        elems (seq-elems parent-zloc)

        origin-idx (elem-index-by-cursor-position (z-cursor-position zloc) elems)
        ;; Here we begin to treat elements like pairs.
        origin-idx (cond-> origin-idx
                     ;; if on on value, go back to key
                     (odd? origin-idx) dec)
        dest-idx   (dir (dir origin-idx))]
    (when (can-swap? origin-idx dest-idx elems)
      (let [earlier-idx  (min origin-idx dest-idx)
            [before rst] (split-elems-at-idx elems earlier-idx)

            [earlier-pair rst] (split-at 3 rst) ;; key, padding, value
            [interstitial rst] (split-at 1 rst) ;; padding
            [later-pair rst]   (split-at 3 rst) ;; key, padding, value

            swapped     (concat before
                                later-pair
                                interstitial
                                earlier-pair
                                rst)
            parent-zloc (-> parent-zloc
                            (edit-collection swapped)
                            ;; align with last key, not value
                            (fix-trailing-comment z/left))]
        [parent-zloc (first earlier-pair) (concat later-pair interstitial)]))))

(defn ^:private move-element-zloc
  "Move an element around `zloc` in direction `dir` considering multiple
  comments cases."
  [zloc dir]
  (let [parent-zloc (z/up zloc)

        elems (seq-elems parent-zloc)

        origin-idx (elem-index-by-cursor-position (z-cursor-position zloc) elems)
        dest-idx   (dir origin-idx)]
    (when (can-swap? origin-idx dest-idx elems)
      (let [earlier-idx  (min origin-idx dest-idx)
            [before rst] (split-elems-at-idx elems earlier-idx)

            [[earlier-elem padding later-elem] rst] (split-at 3 rst)

            swapped (concat before
                            [later-elem
                             padding
                             earlier-elem]
                            rst)

            parent-zloc (-> parent-zloc
                            (edit-collection swapped)
                            (fix-trailing-comment))]
        [parent-zloc earlier-elem [later-elem padding]]))))

(def ^:private common-binding-syms
  "Symbols that are typically used to define bindings."
  ;; We aren't concerned with macros like `if-let` that establish one binding
  ;; only. They can be treated like regular 'move-element' vectors.
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
    (or (when-let [outer-zloc (z/left zloc)]
          (and (z/leftmost? outer-zloc)
               (contains? common-binding-syms (z/sexpr outer-zloc))))
        (let [z-pos   (z-cursor-position (z/down zloc))
              z-scope {:name-row     (:row z-pos)
                       :name-col     (:col z-pos)
                       :name-end-row (:end-row z-pos)
                       :name-end-col (:end-col z-pos)}]
          (q/find-first (fn [element]
                          (and (= :locals (:bucket element))
                               (shared/inside? element z-scope)))
                        (get analysis (shared/uri->filename uri)))))))

(defn ^:private balanced-pairs? [parent-zloc]
  (even? (count (z/child-sexprs parent-zloc))))

(defn ^:private move-pair-up-breadth [zloc]
  (when (and (balanced-pairs? (z/up zloc))
             (>= (count-siblings-left zloc) 2))
    :pairwise))

(defn ^:private move-element-up-breadth [zloc]
  (when (>= (count-siblings-left zloc) 1)
    :elementwise))

(defn ^:private move-pair-down-breadth [zloc]
  (when (and (balanced-pairs? (z/up zloc))
             (>= (count-siblings-right zloc) 2))
    :pairwise))

(defn ^:private move-element-down-breadth [zloc]
  (when (>= (count-siblings-right zloc) 1)
    :elementwise))

(defn move-up-breadth [zloc uri db]
  (let [parent-zloc (z/up zloc)]
    (case (some-> parent-zloc z/tag)
      :map         (move-pair-up-breadth zloc)
      :vector      (if (establishes-bindings? parent-zloc uri db)
                     (move-pair-up-breadth zloc)
                     (move-element-up-breadth zloc))
      (:list :set) (move-element-up-breadth zloc)
      nil)))

(defn move-down-breadth [zloc uri db]
  (let [parent-zloc (z/up zloc)]
    (case (some-> parent-zloc z/tag)
      :map         (move-pair-down-breadth zloc)
      :vector      (if (establishes-bindings? parent-zloc uri db)
                     (move-pair-down-breadth zloc)
                     (move-element-down-breadth zloc))
      (:list :set) (move-element-down-breadth zloc)
      nil)))

(defn can-move-entry-up? [zloc uri db]
  (boolean (move-up-breadth zloc uri db)))

(defn can-move-entry-down? [zloc uri db]
  (boolean (move-down-breadth zloc uri db)))

(defn ^:private movement [zloc breadth dir]
  (case breadth
    :pairwise    (move-pair-zloc zloc dir)
    :elementwise (move-element-zloc zloc dir)
    nil))

(defn ^:private final-top-cursor
  "Returns the position where the cursor should be placed after the swap in
  order to end up on the (eventual) top element.

  This is calculated from the beginning of the swapped elements `orig-top-elem`."
  [orig-top-elem]
  (z-cursor-position (first (:locs orig-top-elem))))

(defn ^:private final-bottom-cursor
  "Returns the position where the cursor should be placed after the swap in
  order to end up on the (eventual) bottom element.

  This is calculated by starting at the beginning of the swapped elements
  `orig-top-elem`, and adjusting that position by adding the extent of the
  `intervening-elems`. The extent is calculated by re-parsing the
  intervening-elems, counting their rows and columns by looking at their string
  representations. This is probably slow, but it avoids needing to use
  {:track-position? true} in the parser. If someday this project uses
  :track-position?, this code probably should be changed to read the revised
  position of the original zloc."
  [orig-top-elem intervening-elems]
  (let [top-position (final-top-cursor orig-top-elem)

        [bottom-row bottom-col]
        (->> intervening-elems
             (mapcat :locs)
             (reduce (fn [pos zloc]
                       (n.protocols/+extent pos (n.protocols/extent (z/node zloc))))
                     [(:row top-position) (:col top-position)]))]
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

(defn move-up [zloc uri db]
  (when-let [[parent-zloc orig-top-elem _]
             (movement zloc (move-up-breadth zloc uri db) dec)]
    (changes uri parent-zloc (final-top-cursor orig-top-elem))))

(defn move-down [zloc uri db]
  (when-let [[parent-zloc orig-top-elem intervening-elems]
             (movement zloc (move-down-breadth zloc uri db) inc)]
    (changes uri parent-zloc (final-bottom-cursor orig-top-elem intervening-elems))))
