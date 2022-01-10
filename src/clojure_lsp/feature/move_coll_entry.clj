(ns clojure-lsp.feature.move-coll-entry
  (:require
   [clojure.string :as string]
   [rewrite-clj.node :as n]
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

(defn newline-comment? [n]
  (and (n/comment? n)
       (string/ends-with? (:s n) "\n")))

(defn z-take-while [zloc f p?]
  (->> zloc
       (iterate f)
       (take-while identity)
       (take-while (complement z/end?))
       (take-while p?)))

(defn z-split-with
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
   :locs <sequence of zipper locations whose nodes make up the padding>}

  The children of the original `seq-zloc` could be reconstructed by
  concatenating all the `:locs` together.
  "
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

         state    :in-padding
         elem-idx 0
         result   []]
    (cond
      (nil? zloc) ;; rightmost?*
      ;; everything processed
      result

      (= :in-padding state)
      (let [[padding zloc] (z-split-with zloc z/whitespace?)]
        (recur zloc
               :on-elem
               elem-idx
               (cond-> result
                 (seq padding) (conj {:type :padding :locs padding}))))

      (= :on-elem state)
      (let [[prefix elem-loc] (z-split-with zloc z/whitespace-or-comment?)]
        (if-not elem-loc
          ;; We've processed all the elements and this is trailing
          ;; whitespace.
          (cond-> result
            (seq prefix) (conj {:type :padding :locs prefix}))
          (let [;; find the start of the next padding
                zloc    (->> elem-loc
                             z/right*
                             ;; move right until the first newline, comma or key
                             ;; TODO: handle case where comma precedes comment
                             ;; `a 1, ;; one comment`
                             ;; Conceptually, the comma should stay put while
                             ;; the entry and comment move.
                             (z/skip z/right* (comp #{:whitespace :comment}
                                                    z/tag))
                             z/left*
                             ;; then give up as much whitespace as possible,
                             ;; back to the (optional) comment
                             (z/skip z/left* z/whitespace?)
                             z/right*)
                ;; _ (prn (z/node zloc) (z/node (z/right* zloc)))
                postfix (z-take-while (z/right* elem-loc)
                                      z/right*
                                      #(not= zloc %))]
            (recur zloc
                   :in-padding
                   (inc elem-idx)
                   (conj result
                         {:type :elem
                          :idx  elem-idx
                          :locs (concat prefix [elem-loc] postfix)}))))))))

(defn ^:private move-entry-zloc
  "Move `zloc` to direction `dir` considering multiple comments cases."
  [zloc dir]
  (let [parent-zloc (z/up zloc)
        elems       (seq-elems parent-zloc)
        origin-elem (->> elems
                         (filter (fn [{:keys [locs]}]
                                   (some (fn [elem-loc]
                                           ;; compare meta, not full loc, because
                                           ;; comments have been separated from
                                           ;; their newlines
                                           (= (meta (z/node elem-loc))
                                              (meta (z/node zloc))))
                                         locs)))
                         first)

        ;; TODO we are assuming origin-elem is an elem. Handle case that
        ;; origin-elem is padding
        origin-idx (if (odd? (:idx origin-elem))
                     (dec (:idx origin-elem))
                     (:idx origin-elem))
        dest-idx   (dir (dir origin-idx))

        dest-elem (->> elems
                       (filter (fn [{:keys [type idx]}]
                                 (and (= :elem type)
                                      (= idx dest-idx))))
                       first)

        earlier-idx        (min origin-idx dest-idx)
        [before rst]       (split-with (fn [{:keys [type idx]}]
                                         (or (= :padding type)
                                             (< idx earlier-idx)))
                                       elems)
        [earlier-pair rst] (split-at 3 rst) ;; key, padding, value
        [interstitial rst] (split-at 1 rst) ;; padding
        [later-pair rst]   (split-at 3 rst) ;; key, padding, value

        swapped     (concat before
                            later-pair
                            interstitial
                            earlier-pair
                            rst)
        ;; Put revised entries back into parent.
        parent-zloc (z/replace parent-zloc
                               (n/replace-children (z/node parent-zloc)
                                                   (->> swapped
                                                        (mapcat :locs)
                                                        (map z/node))))

        ;; If after reordering, the last component has become a comment,
        ;; we need to add a newline or else the closing bracket will be
        ;; commented out.
        last-zloc   (-> parent-zloc z/down z/rightmost*)
        parent-zloc (if (-> last-zloc z/node n/comment?)
                      ;; align bracket with last key
                      (let [last-key (-> parent-zloc z/down z/rightmost z/left)
                            col      (:col (meta (z/node last-key)))]
                        (-> last-zloc
                            (z/insert-space-right (dec col))
                            z/insert-newline-right
                            z/up))
                      parent-zloc)]
    [parent-zloc (first (:locs dest-elem))]))

(defn ^:private can-move-entry? [zloc]
  (and (contains? #{:map :vector} (some-> zloc z/up z/tag))
       (even? (count (z/child-sexprs (z/up zloc))))))

(defn can-move-entry-up? [zloc]
  (and (can-move-entry? zloc)
       (>= (count-siblings-left zloc) 2)))

(defn can-move-entry-down? [zloc]
  (and (can-move-entry? zloc)
       (>= (count-siblings-right zloc) 2)))

(defn ^:private move-entry [zloc uri dir]
  (when-let [[parent-loc dest-loc] (move-entry-zloc zloc dir)]
    {:show-document-after-edit {:uri         uri
                                :take-focus? true
                                :range       (meta (z/node dest-loc))}
     :changes-by-uri           {uri
                                [{:range (meta (z/node parent-loc))
                                  :loc   parent-loc}]}}))

(defn move-up [zloc uri]
  (when (can-move-entry-up? zloc)
    (move-entry zloc uri dec)))

(defn move-down [zloc uri]
  (when (can-move-entry-down? zloc)
    (move-entry zloc uri inc)))
