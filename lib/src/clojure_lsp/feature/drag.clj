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
   [clojure-lsp.feature.clauses :as f.clauses]
   [rewrite-clj.node :as n]
   [rewrite-clj.node.protocols :as n.protocols]))

(set! *warn-on-reflection* true)

(defn ^:private count-siblings-left
  "Count the number of sibling nodes to the left of the child node `zloc`."
  [zloc]
  (-> (f.clauses/z-left zloc)
      (f.clauses/z-take-while f.clauses/z-left identity)
      count))

(defn ^:private count-siblings-right
  "Count the number of sibling nodes to the right of the child node `zloc`."
  [zloc]
  (-> (f.clauses/z-right zloc)
      (f.clauses/z-take-while f.clauses/z-right identity)
      count))

;;;; Main algorithm

;;;; Drag a clause

;; 1. Identify clauses.
;; 2. Notice which clause contains the cursor.
;; 3. Swap that clause with the previous or next clause, depending on the
;;    direction of movement.
;; 4. Reposition the cursor so that it appears to maintain its position within
;;    the original clause.

(defn ^:private edited-nodes [before earlier-clause later-clause after]
  (let [trailing-node (or (last after) (last earlier-clause))
        trailing-comment-fix (when (some-> trailing-node n/comment?)
                               (let [orig-leading-node (or (first before) (first earlier-clause))
                                     col (:col (meta orig-leading-node))]
                                 [(n/newline-node "\n") (n/spaces (dec col))]))]
    ;; The actual swap.
    [{:range (f.clauses/nodes-range earlier-clause)
      :loc (f.clauses/loc-of-nodes later-clause)}
     {:range (f.clauses/nodes-range later-clause)
      :loc (f.clauses/loc-of-nodes (concat earlier-clause trailing-comment-fix))}]))

(defn ^:private final-position [dir cursor-offset earlier-clause interstitial later-clause]
  ;; The cursor-offset is how far the cursor was from the beginning of the origin clause.
  ;; If we are moving backward, we want to relocate the cursor this same offset
  ;; from the start of the earlier clause.
  ;; If we are moving forward, it's slightly more complicated. We want to move
  ;; from that same start point, over the later-clause (which is taking the
  ;; place of the earlier-clause), over interstitial padding, and finally that
  ;; same cursor offset.
  (let [earlier-point (f.clauses/start-point (first earlier-clause))
        [row col] (reduce n.protocols/+extent
                          earlier-point
                          (case dir
                            :backward [cursor-offset]
                            :forward [(f.clauses/nodes-offset later-clause)
                                      (f.clauses/nodes-offset interstitial)
                                      cursor-offset]))]
    {:row row :col col
     :end-row row :end-col col}))

(defn ^:private drag-clause
  "Drag a clause of `breadth` elements around `zloc` in direction of `dir`
  ignoring elements in `rind`.

  Returns two pieces of data:
  - The edits that swap the clauses, each with the old range and the new loc.
  - The position where the cursor should be placed after the edits."
  [dir cursor-position clause-spec]
  (let [{:keys [rind-before rind-after clauses+padding clause-count origin-clause]}
        (f.clauses/identify clause-spec)]
    (when origin-clause ;; otherwise, cursor wasn't in a clause
      (let [origin-idx (:idx origin-clause)
            destination-idx ((if (= dir :forward) inc dec) origin-idx)
            last-clause-idx (dec clause-count)
            in-pulp? (and (<= 0 origin-idx last-clause-idx)
                          (<= 0 destination-idx last-clause-idx))]
        (when in-pulp? ;; otherwise, drag would extend outside of pulp
          (let [earlier-idx (min origin-idx destination-idx)
                [pulp-before rst] (split-at (* 2 earlier-idx) clauses+padding)
                [earlier-clause interstitial later-clause & pulp-after] rst

                before (mapcat :nodes (concat rind-before pulp-before))
                earlier-clause (:nodes earlier-clause)
                interstitial (:nodes interstitial)
                later-clause (:nodes later-clause)
                after (mapcat :nodes (concat pulp-after rind-after))

                cursor-offset (f.clauses/offset
                                (f.clauses/start-point (first (:nodes origin-clause)))
                                [(:row cursor-position)
                                 (:col cursor-position)])]
            [(edited-nodes before earlier-clause later-clause after)
             (final-position dir cursor-offset
                             earlier-clause interstitial later-clause)]))))))

;;;; Plan
;; Form a plan about how to drag

(defn ^:private probable-valid-movement?
  "Checks whether `zloc` can be dragged in direction `dir`, assuming it is part of
  a clause as described by `clause-spec`.

  This isn't a perfect test. May return true even when the zloc cannot actually
  be dragged. This can happen when the cursor is on padding between clauses, or
  when dragging backwards from comments or padding after the first clause, or
  when dragging forwards from comments or padding before the last clause."
  [zloc dir {:keys [breadth rind] :as clause-spec}]
  (when clause-spec
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

(defn ^:private plan [zloc dir uri db]
  (let [{:keys [zloc] :as clause-spec} (f.clauses/clause-spec zloc uri db)]
    (when (probable-valid-movement? zloc dir clause-spec)
      clause-spec)))

;;;; Public API

;; Drag zloc's clause forward or backward.

(defn ^:private can-drag? [zloc dir uri db] (boolean (plan zloc dir uri db)))
(defn can-drag-backward? [zloc uri db] (can-drag? zloc :backward uri db))
(defn can-drag-forward? [zloc uri db] (can-drag? zloc :forward uri db))

(defn ^:private drag [zloc dir cursor-position uri db]
  (when-let [clause-spec (plan zloc dir uri db)]
    (when-let [[edits cursor-position] (drag-clause dir cursor-position clause-spec)]
      {:show-document-after-edit {:uri        uri
                                  :take-focus true
                                  :range      cursor-position}
       :changes-by-uri {uri edits}})))

(defn drag-backward [zloc cursor-position uri db] (drag zloc :backward cursor-position uri db))
(defn drag-forward [zloc cursor-position uri db] (drag zloc :forward cursor-position uri db))
