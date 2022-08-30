(ns clojure-lsp.feature.drag
  "Drag clauses forward or backward.

  {:a 1, :b 2} -> {:b 2, :a 1}

  Though this feature was originally called 'move collection entry', it is now
  more generally used to drag any clause forward or backward, even if it isn't
  in an immutable collection."
  (:require
   [clojure-lsp.feature.clauses :as f.clauses]
   [rewrite-clj.node :as n]
   [rewrite-clj.node.protocols :as n.protocols]
   [clojure-lsp.refactor.edit :as edit]))

(set! *warn-on-reflection* true)

;;;; Main algorithm

;;;; Drag a clause

;; 1. Identify clauses.
;; 2. Notice which clause contains the cursor.
;; 3. Swap that clause with the previous or next clause, depending on the
;;    direction of movement.
;; 4. Reposition the cursor so that it appears to maintain its position within
;;    the original clause.

(defn node-edits
  [{:keys [nodes-before earlier-clause-nodes later-clause-nodes nodes-after]}]
  (let [trailing-node (or (last nodes-after) (last earlier-clause-nodes))
        trailing-comment-fix (when (some-> trailing-node n/comment?)
                               (let [orig-leading-node (or (first nodes-before) (first earlier-clause-nodes))
                                     col (:col (meta orig-leading-node))]
                                 [(n/newline-node "\n") (n/spaces (dec col))]))]
    ;; The actual swap.
    [{:range (f.clauses/nodes-range earlier-clause-nodes)
      :loc (f.clauses/loc-of-nodes later-clause-nodes)}
     {:range (f.clauses/nodes-range later-clause-nodes)
      :loc (f.clauses/loc-of-nodes (concat earlier-clause-nodes trailing-comment-fix))}]))

(defn ^:private nodes-start [nodes]
  (let [{:keys [row col]} (meta (first nodes))]
    [row col]))

(defn ^:private nodes-end [nodes]
  (let [{:keys [end-row end-col]} (meta (last nodes))]
    [end-row end-col]))

(defn ^:private offset [[start-row start-col] [end-row end-col]]
  ;; See n.protocols/extent
  (let [rows (- end-row start-row)]
    [rows (cond-> end-col (zero? rows) (- start-col))]))

(defn ^:private nodes-offset [nodes]
  (offset (nodes-start nodes) (nodes-end nodes)))

(defn ^:private reposition-cursor
  "Move the cursor so that it appears to stay at the same position within the
  origin-clause."
  [cursor-position dir {:keys [origin-nodes earlier-clause-nodes interstitial-nodes later-clause-nodes]}]
  (let [cursor-offset (offset (nodes-start origin-nodes)
                              [(:row cursor-position) (:col cursor-position)])
        [row col] (reduce n.protocols/+extent
                          (nodes-start earlier-clause-nodes)
                          (case dir
                            :backward [cursor-offset]
                            :forward [(nodes-offset later-clause-nodes)
                                      (nodes-offset interstitial-nodes)
                                      cursor-offset]))]
    {:row row :col col
     :end-row row :end-col col}))

(defn identify-clauses
  "Like f.clauses/identify, but adds origin-clause, after ensuring cursor was in
  a clause."
  [clause-spec]
  (let [{:keys [clauses+padding] :as identify-data}
        (f.clauses/identify (update clause-spec :zloc edit/mark-position ::orig))
        origin-clause (->> clauses+padding
                           (filter :idx) ;; exclude padding
                           (filter (fn [{:keys [nodes]}]
                                     (some #(edit/node-marked? % ::orig) nodes)))
                           first)]
    (when origin-clause ;; otherwise, cursor wasn't in a clause
      (assoc identify-data :origin-clause origin-clause))))

(defn nodes-to-drag
  "Identifies the nodes that will participate in dragging the `origin-clause` in
  direction of `dir`."
  [{:keys [rind-before rind-after clauses+padding clause-count origin-clause]} dir]
  (let [origin-idx (:idx origin-clause)
        destination-idx ((if (= dir :forward) inc dec) origin-idx)
        last-clause-idx (dec clause-count)
        in-pulp? (and (<= 0 origin-idx last-clause-idx)
                      (<= 0 destination-idx last-clause-idx))]
    (when in-pulp? ;; otherwise, drag would extend outside of pulp
      (let [earlier-idx (min origin-idx destination-idx)
            [pulp-before rst] (split-at (* 2 earlier-idx) clauses+padding)
            [earlier-clause interstitial later-clause & pulp-after] rst]
        {:nodes-before (mapcat :nodes (concat rind-before pulp-before))
         :earlier-clause-nodes (:nodes earlier-clause)
         :interstitial-nodes (:nodes interstitial)
         :later-clause-nodes (:nodes later-clause)
         :nodes-after (mapcat :nodes (concat pulp-after rind-after))
         :origin-nodes (:nodes origin-clause)}))))

(defn drag-clause
  "Drag the `origin-clause` in direction of `dir`, adjusting the
  `cursor-position` to move with the clause.

  Instructs the client to:
  - Apply edits that swap the clauses.
  - Re-position the cursor after the edits."
  [clause-data dir cursor-position uri]
  (when-let [nodes (nodes-to-drag clause-data dir)]
    {:show-document-after-edit {:uri        uri
                                :take-focus true
                                :range      (reposition-cursor cursor-position dir nodes)}
     :changes-by-uri {uri (node-edits nodes)}}))

;;;; Plan
;; Form a plan about how to drag

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

(defn ^:private probable-valid-movement?
  "Checks whether `zloc` can be dragged in direction `dir`, assuming it is part of
  a clause as described by `clause-spec`.

  This isn't a perfect test. May return true even when the zloc cannot actually
  be dragged. This can happen when the cursor is on padding between clauses, or
  when dragging backwards from comments or padding after the first clause, or
  when dragging forwards from comments or padding before the last clause."
  [{:keys [breadth rind zloc] :as clause-spec} dir]
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

(defn plan [zloc dir uri db]
  (let [clause-spec (f.clauses/clause-spec zloc uri db)]
    (when (probable-valid-movement? clause-spec dir)
      clause-spec)))

;;;; Public API

;; Drag zloc's clause forward or backward.

(defn ^:private can-drag? [zloc dir uri db] (boolean (plan zloc dir uri db)))
(defn can-drag-backward? [zloc uri db] (can-drag? zloc :backward uri db))
(defn can-drag-forward? [zloc uri db] (can-drag? zloc :forward uri db))

(defn ^:private drag [zloc dir cursor-position uri db]
  (some-> (plan zloc dir uri db)
          identify-clauses
          (drag-clause dir cursor-position uri)))

(defn drag-backward [zloc cursor-position uri db] (drag zloc :backward cursor-position uri db))
(defn drag-forward [zloc cursor-position uri db] (drag zloc :forward cursor-position uri db))
