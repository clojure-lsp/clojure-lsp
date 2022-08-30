(ns clojure-lsp.feature.sort-clauses
  (:require
   [clojure-lsp.feature.clauses :as f.clauses]
   [medley.core :as medley]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn ^:private clause-spec [zloc uri db]
  (when zloc
    (let [zloc (or (and (n/inner? (z/node zloc)) ;; sort from map/vector/list bracket
                        (z/down zloc))
                   zloc)
          clause-spec (f.clauses/clause-spec zloc uri db)]
      ;; don't sort in places that establish bindings, which are order dependent
      (when-not (contains? #{:forms :binding} (:context clause-spec))
        clause-spec))))

(defn can-sort? [zloc uri db]
  (clause-spec zloc uri db))

(defn sort-clauses [zloc uri db]
  (when-let [clause-spec (clause-spec zloc uri db)]
    (let [{:keys [rind-before clauses+padding rind-after]} (f.clauses/identify clause-spec)
          rind-nodes-before (:nodes rind-before)
          rind-nodes-after (:nodes rind-after)
          clauses-nodes (->> clauses+padding (filter :idx) (map :nodes))
          padding-nodes (->> clauses+padding (remove :idx) (map :nodes))
          original-pulp-nodes (mapcat :nodes clauses+padding)
          sorted-clauses (sort-by (fn [clause-nodes]
                                    (->> clause-nodes
                                         (remove (comp f.clauses/whitespace-or-comment-tags n/tag))
                                         first
                                         n/string))
                                  clauses-nodes)
          new-pulp-nodes (flatten (medley/interleave-all sorted-clauses padding-nodes))
          trailing-node (or (last rind-nodes-after) (last new-pulp-nodes))
          trailing-comment-fix (when (some-> trailing-node n/comment?)
                                 (let [orig-leading-node (or (first rind-nodes-before) (first original-pulp-nodes))
                                       col (:col (meta orig-leading-node))]
                                   [(n/newline-node "\n") (n/spaces (dec col))]))]
      [{:range (f.clauses/nodes-range original-pulp-nodes)
        :loc (f.clauses/loc-of-nodes (concat new-pulp-nodes trailing-comment-fix))}])))
