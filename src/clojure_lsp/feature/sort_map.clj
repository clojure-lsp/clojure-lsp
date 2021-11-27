(ns clojure-lsp.feature.sort-map
  (:require
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defn sortable-map?
  "A node is a candidate for sorting if it is a map that contains no discards,
  and has more than 0 key-value pairs"
  [zloc]
  (boolean
    (and (= :map (z/tag zloc))
         (not (z/find-tag (z/down* zloc) z/right* :uneval))
         (-> zloc z/down)
         (even? (->> zloc
                     z/down
                     (iterate z/right)
                     (take-while identity)
                     count)))))

(defn ^:private partition-map-children
  "Partition and distinguish <k><other nodes>+<v>(<whitespace><comment>)* from <nodes outside key-value pair>+
    [{:k <key-node> :s [<key-node> <other nodes> <value-node> <optionally trailing comment>]}
     {:s [<space/comment nodes outside key-value pair>]}
     {:k <key-node> :s [...]}]"
  [map-children]
  (loop [children map-children
         k nil
         aseq []
         res []]
    (let [n (first children)
          t (when n (n/tag n))]
      (cond
        (nil? n)
        (if (seq aseq)
          (conj res {:s aseq})
          res)

        ;; spaces and such
        (some #{t} [:comment :whitespace :comma :newline])
        (if (and (= :comment t)
                 (not k)
                 (:k (last res))
                 (not (seq (filter #(= :newline (n/tag %)) aseq))))
          ;; tack on found trailing comment nodes to last kv seq
          (recur (rest children)
                 k
                 ;; comments include newlines, imperfect to leave a compensating newline
                 ;; but better than not leaving one..
                 [(n/newlines 1)]
                 (conj (vec (butlast res))
                       (merge-with concat (last res) {:s (conj aseq n)})))
          (recur (rest children)
                 k
                 (conj aseq n)
                 res))

        ;; if we found a key, then we must be on a value
        k
        (recur (rest children)
               nil
               []
               (conj res {:k k :s (conj aseq n)}))

        ;; not key, so must be a value
        :else
        (recur (rest children)
               n
               [n]
               (if (seq aseq)
                 (conj res {:s aseq})
                 res))))))

(defn ^:private sort-map-by-key
  "Return zloc with current map node sorted.

  - map is sorted by string version of its key
  - no nested sorting
  - leaves spacing surrounding key-value pairs as is.
  - no special handling for multi-line nodes, some formatting cleanup may be
  required after sort."
  [zloc]
  (let [children (->> zloc z/node n/children)
        node-seqs (partition-map-children children)
        kv-seqs (filter :k node-seqs)
        kv-seqs-sorted (sort-by #(-> % :k n/string) kv-seqs)
        ;; assemble sorted kv seqs back preserving any surrounding whitespace
        children-sorted (loop [node-seqs node-seqs
                               kv-seqs-sorted kv-seqs-sorted
                               res []]
                          (let [orig-seq (first node-seqs)]
                            (cond
                              (nil? orig-seq)
                              res

                              (:k orig-seq)
                              (recur
                                (rest node-seqs)
                                (rest kv-seqs-sorted)
                                (concat res (-> kv-seqs-sorted first :s)))

                              :else
                              (recur
                                (rest node-seqs)
                                kv-seqs-sorted
                                (concat res (-> node-seqs first :s))))))]
    (z/replace* zloc (-> zloc
                         z/node
                         (n/replace-children children-sorted)))))

(defn sort-map [zloc]
  (when (sortable-map? zloc)
    (let [sorted-zloc (sort-map-by-key zloc)]
      [{:range (meta (z/node sorted-zloc))
        :loc sorted-zloc}])))
