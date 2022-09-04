(ns clojure-lsp.feature.destructure-keys
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [medley.core :as medley]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn ^:private loc-destructuring-key
  "When `loc` is a val inside a map and its key is a keyword, returns its key.
  `{:as |x} ;; => :as`"
  [loc]
  (let [up-loc (z/up loc)
        left-loc (z/left loc)]
    (when (and up-loc left-loc
               (z/map? up-loc)
               (n/keyword-node? (z/node left-loc)))
      (z/sexpr left-loc))))

(defn local-to-destructure [zloc uri db]
  (when zloc
    (let [{:keys [row col]} (meta (z/node zloc))]
      (when-let [def-elem (q/find-definition-from-cursor db uri row col)]
        (when (= :locals (:bucket def-elem))
          (let [top-zloc (edit/to-top zloc)
                def-zloc (edit/find-at-pos top-zloc (:row def-elem) (:col def-elem))
                up-loc (z/up def-zloc)
                destructuring-key (when (and up-loc (= :vector (z/tag up-loc)))
                                    (loc-destructuring-key up-loc))]
            ;; rule out things that are already bound in a destructuring form
            (when (or (not destructuring-key)
                      (not (contains? #{"keys" "syms" "strs"}
                                      (name destructuring-key))))
              {:def-elem def-elem
               :top-zloc top-zloc
               :def-zloc def-zloc})))))))

(defn can-destructure-keys? [zloc uri db]
  (boolean (local-to-destructure zloc uri db)))

(defn ^:private usage-datum [usage-loc]
  (let [left-loc (z/left usage-loc)
        up-loc (z/up usage-loc)]
    (or (when (and left-loc
                   (z/leftmost? left-loc)
                   (= :list (z/tag up-loc)))
          (let [left-node (z/node left-loc)]
            (cond
              (n/keyword-node? left-node)
              (let [kw (n/sexpr left-node)
                    local-usage (symbol (name kw))]
                {:destructure? true
                 :destructure-key :keys
                 :replacing-loc up-loc
                 :local (if (qualified-ident? kw)
                          (if (:auto-resolved? left-node)
                            left-node
                            (symbol (namespace kw) (name kw)))
                          local-usage)
                 :local-usage local-usage})
              (= :quote (n/tag left-node))
              (let [sym (z/sexpr (z/down left-loc))]
                {:destructure? true
                 :destructure-key :syms
                 :replacing-loc up-loc
                 :local sym
                 :local-usage (symbol (name sym))})
              :else nil)))
        {:destructure? false})))

(defn ^:private merge-destructuring
  "Similar to shared/deep-merge, but preserves key order."
  [prior-destructuring new-destructuring]
  (->> (concat (keys prior-destructuring)
               (keys new-destructuring))
       distinct
       (reduce (fn [m k]
                 (assoc m k
                        (let [v (get prior-destructuring k)
                              v' (get new-destructuring k)]
                          (if (keyword? k)
                            ;; {:keys [a]}, {:keys [b]}
                            (vec (concat v v'))
                            ;; {a :a}
                            v))))
               (array-map))))

(defn destructure-keys [zloc uri db]
  (when-let [{:keys [def-elem top-zloc def-zloc]} (local-to-destructure zloc uri db)]
    (let [usage-data (->> (q/find-references db def-elem false)
                          (map (fn [{:keys [row col]}]
                                 (edit/find-at-pos top-zloc row col)))
                          (map usage-datum))]
      (when (some :destructure? usage-data)
        (let [prior-destructuring (when (= :as (loc-destructuring-key def-zloc))
                                    ;; NOTE: z/sexpr loses comments. Fix?
                                    (dissoc (z/sexpr (z/up def-zloc)) :as))
              new-destructuring (->> usage-data
                                     (filter :destructure?)
                                     (group-by :destructure-key)
                                     (medley/map-vals #(into []
                                                             (comp (map :local)
                                                                   (distinct))
                                                             %)))
              destructuring (merge-destructuring prior-destructuring new-destructuring)
              destructuring (cond-> destructuring
                              (not-every? :destructure? usage-data)
                              (assoc :as (symbol (:name def-elem))))]
          (into [{:range (meta (z/node (if (seq prior-destructuring)
                                         (z/up def-zloc)
                                         def-zloc)))
                  :loc (z/of-node (n/coerce destructuring))}]
                (keep (fn [{:keys [local-usage replacing-loc]}]
                        (when replacing-loc
                          {:range (meta (z/node replacing-loc))
                           :loc   (z/of-node local-usage)}))
                      usage-data)))))))
