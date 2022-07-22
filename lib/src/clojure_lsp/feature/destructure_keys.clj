(ns clojure-lsp.feature.destructure-keys
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [medley.core :as medley]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn local-def-elem-to-destructure [zloc uri db]
  (when zloc
    (let [filename (shared/uri->filename uri)
          {:keys [row col]} (meta (z/node zloc))]
      (when-let [local-elem (q/find-definition-from-cursor db filename row col)]
        (when (= :locals (:bucket local-elem))
          local-elem)))))

(defn can-destructure-keys? [zloc uri db]
  (boolean (local-def-elem-to-destructure zloc uri db)))

(defn ^:private usage-data [usage-zlocs]
  (map (fn [usage-loc]
         (let [left-loc (z/left usage-loc)
               up-loc (z/up usage-loc)]
           (or (when (and left-loc
                          (z/leftmost? left-loc)
                          (= :list (z/tag up-loc)))
                 (cond
                   (n/keyword-node? (z/node left-loc))
                   (let [kw (z/sexpr left-loc)
                         local-usage (symbol (name kw))]
                     {:node-type :keys
                      :replacing-loc up-loc
                      :local (if (qualified-ident? kw)
                               (if (:auto-resolved? (z/node left-loc))
                                 (z/node left-loc)
                                 (symbol (namespace kw) (name kw)))
                               local-usage)
                      :local-usage local-usage})
                   (= :quote (z/tag left-loc))
                   (let [sym (z/sexpr (z/down left-loc))]
                     {:node-type :syms
                      :replacing-loc up-loc
                      :local sym
                      :local-usage (symbol (name sym))})
                   :else nil))
               {:node-type :keep})))
       usage-zlocs))

(defn ^:private keep-usage? [{:keys [node-type]}]
  (= :keep node-type))

(defn destructure-keys [zloc uri db]
  (when-let [def-elem (local-def-elem-to-destructure zloc uri db)]
    (let [top-zloc (edit/to-top zloc)
          def-zloc (edit/find-at-pos top-zloc (:row def-elem) (:col def-elem))
          usage-zlocs (map (fn [{:keys [row col]}]
                             (edit/find-at-pos top-zloc row col))
                           (q/find-references db def-elem false))
          usage-data (usage-data usage-zlocs)
          keep-def? (some keep-usage? usage-data)
          usage-data (remove keep-usage? usage-data)
          destructuring (->> usage-data
                             (group-by :node-type)
                             (medley/map-vals #(into []
                                                     (comp (map :local)
                                                           (distinct))
                                                     %)))
          destructuring (cond-> destructuring
                          keep-def? (assoc :as (symbol (:name def-elem))))]
      (into [{:range (meta (z/node def-zloc))
              :loc (z/of-node (n/coerce destructuring))}]
            (map (fn [{:keys [local-usage replacing-loc]}]
                   {:range (meta (z/node replacing-loc))
                    :loc   (z/of-node local-usage)})
                 usage-data)))))
