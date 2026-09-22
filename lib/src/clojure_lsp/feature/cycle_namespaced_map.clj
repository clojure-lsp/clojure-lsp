(ns clojure-lsp.feature.cycle-namespaced-map
  (:require
   [clojure-lsp.refactor.edit :as edit]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defn ^:private enclosing-map-loc
  "Returns the loc of the map or namespaced map at `zloc` or directly
  enclosing it, preferring the namespaced map when zloc is inside one."
  [zloc]
  (when-let [map-loc (cond
                       (and zloc
                            (contains? #{:map :namespaced-map} (z/tag zloc)))
                       zloc

                       (some->> zloc z/up z/tag (contains? #{:map :namespaced-map}))
                       (z/up zloc)

                       :else nil)]
    (if (some-> map-loc z/up z/tag (= :namespaced-map))
      (z/up map-loc)
      map-loc)))

(defn ^:private map-key-nodes [map-node]
  (->> (n/children map-node)
       (remove n/printable-only?)
       (take-nth 2)))

(defn ^:private update-map-keys
  "Returns `map-node` with `f` applied to every key node."
  [map-node f]
  (n/replace-children
    map-node
    (loop [children (n/children map-node)
           key-position? true
           result []]
      (if-let [child (first children)]
        (if (n/printable-only? child)
          (recur (rest children) key-position? (conj result child))
          (recur (rest children) (not key-position?) (conj result (cond-> child key-position? f))))
        result))))

(defn ^:private key-qualifier
  "Returns the namespace qualifier of a keyword or symbol key node,
  or nil when unqualified or not a keyword/symbol."
  [node]
  (cond
    (n/keyword-node? node)
    (when (or (:auto-resolved? node) (namespace (:k node)))
      {:auto-resolved? (boolean (:auto-resolved? node))
       :prefix (namespace (:k node))})

    (n/symbol-node? node)
    (when-let [prefix (namespace (:value node))]
      {:auto-resolved? false
       :prefix prefix})))

(defn ^:private most-frequent-qualifier [key-nodes]
  (let [qualifiers (keep key-qualifier key-nodes)]
    (when (seq qualifiers)
      (let [freqs (frequencies qualifiers)
            max-count (apply max (vals freqs))]
        (first (filter #(= max-count (get freqs %)) (distinct qualifiers)))))))

(defn ^:private map->namespaced-map-status [map-loc]
  (let [key-nodes (map-key-nodes (z/node map-loc))]
    (when (and
            ;; keys with a `_` namespace can't be represented inside a namespaced map
            (not-any? #(= "_" (:prefix (key-qualifier %))) key-nodes)
            (most-frequent-qualifier key-nodes))
      {:status :from-map-to-namespaced
       :map-loc map-loc})))

(defn ^:private namespaced-map->map-status [map-loc]
  (let [nsmap-node (z/node map-loc)
        qualifier (first (n/children nsmap-node))
        key-nodes (map-key-nodes (last (n/children nsmap-node)))]
    (when (and
            ;; comments between the qualifier and the map would be lost
            (not-any? n/comment? (n/children nsmap-node))
            ;; auto-resolved `_` prefixed keys are too ambiguous to convert safely
            (not-any? #(let [key-qualifier (key-qualifier %)]
                         (and (:auto-resolved? key-qualifier)
                              (= "_" (:prefix key-qualifier))))
                      key-nodes)
            ;; symbols can't be auto-resolved, so `#::b{sym 1}` can't be converted
            (or (not (:auto-resolved? qualifier))
                (not-any? #(and (n/symbol-node? %)
                                (nil? (namespace (:value %))))
                          key-nodes)))
      {:status :from-namespaced-to-map
       :map-loc map-loc})))

(defn cycle-namespaced-map-status [zloc]
  (when-let [map-loc (enclosing-map-loc zloc)]
    (if (= :namespaced-map (z/tag map-loc))
      (namespaced-map->map-status map-loc)
      (map->namespaced-map-status map-loc))))

(defn ^:private qualify-key
  "Transforms a key node for use inside a namespaced map qualified by `target`."
  [target node]
  (let [qualifier (key-qualifier node)]
    (cond
      (not (or (n/keyword-node? node) (n/symbol-node? node)))
      node

      ;; qualified by the namespaced map itself
      (= target qualifier)
      (if (n/keyword-node? node)
        (n/keyword-node (keyword (name (:k node))))
        (n/token-node (symbol (name (:value node)))))

      ;; unqualified keys keep their meaning via the `_` namespace
      (nil? qualifier)
      (if (n/keyword-node? node)
        (n/keyword-node (keyword "_" (name (:k node))))
        (n/token-node (symbol "_" (name (:value node)))))

      :else node)))

(defn ^:private dequalify-key
  "Transforms a key node of a map namespaced by `qualifier` for use in a plain map."
  [qualifier node]
  (cond
    (n/keyword-node? node)
    (let [k (:k node)]
      (cond
        (and (not (:auto-resolved? node)) (nil? (namespace k)))
        (if (:auto-resolved? qualifier)
          (n/keyword-node (if-let [prefix (:prefix qualifier)]
                            (keyword prefix (name k))
                            (keyword (name k)))
                          true)
          (n/keyword-node (keyword (:prefix qualifier) (name k))))

        (and (not (:auto-resolved? node)) (= "_" (namespace k)))
        (n/keyword-node (keyword (name k)))

        :else node))

    (n/symbol-node? node)
    (let [value (:value node)]
      (cond
        (nil? (namespace value)) (n/token-node (symbol (:prefix qualifier) (name value)))
        (= "_" (namespace value)) (n/token-node (symbol (name value)))
        :else node))

    :else node))

(defn ^:private map->namespaced-map [map-loc]
  (let [map-node (z/node map-loc)
        target (most-frequent-qualifier (map-key-nodes map-node))
        new-loc (edit/z-replace-preserving-meta
                  map-loc
                  (n/namespaced-map-node
                    [(n/map-qualifier-node (:auto-resolved? target) (:prefix target))
                     (update-map-keys map-node (partial qualify-key target))]))]
    [{:range (meta (z/node new-loc))
      :loc new-loc}]))

(defn ^:private namespaced-map->map [nsmap-loc]
  (let [nsmap-node (z/node nsmap-loc)
        qualifier (first (n/children nsmap-node))
        map-node (last (n/children nsmap-node))
        new-loc (edit/z-replace-preserving-meta
                  nsmap-loc
                  (update-map-keys map-node (partial dequalify-key qualifier)))]
    [{:range (meta (z/node new-loc))
      :loc new-loc}]))

(defn cycle-namespaced-map [zloc]
  (when-let [{:keys [status map-loc]} (cycle-namespaced-map-status zloc)]
    (if (= :from-namespaced-to-map status)
      (namespaced-map->map map-loc)
      (map->namespaced-map map-loc))))
