(ns clojure-lsp.feature.thread-get
  (:require
   [clojure-lsp.refactor.edit :as edit]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn ^:private z-children [zloc]
  (->> zloc
       z/down
       (iterate z/right)
       (take-while (complement z/end?))
       (filter z/sexpr-able?)))

(defn ^:private z-of-list-locs [children]
  (->> children
       (map z/node)
       (reduce z/append-child
               (z/of-node (n/list-node [])))))

(defn ^:private get-in-more-data [zloc]
  (when (z/list? zloc)
    (let [[op-loc & more-locs] (z-children zloc)
          op-sexpr (some-> op-loc z/sexpr)]
      (cond
        (contains? '#{get-in get} op-sexpr)
        , (let [[map-loc key-or-path-loc default-loc & more-locs] more-locs]
            (when (and (not (seq more-locs))
                       (some-> map-loc z/list?)
                       key-or-path-loc)
              (let [[new-key-loc new-map-loc more-arg-locs] (z-children map-loc)]
                (when (and (not (seq more-arg-locs)) new-key-loc new-map-loc)
                  {:replace-with (z/of-node 'get-in)
                   :map-loc new-map-loc
                   :key-or-path-loc (cond-> key-or-path-loc
                                      (not (z/vector? key-or-path-loc)) (edit/wrap-around :vector)
                                      true (z/insert-child (z/node new-key-loc)))
                   :default-loc default-loc}))))
        (not (nil? op-sexpr))
        , (let [key-loc op-loc
                [map-loc default-loc & more-locs] more-locs]
            (when (and (not (seq more-locs)) key-loc map-loc)
              {:replace-with (z/of-node 'get)
               :map-loc map-loc
               :key-or-path-loc key-loc
               :default-loc default-loc}))))))

(defn ^:private get-in-more* [{:keys [replace-with map-loc key-or-path-loc default-loc]}]
  (let [interior (cond-> [replace-with map-loc key-or-path-loc]
                   default-loc (conj default-loc))]
    (z-of-list-locs interior)))

(defn ^:private get-in-all* [zloc]
  (if-let [data (get-in-more-data zloc)]
    (recur (get-in-more* data))
    zloc))

(defn ^:private get-in-less-data [zloc]
  (when (z/list? zloc)
    (let [[op-loc map-loc key-or-path-loc default-loc & more-locs] (z-children zloc)]
      (when-not (seq more-locs)
        (when-let [{:keys [key-loc], :as data}
                   (case (some-> op-loc z/sexpr)
                     get-in
                     , (when (z/vector? key-or-path-loc)
                         (when-let [new-key-loc (z/down key-or-path-loc)]
                           (let [new-path-loc (z/remove new-key-loc)
                                 data (case (count (z-children new-path-loc))
                                        0 {:replace-with :key}
                                        1 {:replace-with (z/of-node 'get)
                                           :path-loc (z/down new-path-loc)}
                                        , {:replace-with (z/of-node 'get-in)
                                           :path-loc new-path-loc})]
                             (assoc data :key-loc new-key-loc))))
                     get
                     , {:replace-with :key
                        :key-loc key-or-path-loc}
                     nil)]
          (when (or (not default-loc)
                    ;; Check whether arity-3 (get/get-in with default-value) is
                    ;; being converted to symbol/keyword IFn arity-2
                    (n/keyword-node? (z/node key-loc))
                    (and (= :quote (z/tag key-loc))
                         (= :token (z/tag (z/down key-loc)))))
            (assoc data
                   :map-loc map-loc
                   :default-loc default-loc)))))))

(defn ^:private get-in-less* [{:keys [replace-with map-loc key-loc path-loc default-loc]}]
  (let [interior (if (= :key replace-with)
                   [key-loc map-loc]
                   [replace-with (z-of-list-locs [key-loc map-loc]) path-loc])
        interior (cond-> interior default-loc (conj default-loc))]
    (z-of-list-locs interior)))

(defn ^:private get-in-none* [zloc]
  (if-let [data (get-in-less-data zloc)]
    (recur (get-in-less* data))
    zloc))

(defn ^:private edits [original-zloc new-zloc]
  [{:range (meta (z/node original-zloc))
    :loc new-zloc}])

(defn can-get-in-more? [zloc] (boolean (get-in-more-data zloc)))
(defn get-in-more [zloc] (some->> zloc get-in-more-data get-in-more* (edits zloc)))
(defn get-in-all [zloc] (->> zloc get-in-all* (edits zloc)))

(defn can-get-in-less? [zloc] (boolean (get-in-less-data zloc)))
(defn get-in-less [zloc] (some->> zloc get-in-less-data get-in-less* (edits zloc)))
(defn get-in-none [zloc] (->> zloc get-in-none* (edits zloc)))
