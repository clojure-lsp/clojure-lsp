(ns my-org.my-command
  (:require
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn get-pos [zloc]
  (-> zloc z/node meta))

(defn join-positions
  [{:keys [col row]}
   {:keys [end-col end-row]}]
  {:col col
   :row row
   :end-col end-col
   :end-row end-row})

(defn skip-left-whitespace [zloc]
  (loop [zloc zloc]
    (if (n/whitespace? (z/node (z/prev* zloc)))
      (recur (z/prev* zloc))
      zloc)))

(defn vector-node? [node]
  (= :vector (n/tag node)))

(defn maybe-here-it-reloads [loc uri db]
  (let [referred-sym (z/sexpr loc)
        alias (z/sexpr (z/next (z/find-value (z/up (z/up loc)) z/next :as)))
        fq-sym (symbol (name alias) (name referred-sym))
        zloc-after-ns (z/right (z/up (z/up (z/up (z/up loc)))))
        vector-zloc (let [prev-zloc (z/remove* loc)]
                      (if (not (vector-node? (z/node prev-zloc)))
                        (z/up prev-zloc)
                        prev-zloc))
        trim-refer-pos (if (every? n/whitespace? (n/children (z/node vector-zloc)))
                         (join-positions
                           (-> (z/find-value vector-zloc z/prev :refer)
                               (skip-left-whitespace)
                               (get-pos))
                           (get-pos vector-zloc))
                         (let [wzloc (skip-left-whitespace loc)]
                           (join-positions
                             (get-pos wzloc)
                             (get-pos loc))))]
    (concat [{:range trim-refer-pos
              :new-text ""}]
            (loop [zloc zloc-after-ns
                   changes []]
              (if-let [new-zloc (z/find-value zloc z/next referred-sym)]
                (recur (z/next new-zloc)
                       (conj changes {:range (meta (z/node new-zloc))
                                      :loc (z/of-string (str fq-sym))}))
                changes)))))

(defn underef [{:keys [loc uri db]}]
  (maybe-here-it-reloads loc uri db))
