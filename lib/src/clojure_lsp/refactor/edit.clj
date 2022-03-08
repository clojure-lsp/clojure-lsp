(ns clojure-lsp.refactor.edit
  (:require
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(defn top? [loc]
  (= :forms (z/tag (z/up loc))))

(defn to-top [loc]
  (when loc
    (if (top? loc)
      loc
      (recur (z/up loc)))))

;; From rewrite-cljs
(defn in-range? [{:keys [row col end-row end-col] :as form-pos}
                 {r :row c :col er :end-row ec :end-col :as selection-pos}]
  (or (nil? form-pos)
      (nil? selection-pos)
      (and (>= r row)
           (<= er end-row)
           (if (= r row) (>= c col) true)
           (if (= er end-row) (< ec end-col) true))))

(defn z-filter
  "Return list of nodes satisfying the given predicate `p?`, moving in direction
  `f` from initial zipper location `zloc`."
  ([zloc f p?]
   (->> zloc
        (iterate f)
        (take-while identity)
        (take-while (complement z/end?))
        (filter p?))))

(defn find-forms
  "Find sexpr-able nodes satisfying the given predicate depth first from initial
  zipper location."
  [zloc p?]
  (z-filter zloc z/next p?))

(defn ^:private zloc-in-range?
  "Checks whether the `loc`s node is [[in-range?]] of the given `pos`."
  [loc pos]
  (some-> loc z/node meta (in-range? pos)))

(defn right-or-up* [zloc]
  (loop [p zloc]
    (or
      (z/right* p)
      (some-> p z/up* recur))))

(defn find-by-heritability
  "Find the deepest zloc from `start-zloc` that satisfies `inherits?`.
  `inherits?` must be a function such that if zloc satisifies it then so will
  all of its ancestors. If a parent node satisifies `inherits?` but none of its
  children do, then this returns the parent, on the assumption that the parent
  is the last in its lineage with the trait.

  This works by scanning right from start-zloc, finding the first ancestor that
  satisifies `inherits?`, descending into that node, and recurring. As such, it
  can be much faster than algorithms based on z/next*, which must inspect all
  children and grandchildren, even if information in the grandparent excludes
  the entirely family.

  If no locs can be found in `start-loc`'s generation, then moves up and to the
  right, assuming that perhaps some other family satisifies `inherits?`."
  [start-zloc inherits?]
  (loop [zloc (cond-> start-zloc
                (= :forms (z/tag start-zloc)) z/down*)]
    (if (z/end? zloc)
      zloc
      (if (inherits? zloc)
        (if-let [inner (some-> zloc z/down* (z/find z/right* inherits?))]
          (recur inner)
          zloc)
        (recur (right-or-up* zloc))))))

(defn find-at-pos
  "Find the deepest zloc whose node is at the given `row` and `col`, seeking
  from initial zipper location `zloc`.

  This is similar to z/find-last-by-pos, but is faster, and doesn't require
  {:track-position? true}."
  [zloc row col]
  (let [exact-position {:row row, :col col, :end-row row, :end-col col}]
    (find-by-heritability zloc #(zloc-in-range? % exact-position))))

(defn find-op
  [zloc]
  (loop [op-loc (or (and (= :list (z/tag zloc))
                         (z/down zloc))
                    (z/leftmost zloc))]
    (let [up-loc (z/up op-loc)]
      (cond
        (nil? up-loc) nil
        (= :list (z/tag up-loc)) op-loc
        :else (recur (z/leftmost up-loc))))))

(defn find-ops-up
  [zloc & op-strs]
  (loop [op-loc (find-op zloc)]
    (cond
      (nil? op-loc)
      nil

      (and (= :token (z/tag op-loc))
           (contains? (set op-strs)
                      (let [sexpr (-> op-loc z/string symbol)]
                        (if (qualified-ident? sexpr)
                          (name sexpr)
                          (str sexpr)))))
      op-loc

      :else
      (recur (z/leftmost (z/up op-loc))))))

(defn var-name-loc-from-op [loc]
  (cond
    (not loc)
    nil

    (= :map (-> loc z/next z/tag))
    (-> loc z/next z/right)

    (and (= :meta (-> loc z/next z/tag))
         (= :map (-> loc z/next z/next z/tag)))
    (-> loc z/next z/down z/rightmost)

    (= :meta (-> loc z/next z/tag))
    (-> loc z/next z/next z/next)

    :else
    (z/next loc)))

(defn find-var-definition-name-loc [loc filename db]
  (when-let [root-loc (to-top loc)]
    (when-let [fn-name-loc (some-> loc to-top z/next var-name-loc-from-op)]
      (let [fn-name-loc-meta (meta (z/node fn-name-loc))
            var-definition-ops (into []
                                     (comp
                                       (filter #(and (identical? :var-definitions (:bucket %))
                                                     (= (:row fn-name-loc-meta) (:name-row %))
                                                     (= (:col fn-name-loc-meta) (:name-col %))))
                                       (keep #(find-at-pos root-loc (:name-row %) (:name-col %))))
                                     (get-in @db [:analysis filename]))]
        (when (seq var-definition-ops)
          fn-name-loc)))))

(defn find-function-usage-name-loc [zloc]
  (some-> zloc
          (z/find-tag z/up :list)
          z/down))

(defn single-child?
  [zloc]
  (let [child (z/down zloc)]
    (and child
         (z/leftmost? child)
         (z/rightmost? child))))

;; from rewrite-cljs
(defn raise
  "Delete siblings and raise node at zloc one level up
  - `[1 [2 |3 4]] => [1 |3]`"
  [zloc]
  (if-let [containing (z/up zloc)]
    (-> containing
        (z/replace (z/node zloc)))
    zloc))

(defn map-children [parent-zloc f]
  (if (z/down parent-zloc)
    (z/subedit->> parent-zloc
                  z/down
                  (iterate (fn [zloc]
                             (when (not (z/end? zloc))
                               (-> zloc f z/next))))
                  (take-while identity)
                  last)
    parent-zloc))

(defn wrap-around [zloc tag]
  (let [node (z/node zloc)
        node-meta (meta node)]
    (-> zloc
        (z/replace (-> (case tag
                         :list (n/list-node [])
                         :vector (n/vector-node [])
                         :set (n/set-node [])
                         :map (n/map-node []))
                       (with-meta node-meta)))
        (z/insert-child node))))

(defn parent-let? [zloc]
  (let [parent-op (-> zloc z/leftmost)]
    (when (= 'let (-> parent-op z/sexpr))
      (z/up parent-op))))

(defn join-let
  "if a let is directly above a form, will join binding forms and remove the inner let"
  [let-loc]
  (if (parent-let? let-loc)
    (let [bind-node (z/node (z/right (z/down let-loc)))]
      (-> let-loc
          (z/down)
          (z/right) ; move to inner binding
          (z/remove) ; remove inner binding
          (z/remove) ; remove inner let moving to prev; the surrounding list
          (z/splice) ; splice let body into outer let body
          (z/leftmost) ; move to let
          (z/right) ; move to parent binding
          (z/append-child bind-node) ; place into binding
          (z/down) ; move into binding
          (z/rightmost) ; move to nested binding
          (z/splice) ; remove nesting
          z/left
          (z/insert-right* (n/newlines 1))
          (z/up) ; move to new binding
          (z/up))) ; move to let-form
    let-loc))

(defn inside-require? [zloc]
  (or (and (find-ops-up zloc "ns")
           (find-ops-up zloc ":require"))
      (find-ops-up zloc "require")))

(defn inside-refer? [zloc]
  (and (inside-require? zloc)
       (or (and (= :vector (z/tag zloc))
                (= :refer (-> zloc z/left z/sexpr)))
           (and (= :token (z/tag zloc))
                (= :refer (-> zloc z/up z/left z/sexpr))))))

(defn find-refer-ns [zloc]
  (when (inside-refer? zloc)
    (if (= :vector (z/tag zloc))
      (z/leftmost zloc)
      (z/leftmost (z/up zloc)))))

(defn find-namespace [zloc]
  (-> zloc
      (z/find z/up top?)
      (z/leftmost)
      (z/find-value z/next 'ns) ; go to ns
      (z/up))) ; ns form

(defn find-namespace-name [zloc]
  (some-> zloc
          find-namespace
          z/down
          z/next
          z/sexpr
          str))

(defn back-to-mark-or-nil
  [zloc marker]
  (z/find zloc z/prev (fn [loc] (contains? (get (z/node loc) ::markers) marker))))

(defn mark-position
  [zloc marker]
  (z/replace zloc (update (z/node zloc) ::markers (fnil conj #{}) marker)))

(defn mark-position-when
  [zloc marker p?]
  (if p?
    (mark-position zloc marker)
    zloc))
