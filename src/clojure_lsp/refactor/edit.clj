(ns clojure-lsp.refactor.edit
  (:require
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn top? [loc]
  (= :forms (z/tag (z/up loc))))

(defn to-top [loc]
  (if (top? loc)
    loc
    (recur (z/up loc))))

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
      (nil? op-loc) nil
      (contains? (set op-strs) (z/string op-loc)) op-loc
      :else (recur (z/leftmost (z/up op-loc))))))

(def function-definition-symbols
  '#{defn defn- def defmacro defmulti defonce deftype defrecord s/def s/defn})

(defn find-function-definition-name-loc [loc]
  (let [function-loc (apply find-ops-up loc (mapv str function-definition-symbols))]
    (cond
      (not function-loc)
      nil

      (= :map (-> function-loc z/next z/tag))
      (-> function-loc z/next z/right)

      (and (= :meta (-> function-loc z/next z/tag))
           (= :map (-> function-loc z/next z/next z/tag)))
      (-> function-loc z/next z/down z/rightmost)

      (= :meta (-> function-loc z/next z/tag))
      (-> function-loc z/next z/next z/next)

      :else
      (z/next function-loc))))

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
