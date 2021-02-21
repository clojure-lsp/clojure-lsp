(ns clojure-lsp.refactor.edit
  (:require
   [rewrite-clj.custom-zipper.core :as cz]
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
  [zloc & op-syms]
  (loop [op-loc (find-op zloc)]
    (cond
      (nil? op-loc) nil
      (contains? (set op-syms) (z/sexpr op-loc)) op-loc
      :else (recur (z/leftmost (z/up op-loc))))))

(def var-definition-macros
  '#{defn
     defn-
     def
     defmacro
     defmulti
     defonce
     deftype
     defrecord
     s/def
     s/defn})

(defn find-function-definition-name [loc]
  (let [function-loc (apply find-ops-up loc var-definition-macros)]
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

(defn find-function-usage-name [zloc]
  (when-let [function-form (z/find-tag zloc z/up :list)]
    (-> function-form
        z/down)))

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
  (z/subedit->> parent-zloc
                z/down
                (iterate (fn [zloc]
                           (when (not (z/end? zloc))
                             (-> zloc f z/next))))
                (take-while identity)
                last))

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

(defn wrap-meta [zloc metadata]
  (let [node (z/node zloc)
        node-meta (meta node)]
    (-> zloc
        (z/replace (-> (n/meta-node (z/node metadata) (z/sexpr zloc))
                       (with-meta node-meta))))))

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
          (cz/insert-right (n/newlines 1))
          (z/up) ; move to new binding
          (z/up))) ; move to let-form
    let-loc))

(defn inside? [zloc possible-parent]
  (z/find zloc z/up (fn [loc]
                      (= possible-parent (z/up loc)))))


(defn inside-require? [zloc]
  (or (and (find-ops-up zloc 'ns)
           (find-ops-up zloc :require))
      (find-ops-up zloc 'require)))

(defn skip-over [loc]
  (if (z/down loc)
    (->> loc
         z/down
         z/rightmost
         (z/skip z/up z/rightmost?)
         z/right)
    (z/next loc)))

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

(defn back-to-mark
  [zloc marker]
  (if-let [mloc (back-to-mark-or-nil zloc marker)]
    mloc
    zloc))

(defn mark-position
  [zloc marker]
  (z/replace zloc (update (z/node zloc) ::markers (fnil conj #{}) marker)))

(defn mark-position-when
  [zloc marker p?]
  (if p?
    (mark-position zloc marker)
    zloc))
