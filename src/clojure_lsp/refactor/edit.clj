(ns clojure-lsp.refactor.edit
  (:require
   [rewrite-clj.node :as n]
   [rewrite-clj.custom-zipper.core :as cz]
   [rewrite-clj.zip :as z]
   [clojure.tools.logging :as log]))

(defmacro zspy [loc]
  `(do
     (log/warn '~loc (pr-str (z/string ~loc)))
     ~loc))

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
          (cz/insert-right (n/newlines 1))
          (z/up) ; move to new binding
          (z/up))) ; move to let-form
    let-loc))

(defn inside? [zloc possible-parent]
  (z/find zloc z/up (fn [loc]
                      (= possible-parent (z/up loc)))))

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

(comment
 (z/sexpr (find-namespace (z/rightmost (z/of-string "(ns foo) (a)"))))

 (defn remove-left [zloc]
   (-> zloc
       (zu/remove-left-while ws/whitespace?)
       (zu/remove-left-while (complement ws/whitespace?))))

 (defn transpose-with-right
   [zloc]
   (if (z/rightmost? zloc)
     zloc
     (let [right-node (z/node (z/right zloc))]
       (-> zloc
           (remove-right)
           (z/insert-left right-node)))))

 (defn transpose-with-left
   [zloc]
   (if (z/leftmost? zloc)
     zloc
     (let [left-node (z/node (z/left zloc))]
       (-> zloc
           (z/left)
           (transpose-with-right)))))


  ;; TODO this can probably escape the ns form - need to root the search it somehow (z/.... (z/node zloc))
 (defn find-or-create-libspec [zloc v]
   (if-let [zfound (z/find-next-value zloc z/next v)]
     zfound
     (-> zloc
         (z/append-child (n/newline-node "\n"))
         (z/append-child (list v))
         z/down
         z/rightmost
         z/down
         z/down)))

 (defn remove-children
   [zloc]
   (if (z/seq? zloc)
     (z/replace zloc (n/replace-children (z/node zloc) []))
     zloc))

 (defn remove-all-after
   [zloc]
   (loop [loc (zu/remove-right-while (remove-children zloc) (constantly true))]
     (if-let [uploc (z/up loc)]
       (recur (zu/remove-right-while uploc (constantly true)))
       loc)))

 (defn read-position
   [old-pos zloc offset]
   (let [n (-> zloc
               (remove-all-after)
               (z/root-string)
               (z/of-string)
               (z/rightmost)
               (z/find-next-depth-first (comp z/end? z/next)))]
     (if n
       (-> n
           (z/node)
           (meta)
           ((juxt :row (comp (partial + offset) :col))))
       old-pos)))

 (defn mark-position
   [zloc marker]
   (z/replace zloc (update (z/node zloc) ::markers (fnil conj #{}) marker)))

 (defn find-mark-or-nil
   [zloc marker]
   (z/find (to-first-top zloc) z/next (fn [loc] (contains? (get (z/node loc) ::markers) marker))))

 (defn find-mark
   [zloc marker]
   (if-let [mloc (find-mark-or-nil zloc marker)]
     mloc
     zloc))

 (defn remove-mark
   [zloc marker]
   (z/replace zloc (update (z/node zloc) ::markers disj marker)))

 (defn find-first-sexpr
   [zloc search-sexpr]
   (-> zloc
       (to-first-top)
       (z/find z/next #(= (z/sexpr %) search-sexpr))))

 (defn replace-all-sexpr
   [zloc sexpr def-name mark?]
   (if-let [found-loc (find-first-sexpr zloc sexpr)]
     (let [new-loc (if mark?
                     (mark-position (z/replace found-loc def-name) :new-cursor)
                     (z/replace found-loc def-name))]
       (recur (mark-position new-loc :reformat) sexpr def-name false))
     zloc))

 (defn format-form
   [zloc]
   (let [expr-loc (to-top zloc)
         formatted-node (fmt/reformat-form (z/node expr-loc) {})]
     (z/replace expr-loc formatted-node)))

 (defn format-all
   [zloc]
   (loop [top-loc (to-first-top zloc)]
     (let [formatted (format-form top-loc)]
       (if (z/rightmost? formatted)
         formatted
         (recur (z/right formatted))))))

 (defn format-marked
   [zloc]
   (let [floc (find-mark-or-nil (to-first-top zloc) :reformat)]
     (cond
       floc (recur (z/replace floc (fmt/reformat-form (z/node (remove-mark floc :reformat)) {})))
       :else zloc))))
