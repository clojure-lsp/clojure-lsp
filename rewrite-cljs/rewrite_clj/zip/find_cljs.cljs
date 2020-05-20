(ns rewrite-clj.zip.find-cljs
  (:refer-clojure :exclude [find])
  (:require [rewrite-clj.zip.move :as m]
            [rewrite-clj.zip.base :as base]))

(defn find-z
  "Find node satisfying the given predicate by repeatedly
   applying the given movement function to the initial zipper
   location."
  ([zloc p?]
   (find-z zloc m/right p?))
  ([zloc f p?]
   (->> zloc
        (iterate f)
        (take-while identity)
        (take-while (complement m/end?))
        (drop-while (complement p?))
        (first))))

(defn find-tag
  "Find node with the given tag by repeatedly applying the given
   movement function to the initial zipper location."
  ([zloc t]
   (find-tag zloc m/right t))
  ([zloc f t]
   (find-z zloc f #(= (base/tag %) t))))
