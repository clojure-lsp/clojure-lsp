(ns rewrite-clj.zip.subedit
  (:require [rewrite-clj.zip :as z]
            [rewrite-clj.zip.base :as base]))

(defn subzip
  "Create zipper whose root is the current node."
  [zloc]
  (let [zloc' (some-> zloc z/node base/edn*)]
    (assert zloc' "could not create subzipper.")
    zloc'))
