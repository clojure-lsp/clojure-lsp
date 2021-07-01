(ns rename.a
  (:require [clojure.spec.alpha :as s]))

(defn your-func [a b]
  (+ a b))

(your-func 1 2)

(your-func 3 4)

(s/def :my-key 1)

(let [{:keys [:my-key]} {:my-key 1}]
  (+ 1 my-key))

::click

:rename.a/click
