(ns sample-test.rename.a
  (:require [clojure.spec.alpha :as s]))

(defn my-func [a b]
  (+ a b))

(my-func 1 2)

(my-func 3 4)

(s/def :my-key 1)

(let [{:keys [:my-key]} {:my-key 1}]
  (+ 1 my-key))

::click

:sample-test.rename.a/click
