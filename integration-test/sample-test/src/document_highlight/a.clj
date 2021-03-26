(ns document-highlight.a
  (:require
    [clojure.string :as str]))

(defn my-func [a b]
  (+ a b))

(my-func 1 2)

(my-func 3 4)

(str/join "," "")

(let [my-func #(+ 1 %)]
  (my-func 2))
