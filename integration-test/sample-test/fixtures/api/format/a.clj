(ns api.format.a
  (:require
   [api.format.b :refer [b  c a]]))

a       b
c

(defn ^:private  foo
  [a
   b]
  (+ 2 4))

(ns api.format.a.other)
