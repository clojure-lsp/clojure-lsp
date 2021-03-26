(ns rename.b
  (:require [rename.a :as a :refer [my-func]]))

(a/my-func 5 6)

(my-func 7 8)

::a/click

:rename.a/click
