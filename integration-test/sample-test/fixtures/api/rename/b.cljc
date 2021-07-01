(ns rename.b
  (:require [rename.a :as a :refer [your-func]]))

(a/your-func 5 6)

(your-func 7 8)

::a/click

:rename.a/click
