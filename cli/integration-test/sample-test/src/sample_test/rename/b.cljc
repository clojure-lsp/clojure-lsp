(ns sample-test.rename.b
  (:require [sample-test.rename.a :as a :refer [my-func]]))

(a/my-func 5 6)

(my-func 7 8)

::a/click

:sample-test.rename.a/click

(def my-def 1)
