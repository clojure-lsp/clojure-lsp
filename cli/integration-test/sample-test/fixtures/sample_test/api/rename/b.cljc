(ns sample-test.rename.b
  (:require [sample-test.rename.a :as a :refer [your-func]]))

(a/your-func 5 6)

(your-func 7 8)

::a/click

:sample-test.rename.a/click

(def my-def 1)
