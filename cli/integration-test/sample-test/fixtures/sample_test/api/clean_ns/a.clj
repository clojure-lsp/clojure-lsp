(ns sample-test.api.clean-ns.a
  (:require
   [sample-test.api.clean-ns.b :refer [a b c]]))

a
b
c

(ns sample-test.api.clean-ns.a.other)
