(ns sample-test.api.clean-ns.a
  (:require
   [clojure.string :as string]
   [sample-test.api.clean-ns.b :refer [b c a]]))

a
b
c

(ns sample-test.api.clean-ns.a.other)
