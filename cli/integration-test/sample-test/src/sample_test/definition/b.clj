(ns sample-test.definition.b
  (:require [sample-test.definition.a :as a]))

(a/some-public-func 2)

:my-key

::my-key

:sample-test.definition.a/my-key

::a/my-key
