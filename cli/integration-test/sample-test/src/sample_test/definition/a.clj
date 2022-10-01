(ns sample-test.definition.a
  (:require [clojure.spec.alpha :as s]))

(def some-var 1)

(defn some-public-func [a]
  a)

(defn ^:private some-private-func []
  (+ 1 some-var))

(some-private-func)

(s/def ::my-key 1)

(cond-> [])
