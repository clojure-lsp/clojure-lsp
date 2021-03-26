(ns document-symbol.a
  (:require [clojure.spec.alpha :as s]))

(def a-some-var 1)

(defn a-some-public-function [a b]
  (+ a b))

(s/def ::a-my-key 2)

(let [a 1 b 2]
  (a-some-public-function a b))
