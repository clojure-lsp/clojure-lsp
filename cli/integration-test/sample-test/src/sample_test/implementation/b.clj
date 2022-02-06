(ns sample-test.implementation.b
  (:require [sample-test.implementation.a :as a]))

(defrecord FooImpl1 []
  a/Foo
  (something [_this]
    123))

(defrecord FooImpl2 []
  a/Foo
  (^void something [_this]
    123
    456))

(a/something (->FooImpl1))

(a/something (->FooImpl2))
