#!/usr/bin/env bb

(require '[clojure.test :as t]
         '[babashka.classpath :as cp])

(cp/add-classpath "integration-test")

(def namespaces
  '[integration.common
    integration.initialize])

(apply require namespaces)

(def test-results (apply t/run-tests namespaces))

(def failures-and-errors
  (let [{:keys [fail error]} test-results]
    (+ fail error)))

(System/exit failures-and-errors)
