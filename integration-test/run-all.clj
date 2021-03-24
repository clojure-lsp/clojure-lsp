#!/usr/bin/env bb

(when-not (first *command-line-args*)
  (println "First arg must be path to clojure-lsp binary")
  (System/exit 0))

(require '[clojure.test :as t]
         '[babashka.classpath :as cp]
         '[clojure.java.io :as io])

(cp/add-classpath (-> *file* io/file .getParent))

(require '[integration.helper])

(def namespaces
  '[integration.initialize-test])

(defn timeout [timeout-ms callback]
  (let [fut (future (callback))
        ret (deref fut timeout-ms :timed-out)]
    (when (= ret :timed-out)
      (future-cancel fut))
    ret))

(apply require namespaces)

(def test-results
  (timeout 120000
    #(apply t/run-tests namespaces)))

(when (= test-results :timed-out)
  (println "Timeout running integration tests!")
  (System/exit 1))

(def failures-and-errors
  (let [{:keys [fail error]} test-results]
    (+ fail error)))

(System/exit failures-and-errors)
