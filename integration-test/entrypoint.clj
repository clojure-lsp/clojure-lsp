(ns entrypoint
  (:require
   [clojure.test :as t]))

(def namespaces
  '[integration.initialize-test
    integration.definition-test
    integration.diagnostics-test
    integration.formatting-test
    integration.rename-test
    integration.document-highlight-test
    integration.document-symbol-test
    integration.api.version-test
    integration.api.clean-ns-test
    integration.api.format-test
    integration.api.rename-test])

(defn timeout [timeout-ms callback]
  (let [fut (future (callback))
        ret (deref fut timeout-ms :timed-out)]
    (when (= ret :timed-out)
      (future-cancel fut))
    ret))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn run-all [& args]
  (when-not (first args)
    (println "First arg must be path to clojure-lsp binary")
    (System/exit 0))

  (apply require namespaces)

  (let [test-results (timeout 600000
                              #(apply t/run-tests namespaces))]

    (when (= test-results :timed-out)
      (println "Timeout running integration tests!")
      (System/exit 1))

    (let [{:keys [fail error]} test-results]
      (System/exit (+ fail error)))))
