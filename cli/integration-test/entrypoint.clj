(ns entrypoint
  (:require
   [clojure.test :as t]
   [clojure.java.shell :as sh]))

(def namespaces
  '[
    integration.initialize-test
    integration.definition-test
    integration.declaration-test
    integration.implementation-test
    integration.code-action-test
    integration.completion-test
    integration.diagnostics-test
    integration.settings-change-test
    integration.formatting-test
    integration.rename-test
    integration.document-highlight-test
    integration.document-symbol-test
    integration.linked-editing-range-test
    integration.cursor-info-test
    integration.java-interop-test
    integration.stubs-test
    integration.api.version-test
    integration.api.clean-ns-test
    integration.api.diagnostics-test
    integration.api.format-test
    integration.api.rename-test
    ])

(defn timeout [timeout-ms callback]
  (let [fut (future (callback))
        ret (deref fut timeout-ms :timed-out)]
    (when (= ret :timed-out)
      (future-cancel fut))
    ret))

(defn log-tail []
  (:out (sh/sh "tail" "-n" "300" "clojure-lsp.integration-test.out" :dir "integration-test/sample-test/")))

(defn print-log-tail! []
  (binding [*out* *err*]
    (println "--- RECENT LOG OUTPUT ---")
    (print (log-tail))
    (println "--- END RECENT LOG OUTPUT ---")))

(declare ^:dynamic original-report)

(defn log-tail-report [data]
  (when (contains? #{:fail :error} (:type data))
    (print-log-tail!))
  (original-report data))

(defmacro with-log-tail-report
  "Execute body with modified test reporting functions that prints log tail on failure."
  [& body]
  `(binding [original-report t/report
             t/report log-tail-report]
     ~@body))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn run-all [& args]
  (when-not (first args)
    (println "First arg must be path to clojure-lsp binary")
    (System/exit 0))

  (apply require namespaces)

  (let [test-results (timeout 600000
                              #(with-log-tail-report
                                 (apply t/run-tests namespaces)))]

    (when (= test-results :timed-out)
      (print-log-tail!)
      (println)
      (println "Timeout running integration tests!")
      (System/exit 1))

    (let [{:keys [fail error]} test-results]
      (System/exit (+ fail error)))))
