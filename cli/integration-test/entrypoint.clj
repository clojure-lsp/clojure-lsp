(ns entrypoint
  (:require
   [clojure.java.shell :as sh]
   [clojure.test :as t]
   [medley.core :as medley]))

(def namespaces
  '[integration.initialize-test
    integration.definition-test
    integration.declaration-test
    integration.implementation-test
    integration.text-change-test
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
    integration.classpath-test
    integration.api.version-test
    integration.api.clean-ns-test
    integration.api.diagnostics-test
    integration.api.format-test
    integration.api.rename-test
    integration.api.dump-test])

(defn timeout [timeout-ms callback]
  (let [fut (future (callback))
        ret (deref fut timeout-ms :timed-out)]
    (when (= ret :timed-out)
      (future-cancel fut))
    ret))

(defn log-tail [file lines]
  (:out (sh/sh "tail" "-n" (str lines) file :dir "integration-test/sample-test/")))

(def first-print-log-tail?* (atom true))

(defn print-log-tail! []
  (when (medley/deref-reset! first-print-log-tail?* false)
    (binding [*out* *err*]
      (println "--- RECENT LOG OUTPUT ---")
      (print (log-tail "clojure-lsp.integration-test.out" 300))
      (println "--- END RECENT LOG OUTPUT ---"))))

(declare ^:dynamic original-report)

(defn log-tail-report [data]
  (original-report data)
  (when (contains? #{:fail :error} (:type data))
    (print-log-tail!)))

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

  (let [timeout-minutes (if (re-find #"(?i)win|mac" (System/getProperty "os.name"))
                          20 ;; win and mac ci runs take longer
                          12)
        test-results (timeout (* timeout-minutes 60 1000)
                              #(with-log-tail-report
                                 (apply t/run-tests namespaces)))]

    (when (= test-results :timed-out)
      (print-log-tail!)
      (println)
      (println (format "Timeout after %d minutes running integration tests!" timeout-minutes))
      (System/exit 1))

    (let [{:keys [fail error]} test-results]
      (System/exit (+ fail error)))))
