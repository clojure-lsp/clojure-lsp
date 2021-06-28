(ns integration.api.clean-ns-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [integration.lsp :as lsp]))

(lsp/clean-after-test)

(def a-subject-path "./integration-test/sample-test/src/api/clean_ns/a.clj")
(def a-subject-text (slurp a-subject-path))
(def a-expected-path "./integration-test/sample-test/fixtures/api/clean_ns/a.clj")
(def a-expected-text (slurp a-expected-path))

(deftest clean-ns
  (testing "passing a single namespace"
    (with-open [rdr (lsp/cli! "clean-ns"
                              "--project-root" "./integration-test/sample-test"
                              "--namespace" "api.clean-ns.a")]
      (is (= "Cleaned api.clean-ns.a\n" (slurp rdr)))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "passing multiple namespaces but only one is cleanable"
    (with-open [rdr (lsp/cli! "clean-ns"
                              "--project-root" "./integration-test/sample-test"
                              "--namespace" "api.clean-ns.b"
                              "--namespace" "api.clean-ns.a")]
      (is (= "Cleaned api.clean-ns.a\n" (slurp rdr)))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text)))
