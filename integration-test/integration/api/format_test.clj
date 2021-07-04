(ns integration.api.format-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [integration.lsp :as lsp]
    [clojure.string :as string]))

(lsp/clean-after-test)

(def a-subject-path "integration-test/sample-test/src/api/format/a.clj")
(def a-subject-text (slurp a-subject-path))
(def a-expected-path "integration-test/sample-test/fixtures/api/format/a.clj")
(def a-expected-text (slurp a-expected-path))

(deftest format
  (testing "passing a single namespace"
    (with-open [rdr (lsp/cli! "format"
                              "--project-root" "./integration-test/sample-test"
                              "--namespace" "api.format.a")]
      (is (string/includes? (slurp rdr) "Formatted api.format.a\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "passing multiple namespaces but only one is formatable"
    (with-open [rdr (lsp/cli! "format"
                              "--project-root" "./integration-test/sample-test"
                              "--namespace" "api.format.b"
                              "--namespace" "api.format.a")]
      (is (string/includes? (slurp rdr) "Formatted api.format.a\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "when running with dry"
    (with-open [rdr (lsp/cli! "format"
                              "--project-root" "./integration-test/sample-test"
                              "--namespace" "api.format.a"
                              "--dry")]
      (is (string/includes? (slurp rdr) a-subject-path))
      (is (= a-subject-text (slurp a-subject-path))))))
