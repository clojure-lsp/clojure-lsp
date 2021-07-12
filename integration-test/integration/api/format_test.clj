(ns integration.api.format-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(def a-subject-path (h/path->abs-path "integration-test/sample-test/src/api/format/a.clj"))
(def a-subject-text (slurp a-subject-path))
(def a-expected-path (h/path->abs-path "integration-test/sample-test/fixtures/api/format/a.clj"))
(def a-expected-text (slurp a-expected-path))

(deftest format-test
  (testing "passing a single namespace"
    (with-open [rdr (lsp/cli! "format"
                              "--project-root" h/root-project-path
                              "--namespace" "api.format.a")]
      (is (string/includes? (slurp rdr) "Formatted api.format.a\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "passing multiple namespaces but only one is formatable"
    (with-open [rdr (lsp/cli! "format"
                              "--project-root" h/root-project-path
                              "--namespace" "api.format.b"
                              "--namespace" "api.format.a")]
      (is (string/includes? (slurp rdr) "Formatted api.format.a\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "when running with dry"
    (with-open [rdr (lsp/cli! "format"
                              "--project-root" h/root-project-path
                              "--namespace" "api.format.a"
                              "--dry")]
      (is (string/includes? (slurp rdr) a-subject-path))
      (is (= a-subject-text (slurp a-subject-path))))))
