(ns integration.api.clean-ns-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(def a-subject-path (h/project-path->canon-path "src/sample_test/api/clean_ns/a.clj"))
(def a-subject-text (slurp a-subject-path))
(def a-expected-path (h/project-path->canon-path "fixtures/sample_test/api/clean_ns/a.clj"))
(def a-expected-text (slurp a-expected-path))

(deftest clean-ns
  (testing "passing a single namespace"
    (with-open [rdr (lsp/cli! "clean-ns"
                              "--project-root" h/root-project-path
                              "--namespace" "sample-test.api.clean-ns.a")]
      (is (h/string= "Cleaned 1 namespaces\n" (slurp rdr)))
      (is (h/string= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "passing multiple namespaces but only one is cleanable"
    (with-open [rdr (lsp/cli! "clean-ns"
                              "--project-root" h/root-project-path
                              "--namespace" "sample-test.api.clean-ns.b"
                              "--namespace" "sample-test.api.clean-ns.a")]
      (is (h/string= "Cleaned 1 namespaces\n" (slurp rdr)))
      (is (h/string= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "when running with dry"
    (with-open [rdr (lsp/cli! "clean-ns"
                              "--project-root" h/root-project-path
                              "--namespace" "sample-test.api.clean-ns.a"
                              "--dry")]
      (is (string/includes? (slurp rdr) (h/file-path "src/sample_test/api/clean_ns/a.clj")))
      (is (h/string= a-subject-text (slurp a-subject-path))))))
