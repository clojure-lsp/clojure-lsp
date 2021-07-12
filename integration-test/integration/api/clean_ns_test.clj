(ns integration.api.clean-ns-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(def a-subject-path (h/project-path->abs-path "src/api/clean_ns/a.clj"))
(def a-subject-text (slurp a-subject-path))
(def a-expected-path (h/project-path->abs-path "fixtures/api/clean_ns/a.clj"))
(def a-expected-text (slurp a-expected-path))

(deftest clean-ns
  (testing "passing a single namespace"
    (with-open [rdr (lsp/cli! "clean-ns"
                              "--project-root" h/root-project-path
                              "--namespace" "api.clean-ns.a")]
      (is (string/includes? (slurp rdr) "Cleaned api.clean-ns.a\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "passing multiple namespaces but only one is cleanable"
    (with-open [rdr (lsp/cli! "clean-ns"
                              "--project-root" h/root-project-path
                              "--namespace" "api.clean-ns.b"
                              "--namespace" "api.clean-ns.a")]
      (is (string/includes? (slurp rdr) "Cleaned api.clean-ns.a\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "when running with dry"
    (with-open [rdr (lsp/cli! "clean-ns"
                              "--project-root" h/root-project-path
                              "--namespace" "api.clean-ns.a"
                              "--dry")]
      (is (string/includes? (slurp rdr) a-subject-path))
      (is (= a-subject-text (slurp a-subject-path))))))
