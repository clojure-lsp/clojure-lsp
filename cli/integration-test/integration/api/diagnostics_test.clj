(ns integration.api.diagnostics-test
  (:require
   [clojure.edn :as edn]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest diagnostics
  (testing "passing a single namespace"
    (with-open [rdr (lsp/cli! "diagnostics"
                              "--project-root" h/root-project-path
                              "--namespace" "sample-test.api.diagnostics.a")]
      (is (string/includes? (slurp rdr) (format "%s:2:0: error: [unresolved-symbol] Unresolved symbol: some-unknown-var"
                                                (h/file-path "src/sample_test/api/diagnostics/a.clj"))))))
  (testing "output format edn"
    (with-open [rdr (lsp/cli! "diagnostics"
                              "--project-root" h/root-project-path
                              "--output" "{:format :edn}"
                              "--namespace" "sample-test.api.diagnostics.a")]
      (is (seq (edn/read-string (slurp rdr))))))
  (testing "passing multiple namespaces but only one has diagnostics"
    (with-open [rdr (lsp/cli! "diagnostics"
                              "--project-root" h/root-project-path
                              "--namespace" "sample-test.api.diagnostics.b"
                              "--namespace" "sample-test.api.diagnostics.a")]
      (let [result (slurp rdr)]
        (is (string/includes? result (format "%s:2:0: error: [unresolved-symbol] Unresolved symbol: some-unknown-var"
                                             (h/file-path "src/sample_test/api/diagnostics/a.clj"))))
        (is (not (string/includes? result (h/file-path "src/sample_test/api/diagnostics/b.clj")))))))
  (testing "testing unusued-public-var custom lint"
    (testing "passing multiple namespaces but only one has diagnostics"
      (with-open [rdr (lsp/cli! "diagnostics"
                                "--project-root" h/root-project-path
                                "--namespace" "sample-test.api.diagnostics.d")]
        (is (string/includes? (slurp rdr) (format "%s:2:6: info: [clojure-lsp/unused-public-var] Unused public var 'sample-test.api.diagnostics.d/unused-public-var'"
                                                  (h/file-path "src/sample_test/api/diagnostics/d.clj")))))))
  (testing "When output has canonical-paths as true"
    (with-open [rdr (lsp/cli! "diagnostics"
                              "--project-root" h/root-project-path
                              "--output" "{:canonical-paths true}"
                              "--namespace" "sample-test.api.diagnostics.a")]
      (is (string/includes? (slurp rdr) (format "%s:2:0: error: [unresolved-symbol] Unresolved symbol: some-unknown-var"
                                                (h/project-path->canon-path "src/sample_test/api/diagnostics/a.clj")))))))
