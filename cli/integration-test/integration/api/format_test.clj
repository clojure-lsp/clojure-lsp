(ns integration.api.format-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(def a-subject-path (h/project-path->canon-path "src/sample_test/api/format/a.clj"))
(def a-subject-text (slurp a-subject-path))
(def a-expected-path (h/project-path->canon-path "fixtures/sample_test/api/format/a.clj"))
(def a-expected-text (slurp a-expected-path))

(deftest format-test
  (testing "passing a single namespace"
    (with-open [rdr (lsp/cli! "format"
                              "--project-root" h/root-project-path
                              "--namespace" "sample-test.api.format.a")]
      (is (h/str-includes? (slurp rdr) "Formatted 1 namespaces\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "passing multiple namespaces but only one is formatable"
    (with-open [rdr (lsp/cli! "format"
                              "--project-root" h/root-project-path
                              "--namespace" "sample-test.api.format.b"
                              "--namespace" "sample-test.api.format.a")]
      (is (h/str-includes? (slurp rdr) "Formatted 1 namespaces\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "passing multiple filenames separated by double colon but only one is formatable"
    (with-open [rdr (lsp/cli! "format"
                              "--project-root" h/root-project-path
                              "--filenames" "src/sample_test/api/format/b.clj:src/sample_test/api/format/a.clj")]
      (is (h/str-includes? (slurp rdr) "Formatted 1 namespaces\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "passing multiple filenames separated by comma but only one is formatable"
    (with-open [rdr (lsp/cli! "format"
                              "--project-root" h/root-project-path
                              "--filenames" "src/sample_test/api/format/b.clj,src/sample_test/api/format/a.clj")]
      (is (h/str-includes? (slurp rdr) "Formatted 1 namespaces\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "passing filename folder"
    (with-open [rdr (lsp/cli! "format"
                              "--project-root" h/root-project-path
                              "--filenames" "src/sample_test/api/format")]
      (is (h/str-includes? (slurp rdr) "Formatted 1 namespaces\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "when running with dry"
    (with-open [rdr (lsp/cli! "format"
                              "--project-root" h/root-project-path
                              "--namespace" "sample-test.api.format.a"
                              "--dry")]
      (is (h/str-includes? (slurp rdr) (h/file-path "src/sample_test/api/format/a.clj")))
      (is (= a-subject-text (slurp a-subject-path))))))
