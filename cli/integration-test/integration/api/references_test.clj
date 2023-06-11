(ns integration.api.references-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(def a-subject-path (h/project-path->canon-path "src/sample_test/rename/a.cljc"))
(def b-subject-path (h/project-path->canon-path "src/sample_test/rename/b.cljc"))

(deftest references
  (testing "finding all references of a full qualified symbol"
    (with-open [rdr (lsp/cli! "references"
                              "--project-root" h/root-project-path
                              "--from" "sample-test.rename.a/my-func")]
      (let [result (slurp rdr)]
        (is (h/str-includes? result
                             ":4:7"))
        (is (h/str-includes? result
                             ":2:49"))))))
