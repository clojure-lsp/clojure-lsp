(ns integration.api.rename-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [integration.lsp :as lsp]
    [clojure.string :as string]))

(lsp/clean-after-test)

(def a-subject-path "./integration-test/sample-test/src/rename/a.cljc")
(def a-subject-text (slurp a-subject-path))
(def a-expected-path "./integration-test/sample-test/fixtures/api/rename/a.cljc")
(def a-expected-text (slurp a-expected-path))

(deftest rename
  (testing "passing a valid from and too"
    (with-open [rdr (lsp/cli! "rename"
                              "--project-root" "./integration-test/sample-test"
                              "--from" "rename.a/my-func"
                              "--to" "rename.a/your-func")]
      (is (string/includes? (slurp rdr) "Renamed rename.a/my-func to rename.a/your-func\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text)))
