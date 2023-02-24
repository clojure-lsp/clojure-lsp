(ns integration.api.rename-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(def a-subject-path (h/project-path->canon-path "src/sample_test/rename/a.cljc"))
(def a-subject-text (slurp a-subject-path))
(def a-expected-path (h/project-path->canon-path "fixtures/sample_test/api/rename/a.cljc"))
(def a-expected-text (slurp a-expected-path))

(def b-subject-path (h/project-path->canon-path "src/sample_test/rename/b.cljc"))
(def b-subject-text (slurp b-subject-path))
(def c-expected-path (h/project-path->canon-path "src/sample_test/rename/foo/c.cljc"))
(def c-expected-text (slurp (h/project-path->canon-path "fixtures/sample_test/api/rename/foo/c.cljc")))
(def d-subject-path (h/project-path->canon-path "src/sample_test/rename/d.clj"))
(def d-subject-text (slurp d-subject-path))

(deftest rename
  (testing "passing a valid from and too"
    (with-open [rdr (lsp/cli! "rename"
                              "--project-root" h/root-project-path
                              "--from" "sample-test.rename.a/my-func"
                              "--to" "sample-test.rename.a/your-func")]
      (is (h/str-includes? (slurp rdr) "Renamed sample-test.rename.a/my-func to sample-test.rename.a/your-func\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text))
  (testing "renaming a namespace"
    (with-open [rdr (lsp/cli! "rename"
                              "--project-root" h/root-project-path
                              "--from" "sample-test.rename.b"
                              "--to" "sample-test.rename.foo.c")]
      (is (h/str-includes? (slurp rdr) "Renamed sample-test.rename.b to sample-test.rename.foo.c\n"))
      (is (= c-expected-text (slurp c-expected-path))))
    (spit b-subject-path b-subject-text)
    (spit d-subject-path d-subject-text)
    (io/delete-file c-expected-path))
  (testing "dry renaming a namespace"
    (with-open [rdr (lsp/cli! "rename"
                              "--project-root" h/root-project-path
                              "--from" "sample-test.rename.b"
                              "--to" "sample-test.rename.foo.c"
                              "--dry"
                              "--raw")]
      (is (h/str-includes? (slurp rdr) "rename from src/sample_test/rename/b.cljc\nrename to src/sample_test/rename/foo/c.cljc\n")))))
