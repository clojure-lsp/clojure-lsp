(ns integration.api.rename-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(def a-subject-path (h/project-path->abs-path "src/rename/a.cljc"))
(def a-subject-text (slurp a-subject-path))
(def a-expected-path (h/project-path->abs-path "fixtures/api/rename/a.cljc"))
(def a-expected-text (slurp a-expected-path))

(def b-subject-path (h/project-path->abs-path "src/rename/b.cljc"))
(def b-subject-text (slurp b-subject-path))

(deftest rename
  (testing "passing a valid from and too"
    (with-open [rdr (lsp/cli! "rename"
                              "--project-root" h/root-project-path
                              "--from" "rename.a/my-func"
                              "--to" "rename.a/your-func")]
      (is (string/includes? (slurp rdr) "Renamed rename.a/my-func to rename.a/your-func\n"))
      (is (= a-expected-text (slurp a-subject-path))))
    (spit a-subject-path a-subject-text)
    (spit b-subject-path b-subject-text)))
