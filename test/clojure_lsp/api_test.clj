(ns clojure-lsp.api-test
  (:require
   [clojure-lsp.api :as api]
   [clojure-lsp.db :as db]
   [clojure-lsp.test-helper :as h]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(deftest clean-ns!
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/clean-ns! :project-root "integration-test/sample-test"))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/clean-ns! :project-root (io/file "integration-test/sample-test/bla")))))
  (testing "when project-root is a valid file"
    (testing "when a single namespace is specified"
      (reset! db/db {})
      (with-redefs [spit #(is (= (slurp "integration-test/sample-test/fixtures/api/clean_ns/a.clj") %2))]
        (api/clean-ns! :project-root (io/file "integration-test/sample-test")
                       :namespace '[api.clean-ns.a])))))

(deftest rename!
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/rename! :project-root "integration-test/sample-test"))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/rename! :project-root (io/file "integration-test/sample-test/bla")))))
  (testing "when project-root is a valid file"
    (testing "foo option assertions"
      (is (thrown? AssertionError (api/rename! :project-root (io/file "integration-test/sample-test"))))
      (is (thrown? AssertionError (api/rename! :project-root (io/file "integration-test/sample-test")
                                               :from 'foo))))
    (testing "to option assertions"
      (is (thrown? AssertionError (api/rename! :project-root (io/file "integration-test/sample-test")
                                               :from 'a.rename/foo)))
      (is (thrown? AssertionError (api/rename! :project-root (io/file "integration-test/sample-test")
                                               :from 'a.rename/foo
                                               :to 'foo)))))
  (testing "renaming a function"
    (reset! db/db {})
    (with-redefs [spit #(is (= (slurp (string/replace %1 "src" "fixtures/api")) %2))]
      (api/rename! :project-root (io/file "integration-test/sample-test")
                   :from 'rename.a/my-func
                   :to 'rename.a/your-func))))
