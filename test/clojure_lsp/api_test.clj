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
                 (api/clean-ns! {:project-root "integration-test/sample-test"}))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/clean-ns! {:project-root (io/file "integration-test/sample-test/bla")}))))
  (testing "when project-root is a valid file"
    (testing "when a single namespace is specified"
      (reset! db/db {})
      (with-redefs [spit #(is (= (slurp (string/replace %1 "src" "fixtures")) %2))]
        (api/clean-ns! {:project-root (io/file "integration-test/sample-test")
                        :namespace '[sample-test.api.clean-ns.a]
                        :raw? true})))
    (testing "when a single namespace is specified with dry option"
      (reset! db/db {})
      (let [result (api/clean-ns! {:project-root (io/file "integration-test/sample-test")
                                   :namespace '[sample-test.api.clean-ns.a]
                                   :dry? true
                                   :raw? true})]
        (is (= 1 (:result-code result)))
        (is (:message result))))
    (testing "when ns does not matches uri"
      (reset! db/db {})
      (let [result (api/clean-ns! {:project-root (io/file "integration-test/sample-test")
                                   :namespace '[sample-test.api.clean-ns.a.other]
                                   :dry? true
                                   :raw? true})]
        (is (= 1 (:result-code result)))
        (is (:message result))))
    (testing "when ns is already clear"
      (reset! db/db {})
      (let [result (api/clean-ns! {:project-root (io/file "integration-test/sample-test")
                                   :namespace '[sample-test.api.clean-ns.b]
                                   :dry? true
                                   :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to clear!" (:message result)))))
    (testing "specifying a ns-exclude-regex"
      (reset! db/db {})
      (let [result (api/clean-ns! {:project-root (io/file "integration-test/sample-test")
                                   :ns-exclude-regex #".*"
                                   :dry? true
                                   :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to clear!" (:message result)))))))

(deftest diagnostics
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/format! {:project-root "integration-test/sample-test"}))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/format! {:project-root (io/file "integration-test/sample-test/bla")}))))
  (testing "when project-root is a valid file"
    (testing "when a single namespace is specified"
      (reset! db/db {})
      (let [result (api/diagnostics {:project-root (io/file "integration-test/sample-test")
                                     :namespace '[sample-test.api.diagnostics.a]
                                     :raw? true})]
        (is (= 1 (:result-code result)))
        (is (= "src/sample_test/api/diagnostics/a.clj:2:0: error: [unresolved-symbol] Unresolved symbol: some-unknown-var" (:message result)))
        (is (= 1 (count (:diagnostics result))))))
    (testing "when namespace does not exists"
      (reset! db/db {})
      (let [result (api/diagnostics {:project-root (io/file "integration-test/sample-test")
                                     :namespace '[sample-test.api.diagnostics.c]
                                     :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "No diagnostics found!" (:message result)))
        (is (= 0 (count (:diagnostics result))))))
    (testing "With canonical-paths output"
      (reset! db/db {})
      (let [result (api/diagnostics {:project-root (io/file "integration-test/sample-test")
                                     :output {:canonical-paths true}
                                     :namespace '[sample-test.api.diagnostics.a]
                                     :raw? true})]
        (is (= 1 (:result-code result)))
        (is (= (format "%s:2:0: error: [unresolved-symbol] Unresolved symbol: some-unknown-var"
                       (.getCanonicalPath (io/file "integration-test/sample-test/src/sample_test/api/diagnostics/a.clj")))
               (:message result)))
        (is (= 1 (count (:diagnostics result))))))
    (testing "when namespace has no diagnostics"
      (reset! db/db {})
      (let [result (api/diagnostics {:project-root (io/file "integration-test/sample-test")
                                     :namespace '[sample-test.api.diagnostics.b]
                                     :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "No diagnostics found!" (:message result)))
        (is (nil? (:diagnostics result)))))
    (testing "specifying a ns-exclude-regex"
      (reset! db/db {})
      (let [result (api/diagnostics {:project-root (io/file "integration-test/sample-test")
                                     :ns-exclude-regex #".*"
                                     :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "No diagnostics found!" (:message result)))))))

(deftest format!
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/format! {:project-root "integration-test/sample-test"}))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/format! {:project-root (io/file "integration-test/sample-test/bla")}))))
  (testing "when project-root is a valid file"
    (testing "when a single namespace is specified"
      (reset! db/db {})
      (with-redefs [spit #(is (= (slurp (string/replace %1 "src" "fixtures")) %2))]
        (api/format! {:project-root (io/file "integration-test/sample-test")
                      :namespace '[sample-test.api.format.a]
                      :raw? true})))
    (testing "when a single namespace is specified with dry option"
      (reset! db/db {})
      (let [result (api/format! {:project-root (io/file "integration-test/sample-test")
                                 :namespace '[sample-test.api.format.a]
                                 :dry? true
                                 :raw? true})]
        (is (= 1 (:result-code result)))
        (is (:message result))))
    (testing "when ns does not matches uri"
      (reset! db/db {})
      (let [result (api/format! {:project-root (io/file "integration-test/sample-test")
                                 :namespace '[sample-test.api.format.a.other]
                                 :dry? true
                                 :raw? true})]
        (is (= 1 (:result-code result)))
        (is (:message result))))
    (testing "when ns is already formatted"
      (reset! db/db {})
      (let [result (api/format! {:project-root (io/file "integration-test/sample-test")
                                 :namespace '[sample-test.api.format.b]
                                 :dry? true
                                 :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to format!" (:message result)))))
    (testing "specifying a ns-exclude-regex"
      (reset! db/db {})
      (let [result (api/format! {:project-root (io/file "integration-test/sample-test")
                                 :ns-exclude-regex #".*"
                                 :dry? true
                                 :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to format!" (:message result)))))))

(deftest rename!
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/rename! {:project-root "integration-test/sample-test"}))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/rename! {:project-root (io/file "integration-test/sample-test/bla")}))))
  (testing "when project-root is a valid file"
    (testing "foo option assertions"
      (is (thrown? AssertionError (api/rename! {:project-root (io/file "integration-test/sample-test")
                                                :raw? true})))
      (is (thrown? AssertionError (api/rename! {:project-root (io/file "integration-test/sample-test")
                                                :from 'foo
                                                :raw? true}))))
    (testing "to option assertions"
      (is (thrown? AssertionError (api/rename! {:project-root (io/file "integration-test/sample-test")
                                                :from 'a.rename/foo
                                                :raw? true})))
      (is (thrown? AssertionError (api/rename! {:project-root (io/file "integration-test/sample-test")
                                                :from 'a.rename/foo
                                                :to 'foo
                                                :raw? true})))))
  (testing "renaming a function"
    (reset! db/db {})
    (with-redefs [spit #(is (= (slurp (string/replace %1 "src" "fixtures/api")) %2))]
      (api/rename! {:project-root (io/file "integration-test/sample-test")
                    :from 'rename.a/my-func
                    :to 'rename.a/your-func
                    :raw? true}))))
