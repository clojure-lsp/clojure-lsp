(ns clojure-lsp.api-test
  (:require
   [clojure-lsp.api :as api]
   [clojure-lsp.test-helper :as h]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test :api-test)

(defn- replace-last
  "Replaces the last occurrence of `old-value` in `s` with `new-value`."
  [s old-value new-value]
  (string/reverse
   (string/replace-first (string/reverse s) (string/reverse old-value) (string/reverse new-value))))

(deftest analyze-project-and-deps!
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/analyze-project-and-deps! {:project-root "../cli/integration-test/sample-test"}))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/analyze-project-and-deps! {:project-root (io/file "../cli/integration-test/sample-test/bla")}))))
  (testing "when project was not analyzed before, analyzes and return a truthy value"
    (h/clean-db! :api-test)
    (is (api/analyze-project-and-deps! {:project-root (io/file "../cli/integration-test/sample-test")
                                        :raw? true})))
  (testing "when project was already analyzed before return a falsey value"
    (is (not (api/analyze-project-and-deps! {:project-root (io/file "../cli/integration-test/sample-test")
                                             :raw? true})))))

(deftest analyze-project-only!
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/analyze-project-only! {:project-root "../cli/integration-test/sample-test"}))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/analyze-project-only! {:project-root (io/file "../cli/integration-test/sample-test/bla")}))))
  (testing "when project was not analyzed before, analyzes and return a truthy value"
    (h/clean-db! :api-test)
    (is (api/analyze-project-only! {:project-root (io/file "../cli/integration-test/sample-test")
                                    :raw? true})))
  (testing "when project was already analyzed before return a falsey value"
    (is (not (api/analyze-project-only! {:project-root (io/file "../cli/integration-test/sample-test")
                                         :raw? true})))))

(deftest clean-ns!
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/clean-ns! {:project-root "../cli/integration-test/sample-test"}))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/clean-ns! {:project-root (io/file "../cli/integration-test/sample-test/bla")}))))
  (testing "when project-root is a valid file"
    (testing "when a single namespace is specified"
      (h/clean-db! :api-test)
      (with-redefs [spit #(is (= (slurp (replace-last %1 "src" "fixtures")) %2))]
        (api/clean-ns! {:project-root (io/file "../cli/integration-test/sample-test")
                        :namespace '[sample-test.api.clean-ns.a]
                        :raw? true})))
    (testing "when a single namespace is specified with dry option"
      (h/clean-db! :api-test)
      (let [result (api/clean-ns! {:project-root (io/file "../cli/integration-test/sample-test")
                                   :namespace '[sample-test.api.clean-ns.a]
                                   :dry? true
                                   :raw? true})]
        (is (= 1 (:result-code result)))
        (is (:message result))))
    (testing "when ns does not matches uri"
      (h/clean-db! :api-test)
      (let [result (api/clean-ns! {:project-root (io/file "../cli/integration-test/sample-test")
                                   :namespace '[sample-test.api.clean-ns.a.other]
                                   :dry? true
                                   :raw? true})]
        (is (= 1 (:result-code result)))
        (is (:message result))))
    (testing "when ns is already clear"
      (h/clean-db! :api-test)
      (let [result (api/clean-ns! {:project-root (io/file "../cli/integration-test/sample-test")
                                   :namespace '[sample-test.api.clean-ns.b]
                                   :dry? true
                                   :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to clear!" (:message result)))))
    (testing "specifying a ns-exclude-regex"
      (h/clean-db! :api-test)
      (let [result (api/clean-ns! {:project-root (io/file "../cli/integration-test/sample-test")
                                   :ns-exclude-regex #".*"
                                   :dry? true
                                   :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to clear!" (:message result)))))))

(deftest diagnostics
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/diagnostics {:project-root "../cli/integration-test/sample-test"}))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test/bla")}))))
  (testing "when project-root is a valid file"
    (testing "when a single namespace is specified"
      (h/clean-db! :api-test)
      (let [result (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test")
                                     :namespace '[sample-test.api.diagnostics.a]
                                     :raw? true})]
        (is (= 3 (:result-code result)))
        (is (= "src/sample_test/api/diagnostics/a.clj:2:0: error: [unresolved-symbol] Unresolved symbol: some-unknown-var" (:message result)))
        (is (= 1 (count (:diagnostics result))))))
    (testing "unused-public-var custom lint fn returning only info"
      (h/clean-db! :api-test)
      (let [result (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test")
                                     :namespace '[sample-test.api.diagnostics.d]
                                     :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "src/sample_test/api/diagnostics/d.clj:2:6: info: [clojure-lsp/unused-public-var] Unused public var 'sample-test.api.diagnostics.d/unused-public-var'" (:message result)))
        (is (= 1 (count (:diagnostics result))))))
    (testing "when namespace does not exists"
      (h/clean-db! :api-test)
      (let [result (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test")
                                     :namespace '[sample-test.api.diagnostics.c]
                                     :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "No diagnostics found!" (:message result)))
        (is (= 0 (count (:diagnostics result))))))
    (testing "With canonical-paths output"
      (h/clean-db! :api-test)
      (let [result (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test")
                                     :output {:canonical-paths true}
                                     :namespace '[sample-test.api.diagnostics.a]
                                     :raw? true})]
        (is (= 3 (:result-code result)))
        (is (= (format "%s:2:0: error: [unresolved-symbol] Unresolved symbol: some-unknown-var"
                       (.getCanonicalPath (io/file "../cli/integration-test/sample-test/src/sample_test/api/diagnostics/a.clj")))
               (:message result)))
        (is (= 1 (count (:diagnostics result))))))
    (testing "when namespace has no diagnostics"
      (h/clean-db! :api-test)
      (let [result (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test")
                                     :namespace '[sample-test.api.diagnostics.b]
                                     :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "No diagnostics found!" (:message result)))
        (is (nil? (:diagnostics result)))))
    (testing "specifying a ns-exclude-regex"
      (h/clean-db! :api-test)
      (let [result (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test")
                                     :ns-exclude-regex #".*"
                                     :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "No diagnostics found!" (:message result)))))))

(deftest format!
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/format! {:project-root "../cli/integration-test/sample-test"}))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/format! {:project-root (io/file "../cli/integration-test/sample-test/bla")}))))
  (testing "when project-root is a valid file"
    (testing "when a single namespace is specified"
      (h/clean-db! :api-test)
      (with-redefs [spit #(is (= (slurp (replace-last %1 "src" "fixtures")) %2))]
        (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                      :namespace '[sample-test.api.format.a]
                      :raw? true})))
    (testing "when a single namespace is specified with dry option"
      (h/clean-db! :api-test)
      (let [result (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                                 :namespace '[sample-test.api.format.a]
                                 :dry? true
                                 :raw? true})]
        (is (= 1 (:result-code result)))
        (is (:message result))))
    (testing "when ns does not matches uri"
      (h/clean-db! :api-test)
      (let [result (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                                 :namespace '[sample-test.api.format.a.other]
                                 :dry? true
                                 :raw? true})]
        (is (= 1 (:result-code result)))
        (is (:message result))))
    (testing "when ns is already formatted"
      (h/clean-db! :api-test)
      (let [result (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                                 :namespace '[sample-test.api.format.b]
                                 :dry? true
                                 :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to format!" (:message result)))))
    (testing "when single filename is specified"
      (h/clean-db! :api-test)
      (let [result (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                                 :filenames [(io/file "src/sample_test/api/format/b.clj")]
                                 :dry? true
                                 :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to format!" (:message result)))))
    (testing "when filenames are specified"
      (h/clean-db! :api-test)
      (let [result (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                                 :filenames [(io/file "src/sample_test/api/format/b.clj")
                                             (io/file "src/sample_test/api/format/a.clj")]
                                 :dry? true
                                 :raw? true})]
        (is (= 1 (:result-code result)))
        (is (:message result))))
    (testing "specifying a ns-exclude-regex"
      (h/clean-db! :api-test)
      (let [result (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                                 :ns-exclude-regex #".*"
                                 :dry? true
                                 :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to format!" (:message result)))))))

(deftest rename!
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/rename! {:project-root "../cli/integration-test/sample-test"}))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/rename! {:project-root (io/file "../cli/integration-test/sample-test/bla")}))))
  (testing "when project-root is a valid file"
    (testing "foo option assertions"
      (is (thrown? AssertionError (api/rename! {:project-root (io/file "../cli/integration-test/sample-test")
                                                :raw?         true})))
      (is (thrown? AssertionError (api/rename! {:project-root (io/file "../cli/integration-test/sample-test")
                                                :from         'foo
                                                :raw?         true}))))
    (testing "to option assertions"
      (is (thrown? AssertionError (api/rename! {:project-root (io/file "../cli/integration-test/sample-test")
                                                :from         'a.rename/foo
                                                :raw?         true})))
      (is (thrown? AssertionError (api/rename! {:project-root (io/file "../cli/integration-test/sample-test")
                                                :from         'a.rename/foo
                                                :to           'foo
                                                :raw?         true})))
      (is (thrown? AssertionError (api/rename! {:project-root (io/file "../cli/integration-test/sample-test")
                                                :from         'foo
                                                :to           'a.rename/foo
                                                :raw?         true})))))
  (testing "renaming a function"
    (h/clean-db! :api-test)
    (with-redefs [spit #(is (= (slurp (replace-last %1 "src/sample_test" "fixtures/sample_test/api")) %2))]
      (is (= 0 (:result-code (api/rename! {:project-root (io/file "../cli/integration-test/sample-test")
                                           :from         'sample-test.rename.a/my-func
                                           :to           'sample-test.rename.a/your-func
                                           :raw?         true}))))))
  (testing "renaming a namespace"
    (h/clean-db! :api-test)
    (is (= 0 (:result-code (api/rename! {:project-root (io/file "../cli/integration-test/sample-test")
                                         :from         'sample-test.rename.a
                                         :to           'sample-test.rename.b
                                         :dry?         true
                                         :raw?         true}))))))
