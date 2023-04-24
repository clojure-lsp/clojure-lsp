(ns clojure-lsp.api-test
  (:require
   [babashka.fs :as fs]
   [cheshire.core :as json]
   [clojure-lsp.api :as api]
   [clojure-lsp.internal-api :as internal-api]
   [clojure-lsp.test-helper :as h]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :as t :refer [deftest is testing]]))

(defmacro ignoring-prints
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s# *err* s#]
       ~@body)))

(defn clean-api-db! []
  (internal-api/clean-db! :api-test))

(t/use-fixtures :each (fn [f] (clean-api-db!) (f)))

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
    (clean-api-db!)
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
    (clean-api-db!)
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
      (clean-api-db!)
      (with-redefs [spit #(is (h/string= (slurp (replace-last %1 "src" "fixtures")) %2))]
        (api/clean-ns! {:project-root (io/file "../cli/integration-test/sample-test")
                        :namespace '[sample-test.api.clean-ns.a]
                        :raw? true})))
    (let [expected-msg
          (h/lf->sys (format "--- a/%s\n+++ b/%s\n@@ -1,7 +1,6 @@\n (ns sample-test.api.clean-ns.a\n   (:require\n-   [clojure.string :as string]\n-   [sample-test.api.clean-ns.b :refer [b c a]]))\n+   [sample-test.api.clean-ns.b :refer [a b c]]))\n \n a\n b"
                             (h/file-path "src/sample_test/api/clean_ns/a.clj")
                             (h/file-path "src/sample_test/api/clean_ns/a.clj")))]
      (testing "when a single namespace is specified with dry option"
        (clean-api-db!)
        (let [result (api/clean-ns! {:project-root (io/file "../cli/integration-test/sample-test")
                                     :namespace '[sample-test.api.clean-ns.a]
                                     :dry? true
                                     :raw? true})]
          (is (= 1 (:result-code result)))
          (is (= expected-msg  (apply (:message-fn result) [])))))
      (testing "when ns does not matches uri"
        (clean-api-db!)
        (let [result (api/clean-ns! {:project-root (io/file "../cli/integration-test/sample-test")
                                     :namespace '[sample-test.api.clean-ns.a.other]
                                     :dry? true
                                     :raw? true})]
          (is (= 1 (:result-code result)))
          (is (= expected-msg (apply (:message-fn result) []))))))
    (testing "when ns is already clear"
      (clean-api-db!)
      (let [result (api/clean-ns! {:project-root (io/file "../cli/integration-test/sample-test")
                                   :namespace '[sample-test.api.clean-ns.b]
                                   :dry? true
                                   :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to clear!" (apply (:message-fn result) [])))))
    (testing "specifying a ns-exclude-regex"
      (clean-api-db!)
      (let [result (api/clean-ns! {:project-root (io/file "../cli/integration-test/sample-test")
                                   :ns-exclude-regex #".*"
                                   :dry? true
                                   :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to clear!" (apply (:message-fn result) [])))))
    (testing "different line endings types"
      (clean-api-db!)
      (let [result (api/clean-ns! {:project-root (io/file "../cli/integration-test/sample-test")
                                   :namespace '[sample-test.api.clean-ns.dos
                                                sample-test.api.clean-ns.unix]
                                   :dry? true
                                   :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to clear!" (apply (:message-fn result) [])))))))

(deftest diagnostics
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/diagnostics {:project-root "../cli/integration-test/sample-test"}))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test/bla")}))))
  (testing "when project-root is a valid file"
    (testing "when a single namespace is specified"
      (clean-api-db!)
      (let [result (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test")
                                     :namespace '[sample-test.api.diagnostics.a]
                                     :raw? true})]
        (is (= 3 (:result-code result)))
        (is (= (str (h/file-path "src/sample_test/api/diagnostics/a.clj") ":3:1: error: [unresolved-symbol] Unresolved symbol: some-unknown-var") (apply (:message-fn result) [])))
        (is (= 1 (count (:diagnostics result))))))
    (testing "unused-public-var custom lint fn returning only info"
      (clean-api-db!)
      (let [result (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test")
                                     :namespace '[sample-test.api.diagnostics.d]
                                     :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= (str (h/file-path "src/sample_test/api/diagnostics/d.clj") ":3:7: info: [clojure-lsp/unused-public-var] Unused public var 'sample-test.api.diagnostics.d/unused-public-var'") (apply (:message-fn result) [])))
        (is (= 1 (count (:diagnostics result))))))
    (testing "when namespace does not exists"
      (clean-api-db!)
      (let [result (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test")
                                     :namespace '[sample-test.api.diagnostics.c]
                                     :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "No diagnostics found!" (apply (:message-fn result) [])))
        (is (= 0 (count (:diagnostics result))))))
    (testing "With canonical-paths output"
      (clean-api-db!)
      (let [result (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test")
                                     :output {:canonical-paths true}
                                     :namespace '[sample-test.api.diagnostics.a]
                                     :raw? true})]
        (is (= 3 (:result-code result)))
        (is (= (format "%s:3:1: error: [unresolved-symbol] Unresolved symbol: some-unknown-var"
                       (.getCanonicalPath (io/file "../cli/integration-test/sample-test/src/sample_test/api/diagnostics/a.clj")))
               (apply (:message-fn result) [])))
        (is (= 1 (count (:diagnostics result))))))
    (testing "when namespace has no diagnostics"
      (clean-api-db!)
      (let [result (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test")
                                     :namespace '[sample-test.api.diagnostics.b]
                                     :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "No diagnostics found!" (apply (:message-fn result) [])))
        (is (nil? (:diagnostics result)))))
    (testing "specifying a ns-exclude-regex"
      (clean-api-db!)
      (let [result (api/diagnostics {:project-root (io/file "../cli/integration-test/sample-test")
                                     :ns-exclude-regex #".*"
                                     :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "No diagnostics found!" (apply (:message-fn result) [])))))))

(deftest format!
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/format! {:project-root "../cli/integration-test/sample-test"}))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/format! {:project-root (io/file "../cli/integration-test/sample-test/bla")}))))
  (testing "when project-root is a valid file"
    (testing "when a single namespace is specified"
      (clean-api-db!)
      (with-redefs [spit #(is (h/string= (slurp (replace-last %1 "src" "fixtures")) %2))]
        (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                      :namespace '[sample-test.api.format.a]
                      :raw? true})))
    (testing "when a single namespace is specified with dry option"
      (clean-api-db!)
      (let [result (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                                 :namespace '[sample-test.api.format.a]
                                 :dry? true
                                 :raw? true})]
        (is (= 1 (:result-code result)))
        (is (apply (:message-fn result) []))))
    (testing "when ns does not matches uri"
      (clean-api-db!)
      (let [result (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                                 :namespace '[sample-test.api.format.a.other]
                                 :dry? true
                                 :raw? true})]
        (is (= 1 (:result-code result)))
        (is (apply (:message-fn result) []))))
    (testing "when ns is already formatted"
      (clean-api-db!)
      (let [result (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                                 :namespace '[sample-test.api.format.b]
                                 :dry? true
                                 :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to format!" (apply (:message-fn result) [])))))
    (testing "when single filename is specified"
      (clean-api-db!)
      (let [result (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                                 :filenames [(io/file "src/sample_test/api/format/b.clj")]
                                 :dry? true
                                 :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to format!" (apply (:message-fn result) [])))))
    (testing "when filenames are specified"
      (clean-api-db!)
      (let [result (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                                 :filenames [(io/file "src/sample_test/api/format/b.clj")
                                             (io/file "src/sample_test/api/format/a.clj")]
                                 :dry? true
                                 :raw? true})]
        (is (= 1 (:result-code result)))
        (is (apply (:message-fn result) []))))
    (testing "specifying a ns-exclude-regex"
      (clean-api-db!)
      (let [result (api/format! {:project-root (io/file "../cli/integration-test/sample-test")
                                 :ns-exclude-regex #".*"
                                 :dry? true
                                 :raw? true})]
        (is (= 0 (:result-code result)))
        (is (= "Nothing to format!" (apply (:message-fn result) [])))))))

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
    (clean-api-db!)
    (with-redefs [spit #(is (= (slurp (replace-last %1 "src/sample_test" "fixtures/sample_test/api")) %2))]
      (is (= 0 (:result-code (api/rename! {:project-root (io/file "../cli/integration-test/sample-test")
                                           :from         'sample-test.rename.a/my-func
                                           :to           'sample-test.rename.a/your-func
                                           :raw?         true}))))))
  (testing "renaming a namespace"
    (clean-api-db!)
    (is (= 0 (:result-code (api/rename! {:project-root (io/file "../cli/integration-test/sample-test")
                                         :from         'sample-test.rename.a
                                         :to           'sample-test.rename.b
                                         :dry?         true
                                         :raw?         true}))))))

(deftest dump
  (testing "when project-root is not a file"
    (is (thrown? AssertionError
                 (api/dump {:project-root "../cli/integration-test/sample-test"}))))
  (testing "when project-root is not a existent file"
    (is (thrown? AssertionError
                 (api/dump {:project-root (io/file "../cli/integration-test/sample-test/bla")}))))
  (testing "when project-root is valid"
    (testing "dumping all fields as edn"
      (let [{:keys [message-fn result result-code]}
            (ignoring-prints
              (api/dump {:project-root (io/file "../cli/integration-test/sample-test")}))]
        (is (= 0 result-code))
        (is result)
        (is (= [:classpath
                :analysis
                :dep-graph
                :findings
                :settings
                :project-root
                :source-paths]
               (keys (edn/read-string (apply message-fn [])))))))
    (testing "dumping all fields as json"
      (let [{:keys [result result-code message-fn]}
            (ignoring-prints
              (api/dump {:project-root (io/file "../cli/integration-test/sample-test")
                         :output {:format :json}}))]
        (is (= 0 result-code))
        (is result)
        (is (= ["classpath"
                "analysis"
                "dep-graph"
                "findings"
                "settings"
                "project-root"
                "source-paths"]
               (keys (json/parse-string (apply message-fn [])))))))
    (testing "dumping specific fields"
      (let [{:keys [result result-code message-fn]}
            (ignoring-prints
              (api/dump {:project-root (io/file "../cli/integration-test/sample-test")
                         :output {:filter-keys [:project-root :source-paths]}}))
            output (edn/read-string (apply message-fn []))]
        (is (= 0 result-code))
        (is result)
        (is (= [:project-root
                :source-paths]
               (keys output)))
        (h/assert-submap
          {:project-root (str (fs/canonicalize (io/file "../cli/integration-test/sample-test")))
           :source-paths #{(str (fs/canonicalize (io/file "../cli/integration-test/sample-test/test")))
                           (str (fs/canonicalize (io/file "../cli/integration-test/sample-test/src")))}}
          (update output :source-paths set))))
    (testing "dumping with different analysis type"
      (let [{:keys [result result-code]}
            (ignoring-prints
              (api/dump {:project-root (io/file "../cli/integration-test/sample-test")
                         :analysis {:type :project-and-full-dependencies}}))]
        (is result)
        (is (= 0 result-code))))))
