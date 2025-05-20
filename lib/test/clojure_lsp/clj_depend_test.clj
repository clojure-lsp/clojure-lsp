(ns clojure-lsp.clj-depend-test
  (:require
   [clj-depend.api :as clj-depend.api]
   [clojure-lsp.clj-depend :as clj-depend]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(def ^:private project-root-uri (h/file-uri "file:///project"))
(def ^:private project-root (io/file (h/file-path "/project")))
(def ^:private source-paths [(h/file-path "/project/src")
                             (h/file-path "/project/test")])
(def ^:private foo-file-uri (h/file-uri "file:///project/src/foo.clj"))
(def ^:private foo-file (io/file (h/file-path "/project/src/foo.clj")))

(def ^:private clj-depend-legacy-config
  {:layers {:foo {:namespaces      #{'foo}
                  :accesses-layers #{}}
            :bar {:namespaces      #{'bar}
                  :accesses-layers #{:foo}}}})

(def ^:private clj-depend-config
  {:config clj-depend-legacy-config})

(defn ^:private clj-depend-settings
  [& {:keys [namespaces files config snapshot?]}]
  (shared/assoc-some {:project-root project-root
                      :config       (-> (:config clj-depend-config)
                                        (assoc :source-paths #{"src" "test"})
                                        (merge config))}
                     :namespaces namespaces
                     :files files
                     :snapshot? snapshot?))

(defn ^:private clj-depend-settings-without-layer-config
  [& args]
  (-> (apply clj-depend-settings args)
      (update :config dissoc :layers)))

(defn ^:private clj-depend-analyze-response
  [& violations]
  {:result-code 1
   :violations  violations
   :message     "Any message"})

(defn ^:private clj-depend-violations-grouped-by-namespace
  [& violations]
  {:violations (group-by :namespace violations)})

(def ^:private foo-namespace-violation
  {:namespace            'foo
   :dependency-namespace 'bar
   :layer                :c
   :dependency-layer     :b
   :message              "\"foo\" should not depend on \"bar\" (layer \":c\" on \":b\")"})

(def ^:private bar-namespace-violation
  {:namespace 'bar
   :message   "Circular dependency between \"bar\" and \"bar\""})

(defn ^:private verify-no-interactions
  []
  (fn [& _args]
    (throw (Exception. "Mocked function called when it is expected to have no interactions"))))

(defn ^:private mock-fn-when-called-with-params
  [mocked-fn & expected-args]
  (fn [& args]
    (is (= args expected-args) "Mocked function should be called with the expected parameters")
    (mocked-fn)))

(deftest analyze-uri!-test
  (testing "When clj-depend is configured in clojure-lsp"
    (with-redefs [clj-depend.api/configured? (verify-no-interactions)
                  clj-depend.api/analyze (mock-fn-when-called-with-params (constantly (clj-depend-analyze-response foo-namespace-violation))
                                                                          (clj-depend-settings :namespaces #{'foo}))]
      (is (= (clj-depend-violations-grouped-by-namespace foo-namespace-violation)
             (clj-depend/analyze-uri! foo-file-uri {:project-root-uri project-root-uri
                                                    :settings         {:source-paths source-paths
                                                                       :clj-depend   clj-depend-config}})))))

  (testing "When clj-depend is configured in clojure-lsp using legacy config"
    (with-redefs [clj-depend.api/configured? (verify-no-interactions)
                  clj-depend.api/analyze (mock-fn-when-called-with-params (constantly (clj-depend-analyze-response foo-namespace-violation))
                                                                          (clj-depend-settings :namespaces #{'foo}))]
      (is (= (clj-depend-violations-grouped-by-namespace foo-namespace-violation)
             (clj-depend/analyze-uri! foo-file-uri {:project-root-uri project-root-uri
                                                    :settings         {:source-paths source-paths
                                                                       :clj-depend   clj-depend-legacy-config}})))))

  (testing "When clj-depend is configured in clojure-lsp with custom source-paths"
    (with-redefs [clj-depend.api/configured? (verify-no-interactions)
                  clj-depend.api/analyze (mock-fn-when-called-with-params (constantly (clj-depend-analyze-response foo-namespace-violation))
                                                                          (clj-depend-settings :namespaces #{'foo}
                                                                                               :config {:source-paths #{"custom-src"}}))]
      (is (= {:violations {'foo [foo-namespace-violation]}}
             (clj-depend/analyze-uri! foo-file-uri {:project-root-uri project-root-uri
                                                    :settings         {:source-paths source-paths
                                                                       :clj-depend   (assoc-in clj-depend-config [:config :source-paths] #{"custom-src"})}})))))

  (testing "When clj-depend is configured in clojure-lsp and receives snapshot request"
    (with-redefs [clj-depend.api/configured? (verify-no-interactions)
                  clj-depend.api/analyze (mock-fn-when-called-with-params (constantly (clj-depend-analyze-response foo-namespace-violation))
                                                                          (clj-depend-settings :namespaces #{'foo}
                                                                                               :snapshot? true))]

      (is (= {:violations {'foo [foo-namespace-violation]}}
             (clj-depend/analyze-uri! foo-file-uri {:project-root-uri project-root-uri
                                                    :settings         {:source-paths source-paths
                                                                       :clj-depend   (assoc-in clj-depend-config [:snapshot?] true)}})))))

  (testing "When clj-depend is configured but not through clojure-lsp"
    (with-redefs [clj-depend.api/configured? (mock-fn-when-called-with-params (constantly true)
                                                                              project-root)
                  clj-depend.api/analyze (mock-fn-when-called-with-params (constantly (clj-depend-analyze-response foo-namespace-violation))
                                                                          (clj-depend-settings-without-layer-config :namespaces #{'foo}))]
      (is (= {:violations {'foo [foo-namespace-violation]}}
             (clj-depend/analyze-uri! foo-file-uri {:project-root-uri project-root-uri
                                                    :settings         {:source-paths source-paths}}))))))

(deftest analyze-paths!-test
  (testing "When clj-depend is configured in clojure-lsp"
    (with-redefs [clj-depend.api/configured? (verify-no-interactions)
                  clj-depend.api/analyze (mock-fn-when-called-with-params (constantly (clj-depend-analyze-response foo-namespace-violation))
                                                                          (clj-depend-settings :files #{foo-file}))]
      (is (= (clj-depend-violations-grouped-by-namespace foo-namespace-violation)
             (clj-depend/analyze-paths! [foo-file] {:project-root-uri project-root-uri
                                                    :settings         {:source-paths source-paths
                                                                       :clj-depend   clj-depend-config}})))))

  (testing "When clj-depend is configured in clojure-lsp using legacy config"
    (with-redefs [clj-depend.api/configured? (verify-no-interactions)
                  clj-depend.api/analyze (mock-fn-when-called-with-params (constantly (clj-depend-analyze-response foo-namespace-violation))
                                                                          (clj-depend-settings :files #{foo-file}))]
      (is (= (clj-depend-violations-grouped-by-namespace foo-namespace-violation)
             (clj-depend/analyze-paths! [foo-file] {:project-root-uri project-root-uri
                                                    :settings         {:source-paths source-paths
                                                                       :clj-depend   clj-depend-legacy-config}})))))

  (testing "When clj-depend is configured in clojure-lsp with custom source-paths"
    (with-redefs [clj-depend.api/configured? (verify-no-interactions)
                  clj-depend.api/analyze (mock-fn-when-called-with-params (constantly (clj-depend-analyze-response foo-namespace-violation))
                                                                          (clj-depend-settings :files #{foo-file}
                                                                                               :config {:source-paths #{"custom-src"}}))]
      (is (= {:violations {'foo [foo-namespace-violation]}}
             (clj-depend/analyze-paths! [foo-file] {:project-root-uri project-root-uri
                                                    :settings         {:source-paths source-paths
                                                                       :clj-depend   (assoc-in clj-depend-config [:config :source-paths] #{"custom-src"})}})))))

  (testing "When clj-depend is configured in clojure-lsp and receives snapshot request"
    (with-redefs [clj-depend.api/configured? (verify-no-interactions)
                  clj-depend.api/analyze (mock-fn-when-called-with-params (constantly (clj-depend-analyze-response foo-namespace-violation))
                                                                          (clj-depend-settings :files #{foo-file}
                                                                                               :snapshot? true))]

      (is (= {:violations {'foo [foo-namespace-violation]}}
             (clj-depend/analyze-paths! [foo-file] {:project-root-uri project-root-uri
                                                    :settings         {:source-paths source-paths
                                                                       :clj-depend   (assoc-in clj-depend-config [:snapshot?] true)}})))))

  (testing "When clj-depend is configured but not through clojure-lsp"
    (with-redefs [clj-depend.api/configured? (mock-fn-when-called-with-params (constantly true)
                                                                              project-root)
                  clj-depend.api/analyze (mock-fn-when-called-with-params (constantly (clj-depend-analyze-response foo-namespace-violation))
                                                                          (clj-depend-settings-without-layer-config :files #{foo-file}))]
      (is (= {:violations {'foo [foo-namespace-violation]}}
             (clj-depend/analyze-paths! [foo-file] {:project-root-uri project-root-uri
                                                    :settings         {:source-paths source-paths}}))))))

(deftest db-with-results-test
  (testing "Should return db with :clj-depend violations"
    (is (= {:diagnostics {:clj-depend {'foo foo-namespace-violation}}}
           (clj-depend/db-with-results {} {:violations {'foo foo-namespace-violation}}))))

  (testing "Should return db with :diagnostics :clj-depend violations when there are already violations"
    (is (= {:diagnostics {:clj-depend {'foo foo-namespace-violation
                                       'bar bar-namespace-violation}}}
           (clj-depend/db-with-results {:diagnostics {:clj-depend {'foo foo-namespace-violation}}} {:violations {'bar bar-namespace-violation}})))))
