(ns clojure-lsp.feature.diagnostics.custom-test
  (:require
   [clojure-lsp.feature.diagnostics.custom :as f.diagnostics.custom]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(deftest analyze-uri!-test
  (testing "Basic reg-diagnostic"
    (h/reset-components!)
    (swap! (h/db*) assoc-in
           [:settings :linters] {:custom {'foo.bar/baz {:level :warning}}})
    (with-redefs [f.diagnostics.custom/file-content-from-classpath
                  (constantly (format (h/code "(ns foo.bar)"
                                              "(defn baz [{:keys [params db reg-diagnostic!]}]"
                                              "  (reg-diagnostic! {:uri \"%s\""
                                              "                    :level :warning"
                                              "                    :message \"Some linter\""
                                              "                    :source \"some-source\""
                                              "                    :code \"some-code\""
                                              "                    :range {:row 1 :col 2 :end-row 3 :end-col 4}"
                                              "                    }))")
                                      h/default-uri))]
      (h/assert-submap
        {h/default-uri [{:severity 2
                         :message "Some linter"
                         :source "some-source"
                         :code "some-code"
                         :range {:start {:line 0 :character 1}
                                 :end {:line 2 :character 3}}}]}
        (f.diagnostics.custom/analyze-uri! h/default-uri (h/db)))))
  (testing "API usage - internal analysis"
    (h/reset-components!)
    (swap! (h/db*) assoc-in
           [:settings :linters] {:custom {'foo.bar/qux {:level :warning}}})
    (with-redefs [f.diagnostics.custom/file-content-from-classpath
                  (constantly (format (h/code "(ns foo.bar"
                                              " (:require [clojure-lsp.custom-linters-api :as api]))"
                                              "(defn qux [{:keys [params db reg-diagnostic!]}]"
                                              "  (reg-diagnostic! {:uri \"%s\""
                                              "                    :level (:level params)"
                                              "                    :message (str \"var-definitions: \" (count (api/internal-analysis db)))"
                                              "                    :source \"some-source\""
                                              "                    :code \"some-code\""
                                              "                    :range {:row 1 :col 2 :end-row 3 :end-col 4}"
                                              "                    }))")
                                      h/default-uri))]
      (h/load-code (h/code "(ns some-ns)"
                           "(def my-var 1)"))
      (h/load-code (h/code "(ns other-ns)"
                           "(def my-var-2 1)") (h/file-uri "file:///b.clj"))
      (h/assert-submap
        {h/default-uri [{:severity 2
                         :message "var-definitions: 2"
                         :source "some-source"
                         :code "some-code"
                         :range {:start {:line 0 :character 1}
                                 :end {:line 2 :character 3}}}]}
        (f.diagnostics.custom/analyze-uri! h/default-uri (h/db)))))
  (testing "API usage - find-node-from-sym"
    (h/reset-components!)
    (swap! (h/db*) assoc-in
           [:settings :linters] {:custom {'foo.bar/qux {:level :warning}}})
    (with-redefs [f.diagnostics.custom/file-content-from-classpath
                  (constantly (format (h/code "(ns foo.bar"
                                              " (:require [clojure-lsp.custom-linters-api :as api]"
                                              "           [rewrite-clj.zip :as z]))"
                                              "(defn qux [{:keys [params db reg-diagnostic!]}]"
                                              "  (reg-diagnostic! {:uri \"%s\""
                                              "                    :level (:level params)"
                                              "                    :message (str \"node string: \" (z/string (api/find-node-from-sym db 'some-ns 'my-var)))"
                                              "                    :source \"some-source\""
                                              "                    :code \"some-code\""
                                              "                    :range {:row 1 :col 2 :end-row 3 :end-col 4}"
                                              "                    }))")
                                      h/default-uri))]
      (h/load-code (h/code "(ns some-ns)"
                           "(def my-var 1)"))
      (h/load-code (h/code "(ns other-ns)"
                           "(def my-var-2 1)") (h/file-uri "file:///b.clj"))
      (h/assert-submap
        {h/default-uri [{:severity 2
                         :message "node string: (def my-var 1)"
                         :source "some-source"
                         :code "some-code"
                         :range {:start {:line 0 :character 1}
                                 :end {:line 2 :character 3}}}]}
        (f.diagnostics.custom/analyze-uri! h/default-uri (h/db))))))

(deftest missing-required-fields-test
  (testing "no missing field"
    (h/assert-submaps
      []
      (#'f.diagnostics.custom/missing-required-fields
       {:level :info
        :uri h/default-uri
        :message "var-definitions: 2"
        :source "some-source"
        :code "some-code"
        :range {:row 1 :col 2 :end-row 3 :end-col 4}})))
  (testing "missing fields"
    (is (= [:source :level :code :uri :message]
           (#'f.diagnostics.custom/missing-required-fields
            {:range {:row 1 :col 2 :end-row 3 :end-col 4}}))))
  (testing "missing range fields"
    (is (= [:end-row :col]
           (#'f.diagnostics.custom/missing-required-fields
            {:level :info
             :uri h/default-uri
             :message "var-definitions: 2"
             :source "some-source"
             :code "some-code"
             :range {:row 1 :end-col 4}})))))
