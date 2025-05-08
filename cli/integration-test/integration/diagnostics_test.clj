(ns integration.diagnostics-test
  (:require
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest custom-linters
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "diagnostics/custom_linters.clj"))

  (testing "When a custom linter is defined"
    (h/assert-submaps
      [{:severity 2
        :message "external analysis count: 0"
        :source "some-source"
        :code "some-code"
        :range {:start {:line 1 :character 2} :end {:line 3 :character 4}}}]
      (lsp/client-awaits-server-diagnostics "diagnostics/custom_linters.clj"))))

(deftest different-aliases
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "diagnostics/different_aliases.clj"))

  (testing "When there are different aliases for the same namespace"
    (h/assert-submaps
      [{:range
        {:start {:line 3, :character 34}, :end {:line 3, :character 35}},
        :tags [],
        :message
        "Different aliases #{s string cstring str} found for clojure.string",
        :code "clojure-lsp/different-aliases",
        :langs [],
        :severity 1,
        :source "clj-kondo"}
       {:range
        {:start {:line 4, :character 34}, :end {:line 4, :character 37}},
        :tags [],
        :message
        "Different aliases #{s string cstring str} found for clojure.string",
        :code "clojure-lsp/different-aliases",
        :langs [],
        :severity 1,
        :source "clj-kondo"}
       {:range
        {:start {:line 5, :character 34}, :end {:line 5, :character 40}},
        :tags [],
        :message
        "Different aliases #{s string cstring str} found for clojure.string",
        :code "clojure-lsp/different-aliases",
        :langs [],
        :severity 1,
        :source "clj-kondo"}
       {:range
        {:start {:line 6, :character 34}, :end {:line 6, :character 41}},
        :tags [],
        :message
        "Different aliases #{s string cstring str} found for clojure.string",
        :code "clojure-lsp/different-aliases",
        :langs [],
        :severity 1,
        :source "clj-kondo"}]
      (lsp/client-awaits-server-diagnostics "diagnostics/different_aliases.clj"))))

(deftest unused-public-var
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "diagnostics/unused_public_var.clj"))

  (testing "When a public var is unused"
    (h/assert-submaps
      [{:range {:start {:line 2 :character 6}
                :end {:line 2 :character 9}}
        :severity 3
        :code "clojure-lsp/unused-public-var"
        :source "clojure-lsp"
        :message "Unused public var 'sample-test.diagnostics.unused-public-var/foo'"
        :tags [1]}
       {:range {:start {:line 5 :character 5}
                :end {:line 5 :character 8}}
        :severity 3
        :code "clojure-lsp/unused-public-var"
        :source "clojure-lsp"
        :message "Unused public var 'sample-test.diagnostics.unused-public-var/bar'"
        :tags [1]}]
      (lsp/client-awaits-server-diagnostics "diagnostics/unused_public_var.clj"))))

(deftest report-duplicates-enabled
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "diagnostics/kondo.clj"))

  (testing "when report-duplicates is enabled by default"
    (h/assert-submaps
      [{:range {:start {:line 2 :character 16} :end {:line 2 :character 19}}
        :severity 2
        :code "unused-private-var"
        :source "clj-kondo"
        :message "Unused private var sample-test.diagnostics.kondo/foo"
        :tags [1]}
       {:range {:start {:line 5 :character 0} :end {:line 5 :character 3}}
        :severity 1
        :code "unresolved-symbol"
        :source "clj-kondo"
        :message "Unresolved symbol: bar"
        :tags []}
       {:range {:start {:line 7 :character 0} :end {:line 7 :character 3}}
        :severity 1
        :code "unresolved-symbol"
        :source "clj-kondo"
        :message "Unresolved symbol: bar"
        :tags []}]
      (lsp/client-awaits-server-diagnostics "diagnostics/kondo.clj"))))

(deftest report-duplicates-disabled
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request {:initializationOptions
                                             (assoc fixture/default-init-options
                                                    :linters {:clj-kondo {:report-duplicates false}})}))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "diagnostics/kondo.clj"))

  (testing "when report-duplicates is disabled manually"
    (h/assert-submaps
      [{:range {:start {:line 2 :character 16} :end {:line 2 :character 19}}
        :severity 2
        :code "unused-private-var"
        :source "clj-kondo"
        :message "Unused private var sample-test.diagnostics.kondo/foo"
        :tags [1]}
       {:range {:start {:line 5 :character 0} :end {:line 5 :character 3}}
        :severity 1
        :code "unresolved-symbol"
        :source "clj-kondo"
        :message "Unresolved symbol: bar"
        :tags []}]
      (lsp/client-awaits-server-diagnostics "diagnostics/kondo.clj"))))

(deftest clj-depend-no-config-set
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "diagnostics/depend/a.clj"))
  (lsp/notify! (fixture/did-open-source-path-notification "diagnostics/depend/b.clj"))

  (testing "When there is a wrong namespace dependency relationship"
    (h/assert-submaps
      []
      (lsp/client-awaits-server-diagnostics "diagnostics/depend/b.clj"))))

(deftest clj-depend-basic-config-set
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request {:initializationOptions
                                             (assoc fixture/default-init-options
                                                    :clj-depend {:layers {:a {:defined-by         ".*\\.depend\\.a"
                                                                              :accessed-by-layers #{}}
                                                                          :b {:defined-by         ".*\\.depend\\.b"
                                                                              :accessed-by-layers #{:c}}}})}))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "diagnostics/depend/a.clj"))
  (lsp/notify! (fixture/did-open-source-path-notification "diagnostics/depend/b.clj"))

  (testing "When there is a wrong namespace dependency relationship"
    (h/assert-submaps
      []
      (lsp/client-awaits-server-diagnostics "diagnostics/depend/a.clj"))
    (h/assert-submaps
      [{:range {:start {:line 0 :character 4}
                :end {:line 0 :character 36}}
        :severity 3
        :code "clj-depend"
        :source "clj-depend"
        :message "\"sample-test.diagnostics.depend.b\" should not depend on \"sample-test.diagnostics.depend.a\" (layer \":b\" on \":a\")"
        :tags []}]
      (lsp/client-awaits-server-diagnostics "diagnostics/depend/b.clj"))))
