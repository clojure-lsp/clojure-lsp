(ns integration.diagnostics-test
  (:require
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest unused-public-var
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-notification "diagnostics/unused_public_var.clj"))

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
  (lsp/notify! (fixture/did-open-notification "diagnostics/kondo.clj"))

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
                                                    :linters {:clj-kondo {:report-duplicates  false
                                                                          :async-custom-lint? false}})}))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-notification "diagnostics/kondo.clj"))

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
