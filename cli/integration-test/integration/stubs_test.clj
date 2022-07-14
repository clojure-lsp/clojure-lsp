(ns integration.stubs-test
  (:require
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest stubs-generation
  (h/delete-project-file "../../.lsp/.cache")
  (h/delete-project-file "../../.clj-kondo/.cache")
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request
                  {:initializationOptions (assoc fixture/default-init-options
                                                 :stubs {:generation {:namespaces #{"datomic.api"}}})}))
  (lsp/notify! (fixture/initialized-notification))
  (Thread/sleep 5000) ;; Wait for async stubs generation
  (lsp/notify! (fixture/did-open-notification "stubs/a.clj"))

  (testing "After stub generation we find datomic.api analysis and diagnostics."
    (h/assert-submaps
      [{:range {:start {:line 5 :character 2} :end {:line 5 :character 21}}
        :severity 1
        :code "invalid-arity"
        :source "clj-kondo"
        :message "datomic.api/create-database is called with 0 args but expects 1"
        :tags []}]
      (lsp/client-awaits-server-diagnostics "stubs/a.clj"))

    (h/delete-project-file "../../.lsp/.cache/stubs")))
