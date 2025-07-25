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
  (Thread/sleep 20000) ;; Wait for async stubs generation

  (testing "After stub generation we find datomic.api analysis but no diagnostics."
    ;; TODO how to assert there is no diagnostics via protocol only?
    ;; (h/assert-submaps
    ;;   []
    ;;   (lsp/client-awaits-server-diagnostics "stubs/a.clj")
    (h/delete-project-file "../../.lsp/.cache/stubs")))
