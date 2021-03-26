(ns integration.diagnostics-test
  (:require
    [clojure.test :refer [deftest testing]]
    [integration.fixture :as fixture]
    [integration.helper :as h]
    [integration.lsp :as lsp]))

(h/clean-after-test)

(deftest unused-public-var
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))

  (testing "When a public var is unused"
    (lsp/notify! (fixture/did-open-notification "diagnostics/unused_public_var.clj"))
    (h/assert-submap
      {:diagnostics
       [{:range {:start {:line 2, :character 6}
                 :end {:line 2, :character 9}}
         :severity 3
         :code "unused-public-var"
         :source "clojure-lsp"
         :message "Unused public var 'diagnostics.unused-public-var/foo'"
         :tags [1]}
        {:range {:start {:line 5, :character 5}
                 :end {:line 5, :character 8}}
         :severity 3
         :code "unused-public-var"
         :source "clojure-lsp"
         :message "Unused public var 'diagnostics.unused-public-var/bar'"
         :tags [1]}]}
      (lsp/await-notification :textDocument/publishDiagnostics)))

  (testing "When a private var is unused"
    (lsp/notify! (fixture/did-open-notification "diagnostics/kondo.clj"))
    (h/assert-submap
      {:diagnostics
       [{:range {:start {:line 2 :character 16}
                 :end {:line 2 :character 19}}
         :severity 2
         :code "unused-private-var"
         :source "clj-kondo"
         :message "Unused private var diagnostics.kondo/foo"
         :tags [1]}]}
      (lsp/await-notification :textDocument/publishDiagnostics))))
