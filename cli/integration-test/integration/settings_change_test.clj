(ns integration.settings-change-test
  (:require
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(def current-lsp-config-file (h/source-path->file "../../.lsp/config.edn"))
(def current-lsp-config-content (slurp current-lsp-config-file))

(def new-lsp-config-content
  "{:linters {:clj-kondo {:level :off}}}\n")

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
      (lsp/await-diagnostics "diagnostics/unused_public_var.clj")))

  (spit current-lsp-config-file new-lsp-config-content)

  (Thread/sleep 1100)

  (lsp/notify! (fixture/did-open-notification "diagnostics/unused_public_var.clj"))

  (testing "Config has changed"
    (h/assert-submaps
      []
      (lsp/await-diagnostics "diagnostics/unused_public_var.clj")))

  (spit current-lsp-config-file current-lsp-config-content))
