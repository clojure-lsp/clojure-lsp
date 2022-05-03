(ns integration.settings-change-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(def lsp-config-file (h/source-path->file "../../.lsp/config.edn"))

(defn with-revised-lsp-config-content [new-content f]
  (let [old-content (slurp lsp-config-file)]
    (is (not= new-content old-content))
    (try
      (spit lsp-config-file new-content)
      (f)
      (finally
        (spit lsp-config-file old-content)))))

(def new-lsp-config-content
  (h/code "{:linters {:clj-kondo {:level :off"
          "                       :async-custom-lint? false}}}"
          ""))

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
      (lsp/client-awaits-server-diagnostics "diagnostics/unused_public_var.clj")))

  (with-revised-lsp-config-content
    new-lsp-config-content
    (fn []
      (Thread/sleep 1100)

      (lsp/notify! (fixture/did-open-notification "diagnostics/unused_public_var.clj"))

      (testing "Config has changed"
        (h/assert-submaps
          []
          (lsp/client-awaits-server-diagnostics "diagnostics/unused_public_var.clj"))))))
