(ns integration.did-open-test
  (:require
    [clojure.test :refer [deftest testing]]
    [integration.fixture :as fixture]
    [integration.helper :as h]))

(h/clean-after-test)

(deftest did-open
  (h/start-process!)
  (h/request! (fixture/initialize-request))
  (h/notify! (fixture/initialized-notification))

  (testing "opening a file"
    (h/notify! (fixture/did-open-notification "foo.clj"))
    (h/assert-submap
      {:diagnostics []}
      (h/await-notification :textDocument/publishDiagnostics))))
