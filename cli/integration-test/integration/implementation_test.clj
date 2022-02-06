(ns integration.implementation-test
  (:require
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest implementation
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-notification "implementation/a.clj"))
  (lsp/notify! (fixture/did-open-notification "implementation/b.clj"))

  (testing "defprotocol implementations"
    (h/assert-submaps
      [{:uri (h/source-path->uri "implementation/b.clj")
        :range {:start {:line 5 :character 3}
                :end {:line 5 :character 12}}}
       {:uri (h/source-path->uri "implementation/b.clj")
        :range {:start {:line 10 :character 9}
                :end {:line 10 :character 18}}}]
      (lsp/request! (fixture/implementation-request "implementation/a.clj" 3 3)))))
