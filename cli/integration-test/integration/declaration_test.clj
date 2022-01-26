(ns integration.declaration-test
  (:require
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest declaration
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-notification "declaration/a.clj"))
  (lsp/notify! (fixture/did-open-notification "declaration/b.clj"))

  (testing "var-usages"
    (testing "find declaration of usage with alias"
      (h/assert-submap
        {:uri (h/source-path->uri "declaration/b.clj")
         :range {:start {:line 1 :character 43}
                 :end {:line 1 :character 56}}}
        (lsp/request! (fixture/declaration-request "declaration/b.clj" 4 2))))

    (testing "find declaration of usage with refer all"
      (h/assert-submap
        {:uri (h/source-path->uri "declaration/b.clj")
         :range {:start {:line 2 :character 13}
                 :end {:line 2 :character 27}}}
        (lsp/request! (fixture/declaration-request "declaration/b.clj" 6 2))))))
