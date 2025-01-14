(ns integration.linked-editing-range-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest editing-local-var
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "linked_editing_range/a.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "linked_editing_range/b.cljc"))

  (testing "Renaming alias"
    (h/assert-submap
      {:ranges  [{:start {:line 1 :character 52} :end {:line 1 :character 55}}
                 {:start {:line 3 :character 9} :end {:line 3 :character 12}}
                 {:start {:line 3 :character 1} :end {:line 3 :character 4}}]}
      (lsp/request! (fixture/linked-editing-range-request "linked_editing_range/b.cljc" 1 55))))

  (testing "No errors"
    (is (nil?
          (lsp/request! (fixture/linked-editing-range-request "linked_editing_range/b.cljc" 5 2))))))
