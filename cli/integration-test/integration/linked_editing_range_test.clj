(ns integration.linked-editing-range-test
  (:require
   [clojure.test :refer [deftest testing]]
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

  (testing "Renaming locals"
    (h/assert-submap
      {:ranges  [{:start {:line 2 :character 11} :end {:line 2 :character 15}}
                 {:start {:line 3 :character 13} :end {:line 3 :character 17}}
                 {:start {:line 4 :character 9} :end {:line 4 :character 13}}]}
      (lsp/request! (fixture/linked-editing-range-request "linked_editing_range/a.cljc" 2 13))))

  (testing "Renaming function"
    (h/assert-submap
      {:error {:code -32602, :message "There are references on other files for this symbol"}}
      (lsp/request! (fixture/linked-editing-range-request "linked_editing_range/a.cljc" 2 7)))))
