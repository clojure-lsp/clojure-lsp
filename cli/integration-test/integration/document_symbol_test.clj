(ns integration.document-symbol-test
  (:require
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest document-symbol
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-notification "document_symbol/a.clj"))
  (lsp/notify! (fixture/did-open-notification "document_symbol/b.clj"))

  (testing "get file document symbols"
    (h/assert-submaps
      [{:name "sample-test.document-symbol.a"
        :kind 3
        :range {:start {:line 0 :character 0} :end {:line 999999 :character 999999}}
        :selectionRange {:start {:line 0 :character 0} :end {:line 1 :character 40}}
        :children
        [{:name "a-some-var"
          :kind 13
          :range {:start {:line 3 :character 0} :end {:line 3 :character 18}}
          :selectionRange {:start {:line 3 :character 5} :end {:line 3 :character 15}}}
         {:name "a-some-public-function"
          :kind 12
          :range {:start {:line 5 :character 0} :end {:line 6 :character 10}}
          :selectionRange {:start {:line 5 :character 6} :end {:line 5 :character 28}}}]}]
      (lsp/request! (fixture/document-symbol-request "document_symbol/a.clj")))))
