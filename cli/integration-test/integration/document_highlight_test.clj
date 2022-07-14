(ns integration.document-highlight-test
  (:require
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest document-highlight
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-notification "document_highlight/a.clj"))

  (testing "function usages/definition highlight"
    (h/assert-submaps
      [{:range {:start {:line 4 :character 6} :end {:line 4 :character 13}}}
       {:range {:start {:line 7 :character 1} :end {:line 7 :character 8}}}
       {:range {:start {:line 9 :character 1} :end {:line 9 :character 8}}}]
      (lsp/request! (fixture/document-highlight-request "document_highlight/a.clj" 7 2))))

  (testing "alias usages highlight"
    (h/assert-submaps
      [{:range {:start {:line 2 :character 24} :end {:line 2 :character 27}}}
       {:range {:start {:line 11 :character 1} :end {:line 11 :character 9}}}]
      (lsp/request! (fixture/document-highlight-request "document_highlight/a.clj" 2 25))))

  (testing "local function usage highlight"
    (h/assert-submaps
      [{:range {:start {:line 13 :character 6} :end {:line 13 :character 13}}}
       {:range {:start {:line 14 :character 3} :end {:line 14 :character 10}}}]
      (lsp/request! (fixture/document-highlight-request "document_highlight/a.clj" 14 4)))))
