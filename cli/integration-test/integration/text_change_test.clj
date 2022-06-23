(ns integration.text-change-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(def sample-file-path "text_change/a.clj")

(lsp/clean-after-test)

(deftest view-and-execute-code-action
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-notification sample-file-path))

  (testing "Change is applied"
    (is (= "original"
           (-> (lsp/request! (fixture/hover-request sample-file-path 4 2))
               :contents
               (get 2))))

    (lsp/notify! (fixture/did-change-notification sample-file-path 1 [["changed" 2 11 2 19]]))
    (lsp/client-awaits-server-diagnostics sample-file-path)

    (is (= "changed"
           (-> (lsp/request! (fixture/hover-request sample-file-path 4 2))
               :contents
               (get 2))))))
