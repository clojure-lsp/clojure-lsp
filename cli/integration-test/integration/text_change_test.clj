(ns integration.text-change-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [integration.fixture :as fixture]
   [integration.lsp :as lsp]))

(def sample-file-path "text_change/a.clj")

(lsp/clean-after-test)

(deftest apply-changes
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification sample-file-path))
  (lsp/client-awaits-server-diagnostics sample-file-path)

  (testing "Change is applied"
    (is (= "original"
           (-> (lsp/request! (fixture/hover-source-path-request sample-file-path 4 2))
               :contents
               (get 2))))

    (lsp/notify! (fixture/did-change-notification sample-file-path 1 [["changed" 2 11 2 19]]))
    (lsp/client-awaits-server-diagnostics sample-file-path)

    (is (= "changed"
           (-> (lsp/request! (fixture/hover-source-path-request sample-file-path 4 2))
               :contents
               (get 2))))))
