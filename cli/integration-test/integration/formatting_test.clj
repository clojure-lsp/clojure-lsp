(ns integration.formatting-test
  (:require
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest formatting
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/client-awaits-open-diagnostics "formatting.clj")

  (testing "we should format the whole buffer"
    (h/assert-submaps
      [{:range {:start {:line 0, :character 0}
                :end {:line 999999, :character 999999}}
        :newText "(ns sample-test.formatting)\n\n(def foo 123)\n\n(defn bar []\n  (+\n   1 foo))\n\n(bar)\n"}]
      (lsp/request! (fixture/formatting-full-request "formatting.clj"))))

  (testing "we format the whole top-level form otherwise we get wrong spaces on parent forms"
    (h/assert-submaps
      [{:range {:start {:line 4, :character 0}
                :end {:line 7, :character 12}},
        :newText "(defn bar []\n  (+\n   1 foo))"}]
      (lsp/request! (fixture/formatting-range-request "formatting.clj" 6 4 7 11)))))
