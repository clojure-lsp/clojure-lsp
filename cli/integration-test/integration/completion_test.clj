(ns integration.completion-test
  (:require
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest completion
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request
                  {:initializationOptions fixture/default-init-options
                   :capabilities
                   {:textDocument
                    {:completion
                     {:contextSupport true
                      :completionItem {:snippetSupport true
                                       :commitCharactersSupport true
                                       :preselectSupport true
                                       :documentationFormat ["markdown" "plaintext"]
                                       :resolveSupport {:properties ["documentation" "detail" "additionalTextEdits"]}}}}}}))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/client-awaits-open-diagnostics "completion/a.clj")
  (lsp/client-awaits-open-diagnostics "completion/b.clj")

  (testing "normal completions"
    (testing "get completions"
      (h/assert-contains-submaps
        [{:label "definterface"
          :kind 3
          :detail "clojure.core/definterface"
          :data {:filename "/clojure.core.clj", :name "definterface", :ns "clojure.core"}}]
        (lsp/request! (fixture/completion-request "completion/a.clj" 2 3))))
    (testing "get snippets"
      (h/assert-contains-submaps
        [{:label "defn"
          :kind 15
          :detail "Insert public defn"
          :insertText "defn ${1:name} [$2]\n  $0"
          :insertTextFormat 2
          :data {:filename "/clojure.core.clj" :name "defn" :ns "clojure.core", :snippet-kind 3}}]
        (lsp/request! (fixture/completion-request "completion/a.clj" 2 4)))))
  (testing "completions from comment"
    (h/assert-submaps
      []
      (lsp/request! (fixture/completion-request "completion/a.clj" 4 3)))))
