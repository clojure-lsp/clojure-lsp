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
                 {:initializationOptions {:lint-project-files-after-startup? false}
                  :capabilities
                  {:textDocument
                   {:completion
                    {:contextSupport true
                     :completionItemKind {:valueSet (vec (range 1 26))}
                     :completionItem {:snippetSupport true
                                      :commitCharactersSupport true
                                      :preselectSupport true
                                      :documentationFormat ["markdown" "plaintext"]
                                      :resolveSupport {:properties ["documentation" "detail" "additionalTextEdits"]}}}}}}))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-notification "completion/a.clj"))
  (lsp/notify! (fixture/did-open-notification "completion/b.clj"))

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
        ;;  :kind 15 
        ;;  :detail "clojure.core/defn" 
        ;;  :insertText "(defn ${1:name} [$2]\n  ${0:body})"
        ;;  :insertTextFormat 2
         :data {:filename "/clojure.core.clj" :name "defn" :ns "clojure.core", :snippet-kind 6}}]
       (lsp/request! (fixture/completion-request "completion/a.clj" 2 4))))))
