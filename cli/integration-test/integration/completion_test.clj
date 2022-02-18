(ns integration.completion-test
  (:require
   [clojure.test :refer [deftest is testing]]
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
         :kind 15
         :detail "clojure.core/defn"
         :insertText "(defn ${1:name} [$2]\n  ${0:body})"
         :insertTextFormat 2
         :data {:filename "/clojure.core.clj" :name "defn" :ns "clojure.core", :snippet-kind 6}}]
       (lsp/request! (fixture/completion-request "completion/a.clj" 2 4)))))
  (testing "completions from whitespace"
    (let [completions (lsp/request! (fixture/completion-request "completion/a.clj" 3 1))]
      (is (< 800 (count completions)))
      (h/assert-contains-submaps
        [{:label "Map"
          :kind 7
          :detail "java.util.Map"}
         {:label "vec"
          :kind 3
          :detail "clojure.core/vec"
          :data {:filename "/clojure.core.clj", :name "vec", :ns "clojure.core"}}]
        completions))
    (let [completions (lsp/request! (fixture/completion-request "completion/a.cljs" 3 1))]
      (is (< 800 (count completions)))
      (h/assert-contains-submaps
        [{:label "clj->js"
          :kind 18
          :detail "cljs.core/clj->js"
          :data {:filename "/cljs.core.cljs", :name "clj->js", :ns "cljs.core"}}
         {:label "vec"
          :kind 3
          :detail "clojure.core/vec"
          :data {:filename "/clojure.core.clj", :name "vec", :ns "clojure.core"}}]
        completions)))
  (testing "completions from comment"
    (h/assert-submaps
      []
      (lsp/request! (fixture/completion-request "completion/a.clj" 4 3)))))
