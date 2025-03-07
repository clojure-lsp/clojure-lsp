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
  (lsp/notify! (fixture/did-open-source-path-notification "completion/a.clj"))
  (lsp/notify! (fixture/did-open-source-path-notification "completion/b.clj"))

  (testing "normal completions"
    (let [defmacro-item  {:label "defmacro"
                          :kind 3
                          :detail "clojure.core/defmacro"
                          :score 6
                          :data {:unresolved [["documentation" {:uri "file:///clojure.core.clj"
                                                                :name "defmacro"
                                                                :ns "clojure.core"}]]}}]
      (testing "get completions"
        (h/assert-contains-submaps
          [defmacro-item]
          (lsp/request! (fixture/completion-request "completion/a.clj" 2 3)))

        (testing "resolve item"
          (let [result (lsp/request! (fixture/completion-item-resolve-request defmacro-item))]
            (h/assert-submap
              {:label "defmacro"
               :kind 3
               :detail "clojure.core/defmacro"
               :score 6}
              result)
            (is (= "markdown" (:kind (:documentation result))))
            (is (string? (:value (:documentation result))))))))

    (testing "get snippets"
      (h/assert-contains-submaps
        [{:label "defn"
          :kind 15
          :detail "Insert public defn"
          :textEdit {:newText "defn ${1:name} [$2]\n  $0"
                     :range {:start {:line 2, :character 1},
                             :end {:line 2, :character 4}}}
          :insertText "defn ${1:name} [$2]\n  $0"
          :insertTextFormat 2
          :data {:unresolved [["documentation" {:uri "file:///clojure.core.clj"
                                                :name "defn"
                                                :ns "clojure.core"}]]
                 :snippet-kind 3}}]
        (lsp/request! (fixture/completion-request "completion/a.clj" 2 4)))))
  (testing "completions from comment"
    (h/assert-submaps
      []
      (lsp/request! (fixture/completion-request "completion/a.clj" 4 3)))))
