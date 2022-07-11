(ns integration.code-action-test
  (:require
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(def sample-file-name "code_action/a.clj")

(lsp/clean-after-test)

(deftest view-and-execute-code-action
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request
                  {:initializationOptions fixture/default-init-options
                   :capabilities          {:window    {:showDocument {:support true}}
                                           :workspace {:workspaceEdit {:documentChanges true}}}}))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/client-awaits-open-diagnostics sample-file-name)

  (testing "When a code action is available"
    (h/assert-submaps
      [{:title "Move to let"
        :kind "refactor.extract"}
       {:title "Cycle privacy"
        :kind "refactor.rewrite"}
       {:title "Extract function"
        :kind "refactor.extract"}
       {:title "Sort map"
        :kind "refactor.rewrite"}
       {:title "Drag backward"
        :kind "refactor.rewrite"
        :command {:title "Drag backward"
                  :command "drag-backward"
                  :arguments [(h/source-path->uri sample-file-name)
                              5
                              4]}}
       {:title "Drag forward"
        :kind "refactor.rewrite"
        :command {:title "Drag forward"
                  :command "drag-forward"
                  :arguments [(h/source-path->uri sample-file-name)
                              5
                              4]}}
       {:title "Introduce let"
        :kind "refactor.extract"}
       {:title "Clean namespace"
        :kind "source.organizeImports"}]
      (lsp/request! (fixture/code-action-request sample-file-name 5 4)))

    (lsp/mock-response :workspace/applyEdit {:applied true})
    (lsp/mock-response :window/showDocument {:success true})

    (lsp/request! (fixture/execute-command-request "drag-forward"
                                                   (h/source-path->uri sample-file-name)
                                                   5 4))

    (testing "the code action edit is applied asynchronously"
      (h/assert-submaps
        [{:edits [{:range {:start {:line 4, :character 3}
                           :end {:line 5, :character 20}}
                   :newText ":c 3"}
                  {:range {:start {:line 6, :character 3}
                           :end {:line 6, :character 7}}
                   :newText ";; b comment\n   :b 2 ;; 2 comment"}]}]
        (:documentChanges (:edit (lsp/client-awaits-server-request :workspace/applyEdit)))))
    (testing "the cursor is repositioned"
      (h/assert-submap
        {:takeFocus true
         ;; the cursor starts at :b and moves down one line
         :selection {:start {:line      6
                             :character 4}
                     :end   {:line      6
                             :character 4}}}
        (lsp/client-awaits-server-request :window/showDocument)))))
