(ns integration.initialize-test
  (:require
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest initialize
  (lsp/start-process!)
  (testing "initialize request with default config"
    (h/assert-submap
      {:capabilities
       {:workspaceSymbolProvider true
        :documentFormattingProvider true
        :documentRangeFormattingProvider true
        :referencesProvider true
        :callHierarchyProvider true
        :linkedEditingRangeProvider true
        :renameProvider {:prepareProvider true}
        :executeCommandProvider {:commands [;; sorted in clojure-lsp.feature.refactor/available-refactors
                                            "add-import-to-namespace"
                                            "add-missing-import"
                                            "add-missing-libspec"
                                            "add-require-suggestion"
                                            "change-coll"
                                            "clean-ns"
                                            "create-function"
                                            "create-test"
                                            "cycle-coll"
                                            "cycle-keyword-auto-resolve"
                                            "cycle-privacy"
                                            "demote-fn"
                                            "destructure-keys"
                                            "drag-backward"
                                            "drag-forward"
                                            "drag-param-backward"
                                            "drag-param-forward"
                                            "expand-let"
                                            "extract-function"
                                            "extract-to-def"
                                            "get-in-all"
                                            "get-in-less"
                                            "get-in-more"
                                            "get-in-none"
                                            "inline-symbol"
                                            "introduce-let"
                                            "move-coll-entry-down"
                                            "move-coll-entry-up"
                                            "move-form"
                                            "move-to-let"
                                            "promote-fn"
                                            "resolve-macro-as"
                                            "restructure-keys"
                                            "sort-clauses"
                                            "sort-map"
                                            "suppress-diagnostic"
                                            "thread-first"
                                            "thread-first-all"
                                            "thread-last"
                                            "thread-last-all"
                                            "unwind-all"
                                            "unwind-thread"]}
        :signatureHelpProvider {:triggerCharacters []}
        :codeActionProvider {:codeActionKinds ["quickfix"
                                               "refactor"
                                               "refactor.extract"
                                               "refactor.inline"
                                               "refactor.rewrite"
                                               "source"
                                               "source.organizeImports"]}
        :hoverProvider true
        :semanticTokensProvider {:legend {:tokenTypes ["namespace"
                                                       "type"
                                                       "function"
                                                       "macro"
                                                       "keyword"
                                                       "class"
                                                       "variable"
                                                       "method"
                                                       "event"
                                                       "interface"]
                                          :tokenModifiers ["definition"
                                                           "defaultLibrary"
                                                           "implementation"]}
                                 :range true
                                 :full true}
        :codeLensProvider {:resolveProvider true}
        :textDocumentSync {:openClose true
                           :change 1
                           :save {:includeText true}}
        :completionProvider {:resolveProvider true
                             :triggerCharacters [":" "/"]}
        :documentSymbolProvider true
        :definitionProvider true
        :declarationProvider true
        :implementationProvider true
        :documentHighlightProvider true
        :workspace {:fileOperations {:willRename {:filters [{:pattern {:glob "**/*.{clj,cljs,cljc,cljd,edn,bb,clj_kondo}"
                                                                       :matches "file"}
                                                             :scheme "file"}]}}}
        :experimental {:testTree true
                       :cursorInfo true
                       :serverInfo true
                       :clojuredocs true}}}

      (lsp/request! (fixture/initialize-request (merge fixture/default-init-options
                                                       {:workDoneToken "integration-test"
                                                        :capabilities {:workspace {:didChangeWatchedFiles {:dynamicRegistration true}}}})))))

  (h/assert-submap
    {:token "integration-test"
     :value {:kind "begin"
             :title "clojure-lsp"
             :percentage 0}}
    (lsp/client-awaits-server-notification :$/progress))

  (testing "initialized notification"
    (lsp/notify! (fixture/initialized-notification))
    (h/assert-submaps
      [{:registerOptions {:watchers [{:globPattern "**/*.{clj,cljs,cljc,cljd,edn,bb,clj_kondo}"}]}}]
      (:registrations (lsp/client-awaits-server-request :client/registerCapability))))

  (testing "shutdown request"
    (h/assert-submaps
      nil
      (lsp/request! (fixture/shutdown-request)))))
