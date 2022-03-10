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
        :executeCommandProvider {:commands ["add-missing-libspec"
                                            "add-import-to-namespace"
                                            "move-coll-entry-up"
                                            "cycle-privacy"
                                            "cycle-fn-literal"
                                            "create-test"
                                            "change-coll"
                                            "thread-last-all"
                                            "unwind-all"
                                            "move-to-let"
                                            "clean-ns"
                                            "suppress-diagnostic"
                                            "move-coll-entry-down"
                                            "thread-last"
                                            "resolve-macro-as"
                                            "introduce-let"
                                            "add-missing-import"
                                            "sort-map"
                                            "unwind-thread"
                                            "thread-first-all"
                                            "thread-first"
                                            "inline-symbol"
                                            "extract-function"
                                            "cycle-coll"
                                            "expand-let"
                                            "create-function"
                                            "add-require-suggestion"]}
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
                                                       "event"]
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
        :experimental {:testTree true
                       :cursorInfo true
                       :serverInfo true
                       :clojuredocs true}}}

      (lsp/request! (fixture/initialize-request))))

  (h/assert-submap
    {:token "clojure-lsp"
     :value {:kind "begin"
             :title "clojure-lsp"
             :percentage 0}}
    (lsp/await-notification :$/progress))

  (testing "initialized notification"
    (lsp/notify! (fixture/initialized-notification))
    (h/assert-submaps
      [{:registerOptions {:watchers [{:globPattern "**/*.{clj,cljs,cljc,edn}"}]}}]
      (:registrations (lsp/await-client-request :client/registerCapability)))))
