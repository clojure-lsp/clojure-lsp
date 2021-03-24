(ns integration.initialize-test
  (:require
    [clojure.test :refer [deftest testing]]
    [integration.fixture :as fixture]
    [integration.helper :as h]))

(h/clean-after-test)

(deftest initialize
  (h/start-process!)
  (testing "initialize request with default config"
    (h/assert-submap
      {:capabilities
       {:workspaceSymbolProvider true
        :documentFormattingProvider true
        :documentRangeFormattingProvider true
        :referencesProvider true
        :callHierarchyProvider true
        :renameProvider true
        :executeCommandProvider {:commands ["add-missing-libspec" "add-import-to-namespace" "cycle-privacy" "change-coll" "thread-last-all" "unwind-all" "move-to-let" "clean-ns" "thread-last" "introduce-let" "unwind-thread" "thread-first-all" "thread-first" "inline-symbol" "extract-function" "cycle-coll" "expand-let" "create-function"]}
        :signatureHelpProvider {:triggerCharacters []}
        :codeActionProvider {:codeActionKinds ["quickfix" "refactor" "refactor.extract" "refactor.inline" "refactor.rewrite" "source" "source.organizeImports"]
                             :resolveProvider true}
        :hoverProvider true
        :semanticTokensProvider {:legend {:tokenTypes ["type" "function" "macro"]
                                          :tokenModifiers []}
                                 :range true
                                 :full true}
        :codeLensProvider {:resolveProvider true}
        :textDocumentSync {:openClose true
                           :change 2
                           :save {:includeText true}}
        :completionProvider {:resolveProvider true
                             :triggerCharacters []}
        :documentSymbolProvider true
        :definitionProvider true
        :documentHighlightProvider true}}

      (h/request! (fixture/initialize-request))))

  (testing "initialized notification"
    (h/notify! (fixture/initialized-notification))))
