(ns clojure-lsp.feature.code-actions
  (:require
    [clojure-lsp.feature.definition :as f.definition]
    [clojure-lsp.feature.refactor :as f.refactor]
    [clojure-lsp.refactor.transform :as r.transform])
  (:import
    (org.eclipse.lsp4j
      CodeActionKind)))

(defn resolve-code-action
  [{{:keys [id uri line character]} :data :as code-action}
   zloc]
  (->
    (merge code-action
           (case id

             "add-missing-require"
             (let [missing-require (f.refactor/call-refactor {:loc         zloc
                                                              :refactoring :add-missing-libspec
                                                              :uri         uri
                                                              :version     0
                                                              :row         (inc line)
                                                              :col         (inc character)
                                                              :args        {:source :code-action}})]
               {:workspace-edit (f.refactor/refactor-client-seq-changes uri 0 (:result missing-require))})

             "add-missing-import"
             (let [missing-import (r.transform/add-common-import-to-namespace zloc)]
               {:workspace-edit (f.refactor/refactor-client-seq-changes uri 0 (:result missing-import))})

             "refactor-inline-symbol"
             {:command {:title     "Inline symbol"
                        :command   "inline-symbol"
                        :arguments [uri line character]}}

             "refactor-cycle-privacy"
             {:command {:title     "Cycle privacy"
                        :command   "cycle-privacy"
                        :arguments [uri line character]}}

             "refactor-extract-function"
             {:command {:title     "Extract function"
                        :command   "extract-function"
                        :arguments [uri line character "new-function"]}}

             "clean-ns"
             {:command {:title     "Clean namespace"
                        :command   "clean-ns"
                        :arguments [uri line character]}}
             {}))
    (dissoc :data :diagnostics)))

(defn all [zloc uri row col diagnostics client-capabilities]
  (let [workspace-edit-capability? (get-in client-capabilities [:workspace :workspace-edit])
        resolve-support?           (get-in client-capabilities [:text-document :code-action :resolve-support])
        inside-function?           (r.transform/inside-function? zloc)
        [_ {def-uri    :uri
            definition :usage}]    (f.definition/definition-usage uri row col)
        inline-symbol?             (r.transform/inline-symbol? def-uri definition)
        line                       (dec row)
        character                  (dec col)
        has-unknown-ns?            (some #(= (compare "unresolved-namespace" (:code %)) 0) diagnostics)
        unresolved-symbol          (some #(= (compare "unresolved-symbol" (:code %)) 0) diagnostics)
        missing-require            (when (or has-unknown-ns? unresolved-symbol)
                                     (r.transform/find-missing-require zloc))
        missing-import             (when unresolved-symbol
                                     (r.transform/find-missing-import zloc))]
    (cond-> []

      missing-require
      (conj {:title      (str "Add missing '" missing-require "' require")
             :kind       CodeActionKind/QuickFix
             :preferred? true
             :data       {:id        "add-missing-require"
                          :uri       uri
                          :line      line
                          :character character}})

      missing-import
      (conj {:title      (str "Add missing '" missing-import "' import")
             :kind       CodeActionKind/QuickFix
             :preferred? true
             :data       {:id        "add-missing-import"
                          :uri       uri
                          :line      line
                          :character character}})

      inline-symbol?
      (conj {:title "Inline symbol"
             :kind  CodeActionKind/RefactorInline
             :data  {:id        "refactor-inline-symbol"
                     :uri       uri
                     :line      line
                     :character character}})

      inside-function?
      (conj {:title "Cycle privacy"
             :kind  CodeActionKind/RefactorRewrite
             :data  {:id        "refactor-cycle-privacy"
                     :uri       uri
                     :line      line
                     :character character}}
            {:title "Extract function"
             :kind  CodeActionKind/RefactorExtract
             :data  {:id        "refactor-extract-function"
                     :uri       uri
                     :line      line
                     :character character}})

      workspace-edit-capability?
      (conj {:title "Clean namespace"
             :kind  CodeActionKind/SourceOrganizeImports
             :data  {:id        "clean-ns"
                     :uri       uri
                     :line      line
                     :character character}})

      (not resolve-support?)
      (->> (map #(resolve-code-action % zloc))))))
