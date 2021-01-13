(ns clojure-lsp.feature.code-actions
  (:require
    [clojure-lsp.feature.definition :as f.definition]
    [clojure-lsp.feature.refactor :as f.refactor]
    [clojure-lsp.refactor.transform :as r.transform]
    [rewrite-clj.zip :as z])
  (:import
   (org.eclipse.lsp4j
     CodeActionKind)))

(defn resolve-code-action [{{:keys [id uri line character]} :data :as code-action}]
  (->
    (merge code-action
           (case id

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
        resolve-support? (get-in client-capabilities [:text-document :code-action :resolve-support])
        inside-function?           (r.transform/inside-function? zloc)
        [_ {def-uri    :uri
            definition :usage}]    (f.definition/definition-usage uri row col)
        inline-symbol?             (r.transform/inline-symbol? def-uri definition)
        line                       (dec row)
        character                  (dec col)
        has-unknown-ns?            (some #(= (compare "unresolved-namespace" (:code %)) 0) diagnostics)
        unresolved-symbol          (some #(= (compare "unresolved-symbol" (:code %)) 0) diagnostics)
        known-refer?               (when unresolved-symbol
                                     (get r.transform/common-refers->info (z/sexpr zloc)))
        missing-require            (when (or has-unknown-ns? known-refer?)
                                     (f.refactor/call-refactor {:loc         zloc
                                                                :refactoring :add-missing-libspec
                                                                :uri         uri
                                                                :version     0
                                                                :row         row
                                                                :col         col
                                                                :args        {:source :code-action}}))
        missing-import             (when unresolved-symbol
                                     (r.transform/add-common-import-to-namespace zloc))]
    (cond-> []

      (:result missing-require)
      (conj {:title          (str "Add missing '" (-> missing-require :code-action-data :ns-name) "' require")
             :kind           CodeActionKind/QuickFix
             :preferred?     true
             :data           {:id "add-missing-require"}
             :workspace-edit (f.refactor/refactor-client-seq-changes uri 0 (:result missing-require))})

      (:result missing-import)
      (conj {:title          (str "Add missing '" (-> missing-import :code-action-data :import-name) "' import")
             :kind           CodeActionKind/QuickFix
             :preferred?     true
             :data           {:id "add-missing-import"}
             :workspace-edit (f.refactor/refactor-client-seq-changes uri 0 (:result missing-import))})

      inline-symbol?
      (conj {:title   "Inline symbol"
             :kind    CodeActionKind/RefactorInline
             :data    {:id "refactor-inline-symbol"
                       :uri uri
                       :line line
                       :character character}})

      inside-function?
      (conj {:title   "Cycle privacy"
             :kind    CodeActionKind/RefactorRewrite
             :data    {:id "refactor-cycle-privacy"
                       :uri uri
                       :line line
                       :character character}}
            {:title   "Extract function"
             :kind    CodeActionKind/RefactorExtract
             :data    {:id "refactor-extract-function"
                       :uri uri
                       :line line
                       :character character}})

      workspace-edit-capability?
      (conj {:title   "Clean namespace"
             :kind    CodeActionKind/SourceOrganizeImports
             :data    {:id "clean-ns"
                       :uri uri
                       :line line
                       :character character}})

      (not resolve-support?)
      (->> (map resolve-code-action)))))
