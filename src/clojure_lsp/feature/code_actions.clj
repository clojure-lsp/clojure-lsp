(ns clojure-lsp.feature.code-actions
  (:require
   [clojure-lsp.feature.definition :as f.definition]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.refactor.transform :as r.transform]
   [rewrite-clj.zip :as z]))

(defn all [zloc uri row col diagnostics client-capabilities]
  (let [workspace-edit-capability? (get-in client-capabilities [:workspace :workspace-edit])
        inside-function? (r.transform/inside-function? zloc)
        [_ {def-uri :uri
            definition :usage}] (f.definition/definition-usage uri row col)
        inline-symbol? (r.transform/inline-symbol? def-uri definition)
        line (dec row)
        character (dec col)
        has-unknown-ns? (some #(= (compare "unresolved-namespace" (some-> % .getCode .get)) 0) diagnostics)
        unresolved-symbol (first (filter #(= (compare "unresolved-symbol" (some-> % .getCode .get)) 0) diagnostics))
        known-refer? (when unresolved-symbol
                       (get r.transform/common-refers->info (z/sexpr zloc)))
        missing-ns (when (or has-unknown-ns? known-refer?)
                     (f.refactor/call-refactor {:loc zloc
                                                :refactoring :add-missing-libspec
                                                :uri uri
                                                :version 0
                                                :row row
                                                :col col
                                                :args {:source :code-action}}))]
    (cond-> []

      (:result missing-ns)
      (conj {:title          (str "Add missing '" (-> missing-ns :code-action-data :ns-name) "' namespace")
             :kind           :quick-fix
             :preferred?     true
             :workspace-edit (f.refactor/refactor-client-seq-changes uri 0 (:result missing-ns))})

      inline-symbol?
      (conj {:title   "Inline symbol"
             :kind    :refactor-inline
             :command {:title     "Inline symbol"
                       :command   "inline-symbol"
                       :arguments [uri line character]}})

      inside-function?
      (conj {:title   "Cycle privacy"
             :kind    :refactor-rewrite
             :command {:title     "Cycle privacy"
                       :command   "cycle-privacy"
                       :arguments [uri line character]}}
            {:title   "Extract function"
             :kind    :refactor-extract
             :command {:title     "Extract function"
                       :command   "extract-function"
                       :arguments [uri line character "new-function"]}})

      workspace-edit-capability?
      (conj {:title   "Clean namespace"
             :kind    :source-organize-imports
             :command {:title     "Clean namespace"
                       :command   "clean-ns"
                       :arguments [uri line character]}}))))
