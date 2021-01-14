(ns clojure-lsp.feature.code-actions
  (:require
    [clojure-lsp.feature.definition :as f.definition]
    [clojure-lsp.feature.refactor :as f.refactor]
    [clojure-lsp.refactor.transform :as r.transform]
    [clojure.tools.logging :as log]
    [clojure-lsp.parser :as parser])
  (:import
    (org.eclipse.lsp4j
      CodeActionKind)))

(defn ^:private dissoc-if-nil [m k]
  (if (k m)
    m
    (dissoc m k)))

(defn ^:private find-missing-require [uri unknown-ns-diag unresolved-symbol-diag]
  (when (or unknown-ns-diag unresolved-symbol-diag)
    (let [{{:keys [line character] :as position} :start} (or (:range unknown-ns-diag)
                                                             (:range unresolved-symbol-diag))
          unknown-zloc (parser/cursor-zloc uri line character)]
      [(r.transform/find-missing-require unknown-zloc)
       position])))

(defn ^:private find-missing-import [uri unresolved-symbol-diag]
  (when unresolved-symbol-diag
    (let [{{:keys [line character] :as position} :start} (:range unresolved-symbol-diag)
          unresolved-sym-zloc (parser/cursor-zloc uri line character)]
      [(r.transform/find-missing-import unresolved-sym-zloc)
       position])))

(defn resolve-code-action
  [{{:keys [id uri line character]} :data :as code-action}
   zloc]
  (->
    (merge code-action
           (case id

             "add-missing-require"
             (let [missing-require-edit (f.refactor/call-refactor {:loc         zloc
                                                                   :refactoring :add-missing-libspec
                                                                   :uri         uri
                                                                   :version     0
                                                                   :row         (inc line)
                                                                   :col         (inc character)})]
               {:edit missing-require-edit})

             "add-missing-import"
             (let [missing-import-edit (f.refactor/refactor-client-seq-changes uri 0 (r.transform/add-common-import-to-namespace zloc))]
               {:edit missing-import-edit})

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
    (dissoc-if-nil :data)
    (dissoc-if-nil :diagnostics)
    (dissoc-if-nil :command)
    (dissoc-if-nil :edit)))

(defn all [zloc uri row col diagnostics client-capabilities]
  (let [workspace-edit-capability? (get-in client-capabilities [:workspace :workspace-edit])
        resolve-support?           (get-in client-capabilities [:text-document :code-action :resolve-support])
        inside-function?           (r.transform/inside-function? zloc)
        [_ {def-uri    :uri
            definition :usage}]    (f.definition/definition-usage uri row col)
        inline-symbol?             (r.transform/inline-symbol? def-uri definition)
        line                       (dec row)
        character                  (dec col)
        unknown-ns-diag            (first (filter #(= "unresolved-namespace" (:code %)) diagnostics))
        unresolved-symbol-diag     (first (filter #(= "unresolved-symbol" (:code %)) diagnostics))
        [missing-require
         missing-require-position] (find-missing-require uri unknown-ns-diag unresolved-symbol-diag)
        [missing-import
         missing-import-position]  (find-missing-import uri unresolved-symbol-diag)]
    (cond-> []

      missing-require
      (conj {:title      (str "Add missing '" missing-require "' require")
             :kind       CodeActionKind/QuickFix
             :preferred? true
             :data       {:id        "add-missing-require"
                          :uri       uri
                          :line      (:line missing-require-position)
                          :character (:character missing-require-position)}})

      missing-import
      (conj {:title      (str "Add missing '" missing-import "' import")
             :kind       CodeActionKind/QuickFix
             :preferred? true
             :data       {:id        "add-missing-import"
                          :uri       uri
                          :line      (:line missing-import-position)
                          :character (:character missing-import-position)}})

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
