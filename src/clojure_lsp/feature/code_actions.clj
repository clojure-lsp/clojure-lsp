(ns clojure-lsp.feature.code-actions
  (:require
    [clojure-lsp.feature.refactor :as f.refactor]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.refactor.transform :as r.transform]
    [clojure-lsp.queries :as q]
    [clojure-lsp.db :as db]
    [clojure-lsp.shared :as shared]
    [taoensso.timbre :as log])
  (:import
    (org.eclipse.lsp4j
      CodeActionKind)))

(defn ^:private find-missing-require [uri diagnostic]
  (let [{{:keys [line character] :as position} :start} (:range diagnostic)]
    (when-let [diagnostic-zloc (parser/safe-cursor-loc uri line character)]
      (when-let [missing-require (r.transform/find-missing-require diagnostic-zloc)]
        {:missing-require missing-require
         :position        position}))))

(defn ^:private find-missing-requires [uri diagnostics]
  (let [unresolved-ns-diags (filter #(= "unresolved-namespace" (:code %)) diagnostics)
        unresolved-symbol-diags (filter #(= "unresolved-symbol" (:code %)) diagnostics)]
    (->> (cond-> []

           (seq unresolved-ns-diags)
           (into (map (partial find-missing-require uri) unresolved-ns-diags))

           (seq unresolved-symbol-diags)
           (into (map (partial find-missing-require uri) unresolved-symbol-diags)))
         (remove nil?))))

(defn ^:private find-missing-import [uri diagnostic]
  (let [{{:keys [line character] :as position} :start} (:range diagnostic)]
    (when-let [diagnostic-zloc (parser/safe-cursor-loc uri line character)]
      (when-let [missing-import (r.transform/find-missing-import diagnostic-zloc)]
        {:missing-import missing-import
         :position       position}))))

(defn ^:private find-missing-imports [uri diagnostics]
  (let [unresolved-ns-diags (filter #(= "unresolved-namespace" (:code %)) diagnostics)
        unresolved-symbol-diags (filter #(= "unresolved-symbol" (:code %)) diagnostics)]
    (->> (cond-> []

           (seq unresolved-ns-diags)
           (into (map (partial find-missing-import uri) unresolved-ns-diags))

           (seq unresolved-symbol-diags)
           (into (map (partial find-missing-import uri) unresolved-symbol-diags)))
         (remove nil?))))

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
    (dissoc :data)))

(defn all [zloc uri row col diagnostics client-capabilities]
  (let [workspace-edit-capability? (get-in client-capabilities [:workspace :workspace-edit])
        resolve-support? (get-in client-capabilities [:text-document :code-action :resolve-support])
        inside-function? (r.transform/inside-function? zloc)
        definition (q/find-definition-from-cursor (:analysis @db/db) (shared/uri->filename uri) row col)
        inline-symbol? (r.transform/inline-symbol? definition)
        line (dec row)
        character (dec col)
        missing-requires (find-missing-requires uri diagnostics)
        missing-imports (find-missing-imports uri diagnostics)]
    (cond-> []

      (seq missing-requires)
      (into (map (fn [{:keys [missing-require position]}]
                   {:title      (str "Add missing '" missing-require "' require")
                    :kind       CodeActionKind/QuickFix
                    :preferred? true
                    :data       {:id        "add-missing-require"
                                 :uri       uri
                                 :line      (:line position)
                                 :character (:character position)}})
                 missing-requires))

      (seq missing-imports)
      (into (map (fn [{:keys [missing-import position]}]
                   {:title      (str "Add missing '" missing-import "' import")
                    :kind       CodeActionKind/QuickFix
                    :preferred? true
                    :data       {:id        "add-missing-import"
                                 :uri       uri
                                 :line      (:line position)
                                 :character (:character position)}})
                 missing-imports))

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
