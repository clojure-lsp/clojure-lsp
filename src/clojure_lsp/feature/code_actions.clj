(ns clojure-lsp.feature.code-actions
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.feature.resolve-macro :as f.resolve-macro]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [taoensso.timbre :as log])
  (:import
   (org.eclipse.lsp4j
     CodeActionKind)))

(defn ^:private find-alias-suggestion [uri diagnostic]
  (let [{{:keys [line character] :as position} :start} (:range diagnostic)]
    (when-let [diagnostic-zloc (parser/safe-cursor-loc uri line character)]
      (->> (r.transform/find-alias-suggestion diagnostic-zloc)
           (map (fn [{:keys [ns alias]}]
                  {:ns ns
                   :alias alias
                   :position position}))))))

(defn ^:private find-alias-suggestions [uri diagnostics]
  (let [unresolved-ns-diags (filter #(= "unresolved-namespace" (:code %)) diagnostics)]
    (->> unresolved-ns-diags
         (map (partial find-alias-suggestion uri))
         flatten
         (remove nil?))))

(defn ^:private find-missing-require [uri diagnostic]
  (let [{{:keys [line character] :as position} :start} (:range diagnostic)]
    (when-let [diagnostic-zloc (parser/safe-cursor-loc uri line character)]
      (when-let [missing-require (r.transform/find-missing-require diagnostic-zloc)]
        {:missing-require missing-require
         :position position}))))

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
         :position position}))))

(defn ^:private find-missing-imports [uri diagnostics]
  (let [unresolved-ns-diags (filter #(= "unresolved-namespace" (:code %)) diagnostics)
        unresolved-symbol-diags (filter #(= "unresolved-symbol" (:code %)) diagnostics)]
    (->> (cond-> []

           (seq unresolved-ns-diags)
           (into (map (partial find-missing-import uri) unresolved-ns-diags))

           (seq unresolved-symbol-diags)
           (into (map (partial find-missing-import uri) unresolved-symbol-diags)))
         (remove nil?))))

(defn ^:private find-function-to-create [uri diagnostics]
  (when-let [{{{:keys [line character] :as position} :start} :range :as diag} (->> diagnostics
                                                                                   (filter #(= "unresolved-symbol" (:code %)))
                                                                                   first)]
    (when-let [diag-loc (parser/safe-cursor-loc uri line character)]
      (when (r.transform/can-create-function? diag-loc)
        {:name (last (string/split (:message diag) #"Unresolved symbol: "))
         :position position}))))

(defn resolve-code-action
  [{{:keys [id uri line character chosen-alias coll]} :data :as code-action}
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

             "add-alias-suggestion-require"
             (let [alias-suggestion-edit (f.refactor/refactor-client-seq-changes uri 0 (r.transform/add-alias-suggestion zloc chosen-alias))]
               {:edit alias-suggestion-edit})

             "refactor-create-private-function"
             {:command {:title     "Create function"
                        :command   "create-function"
                        :arguments [uri line character]}}

             "refactor-inline-symbol"
             {:command {:title     "Inline symbol"
                        :command   "inline-symbol"
                        :arguments [uri line character]}}

             "refactor-change-coll"
             {:command {:title "Change coll"
                        :command "change-coll"
                        :arguments [uri line character coll]}}

             "refactor-move-to-let"
             {:command {:title     "Move to let"
                        :command   "move-to-let"
                        :arguments [uri line character "new-binding"]}}

             "refactor-cycle-privacy"
             {:command {:title     "Cycle privacy"
                        :command   "cycle-privacy"
                        :arguments [uri line character]}}

             "refactor-extract-function"
             {:command {:title     "Extract function"
                        :command   "extract-function"
                        :arguments [uri line character "new-function"]}}

             "refactor-thread-first-all"
             {:command {:title     "Thread first all"
                        :command   "thread-first-all"
                        :arguments [uri line character]}}

             "refactor-thread-last-all"
             {:command {:title     "Thread last all"
                        :command   "thread-last-all"
                        :arguments [uri line character]}}

             "clean-ns"
             {:command {:title     "Clean namespace"
                        :command   "clean-ns"
                        :arguments [uri line character]}}

             "resolve-macro-as"
             {:command {:title "Resolve macro as..."
                        :command "resolve-macro-as"
                        :arguments [uri line character]}}

             {}))
    (dissoc :data)))

(defn ^:private alias-suggestion-actions
  [uri alias-suggestions]
  (map (fn [{:keys [ns alias position]}]
         {:title      (str "Add require '" ns "' as '" alias "'")
          :kind       CodeActionKind/QuickFix
          :preferred? true
          :data       {:id "add-alias-suggestion-require"
                       :uri uri
                       :line (:line position)
                       :character (:character position)
                       :chosen-alias alias}})
       alias-suggestions))

(defn ^:private missing-require-actions
  [uri missing-requires]
  (map (fn [{:keys [missing-require position]}]
         {:title      (str "Add missing '" missing-require "' require")
          :kind       CodeActionKind/QuickFix
          :preferred? true
          :data       {:id        "add-missing-require"
                       :uri       uri
                       :line      (:line position)
                       :character (:character position)}})
       missing-requires))

(defn ^:private missing-import-actions [uri missing-imports]
  (map (fn [{:keys [missing-import position]}]
         {:title      (str "Add missing '" missing-import "' import")
          :kind       CodeActionKind/QuickFix
          :preferred? true
          :data       {:id        "add-missing-import"
                       :uri       uri
                       :line      (:line position)
                       :character (:character position)}})
       missing-imports))

(defn ^:private change-colls-actions [uri line character other-colls]
  (map (fn [coll]
         {:title      (str "Change coll to " (name coll))
          :kind       CodeActionKind/Refactor
          :data       {:id        "refactor-change-coll"
                       :uri       uri
                       :line      line
                       :character character
                       :coll (name coll)}})
       other-colls))

(defn ^:private inline-symbol-action [uri line character]
  {:title "Inline symbol"
   :kind  CodeActionKind/RefactorInline
   :data  {:id        "refactor-inline-symbol"
           :uri       uri
           :line      line
           :character character}})

(defn ^:private move-to-let-action [uri line character]
  {:title "Move to let"
   :kind  CodeActionKind/RefactorExtract
   :data  {:id        "refactor-move-to-let"
           :uri       uri
           :line      line
           :character character}})

(defn ^:private cycle-privacy-action [uri line character]
  {:title "Cycle privacy"
   :kind  CodeActionKind/RefactorRewrite
   :data  {:id        "refactor-cycle-privacy"
           :uri       uri
           :line      line
           :character character}})

(defn ^:private extract-function-action [uri line character]
  {:title "Extract function"
   :kind  CodeActionKind/RefactorExtract
   :data  {:id        "refactor-extract-function"
           :uri       uri
           :line      line
           :character character}})

(defn ^:private clean-ns-action [uri line character]
  {:title "Clean namespace"
   :kind  CodeActionKind/SourceOrganizeImports
   :data  {:id        "clean-ns"
           :uri       uri
           :line      line
           :character character}})

(defn ^:private thread-first-all-action [uri line character]
  {:title "Thread first all"
   :kind CodeActionKind/RefactorRewrite
   :data {:id "refactor-thread-first-all"
          :uri uri
          :line line
          :character character}})

(defn ^:private thread-last-all-action [uri line character]
  {:title "Thread last all"
   :kind CodeActionKind/RefactorRewrite
   :data {:id "refactor-thread-last-all"
          :uri uri
          :line line
          :character character}})

(defn ^:private create-private-function-action [uri function-to-create]
  {:title (format "Create private function '%s'" (:name function-to-create))
   :kind CodeActionKind/QuickFix
   :data {:id "refactor-create-private-function"
          :uri uri
          :line      (:line (:position function-to-create))
          :character (:character (:position function-to-create))}})

(defn ^:private resolve-macro-as-action [uri row col macro-sym]
  {:title (format "Resolve macro '%s' as..." (str macro-sym))
   :kind CodeActionKind/QuickFix
   :data {:id "resolve-macro-as"
          :uri uri
          :line row
          :character col}})

(defn all [zloc uri row col diagnostics client-capabilities]
  (let [workspace-edit-capability? (get-in client-capabilities [:workspace :workspace-edit])
        resolve-support? (get-in client-capabilities [:text-document :code-action :resolve-support])
        inside-function? (r.transform/find-function-form zloc)
        function-to-create (find-function-to-create uri diagnostics)
        inside-let? (r.transform/find-let-form zloc)
        other-colls (r.transform/find-other-colls zloc)
        definition (q/find-definition-from-cursor (:analysis @db/db) (shared/uri->filename uri) row col)
        inline-symbol? (r.transform/inline-symbol? definition)
        can-thread? (r.transform/can-thread? zloc)
        macro-sym (f.resolve-macro/find-full-macro-symbol-to-resolve uri row col)
        line (dec row)
        character (dec col)
        missing-requires (find-missing-requires uri diagnostics)
        missing-imports (find-missing-imports uri diagnostics)
        alias-suggestions (find-alias-suggestions uri diagnostics)]
    (cond-> []

      (seq missing-requires)
      (into (missing-require-actions uri missing-requires))

      (seq missing-imports)
      (into (missing-import-actions uri missing-imports))

      (seq alias-suggestions)
      (into (alias-suggestion-actions uri alias-suggestions))

      function-to-create
      (conj (create-private-function-action uri function-to-create))

      macro-sym
      (conj (resolve-macro-as-action uri row col macro-sym))

      inline-symbol?
      (conj (inline-symbol-action uri line character))

      other-colls
      (into (change-colls-actions uri line character other-colls))

      inside-let?
      (conj (move-to-let-action uri line character))

      inside-function?
      (conj (cycle-privacy-action uri line character)
            (extract-function-action uri line character))

      can-thread?
      (conj (thread-first-all-action uri line character)
            (thread-last-all-action uri line character))

      workspace-edit-capability?
      (conj (clean-ns-action uri line character))

      (not resolve-support?)
      (->> (map #(resolve-code-action % zloc))))))
