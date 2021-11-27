(ns clojure-lsp.feature.code-actions
  (:require
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.feature.resolve-macro :as f.resolve-macro]
   [clojure-lsp.feature.sort-map :as f.sort-map]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [medley.core :as medley]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log])
  (:import
   (org.eclipse.lsp4j
     CodeActionKind)))

(set! *warn-on-reflection* true)

(defn ^:private find-require-suggestions [uri missing-requires db diagnostic]
  (let [{{:keys [line character] :as position} :start} (:range diagnostic)]
    (when-let [diagnostic-zloc (parser/safe-cursor-loc uri line character db)]
      (->> (f.add-missing-libspec/find-require-suggestions diagnostic-zloc missing-requires db)
           (map #(assoc % :position position))))))

(defn ^:private find-all-require-suggestions [uri diagnostics missing-requires db]
  (let [unresolved-ns-diags (filter #(= "unresolved-namespace" (:code %)) diagnostics)
        unresolved-symbol-diags (filter #(= "unresolved-symbol" (:code %)) diagnostics)]
    (->> (cond-> []

           (seq unresolved-ns-diags)
           (into (map (partial find-require-suggestions uri missing-requires db)) unresolved-ns-diags)

           (seq unresolved-symbol-diags)
           (into (map (partial find-require-suggestions uri missing-requires db)) unresolved-symbol-diags))
         flatten
         (remove nil?))))

(defn ^:private find-missing-require [uri db diagnostic]
  (let [{{:keys [line character] :as position} :start} (:range diagnostic)]
    (when-let [diagnostic-zloc (parser/safe-cursor-loc uri line character db)]
      (when-let [missing-require (f.add-missing-libspec/find-missing-ns-require diagnostic-zloc db)]
        (assoc missing-require :position position)))))

(defn ^:private find-missing-requires [uri diagnostics db]
  (let [unresolved-ns-diags (filter #(= "unresolved-namespace" (:code %)) diagnostics)
        unresolved-symbol-diags (filter #(= "unresolved-symbol" (:code %)) diagnostics)]
    (->> (cond-> []

           (seq unresolved-ns-diags)
           (into (map (partial find-missing-require uri db) unresolved-ns-diags))

           (seq unresolved-symbol-diags)
           (into (map (partial find-missing-require uri db) unresolved-symbol-diags)))
         (remove nil?))))

(defn ^:private find-missing-import [uri db diagnostic]
  (let [{{:keys [line character] :as position} :start} (:range diagnostic)]
    (when-let [diagnostic-zloc (parser/safe-cursor-loc uri line character db)]
      (when-let [missing-import (f.add-missing-libspec/find-missing-import diagnostic-zloc)]
        {:missing-import missing-import
         :position position}))))

(defn ^:private find-missing-imports [uri diagnostics db]
  (let [unresolved-ns-diags (filter #(= "unresolved-namespace" (:code %)) diagnostics)
        unresolved-symbol-diags (filter #(= "unresolved-symbol" (:code %)) diagnostics)]
    (->> (cond-> []

           (seq unresolved-ns-diags)
           (into (map (partial find-missing-import uri db) unresolved-ns-diags))

           (seq unresolved-symbol-diags)
           (into (map (partial find-missing-import uri db) unresolved-symbol-diags)))
         (remove nil?))))

(defn ^:private find-function-to-create [uri diagnostics db]
  (when-let [{{{:keys [line character] :as position} :start} :range :as diag} (->> diagnostics
                                                                                   (filter #(= "unresolved-symbol" (:code %)))
                                                                                   first)]
    (when-let [diag-loc (parser/safe-cursor-loc uri line character db)]
      (when (r.transform/can-create-function? diag-loc)
        {:name (last (string/split (:message diag) #"Unresolved symbol: "))
         :position position}))))

(defn resolve-code-action
  [{{:keys [id uri line character chosen-alias chosen-ns chosen-refer coll diagnostic-code]} :data :as code-action}
   zloc
   db]
  (->
    (merge code-action
           (case id

             "add-missing-require"
             (f.refactor/call-refactor {:loc         zloc
                                        :refactoring :add-missing-libspec
                                        :uri         uri
                                        :version     0
                                        :row         (inc line)
                                        :col         (inc character)}
                                       db)

             "add-missing-import"
             (let [missing-import-edit (f.refactor/refactor-client-seq-changes uri 0 (f.add-missing-libspec/add-common-import-to-namespace zloc db) db)]
               {:edit missing-import-edit})

             "add-require-suggestion"
             (let [alias-suggestion-edit (f.refactor/refactor-client-seq-changes uri 0 (f.add-missing-libspec/add-require-suggestion zloc chosen-ns chosen-alias chosen-refer db) db)]
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

             "refactor-unwind-thread"
             {:command {:title     "Unwind thread once"
                        :command   "unwind-thread"
                        :arguments [uri line character]}}

             "refactor-unwind-all"
             {:command {:title     "Unwind whole thread"
                        :command   "unwind-all"
                        :arguments [uri line character]}}

             "refactor-sort-map"
             {:command {:title     "Sort map"
                        :command   "sort-map"
                        :arguments [uri line character]}}

             "refactor-suppress-diagnostic"
             {:command {:title "Suppress diagnostic"
                        :command "suppress-diagnostic"
                        :arguments [uri line character diagnostic-code]}}

             "refactor-create-test"
             {:command {:title     "Create test"
                        :command   "create-test"
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

(defn ^:private require-suggestion-actions
  [uri alias-suggestions]
  (map (fn [{:keys [ns alias refer position]}]
         {:data       {:character (:character position)
                       :chosen-alias alias
                       :chosen-ns ns
                       :chosen-refer refer
                       :id "add-require-suggestion"
                       :line (:line position)
                       :uri uri}
          :kind       CodeActionKind/QuickFix
          :preferred? true
          :title      (format "Add require '[%s %s %s]'" ns (if alias ":as" ":refer") (or alias (str "[" refer "]")))})
       alias-suggestions))

(defn ^:private missing-require-actions
  [uri missing-requires]
  (map (fn [{:keys [ns alias refer position]}]
         {:title      (format "Add require '[%s %s %s]'" ns (if alias ":as" ":refer") (or alias (str "[" refer "]")))
          :kind       CodeActionKind/QuickFix
          :preferred? true
          :data       {:id        "add-missing-require"
                       :uri       uri
                       :line      (:line position)
                       :character (:character position)}})
       missing-requires))

(defn ^:private missing-import-actions [uri missing-imports]
  (map (fn [{:keys [missing-import position]}]
         {:title      (str "Add import '" missing-import "'")
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

(defn ^:private unwind-thread-action [uri line character]
  {:title "Unwind thread once"
   :kind CodeActionKind/RefactorRewrite
   :data {:id "refactor-unwind-thread"
          :uri uri
          :line line
          :character character}})

(defn ^:private unwind-all-action [uri line character]
  {:title "Unwind whole thread"
   :kind CodeActionKind/RefactorRewrite
   :data {:id "refactor-unwind-all"
          :uri uri
          :line line
          :character character}})

(defn ^:private sort-map-action [uri line character]
  {:title "Sort map"
   :kind CodeActionKind/RefactorRewrite
   :data {:id "refactor-sort-map"
          :uri uri
          :line line
          :character character}})

(defn ^:private suppress-diagnostic-actions [diagnostics uri]
  (->> diagnostics
       (map (fn [{:keys [code]
                  {{:keys [line character]} :start} :range}]
              {:title (format "Suppress '%s' diagnostic" code)
               :kind CodeActionKind/QuickFix
               :data {:id "refactor-suppress-diagnostic"
                      :uri uri
                      :line line
                      :character character
                      :diagnostic-code code}}))
       (medley/distinct-by :title)))

(defn ^:private create-test-action [function-name-loc uri line character]
  {:title (format "Create test for '%s'" (z/string function-name-loc))
   :kind CodeActionKind/RefactorRewrite
   :data {:id "refactor-create-test"
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

(defn all [zloc uri row col diagnostics client-capabilities db]
  (let [line (dec row)
        character (dec col)
        workspace-edit-capability? (get-in client-capabilities [:workspace :workspace-edit])
        resolve-action-support? (get-in client-capabilities [:text-document :code-action :resolve-support])
        inside-function?* (future (r.transform/find-function-form zloc))
        function-to-create* (future (find-function-to-create uri diagnostics db))
        inside-let?* (future (r.transform/find-let-form zloc))
        other-colls* (future (r.transform/find-other-colls zloc))
        can-thread?* (future (r.transform/can-thread? zloc))
        can-unwind-thread?* (future (r.transform/can-unwind-thread? zloc))
        can-create-test?* (future (r.transform/can-create-test? zloc uri db))
        macro-sym* (future (f.resolve-macro/find-full-macro-symbol-to-resolve uri row col db))
        missing-requires* (future (find-missing-requires uri diagnostics db))
        missing-imports* (future (find-missing-imports uri diagnostics db))
        require-suggestions* (future (find-all-require-suggestions uri diagnostics @missing-requires* db))
        allow-sort-map?* (future (f.sort-map/sortable-map? zloc))
        definition (q/find-definition-from-cursor (:analysis @db) (shared/uri->filename uri) row col db)
        inline-symbol?* (future (r.transform/inline-symbol? definition db))]
    (cond-> []

      (seq @missing-requires*)
      (into (missing-require-actions uri @missing-requires*))

      (seq @missing-imports*)
      (into (missing-import-actions uri @missing-imports*))

      (seq @require-suggestions*)
      (into (require-suggestion-actions uri @require-suggestions*))

      @function-to-create*
      (conj (create-private-function-action uri @function-to-create*))

      @macro-sym*
      (conj (resolve-macro-as-action uri row col @macro-sym*))

      @inline-symbol?*
      (conj (inline-symbol-action uri line character))

      @other-colls*
      (into (change-colls-actions uri line character @other-colls*))

      @inside-let?*
      (conj (move-to-let-action uri line character))

      @inside-function?*
      (conj (cycle-privacy-action uri line character)
            (extract-function-action uri line character))

      @can-thread?*
      (conj (thread-first-all-action uri line character)
            (thread-last-all-action uri line character))

      @can-unwind-thread?*
      (conj (unwind-thread-action uri line character)
            (unwind-all-action uri line character))

      (and workspace-edit-capability?
           @allow-sort-map?*)
      (conj (sort-map-action uri line character))

      (and workspace-edit-capability?
           (seq diagnostics))
      (into (suppress-diagnostic-actions diagnostics uri))

      (and workspace-edit-capability?
           @can-create-test?*)
      (conj (create-test-action (:function-name-loc @can-create-test?*) uri line character))

      workspace-edit-capability?
      (conj (clean-ns-action uri line character))

      (not resolve-action-support?)
      (->> (map #(resolve-code-action % zloc db))))))
