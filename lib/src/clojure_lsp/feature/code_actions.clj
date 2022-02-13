(ns clojure-lsp.feature.code-actions
  (:require
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.feature.move-coll-entry :as f.move-coll-entry]
   [clojure-lsp.feature.resolve-macro :as f.resolve-macro]
   [clojure-lsp.feature.sort-map :as f.sort-map]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [medley.core :as medley]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]))

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

(defn ^:private find-private-function-to-create [uri diagnostics db]
  (when-let [{{{:keys [line character] :as position} :start} :range :as diag} (->> diagnostics
                                                                                   (filter #(= "unresolved-symbol" (:code %)))
                                                                                   first)]
    (when-let [diag-loc (parser/safe-cursor-loc uri line character db)]
      (when (r.transform/can-create-function? diag-loc)
        {:name (last (string/split (:message diag) #"Unresolved symbol: "))
         :position position}))))

(defn ^:private find-public-function-to-create [uri diagnostics db]
  (when-let [{{{:keys [line character] :as position} :start} :range} (->> diagnostics
                                                                          (filter #(or (= "unresolved-var" (:code %))
                                                                                       (= "unresolved-namespace" (:code %))))
                                                                          first)]
    (when-let [diag-loc (parser/safe-cursor-loc uri line character db)]
      (when (and (some-> diag-loc z/tag (= :token))
                 (some-> diag-loc z/sexpr namespace))
        (when-let [{:keys [new-ns ns name]} (r.transform/can-create-public-function? diag-loc uri db)]
          {:ns ns
           :new-ns new-ns
           :name name
           :position position})))))

(defn ^:private require-suggestion-actions
  [uri alias-suggestions]
  (map (fn [{:keys [ns alias refer position]}]
         {:title      (format "Add require '[%s %s %s]'" ns (if alias ":as" ":refer") (or alias (str "[" refer "]")))
          :kind       :quick-fix
          :preferred? true
          :command    {:title     "Add require suggestion"
                       :command   "add-require-suggestion"
                       :arguments [uri (:line position) (:character position) ns alias refer]}})
       alias-suggestions))

(defn ^:private missing-require-actions
  [uri missing-requires]
  (map (fn [{:keys [ns alias refer position]}]
         {:title      (format "Add require '[%s %s %s]'" ns (if alias ":as" ":refer") (or alias (str "[" refer "]")))
          :kind       :quick-fix
          :preferred? true
          :command    {:title     "Add missing require"
                       :command   "add-missing-libspec"
                       :arguments [uri (:line position) (:character position)]}})
       missing-requires))

(defn ^:private missing-import-actions [uri missing-imports]
  (map (fn [{:keys [missing-import position]}]
         {:title      (str "Add import '" missing-import "'")
          :kind       :quick-fix
          :preferred? true
          :command    {:title     "Add missing import"
                       :command   "add-missing-import"
                       :arguments [uri (:line position) (:character position)]}})
       missing-imports))

(defn ^:private change-colls-actions [uri line character other-colls]
  (map (fn [coll]
         {:title   (str "Change coll to " (name coll))
          :kind    :refactor
          :command {:title     "Change coll"
                    :command   "change-coll"
                    :arguments [uri line character (name coll)]}})
       other-colls))

(defn ^:private inline-symbol-action [uri line character]
  {:title   "Inline symbol"
   :kind    :refactor-inline
   :command {:title     "Inline symbol"
             :command   "inline-symbol"
             :arguments [uri line character]}})

(defn ^:private move-to-let-action [uri line character]
  {:title   "Move to let"
   :kind    :refactor-extract
   :command {:title     "Move to let"
             :command   "move-to-let"
             :arguments [uri line character "new-binding"]}})

(defn ^:private cycle-privacy-action [uri line character]
  {:title   "Cycle privacy"
   :kind    :refactor-rewrite
   :command {:title     "Cycle privacy"
             :command   "cycle-privacy"
             :arguments [uri line character]}})

(defn ^:private extract-function-action [uri line character]
  {:title   "Extract function"
   :kind    :refactor-extract
   :command {:title     "Extract function"
             :command   "extract-function"
             :arguments [uri line character "new-function"]}})

(defn ^:private clean-ns-action [uri line character]
  {:title   "Clean namespace"
   :kind    :source-organize-imports
   :command {:title     "Clean namespace"
             :command   "clean-ns"
             :arguments [uri line character]}})

(defn ^:private thread-first-all-action [uri line character]
  {:title   "Thread first all"
   :kind    :refactor-rewrite
   :command {:title     "Thread first all"
             :command   "thread-first-all"
             :arguments [uri line character]}})

(defn ^:private thread-last-all-action [uri line character]
  {:title   "Thread last all"
   :kind    :refactor-rewrite
   :command {:title     "Thread last all"
             :command   "thread-last-all"
             :arguments [uri line character]}})

(defn ^:private unwind-thread-action [uri line character]
  {:title   "Unwind thread once"
   :kind    :refactor-rewrite
   :command {:title     "Unwind thread once"
             :command   "unwind-thread"
             :arguments [uri line character]}})

(defn ^:private unwind-all-action [uri line character]
  {:title   "Unwind whole thread"
   :kind    :refactor-rewrite
   :command {:title     "Unwind whole thread"
             :command   "unwind-all"
             :arguments [uri line character]}})

(defn ^:private sort-map-action [uri line character]
  {:title   "Sort map"
   :kind    :refactor-rewrite
   :command {:title     "Sort map"
             :command   "sort-map"
             :arguments [uri line character]}})

(defn ^:private move-coll-entry-up-action [uri line character]
  {:title   "Move coll entry up"
   :kind    :refactor-rewrite
   :command {:title     "Move coll entry up"
             :command   "move-coll-entry-up"
             :arguments [uri line character]}})

(defn ^:private move-coll-entry-down-action [uri line character]
  {:title   "Move coll entry down"
   :kind    :refactor-rewrite
   :command {:title     "Move coll entry down"
             :command   "move-coll-entry-down"
             :arguments [uri line character]}})

(defn ^:private suppress-diagnostic-actions [diagnostics uri]
  (->> diagnostics
       (map (fn [{:keys [code]
                  {{:keys [line character]} :start} :range}]
              {:title   (format "Suppress '%s' diagnostic" code)
               :kind    :quick-fix
               :command {:title     "Suppress diagnostic"
                         :command   "suppress-diagnostic"
                         :arguments [uri line character code]}}))
       (medley/distinct-by :title)))

(defn ^:private create-test-action [function-name-loc uri line character]
  {:title   (format "Create test for '%s'" (z/string function-name-loc))
   :kind    :refactor-rewrite
   :command {:title     "Create test"
             :command   "create-test"
             :arguments [uri line character]}})

(defn ^:private create-private-function-action [uri function-to-create]
  {:title   (format "Create private function '%s'" (:name function-to-create))
   :kind    :quick-fix
   :command {:title     "Create function"
             :command   "create-function"
             :arguments [uri (:line (:position function-to-create)) (:character (:position function-to-create))]}})

(defn ^:private create-public-function-action [uri {:keys [name new-ns ns position]}]
  {:title   (if new-ns
              (format "Create namespace '%s' and '%s' function" new-ns name)
              (format "Create function '%s' on '%s'" name ns))
   :kind    :quick-fix
   :command {:title     "Create function"
             :command   "create-function"
             :arguments [uri (:line position) (:character position)]}})

(defn ^:private resolve-macro-as-action [uri row col macro-sym]
  {:title   (format "Resolve macro '%s' as..." (str macro-sym))
   :kind    :quick-fix
   :command {:title     "Resolve macro as..."
             :command   "resolve-macro-as"
             :arguments [uri row col]}})

(defn all [zloc uri row col diagnostics client-capabilities db]
  (let [line (dec row)
        character (dec col)
        workspace-edit-capability? (get-in client-capabilities [:workspace :workspace-edit])
        inside-function?* (future (r.transform/find-function-form zloc))
        private-function-to-create* (future (find-private-function-to-create uri diagnostics db))
        public-function-to-create* (future (find-public-function-to-create uri diagnostics db))
        inside-let?* (future (r.transform/find-let-form zloc))
        other-colls* (future (r.transform/find-other-colls zloc))
        can-thread?* (future (r.transform/can-thread? zloc))
        can-unwind-thread?* (future (r.transform/can-unwind-thread? zloc))
        can-create-test?* (future (r.transform/can-create-test? zloc uri db))
        macro-sym* (future (f.resolve-macro/find-full-macro-symbol-to-resolve zloc uri db))
        missing-requires* (future (find-missing-requires uri diagnostics db))
        missing-imports* (future (find-missing-imports uri diagnostics db))
        require-suggestions* (future (find-all-require-suggestions uri diagnostics @missing-requires* db))
        allow-sort-map?* (future (f.sort-map/sortable-map-zloc zloc))
        allow-move-entry-up?* (future (f.move-coll-entry/can-move-entry-up? zloc uri db))
        allow-move-entry-down?* (future (f.move-coll-entry/can-move-entry-down? zloc uri db))
        definition (q/find-definition-from-cursor (:analysis @db) (shared/uri->filename uri) row col db)
        inline-symbol?* (future (r.transform/inline-symbol? definition db))]
    (cond-> []

      (seq @missing-requires*)
      (into (missing-require-actions uri @missing-requires*))

      (seq @missing-imports*)
      (into (missing-import-actions uri @missing-imports*))

      (seq @require-suggestions*)
      (into (require-suggestion-actions uri @require-suggestions*))

      @private-function-to-create*
      (conj (create-private-function-action uri @private-function-to-create*))

      @public-function-to-create*
      (conj (create-public-function-action uri @public-function-to-create*))

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
           @allow-move-entry-up?*)
      (conj (move-coll-entry-up-action uri line character))

      (and workspace-edit-capability?
           @allow-move-entry-down?*)
      (conj (move-coll-entry-down-action uri line character))

      (and workspace-edit-capability?
           (seq diagnostics))
      (into (suppress-diagnostic-actions diagnostics uri))

      (and workspace-edit-capability?
           @can-create-test?*)
      (conj (create-test-action (:function-name-loc @can-create-test?*) uri line character))

      workspace-edit-capability?
      (conj (clean-ns-action uri line character)))))
