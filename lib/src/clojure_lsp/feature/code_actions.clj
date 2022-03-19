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
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defn ^:private diagnostics-with-code [code-set diagnostics]
  (filter (comp code-set :code) diagnostics))

(defn ^:private resolvable-diagnostics [diagnostics root-zloc]
  (when root-zloc
    (->> diagnostics
         (diagnostics-with-code #{"unresolved-namespace" "unresolved-symbol" "unresolved-var"})
         (keep (fn [{{{:keys [line character] :as position} :start} :range :as diagnostic}]
                 (when-let [zloc (parser/to-cursor root-zloc line character)]
                   (assoc diagnostic
                          :zloc     zloc
                          :position position)))))))

(defn ^:private find-require-suggestions [uri db {:keys [position zloc]}]
  (->> (f.add-missing-libspec/find-require-suggestions zloc uri db)
       (map #(assoc % :position position))))

(defn ^:private find-all-require-suggestions [diagnostics missing-requires uri db]
  (->> diagnostics
       (mapcat (partial find-require-suggestions uri db))
       (remove (fn [suggestion]
                 (some (comp #{(:ns suggestion)} :ns)
                       missing-requires)))))

(defn ^:private find-missing-require [db {:keys [position zloc]}]
  (some-> (f.add-missing-libspec/find-missing-ns-require zloc db)
          (assoc :position position)))

(defn ^:private find-missing-requires [diagnostics db]
  (keep (partial find-missing-require db) diagnostics))

(defn ^:private find-missing-import [{:keys [position zloc]}]
  (when-let [missing-import (f.add-missing-libspec/find-missing-import zloc)]
    {:missing-import missing-import
     :position       position}))

(defn ^:private find-missing-imports [diagnostics]
  (keep find-missing-import diagnostics))

(defn ^:private find-private-function-to-create [diagnostics]
  (when-let [{:keys [message position zloc]} (->> diagnostics
                                                  (diagnostics-with-code #{"unresolved-symbol"})
                                                  first)]
    (when (r.transform/can-create-function? zloc)
      {:name     (last (string/split message #"Unresolved symbol: "))
       :position position})))

(defn ^:private find-public-function-to-create [uri diagnostics db]
  (when-let [{:keys [zloc position]} (->> diagnostics
                                          (diagnostics-with-code #{"unresolved-var" "unresolved-namespace"})
                                          first)]
    (some-> (r.transform/find-public-function-to-create zloc uri db)
            (assoc :position position))))

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

(defn ^:private introduce-let-action [uri line character]
  {:title   "Introduce let"
   :kind    :refactor-extract
   :command {:title     "Introduce let"
             :command   "introduce-let"
             :arguments [uri line character "new-binding"]}})

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

(defn ^:private cycle-fn-literal-action [uri line character]
  {:title   "Cycle function literal"
   :kind    :refactor-rewrite
   :command {:title     "Cycle function literal"
             :command   "cycle-fn-literal"
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

(defn all [root-zloc uri row col diagnostics client-capabilities db]
  (let [zloc (parser/to-pos root-zloc row col)
        line (dec row)
        character (dec col)
        resolvable-diagnostics (resolvable-diagnostics diagnostics root-zloc)
        workspace-edit-capability? (get-in client-capabilities [:workspace :workspace-edit])
        inside-function?* (future (r.transform/find-function-form zloc))
        private-function-to-create* (future (find-private-function-to-create resolvable-diagnostics))
        public-function-to-create* (future (find-public-function-to-create uri resolvable-diagnostics db))
        inside-let?* (future (r.transform/find-let-form zloc))
        other-colls* (future (r.transform/find-other-colls zloc))
        can-thread?* (future (r.transform/can-thread? zloc))
        can-unwind-thread?* (future (r.transform/can-unwind-thread? zloc))
        can-create-test?* (future (r.transform/can-create-test? zloc uri db))
        macro-sym* (future (f.resolve-macro/find-full-macro-symbol-to-resolve zloc uri db))
        resolvable-require-diagnostics (diagnostics-with-code #{"unresolved-namespace" "unresolved-symbol"} resolvable-diagnostics)
        missing-requires* (future (find-missing-requires resolvable-require-diagnostics db))
        missing-imports* (future (find-missing-imports resolvable-require-diagnostics))
        require-suggestions* (future (find-all-require-suggestions resolvable-require-diagnostics @missing-requires* uri db))
        allow-sort-map?* (future (f.sort-map/sortable-map-zloc zloc))
        allow-move-entry-up?* (future (f.move-coll-entry/can-move-entry-up? zloc uri db))
        allow-move-entry-down?* (future (f.move-coll-entry/can-move-entry-down? zloc uri db))
        can-cycle-fn-literal?* (future (r.transform/can-cycle-fn-literal? zloc))
        definition (q/find-definition-from-cursor (:analysis @db) (shared/uri->filename uri) row col db)
        inline-symbol?* (future (r.transform/inline-symbol? definition db))]
    (cond-> []
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

      @can-cycle-fn-literal?*
      (conj (cycle-fn-literal-action uri line character))

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

      zloc
      (conj (introduce-let-action uri line character))

      (and workspace-edit-capability?
           (seq diagnostics))
      (into (suppress-diagnostic-actions diagnostics uri))

      (and workspace-edit-capability?
           @can-create-test?*)
      (conj (create-test-action (:function-name-loc @can-create-test?*) uri line character))

      workspace-edit-capability?
      (conj (clean-ns-action uri line character)))))
