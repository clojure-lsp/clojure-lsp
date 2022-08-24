(ns clojure-lsp.feature.code-actions
  (:require
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.feature.destructure-keys :as f.destructure-keys]
   [clojure-lsp.feature.drag :as f.drag]
   [clojure-lsp.feature.drag-param :as f.drag-param]
   [clojure-lsp.feature.resolve-macro :as f.resolve-macro]
   [clojure-lsp.feature.restructure-keys :as f.restructure-keys]
   [clojure-lsp.feature.sort-clauses :as f.sort-clauses]
   [clojure-lsp.feature.thread-get :as f.thread-get]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.refactor.edit :as edit]
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
         (keep (fn [{{position :start} :range :as diagnostic}]
                 (let [[row col] (shared/position->row-col position)]
                   (when-let [zloc (parser/to-pos root-zloc row col)]
                     (assoc diagnostic
                            :zloc     zloc
                            :position position))))))))

(defn ^:private find-require-suggestions [uri db {:keys [position zloc]}]
  (->> (f.add-missing-libspec/find-require-suggestions zloc uri db)
       (map #(assoc % :position position))))

(defn ^:private find-all-require-suggestions [diagnostics missing-requires uri db]
  (->> diagnostics
       (mapcat (partial find-require-suggestions uri db))
       (remove (fn [suggestion]
                 (some (comp #{(:ns suggestion)} :ns)
                       missing-requires)))))

(defn ^:private find-missing-require [uri db {:keys [position zloc]}]
  (some-> (f.add-missing-libspec/find-missing-ns-require zloc uri db)
          (assoc :position position)))

(defn ^:private find-missing-requires [diagnostics uri db]
  (keep (partial find-missing-require uri db) diagnostics))

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
  (map (fn [{:keys [ns alias refer position count]}]
         {:title     (format "Add require '[%s%s%s]'%s"
                             ns
                             (if alias (str " :as " alias) "")
                             (if refer (str " :refer [" refer "]") "")
                             (if count (str " × " count) ""))
          :kind      :quick-fix
          :preferred true
          :command   {:title     "Add require suggestion"
                      :command   "add-require-suggestion"
                      :arguments [uri (:line position) (:character position) ns alias refer]}})
       alias-suggestions))

(defn ^:private missing-import-actions [uri missing-imports]
  (map (fn [{:keys [missing-import position]}]
         {:title     (str "Add import '" missing-import "'")
          :kind      :quick-fix
          :preferred true
          :command   {:title     "Add missing import"
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

(defn ^:private destructure-keys-action [uri line character]
  {:title   "Destructure keys"
   :kind    :refactor-rewrite
   :command {:title     "Destructure keys"
             :command   "destructure-keys"
             :arguments [uri line character]}})

(defn ^:private restructure-keys-action [uri line character]
  {:title   "Restructure keys"
   :kind    :refactor-rewrite
   :command {:title     "Restructure keys"
             :command   "restructure-keys"
             :arguments [uri line character]}})

(defn ^:private demote-fn-action [uri line character]
  {:title   "Demote fn to #()"
   :kind    :refactor-rewrite
   :command {:title     "Demote fn to #()"
             :command   "demote-fn"
             :arguments [uri line character]}})

(defn ^:private promote-fn-action [uri line character type]
  (let [title (str "Promote " (case type
                                :literal-to-fn "#() to fn"
                                :fn-to-defn "fn to defn"))]
    {:title   title
     :kind    :refactor-rewrite
     :command {:title     title
               :command   "promote-fn"
               :arguments [uri line character nil]}}))

(defn ^:private extract-function-action [uri line character]
  {:title   "Extract function"
   :kind    :refactor-extract
   :command {:title     "Extract function"
             :command   "extract-function"
             :arguments [uri line character "new-function"]}})

(defn ^:private extract-to-def-action [uri line character]
  {:title   "Extract to def"
   :kind    :refactor-extract
   :command {:title     "Extract to def"
             :command   "extract-to-def"
             :arguments [uri line character nil]}})

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

(defn ^:private sort-clauses-action [uri line character {:keys [context]}]
  (let [title (case context
                :map    "Sort map"
                :vector "Sort vector"
                :set    "Sort set"
                :list   "Sort list"
                "Sort clauses")]
    {:title   title
     :kind    :refactor-rewrite
     :command {:title     title
               :command   "sort-clauses"
               :arguments [uri line character]}}))

(defn ^:private drag-backward-action [uri line character]
  {:title   "Drag backward"
   :kind    :refactor-rewrite
   :command {:title     "Drag backward"
             :command   "drag-backward"
             :arguments [uri line character]}})

(defn ^:private drag-forward-action [uri line character]
  {:title   "Drag forward"
   :kind    :refactor-rewrite
   :command {:title     "Drag forward"
             :command   "drag-forward"
             :arguments [uri line character]}})

(defn ^:private drag-param-backward-action [uri line character]
  {:title   "Drag param backward"
   :kind    :refactor-rewrite
   :command {:title     "Drag param backward"
             :command   "drag-param-backward"
             :arguments [uri line character]}})

(defn ^:private drag-param-forward-action [uri line character]
  {:title   "Drag param forward"
   :kind    :refactor-rewrite
   :command {:title     "Drag param forward"
             :command   "drag-param-forward"
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

(defn ^:private resolve-macro-as-action [uri line character macro-sym]
  {:title   (format "Resolve macro '%s' as..." (str macro-sym))
   :kind    :quick-fix
   :command {:title     "Resolve macro as..."
             :command   "resolve-macro-as"
             :arguments [uri line character]}})

(defn ^:private get-in-more-action [uri line character]
  {:title   "Move another expression to get/get-in"
   :kind    :refactor-rewrite
   :command {:title     "Move another expression to get/get-in"
             :command   "get-in-more"
             :arguments [uri line character]}})

(defn ^:private get-in-all-action [uri line character]
  {:title   "Move all expressions to get/get-in"
   :kind    :refactor-rewrite
   :command {:title     "Move all expressions to get/get-in"
             :command   "get-in-all"
             :arguments [uri line character]}})

(defn ^:private get-in-less-action [uri line character]
  {:title   "Remove one element from get/get-in"
   :kind    :refactor-rewrite
   :command {:title     "Remove one element from get/get-in"
             :command   "get-in-less"
             :arguments [uri line character]}})

(defn ^:private get-in-none-action [uri line character]
  {:title   "Unwind whole get/get-in"
   :kind    :refactor-rewrite
   :command {:title     "Unwind whole get/get-in"
             :command   "get-in-none"
             :arguments [uri line character]}})

(defn all [root-zloc uri row col diagnostics client-capabilities db]
  (let [zloc (parser/to-pos root-zloc row col)
        line (dec row)
        character (dec col)
        resolvable-diagnostics (resolvable-diagnostics diagnostics root-zloc)
        workspace-edit-capability? (get-in client-capabilities [:workspace :workspace-edit])
        inside-function?* (future (r.transform/find-function-form zloc))
        private-function-to-create* (future (find-private-function-to-create resolvable-diagnostics))
        public-function-to-create* (future (find-public-function-to-create uri resolvable-diagnostics db))
        other-colls* (future (r.transform/find-other-colls zloc))
        can-thread?* (future (r.transform/can-thread? zloc))
        can-unwind-thread?* (future (r.transform/can-unwind-thread? zloc))
        can-get-in-more?* (future (f.thread-get/can-get-in-more? zloc))
        can-get-in-less?* (future (f.thread-get/can-get-in-less? zloc))
        can-create-test?* (future (r.transform/can-create-test? zloc uri db))
        macro-sym* (future (f.resolve-macro/find-full-macro-symbol-to-resolve zloc uri db))
        resolvable-require-diagnostics (diagnostics-with-code #{"unresolved-namespace" "unresolved-symbol"} resolvable-diagnostics)
        missing-requires* (future (find-missing-requires resolvable-require-diagnostics uri db))
        missing-imports* (future (find-missing-imports resolvable-require-diagnostics))
        require-suggestions* (future (find-all-require-suggestions resolvable-require-diagnostics @missing-requires* uri db))
        can-sort-clauses?* (future (f.sort-clauses/can-sort? zloc uri db))
        allow-drag-backward?* (future (f.drag/can-drag-backward? zloc uri db))
        allow-drag-forward?* (future (f.drag/can-drag-forward? zloc uri db))
        allow-drag-param-backward?* (future (f.drag-param/can-drag-backward? zloc uri db))
        allow-drag-param-forward?* (future (f.drag-param/can-drag-forward? zloc uri db))
        can-promote-fn?* (future (r.transform/can-promote-fn? zloc))
        can-demote-fn?* (future (r.transform/can-demote-fn? zloc))
        can-destructure-keys?* (future (f.destructure-keys/can-destructure-keys? zloc uri db))
        can-restructure-keys?* (future (f.restructure-keys/can-restructure-keys? zloc uri db))
        can-extract-to-def?* (future (r.transform/can-extract-to-def? zloc))
        inline-symbol?* (future (r.transform/inline-symbol? uri row col db))
        can-add-let? (or (z/skip-whitespace z/right zloc)
                         (when-not (edit/top? zloc) (z/skip-whitespace z/up zloc)))]
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
      (conj (resolve-macro-as-action uri line character @macro-sym*))

      @inline-symbol?*
      (conj (inline-symbol-action uri line character))

      @other-colls*
      (into (change-colls-actions uri line character @other-colls*))

      can-add-let?
      (conj (move-to-let-action uri line character))

      @inside-function?*
      (conj (cycle-privacy-action uri line character)
            (extract-function-action uri line character))

      @can-promote-fn?*
      (conj (promote-fn-action uri line character @can-promote-fn?*))

      @can-demote-fn?*
      (conj (demote-fn-action uri line character))

      @can-destructure-keys?*
      (conj (destructure-keys-action uri line character))

      @can-restructure-keys?*
      (conj (restructure-keys-action uri line character))

      @can-extract-to-def?*
      (conj (extract-to-def-action uri line character))

      @can-thread?*
      (conj (thread-first-all-action uri line character)
            (thread-last-all-action uri line character))

      @can-unwind-thread?*
      (conj (unwind-thread-action uri line character)
            (unwind-all-action uri line character))

      @can-get-in-more?*
      (conj (get-in-more-action uri line character)
            (get-in-all-action uri line character))

      @can-get-in-less?*
      (conj (get-in-less-action uri line character)
            (get-in-none-action uri line character))

      (and workspace-edit-capability?
           @can-sort-clauses?*)
      (conj (sort-clauses-action uri line character @can-sort-clauses?*))

      (and workspace-edit-capability?
           @allow-drag-backward?*)
      (conj (drag-backward-action uri line character))

      (and workspace-edit-capability?
           @allow-drag-forward?*)
      (conj (drag-forward-action uri line character))

      (and workspace-edit-capability?
           @allow-drag-param-backward?*)
      (conj (drag-param-backward-action uri line character))

      (and workspace-edit-capability?
           @allow-drag-param-forward?*)
      (conj (drag-param-forward-action uri line character))

      can-add-let?
      (conj (introduce-let-action uri line character))

      (and workspace-edit-capability?
           (seq diagnostics))
      (into (suppress-diagnostic-actions diagnostics uri))

      (and workspace-edit-capability?
           @can-create-test?*)
      (conj (create-test-action (:function-name-loc @can-create-test?*) uri line character))

      workspace-edit-capability?
      (conj (clean-ns-action uri line character)))))
