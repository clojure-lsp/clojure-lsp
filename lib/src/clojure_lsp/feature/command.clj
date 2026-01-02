(ns clojure-lsp.feature.command
  (:require
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.feature.clean-ns :as f.clean-ns]
   [clojure-lsp.feature.cycle-keyword :as f.cycle-keyword]
   [clojure-lsp.feature.destructure-keys :as f.destructure-keys]
   [clojure-lsp.feature.development-info :as f.development-info]
   [clojure-lsp.feature.drag :as f.drag]
   [clojure-lsp.feature.drag-param :as f.drag-param]
   [clojure-lsp.feature.inline-symbol :as f.inline-symbol]
   [clojure-lsp.feature.move-form :as f.move-form]
   [clojure-lsp.feature.paredit :as f.paredit]
   [clojure-lsp.feature.replace-refer-all :as f.replace-refer-all]
   [clojure-lsp.feature.resolve-macro :as f.resolve-macro]
   [clojure-lsp.feature.restructure-keys :as f.restructure-keys]
   [clojure-lsp.feature.sort-clauses :as f.sort-clauses]
   [clojure-lsp.feature.thread-get :as f.thread-get]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.shared :as shared]
   [medley.core :as medley]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defmulti ^:private run-command :command)

(defmethod ^{:deprecated "Use add-missing-import instead"} run-command :add-import-to-namespace [{:keys [loc uri args db components]}]
  (apply f.add-missing-libspec/add-missing-import loc uri (concat args [db components])))

(defmethod run-command :add-missing-import [{:keys [loc uri db args components]}]
  (apply f.add-missing-libspec/add-missing-import loc uri (concat args [db components])))

(defmethod run-command :add-missing-libspec [{:keys [loc uri db components]}]
  (f.add-missing-libspec/add-missing-libspec loc uri db components))

(defmethod run-command :add-require-suggestion [{:keys [loc uri args db components]}]
  (apply f.add-missing-libspec/add-require-suggestion loc uri (concat args [db components])))

(defmethod run-command :clean-ns [{:keys [loc uri db]}]
  (f.clean-ns/clean-ns-edits loc uri db))

(defmethod run-command :cycle-coll [{:keys [loc]}]
  (r.transform/cycle-coll loc))

(defmethod run-command :change-coll [{:keys [loc args]}]
  (apply r.transform/change-coll loc args))

(defmethod run-command :cycle-privacy [{:keys [loc db]}]
  (r.transform/cycle-privacy loc db))

(defmethod run-command :destructure-keys [{:keys [loc uri db]}]
  (f.destructure-keys/destructure-keys loc uri db))

(defmethod run-command :restructure-keys [{:keys [loc uri db]}]
  (f.restructure-keys/restructure-keys loc uri db))

(defmethod run-command :promote-fn [{:keys [loc uri db args]}]
  (apply r.transform/promote-fn loc uri db args))

(defmethod run-command :demote-fn [{:keys [loc]}]
  (r.transform/demote-fn loc))

(defmethod run-command :expand-let [{:keys [loc uri db]}]
  (r.transform/expand-let loc uri db))

(defmethod run-command :drag-backward [{:keys [loc row col uri db]}]
  (f.drag/drag-backward loc {:row row :col col} uri db))

(defmethod run-command :drag-forward [{:keys [loc row col uri db]}]
  (f.drag/drag-forward loc {:row row :col col} uri db))

(defmethod run-command :drag-param-backward [{:keys [loc row col uri components]}]
  (f.drag-param/drag-backward loc {:row row :col col} uri components))

(defmethod run-command :drag-param-forward [{:keys [loc row col uri components]}]
  (f.drag-param/drag-forward loc {:row row :col col} uri components))

(defmethod run-command :extract-function [{:keys [row col row-end col-end loc loc-end uri args db]}]
  (apply r.transform/extract-function row col row-end col-end loc loc-end uri (concat args [db])))

(defmethod run-command :extract-to-def [{:keys [loc args]}]
  (apply r.transform/extract-to-def loc args))

(defmethod run-command :get-in-more [{:keys [loc]}]
  (f.thread-get/get-in-more loc))

(defmethod run-command :get-in-all [{:keys [loc]}]
  (f.thread-get/get-in-all loc))

(defmethod run-command :get-in-less [{:keys [loc]}]
  (f.thread-get/get-in-less loc))

(defmethod run-command :get-in-none [{:keys [loc]}]
  (f.thread-get/get-in-none loc))

(defmethod run-command :inline-symbol [{:keys [uri row col db]}]
  (f.inline-symbol/inline-symbol uri row col db))

(defmethod run-command :introduce-let [{:keys [loc args]}]
  (apply r.transform/introduce-let loc args))

(defmethod run-command :move-to-let [{:keys [loc uri db args]}]
  (apply r.transform/move-to-let loc uri db args))

(defmethod run-command :thread-first [{:keys [loc db]}]
  (r.transform/thread-first loc db))

(defmethod run-command :thread-first-all [{:keys [loc db]}]
  (r.transform/thread-first-all loc db))

(defmethod run-command :thread-last [{:keys [loc db]}]
  (r.transform/thread-last loc db))

(defmethod run-command :thread-last-all [{:keys [loc db]}]
  (r.transform/thread-last-all loc db))

(defmethod run-command :unwind-all [{:keys [loc]}]
  (r.transform/unwind-all loc))

(defmethod run-command :unwind-thread [{:keys [loc]}]
  (r.transform/unwind-thread loc))

(defmethod run-command :resolve-macro-as [{:keys [loc uri db components]}]
  (f.resolve-macro/resolve-macro-as! loc uri db components))

(defmethod run-command :sort-clauses [{:keys [loc uri db]}]
  (f.sort-clauses/sort-clauses loc uri db))

;; Deprecated. Use sort-clauses
(defmethod run-command :sort-map [args]
  (run-command (assoc args :command :sort-clauses)))

;; Deprecated. Use drag-backward
(defmethod run-command :move-coll-entry-up [args]
  (run-command (assoc args :command :drag-backward)))

;; Deprecated. Use drag-forward
(defmethod run-command :move-coll-entry-down [args]
  (run-command (assoc args :command :drag-forward)))

(defmethod run-command :suppress-diagnostic [{:keys [loc args]}]
  (apply r.transform/suppress-diagnostic loc args))

(defmethod run-command :create-function [{:keys [loc uri db]}]
  (r.transform/create-function loc uri db))

(defmethod run-command :if->cond-refactor [{:keys [loc]}]
  (r.transform/if->cond loc))

(defmethod run-command :cond->if-refactor [{:keys [loc]}]
  (r.transform/cond->if loc))

(defmethod run-command :create-test [{:keys [loc uri db components]}]
  (r.transform/create-test loc uri db components))

(defmethod run-command :move-form [{:keys [loc uri args components]}]
  (apply f.move-form/move-form loc uri components args))

(defmethod run-command :cycle-keyword-auto-resolve  [{:keys [loc uri db components]}]
  (f.cycle-keyword/cycle-keyword-auto-resolve loc uri db components))

(defmethod run-command :replace-refer-all-with-refer  [{:keys [loc args]}]
  (apply f.replace-refer-all/replace-with-refers loc args))

(defmethod run-command :replace-refer-all-with-alias  [{:keys [loc uri db]}]
  (f.replace-refer-all/replace-with-alias loc uri db))

(defmethod run-command :forward-slurp  [{:keys [uri loc row col]}]
  (f.paredit/forward-slurp uri loc row col))

(defmethod run-command :forward-barf  [{:keys [loc uri row col]}]
  (f.paredit/forward-barf uri loc row col))

(defmethod run-command :backward-slurp  [{:keys [loc uri row col]}]
  (f.paredit/backward-slurp uri loc row col))

(defmethod run-command :backward-barf  [{:keys [loc uri row col]}]
  (f.paredit/backward-barf uri loc row col))

(defmethod run-command :raise-sexp  [{:keys [loc uri row col]}]
  (f.paredit/raise uri loc row col))

(defmethod run-command :kill-sexp  [{:keys [loc uri row col]}]
  (f.paredit/kill uri loc row col))

(defmethod run-command :forward  [{:keys [loc uri row col]}]
  (f.paredit/forward uri loc row col))

(defmethod run-command :forward-select  [{:keys [loc uri row col]}]
  (f.paredit/forward-select uri loc row col))

(defmethod run-command :backward  [{:keys [loc uri row col]}]
  (f.paredit/backward uri loc row col))

(defmethod run-command :backward-select  [{:keys [loc uri row col]}]
  (f.paredit/backward-select uri loc row col))

(defmethod run-command :server-info  [{:keys [components]}]
  (f.development-info/server-info-log components))

(defmethod run-command :cursor-info  [{:keys [uri components args]}]
  (apply f.development-info/cursor-info-log uri components args))

(def available-commands
  (->> run-command
       methods
       keys
       (map name)
       sort
       vec))

(defn ^:private command-client-seq-changes [uri version result db]
  (let [changes [{:text-document {:uri uri :version version}
                  :edits (mapv #(medley/update-existing % :range shared/->range) (r.transform/result result))}]]
    (shared/client-changes changes db)))

(defn call-command [command arguments {:keys [db*] :as components}]
  ;; when no selection for command: args is [opaque-args...]
  ;; if there is selection: [opaque-args... line-end character-end],
  ;; where opaque-args could be one or more arguments to the command.  line-end 
  ;; and character-end are put at the end of the argument list so clients that
  ;; had already hardcoded array values (such as Calva changing the new function name
  ;; in args[3]) won't need to change
  (let [[uri line character & args] arguments
        selection? (= :extract-function command)
        line-end (if selection? (nth args 1) line)
        character-end (if selection? (nth args 2) character)
        fn-args (if selection? (drop-last 2 args) args)
        db @db*
        row (inc (int line))
        col (inc (int character))
        row-end (inc (int (or line-end line)))
        col-end (inc (int (or character-end character)))
            ;; TODO Instead of v=0 should I send a change AND a document change
        version (get-in db [:documents uri :v] 0)
        loc (some-> (parser/zloc-of-file db uri)
                    (parser/to-pos row col))
        loc-end (some-> (parser/zloc-of-file db uri)
                        (parser/to-pos row-end col-end))
        result (run-command {:command command
                             :uri uri
                             :db db
                             :loc loc
                             :loc-end loc-end
                             :row row
                             :col col
                             :row-end row-end
                             :col-end col-end
                             :args fn-args
                             :version version
                             :components components})]
    (cond
      (:no-op? result)
      nil

      (:error result)
      result

      (and (not loc)
           (not= :clean-ns command))
      (do (logger/warn (str "Could not find a form at this location. row " row " col " col " file " uri))
          {:error {:message "Could not find a form at this location."
                   :code :invalid-params}})

      (map? result)
      (let [{:keys [changes-by-uri resource-changes show-document-after-edit]} result
            changes (concat resource-changes
                            (vec (for [[doc-id sub-results] changes-by-uri]
                                   {:text-document {:uri doc-id :version (if (= uri doc-id) version -1)}
                                    :edits (mapv #(medley/update-existing % :range shared/->range)
                                                 (r.transform/result sub-results))})))]
        (when-let [change (first (filter #(= "create" (:kind %)) resource-changes))]
          (swap! db* assoc-in [:create-ns-blank-files-denylist (:uri change)] (:kind change)))
        {:show-document-after-edit show-document-after-edit
         :edit (some-> (seq changes) (shared/client-changes db))})

      (seq result)
      {:edit (command-client-seq-changes uri version result db)}

      (empty? result)
      (do (logger/warn command "made no changes" (z/string loc))
          {:error {:message "Nothing to change."
                   :code :invalid-request}})

      :else
      (do
        (logger/warn (str "Could not apply " command " to form: " (z/string loc)))
        {:error {:message "Could not apply command."
                 :code :invalid-request}}))))
