(ns clojure-lsp.feature.refactor
  (:require
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.feature.clean-ns :as f.clean-ns]
   [clojure-lsp.feature.cycle-keyword :as f.cycle-keyword]
   [clojure-lsp.feature.destructure-keys :as f.destructure-keys]
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
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.shared :as shared]
   [medley.core :as medley]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defmulti refactor :refactoring)

(defmethod ^{:deprecated "Use add-missing-import instead"} refactor :add-import-to-namespace [{:keys [loc uri args db components]}]
  (apply f.add-missing-libspec/add-missing-import loc uri (concat args [db components])))

(defmethod refactor :add-missing-import [{:keys [loc uri db args components]}]
  (apply f.add-missing-libspec/add-missing-import loc uri (concat args [db components])))

(defmethod refactor :add-missing-libspec [{:keys [loc uri db components]}]
  (f.add-missing-libspec/add-missing-libspec loc uri db components))

(defmethod refactor :add-require-suggestion [{:keys [loc uri args db components]}]
  (apply f.add-missing-libspec/add-require-suggestion loc uri (concat args [db components])))

(defmethod refactor :clean-ns [{:keys [loc uri db]}]
  (f.clean-ns/clean-ns-edits loc uri db))

(defmethod refactor :cycle-coll [{:keys [loc]}]
  (r.transform/cycle-coll loc))

(defmethod refactor :change-coll [{:keys [loc args]}]
  (apply r.transform/change-coll loc args))

(defmethod refactor :cycle-privacy [{:keys [loc db]}]
  (r.transform/cycle-privacy loc db))

(defmethod refactor :destructure-keys [{:keys [loc uri db]}]
  (f.destructure-keys/destructure-keys loc uri db))

(defmethod refactor :restructure-keys [{:keys [loc uri db]}]
  (f.restructure-keys/restructure-keys loc uri db))

(defmethod refactor :promote-fn [{:keys [loc uri db args]}]
  (apply r.transform/promote-fn loc uri db args))

(defmethod refactor :demote-fn [{:keys [loc]}]
  (r.transform/demote-fn loc))

(defmethod refactor :expand-let [{:keys [loc uri db]}]
  (r.transform/expand-let loc uri db))

(defmethod refactor :drag-backward [{:keys [loc row col uri db]}]
  (f.drag/drag-backward loc {:row row :col col} uri db))

(defmethod refactor :drag-forward [{:keys [loc row col uri db]}]
  (f.drag/drag-forward loc {:row row :col col} uri db))

(defmethod refactor :drag-param-backward [{:keys [loc row col uri components]}]
  (f.drag-param/drag-backward loc {:row row :col col} uri components))

(defmethod refactor :drag-param-forward [{:keys [loc row col uri components]}]
  (f.drag-param/drag-forward loc {:row row :col col} uri components))

(defmethod refactor :extract-function [{:keys [loc uri args db]}]
  (apply r.transform/extract-function loc uri (concat args [db])))

(defmethod refactor :extract-to-def [{:keys [loc args]}]
  (apply r.transform/extract-to-def loc args))

(defmethod refactor :get-in-more [{:keys [loc]}]
  (f.thread-get/get-in-more loc))

(defmethod refactor :get-in-all [{:keys [loc]}]
  (f.thread-get/get-in-all loc))

(defmethod refactor :get-in-less [{:keys [loc]}]
  (f.thread-get/get-in-less loc))

(defmethod refactor :get-in-none [{:keys [loc]}]
  (f.thread-get/get-in-none loc))

(defmethod refactor :inline-symbol [{:keys [uri row col db]}]
  (f.inline-symbol/inline-symbol uri row col db))

(defmethod refactor :introduce-let [{:keys [loc args]}]
  (apply r.transform/introduce-let loc args))

(defmethod refactor :move-to-let [{:keys [loc uri db args]}]
  (apply r.transform/move-to-let loc uri db args))

(defmethod refactor :thread-first [{:keys [loc db]}]
  (r.transform/thread-first loc db))

(defmethod refactor :thread-first-all [{:keys [loc db]}]
  (r.transform/thread-first-all loc db))

(defmethod refactor :thread-last [{:keys [loc db]}]
  (r.transform/thread-last loc db))

(defmethod refactor :thread-last-all [{:keys [loc db]}]
  (r.transform/thread-last-all loc db))

(defmethod refactor :unwind-all [{:keys [loc]}]
  (r.transform/unwind-all loc))

(defmethod refactor :unwind-thread [{:keys [loc]}]
  (r.transform/unwind-thread loc))

(defmethod refactor :resolve-macro-as [{:keys [loc uri db components]}]
  (f.resolve-macro/resolve-macro-as! loc uri db components))

(defmethod refactor :sort-clauses [{:keys [loc uri db]}]
  (f.sort-clauses/sort-clauses loc uri db))

;; Deprecated. Use sort-clauses
(defmethod refactor :sort-map [args]
  (refactor (assoc args :refactoring :sort-clauses)))

;; Deprecated. Use drag-backward
(defmethod refactor :move-coll-entry-up [args]
  (refactor (assoc args :refactoring :drag-backward)))

;; Deprecated. Use drag-forward
(defmethod refactor :move-coll-entry-down [args]
  (refactor (assoc args :refactoring :drag-forward)))

(defmethod refactor :suppress-diagnostic [{:keys [loc args]}]
  (apply r.transform/suppress-diagnostic loc args))

(defmethod refactor :create-function [{:keys [loc uri db]}]
  (r.transform/create-function loc uri db))

(defmethod refactor :create-test [{:keys [loc uri db components]}]
  (r.transform/create-test loc uri db components))

(defmethod refactor :move-form [{:keys [loc uri args components]}]
  (apply f.move-form/move-form loc uri components args))

(defmethod refactor :cycle-keyword-auto-resolve  [{:keys [loc uri db components]}]
  (f.cycle-keyword/cycle-keyword-auto-resolve loc uri db components))

(defmethod refactor :replace-refer-all-with-refer  [{:keys [loc args]}]
  (apply f.replace-refer-all/replace-with-refers loc args))

(defmethod refactor :replace-refer-all-with-alias  [{:keys [loc uri db]}]
  (f.replace-refer-all/replace-with-alias loc uri db))

(defmethod refactor :forward-slurp  [{:keys [loc]}]
  (f.paredit/forward-slurp loc))

(defmethod refactor :forward-barf  [{:keys [loc]}]
  (f.paredit/forward-barf loc))

(defmethod refactor :backward-slurp  [{:keys [loc]}]
  (f.paredit/backward-slurp loc))

(defmethod refactor :backward-barf  [{:keys [loc]}]
  (f.paredit/backward-barf loc))

(defmethod refactor :raise-sexp  [{:keys [loc]}]
  (f.paredit/raise loc))

(defmethod refactor :kill-sexp  [{:keys [loc]}]
  (f.paredit/kill loc))

(def available-refactors
  (->> refactor
       methods
       keys
       (map name)
       sort
       vec))

(defn refactor-client-seq-changes [uri version result db]
  (let [changes [{:text-document {:uri uri :version version}
                  :edits (mapv #(medley/update-existing % :range shared/->range) (r.transform/result result))}]]
    (shared/client-changes changes db)))

(defn call-refactor [{:keys [db loc uri refactoring row col version] :as data} {:keys [db*] :as components}]
  (let [result (refactor (assoc data :components components))]
    (cond
      (:no-op? result)
      nil

      (:error result)
      result

      (and (not loc)
           (not= :clean-ns refactoring))
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
         :edit (shared/client-changes changes db)})

      (seq result)
      {:edit (refactor-client-seq-changes uri version result db)}

      (empty? result)
      (do (logger/warn refactoring "made no changes" (z/string loc))
          {:error {:message "Nothing to change."
                   :code :invalid-request}})

      :else
      (do
        (logger/warn (str "Could not apply " refactoring " to form: " (z/string loc)))
        {:error {:message "Could not apply refactor."
                 :code :invalid-request}}))))
