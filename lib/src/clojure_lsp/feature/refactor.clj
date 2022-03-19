(ns clojure-lsp.feature.refactor
  (:require
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.feature.clean-ns :as f.clean-ns]
   [clojure-lsp.feature.move-coll-entry :as f.move-coll-entry]
   [clojure-lsp.feature.resolve-macro :as f.resolve-macro]
   [clojure-lsp.feature.sort-map :as f.sort-map]
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.shared :as shared]
   [medley.core :as medley]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(defmulti refactor :refactoring)

(defmethod refactor :add-import-to-namespace [{:keys [loc args db]}]
  (apply f.add-missing-libspec/add-import-to-namespace loc (concat args [db])))

(defmethod refactor :add-missing-libspec [{:keys [loc uri db]}]
  (f.add-missing-libspec/add-missing-libspec loc uri db))

(defmethod refactor :add-require-suggestion [{:keys [loc args db]}]
  (apply f.add-missing-libspec/add-require-suggestion loc (concat args [db])))

(defmethod refactor :add-missing-import [{:keys [loc db]}]
  (f.add-missing-libspec/add-common-import-to-namespace loc db))

(defmethod refactor :clean-ns [{:keys [loc uri db]}]
  (f.clean-ns/clean-ns-edits loc uri db))

(defmethod refactor :cycle-coll [{:keys [loc]}]
  (r.transform/cycle-coll loc))

(defmethod refactor :change-coll [{:keys [loc args]}]
  (apply r.transform/change-coll loc args))

(defmethod refactor :cycle-privacy [{:keys [loc db]}]
  (r.transform/cycle-privacy loc db))

(defmethod refactor :cycle-fn-literal [{:keys [loc]}]
  (r.transform/cycle-fn-literal loc))

(defmethod refactor :expand-let [{:keys [loc]}]
  (r.transform/expand-let loc))

(defmethod refactor :extract-function [{:keys [loc uri args db]}]
  (apply r.transform/extract-function loc uri (concat args [db])))

(defmethod refactor :inline-symbol [{:keys [uri row col db]}]
  (r.transform/inline-symbol uri row col db))

(defmethod refactor :introduce-let [{:keys [loc args]}]
  (apply r.transform/introduce-let loc args))

(defmethod refactor :move-to-let [{:keys [loc args]}]
  (apply r.transform/move-to-let loc args))

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

(defmethod refactor :resolve-macro-as [{:keys [loc uri db]}]
  (f.resolve-macro/resolve-macro-as! loc uri db))

(defmethod refactor :sort-map [{:keys [loc]}]
  (f.sort-map/sort-map loc))

(defmethod refactor :move-coll-entry-up [{:keys [loc uri db]}]
  (f.move-coll-entry/move-up loc uri db))

(defmethod refactor :move-coll-entry-down [{:keys [loc uri db]}]
  (f.move-coll-entry/move-down loc uri db))

(defmethod refactor :suppress-diagnostic [{:keys [loc args]}]
  (apply r.transform/suppress-diagnostic loc args))

(defmethod refactor :create-function [{:keys [loc uri db]}]
  (r.transform/create-function loc uri db))

(defmethod refactor :create-test [{:keys [loc uri db]}]
  (r.transform/create-test loc uri db))

(def available-refactors
  (->> refactor
       methods
       keys
       (map name)
       vec))

(defn refactor-client-seq-changes [uri version result db]
  (let [changes [{:text-document {:uri uri :version version}
                  :edits (mapv #(medley/update-existing % :range shared/->range) (r.transform/result result))}]]
    (shared/client-changes changes db)))

(defn call-refactor [{:keys [loc uri refactoring row col version] :as data} db]
  (let [result (refactor (assoc data :db db))]
    (cond
      (:no-op? result)
      nil

      (and (not loc)
           (not= :clean-ns refactoring))
      (log/warn "Could not find a form at this location. row" row "col" col "file" uri)

      (map? result)
      (let [{:keys [changes-by-uri resource-changes show-document-after-edit]} result
            changes (concat resource-changes
                            (vec (for [[doc-id sub-results] changes-by-uri]
                                   {:text-document {:uri doc-id :version (if (= uri doc-id) version -1)}
                                    :edits (mapv #(medley/update-existing % :range shared/->range)
                                                 (r.transform/result sub-results))})))]
        (when-let [change (first (filter #(= "create" (:kind %)) resource-changes))]
          (swap! db assoc-in [:create-ns-blank-files-denylist (:uri change)] (:kind change)))
        {:show-document-after-edit show-document-after-edit
         :edit (shared/client-changes changes db)})

      (seq result)
      {:edit (refactor-client-seq-changes uri version result db)}

      (empty? result)
      (log/warn refactoring "made no changes" (z/string loc))

      :else
      (log/warn "Could not apply" refactoring "to form: " (z/string loc)))))
