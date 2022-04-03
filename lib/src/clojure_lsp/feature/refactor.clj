(ns clojure-lsp.feature.refactor
  (:require
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.feature.clean-ns :as f.clean-ns]
   [clojure-lsp.feature.move-coll-entry :as f.move-coll-entry]
   [clojure-lsp.feature.move-form :as f.move-form]
   [clojure-lsp.feature.resolve-macro :as f.resolve-macro]
   [clojure-lsp.feature.sort-map :as f.sort-map]
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.shared :as shared]
   [lsp4clj.protocols.logger :as logger]
   [medley.core :as medley]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defmulti refactor :refactoring)

(defmethod refactor :add-import-to-namespace [{:keys [loc uri args] {:keys [db]} :components}]
  (apply f.add-missing-libspec/add-missing-import loc uri (concat args [db])))

(defmethod refactor :add-missing-libspec [{:keys [loc uri] {:keys [db]} :components}]
  (f.add-missing-libspec/add-missing-libspec loc uri db))

(defmethod refactor :add-require-suggestion [{:keys [loc uri args] {:keys [db]} :components}]
  (apply f.add-missing-libspec/add-require-suggestion loc uri (concat args [db])))

(defmethod refactor :add-missing-import [{:keys [loc uri] {:keys [db]} :components}]
  (f.add-missing-libspec/add-missing-import loc uri nil db))

(defmethod refactor :clean-ns [{:keys [loc uri] {:keys [db]} :components}]
  (f.clean-ns/clean-ns-edits loc uri @db))

(defmethod refactor :cycle-coll [{:keys [loc]}]
  (r.transform/cycle-coll loc))

(defmethod refactor :change-coll [{:keys [loc args]}]
  (apply r.transform/change-coll loc args))

(defmethod refactor :cycle-privacy [{:keys [loc] {:keys [db]} :components}]
  (r.transform/cycle-privacy loc db))

(defmethod refactor :cycle-fn-literal [{:keys [loc]}]
  (r.transform/cycle-fn-literal loc))

(defmethod refactor :expand-let [{:keys [loc uri] {:keys [db]} :components}]
  (r.transform/expand-let loc uri @db))

(defmethod refactor :extract-function [{:keys [loc uri args] {:keys [db]} :components}]
  (apply r.transform/extract-function loc uri (concat args [db])))

(defmethod refactor :inline-symbol [{:keys [uri row col] {:keys [db]} :components}]
  (r.transform/inline-symbol uri row col db))

(defmethod refactor :introduce-let [{:keys [loc args]}]
  (apply r.transform/introduce-let loc args))

(defmethod refactor :move-to-let [{:keys [loc args uri] {:keys [db]} :components}]
  (apply r.transform/move-to-let loc uri @db args))

(defmethod refactor :thread-first [{:keys [loc] {:keys [db]} :components}]
  (r.transform/thread-first loc @db))

(defmethod refactor :thread-first-all [{:keys [loc] {:keys [db]} :components}]
  (r.transform/thread-first-all loc @db))

(defmethod refactor :thread-last [{:keys [loc] {:keys [db]} :components}]
  (r.transform/thread-last loc @db))

(defmethod refactor :thread-last-all [{:keys [loc] {:keys [db]} :components}]
  (r.transform/thread-last-all loc @db))

(defmethod refactor :unwind-all [{:keys [loc]}]
  (r.transform/unwind-all loc))

(defmethod refactor :unwind-thread [{:keys [loc]}]
  (r.transform/unwind-thread loc))

(defmethod refactor :resolve-macro-as [{:keys [loc uri components]}]
  (f.resolve-macro/resolve-macro-as! loc uri components))

(defmethod refactor :sort-map [{:keys [loc]}]
  (f.sort-map/sort-map loc))

(defmethod refactor :move-coll-entry-up [{:keys [loc uri] {:keys [db]} :components}]
  (f.move-coll-entry/move-up loc uri db))

(defmethod refactor :move-coll-entry-down [{:keys [loc uri] {:keys [db]} :components}]
  (f.move-coll-entry/move-down loc uri db))

(defmethod refactor :suppress-diagnostic [{:keys [loc args]}]
  (apply r.transform/suppress-diagnostic loc args))

(defmethod refactor :create-function [{:keys [loc uri] {:keys [db]} :components}]
  (r.transform/create-function loc uri db))

(defmethod refactor :create-test [{:keys [loc uri] components :components}]
  (r.transform/create-test loc uri components))

(defmethod refactor :move-form [{:keys [loc uri args] {:keys [db]} :components}]
  (apply f.move-form/move-form loc uri db args))

(def available-refactors
  (->> refactor
       methods
       keys
       (map name)
       vec))

;; TODO: deref
(defn refactor-client-seq-changes [uri version result db]
  (let [changes [{:text-document {:uri uri :version version}
                  :edits (mapv #(medley/update-existing % :range shared/->range) (r.transform/result result))}]]
    (shared/client-changes changes @db)))

(defn call-refactor [{:keys [loc uri refactoring row col version] :as data} {:keys [db] :as components}]

  (let [result (refactor (assoc data :components components))]
    (cond
      (:no-op? result)
      nil

      (and (not loc)
           (not= :clean-ns refactoring))
      (logger/warn (str "Could not find a form at this location. row " row " col " col " file " uri))

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
         :edit (shared/client-changes changes @db)})

      (seq result)
      {:edit (refactor-client-seq-changes uri version result db)}

      (empty? result)
      (logger/warn refactoring "made no changes" (z/string loc))

      :else
      (logger/warn (str "Could not apply " refactoring " to form: " (z/string loc))))))
