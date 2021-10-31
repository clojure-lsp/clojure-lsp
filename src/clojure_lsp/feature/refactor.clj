(ns clojure-lsp.feature.refactor
  (:require
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.shared :as shared]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(defn client-changes [changes db]
  (if (get-in @db [:client-capabilities :workspace :workspace-edit :document-changes])
    {:document-changes changes}
    {:changes (into {} (map (fn [{:keys [text-document edits]}]
                              [(:uri text-document) edits])
                            changes))}))

(defmulti refactor :refactoring)

(defmethod refactor :add-import-to-namespace [{:keys [loc args db]}]
  (apply r.transform/add-import-to-namespace loc [args db]))

(defmethod refactor :add-missing-libspec [{:keys [loc db]}]
  (r.transform/add-missing-libspec loc db))

(defmethod refactor :clean-ns [{:keys [loc uri db]}]
  (r.transform/clean-ns loc uri db))

(defmethod refactor :cycle-coll [{:keys [loc]}]
  (r.transform/cycle-coll loc))

(defmethod refactor :change-coll [{:keys [loc args]}]
  (apply r.transform/change-coll loc [args]))

(defmethod refactor :cycle-privacy [{:keys [loc db]}]
  (r.transform/cycle-privacy loc db))

(defmethod refactor :expand-let [{:keys [loc]}]
  (r.transform/expand-let loc))

(defmethod refactor :extract-function [{:keys [loc uri args db]}]
  (apply r.transform/extract-function loc uri [args db]))

(defmethod refactor :inline-symbol [{:keys [uri row col db]}]
  (r.transform/inline-symbol uri row col db))

(defmethod refactor :introduce-let [{:keys [loc args]}]
  (apply r.transform/introduce-let loc [args]))

(defmethod refactor :move-to-let [{:keys [loc args]}]
  (apply r.transform/move-to-let loc [args]))

(defmethod refactor :thread-first [{:keys [loc]}]
  (r.transform/thread-first loc))

(defmethod refactor :thread-first-all [{:keys [loc]}]
  (r.transform/thread-first-all loc))

(defmethod refactor :thread-last [{:keys [loc]}]
  (r.transform/thread-last loc))

(defmethod refactor :thread-last-all [{:keys [loc]}]
  (r.transform/thread-last-all loc))

(defmethod refactor :unwind-all [{:keys [loc]}]
  (r.transform/unwind-all loc))

(defmethod refactor :unwind-thread [{:keys [loc]}]
  (r.transform/unwind-thread loc))

(defmethod refactor :suppress-diagnostic [{:keys [loc args]}]
  (apply r.transform/suppress-diagnostic loc [args]))

(defmethod refactor :create-function [{:keys [loc db]}]
  (r.transform/create-function loc db))

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
                  :edits (mapv #(update % :range shared/->range) (r.transform/result result))}]]
    (client-changes changes db)))

(defn call-refactor [{:keys [loc uri refactoring row col version] :as data} db]
  (let [result (refactor (assoc data :db db))]
    (if (or loc
            (= :clean-ns refactoring))
      (cond
        (map? result)
        (let [{:keys [changes-by-uri show-document-after-edit]} result
              changes (vec (for [[doc-id sub-results] changes-by-uri]
                             {:text-document {:uri doc-id :version version}
                              :edits (mapv #(update % :range shared/->range)
                                           (r.transform/result sub-results))}))]
          {:show-document-after-edit show-document-after-edit
           :edit (client-changes changes db)})

        (seq result)
        {:edit (refactor-client-seq-changes uri version result db)}

        (empty? result)
        (log/warn refactoring "made no changes" (z/string loc))

        :else
        (log/warn "Could not apply" refactoring "to form: " (z/string loc)))
      (log/warn "Could not find a form at this location. row" row "col" col "file" uri))))
