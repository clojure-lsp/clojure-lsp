(ns clojure-lsp.feature.refactor
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.shared :as shared]
   [taoensso.timbre :as log]
   [rewrite-clj.zip :as z]))

(defn client-changes [changes]
  (if (get-in @db/db [:client-capabilities :workspace :workspace-edit :document-changes])
    {:document-changes changes}
    {:changes (into {} (map (fn [{:keys [text-document edits]}]
                              [(:uri text-document) edits])
                            changes))}))

(defmulti refactor (comp :refactoring))

(defmethod refactor :add-import-to-namespace [{:keys [loc args]}]
  (apply r.transform/add-import-to-namespace loc [args]))

(defmethod refactor :add-missing-libspec [{:keys [loc]}]
  (r.transform/add-missing-libspec loc))

(defmethod refactor :clean-ns [{:keys [loc uri]}]
  (r.transform/clean-ns loc uri))

(defmethod refactor :cycle-coll [{:keys [loc]}]
  (r.transform/cycle-coll loc))

(defmethod refactor :cycle-privacy [{:keys [loc]}]
  (r.transform/cycle-privacy loc))

(defmethod refactor :expand-let [{:keys [loc]}]
  (r.transform/expand-let loc))

(defmethod refactor :extract-function [{:keys [loc uri args]}]
  (apply r.transform/extract-function loc uri [args]))

(defmethod refactor :inline-symbol [{:keys [uri row col]}]
  (r.transform/inline-symbol uri row col))

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

(def available-refactors
  (->> refactor
       methods
       keys
       (map name)
       vec))

(defn refactor-client-seq-changes [uri version result]
  (let [changes [{:text-document {:uri uri :version version}
                  :edits (mapv #(update % :range shared/->range) (r.transform/result result))}]]
    (client-changes changes)))

(defn call-refactor [{:keys [loc uri refactoring row col version] :as data}]
  (let [result (refactor data)]
    (if (or loc
            (= :clean-ns refactoring))
      (cond

        (map? result)
        (let [changes (vec (for [[doc-id sub-results] result]
                             {:text-document {:uri doc-id :version version}
                              :edits (mapv #(update % :range shared/->range) (r.transform/result sub-results))}))]
          (client-changes changes))

        (seq result)
        (refactor-client-seq-changes uri version result)

        (empty? result)
        (log/warn refactoring "made no changes" (z/string loc))

        :else
        (log/warn "Could not apply" refactoring "to form: " (z/string loc)))
      (log/warn "Could not find a form at this location. row" row "col" col "file" uri))))
