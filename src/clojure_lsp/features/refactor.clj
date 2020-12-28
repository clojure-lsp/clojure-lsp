(ns clojure-lsp.features.refactor
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.features.references :as f.references]
   [clojure-lsp.features.definition :as f.definition]))

(defmulti refactor (comp :refactoring))

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
  (let [usages (get-in @db/db [:file-envs uri])
        usages-in-form (parser/usages-in-form loc usages)]
    (apply r.transform/extract-function loc (conj (vec args) usages-in-form))))

(defmethod refactor :inline-symbol [{:keys [uri row col]}]
  (let [usage (f.definition/definition-usage uri row col)
        references (f.references/reference-usages uri row col)]
    (r.transform/inline-symbol usage references)))

(defmethod refactor :introduce-let [{:keys [loc args]}]
  (apply r.transform/introduce-let loc (vec args)))

(defmethod refactor :move-to-let [{:keys [loc args]}]
  (apply r.transform/move-to-let loc (vec args)))

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
