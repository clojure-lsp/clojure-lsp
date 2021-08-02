(ns clojure-lsp.client
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.handlers :as handlers]
   [taoensso.timbre :as log]))

(defn edit->summary
  ([uri edit]
   (edit->summary uri edit nil))
  ([uri {:keys [range new-text]} old-text]
   (let [old-text (or old-text
                      (get-in @db/db [:documents uri :text])
                      (slurp uri))
         new-full-text (f.file-management/replace-text
                         old-text
                         new-text
                         (-> range :start :line)
                         (-> range :start :character)
                         (-> range :end :line)
                         (-> range :end :character))]
     (when (not= new-full-text old-text)
       {:uri uri
        :version (get-in @db/db [:documents uri :version] 0)
        :old-text old-text
        :new-text new-full-text}))))

(defn document-change->edit-summary [{:keys [text-document edits]}]
  (let [uri (:uri text-document)]
    (loop [edit-summary nil
           i 0]
      (if-let [edit (nth edits i nil)]
        (when-let [edit-summary (edit->summary uri edit (:new-text edit-summary))]
          (recur edit-summary (inc i)))
        edit-summary))))

(defn apply-workspace-edit-summary!
  [{:keys [uri new-text version changed?] :as change}]
  (spit uri new-text)
  (when (and changed?
             (get-in @db/db [:documents uri :text]))
    (handlers/did-change {:textDocument {:uri uri
                                         :version (inc version)}
                          :contentChanges new-text}))
  change)
