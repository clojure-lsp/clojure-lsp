(ns clojure-lsp.client
  (:require
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.handlers :as handlers]
   [taoensso.timbre :as log]))

(defn edit->summary
  ([db uri edit]
   (edit->summary db uri edit nil))
  ([db uri {:keys [range new-text]} old-text]
   (let [old-text (or old-text
                      (get-in @db [:documents uri :text])
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
        :version (get-in @db [:documents uri :version] 0)
        :old-text old-text
        :new-text new-full-text}))))

(defn document-change->edit-summary [{:keys [text-document edits]} db]
  (let [uri (:uri text-document)]
    (loop [edit-summary nil
           i 0]
      (if-let [edit (nth edits i nil)]
        (when-let [edit-summary (edit->summary db uri edit (:new-text edit-summary))]
          (recur edit-summary (inc i)))
        edit-summary))))

(defn apply-workspace-edit-summary!
  [{:keys [uri new-text version changed?] :as change} db]
  (spit uri new-text)
  (when (and changed?
             (get-in @db [:documents uri :text]))
    (handlers/did-change {:textDocument {:uri uri
                                         :version (inc version)}
                          :contentChanges new-text}))
  change)
