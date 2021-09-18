(ns clojure-lsp.client
  (:require
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
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
       {:kind :change
        :uri uri
        :version (get-in @db [:documents uri :version] 0)
        :old-text old-text
        :new-text new-full-text}))))

(defn document-change->edit-summary [{:keys [text-document edits kind old-uri new-uri]} db]
  (if (= "rename" kind)
    {:kind :rename
     :new-uri new-uri
     :old-uri old-uri}
    (let [uri (:uri text-document)]
      (loop [edit-summary nil
             i 0]
        (if-let [edit (nth edits i nil)]
          (when-let [edit-summary (edit->summary db uri edit (:new-text edit-summary))]
            (recur edit-summary (inc i)))
          edit-summary)))))

(defn apply-workspace-change-edit-summary!
  [{:keys [uri new-text version changed?]} db]
  (spit uri new-text)
  (when (and changed?
             (get-in @db [:documents uri :text]))
    (f.file-management/did-change uri new-text (inc version) db)))

(defn ^:private apply-workspace-rename-edit-summary!
  [{:keys [old-uri new-uri]} db]
  (let [old-file (-> old-uri shared/uri->filename io/file)
        new-file (-> new-uri shared/uri->filename io/file)]
    (io/make-parents new-file)
    (io/copy old-file new-file)
    (io/delete-file old-file)
    (f.file-management/did-close old-uri db)
    (f.file-management/did-open new-uri (slurp new-file) db)))

(defn apply-workspace-edit-summary!
  [change db]
  (if (= :rename (:kind change))
    (apply-workspace-rename-edit-summary! change db)
    (apply-workspace-change-edit-summary! change db))
  change)
