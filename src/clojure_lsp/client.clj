(ns clojure-lsp.client
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.file-management :as f.file-management]
    [clojure-lsp.handlers :as handlers]))

(defn apply-workspace-edit [{:keys [text-document edits]}]
  (loop [uri (:uri text-document)
         text (get-in @db/db [:documents uri :text])
         i 0]
    (if-let [{:keys [range new-text]} (nth edits i nil)]
      (let [new-full-text (f.file-management/replace-text text
                                                          new-text
                                                          (-> range :start :line)
                                                          (-> range :start :character)
                                                          (-> range :end :line)
                                                          (-> range :end :character))]
        (recur uri new-full-text (inc i)))
      {:uri uri
       :version (get-in @db/db [:documents uri :version] 0)
       :new-text text})))

(defn apply-workspace-edits [{:keys [document-changes]}]
  (doseq [{:keys [uri new-text version]} (map apply-workspace-edit document-changes)]
    (spit uri new-text)
    (handlers/did-change {:textDocument {:uri uri
                                         :version (inc version)}
                          :contentChanges new-text})))
