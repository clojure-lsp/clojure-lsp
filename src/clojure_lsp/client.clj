(ns clojure-lsp.client
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.file-management :as f.file-management]
    [clojure-lsp.handlers :as handlers]))

(defn apply-workspace-edit [{:keys [text-document edits]}]
  (let [uri (:uri text-document)
        old-text (get-in @db/db [:documents uri :text])]
    (loop [text old-text
           i 0]
      (if-let [{:keys [range new-text]} (nth edits i nil)]
        (let [new-full-text (f.file-management/replace-text text
                                                            new-text
                                                            (-> range :start :line)
                                                            (-> range :start :character)
                                                            (-> range :end :line)
                                                            (-> range :end :character))]
          (recur new-full-text (inc i)))
        {:uri uri
         :changed? (not= text old-text)
         :version (get-in @db/db [:documents uri :version] 0)
         :new-text text}))))

(defn apply-workspace-edits [{:keys [document-changes]}]
  (->> document-changes
       (map apply-workspace-edit)
       (mapv (fn [{:keys [uri new-text version changed?]}]
              (when changed?
                (spit uri new-text)
                (handlers/did-change {:textDocument {:uri uri
                                                     :version (inc version)}
                                      :contentChanges new-text})
                uri)))
       (remove nil?)))
