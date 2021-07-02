(ns clojure-lsp.client
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.diff :as diff]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.handlers :as handlers]))

(defn ^:private process-edit [{:keys [text-document edits]}]
  (let [uri (:uri text-document)
        old-text (get-in @db/db [:documents uri :text] (slurp uri))]
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
         :old-text old-text
         :new-text text}))))

(defn ^:private apply-workspace-edit
  [dry-run?
   {:keys [uri old-text new-text version]}]
  (if dry-run?
    (diff/unified-diff uri old-text new-text)
    (do
      (spit uri new-text)
      (when (get-in @db/db [:documents uri :text])
        (handlers/did-change {:textDocument {:uri uri
                                             :version (inc version)}
                              :contentChanges new-text}))
      uri)))

(defn apply-workspace-edits
  [{:keys [document-changes]} dry-run?]
  (->> document-changes
       (map process-edit)
       (filter :changed?)
       (mapv (partial apply-workspace-edit dry-run?))
       (remove nil?)))
