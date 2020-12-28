(ns clojure-lsp.features.definition
  (:require [clojure-lsp.db :as db]
            [clojure-lsp.features.references :as f.references]
            [clojure-lsp.shared :as shared]))

(defn definition-usage [doc-id line column]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        file-type (shared/uri->file-type doc-id)
        cursor (f.references/find-under-cursor line column local-env file-type)
        cursor-sym (:sym cursor)]
    [cursor
     (first
       (for [[env-doc-id usages] file-envs
             {:keys [sym tags] :as usage} usages
             :when (= sym cursor-sym)
             :when (and (or (= doc-id env-doc-id) (:public tags))
                        (:declare tags))]
         {:uri env-doc-id :usage usage}))]))
