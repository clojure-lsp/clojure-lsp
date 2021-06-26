(ns clojure-lsp.internal-api
  (:require
   [clojure-lsp.client :as client]
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.interop :as interop]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]))

(defn ^:private start-analysis!
  [project-root]
  (let [project-uri (shared/filename->uri project-root)]
    (crawler/initialize-project
      project-uri
      {:workspace {:workspace-edit {:document-changes true}}}
      (interop/clean-client-settings {:dependency-scheme "jar"})
      {:lint-project-files-after-startup? false
       :text-document-sync-kind :full})))

(defn ^:private ns->uri [namespace]
  (let [source-paths (-> @db/db :settings :source-paths)]
    (when-let [filename (:filename (q/find-namespace-definition-by-namespace (:analysis @db/db) namespace))]
      (shared/namespace->uri (name namespace) source-paths filename))))

(defn ^:private open-file! [uri]
  (handlers/did-open {:textDocument {:uri uri :text (slurp uri)}}))

(defn clean-ns! [{:keys [project-root namespace]}]
  (start-analysis! project-root)
  (let [namespaces (or (seq namespace)
                       (->> (:analysis @db/db)
                         q/filter-project-analysis
                         q/find-all-ns-definitions))]
    (doseq [namespace namespaces]
      (if-let [uri (ns->uri namespace)]
        (do
          (open-file! uri)
          (when-let [edits (handlers/execute-command {:command "clean-ns"
                                                      :arguments [uri 0 0]})]
            (when (seq (client/apply-workspace-edits edits))
              (println "Cleaned" namespace))))
        (println "Namespace" namespace "not found")))))
