(ns clojure-lsp.api
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
  (let [source-paths (-> @db/db :settings :source-paths)
        filename (:filename (q/find-namespace-definition-by-namespace (:analysis @db/db) namespace))]
    (shared/namespace->uri namespace source-paths filename)))

(defn ^:private open-file! [namespace]
  (let [uri (ns->uri namespace)
        text (slurp uri)]
    (handlers/did-open {:textDocument {:uri uri
                                       :text text}})))

(defn clean-ns! [{:keys [project-root namespaces]}]
  (start-analysis! project-root)
  (let [namespaces (or (seq namespaces)
                       (->> (:analysis @db/db)
                            q/filter-project-analysis
                            q/find-all-ns-definitions
                            (map name)))]
    (doseq [namespace namespaces]
      (open-file! namespace)
      (println "Checking" namespace)
      (when-let [edits (handlers/execute-command {:command "clean-ns"
                                                  :arguments [(ns->uri namespace) 0 0]})]
        (client/apply-workspace-edits edits)))))
