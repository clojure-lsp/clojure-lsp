(ns clojure-lsp.internal-api
  (:require
   [clojure-lsp.client :as client]
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.interop :as interop]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :refer [>! alts!! chan go timeout]]
   [taoensso.timbre :as log]))

(defn ^:private cli-print [& msg]
  (when (:cli? @db/db)
    (apply print msg)
    (flush)))

(defn ^:private cli-println [& msg]
  (apply cli-print (update-in (vec msg) [(dec (count msg))] str "\n")))

(defmacro ^:private print-with-time [msg & body]
  `(let [~'_time (System/nanoTime)
         ~'_done-ch (chan)]
     (go
       ~@body
       (>! ~'_done-ch true))
     (loop []
       (cli-print (str "\r" ~@msg " " (quot (- (System/nanoTime) ~'_time) 1000000) "ms"))
       (if (first (alts!! [(timeout 100) ~'_done-ch]))
         (cli-println "")
         (recur)))))

(defn ^:private start-analysis! [project-root settings]
  (print-with-time
    "Analyzing project..."
    (let [project-uri (shared/filename->uri (.getCanonicalPath project-root))]
      (crawler/initialize-project
        project-uri
        {:workspace {:workspace-edit {:document-changes true}}}
        (interop/clean-client-settings {})
        (merge {:lint-project-files-after-startup? false
                :text-document-sync-kind :full}
               settings)))))

(defn ^:private ns->uri [namespace]
  (let [source-paths (-> @db/db :settings :source-paths)]
    (when-let [filename (:filename (q/find-namespace-definition-by-namespace (:analysis @db/db) namespace))]
      (shared/namespace->uri (name namespace) source-paths filename))))

(defn ^:private open-file! [uri]
  (handlers/did-open {:textDocument {:uri uri :text (slurp uri)}}))

(defn clean-ns! [{:keys [project-root namespace settings]}]
  (start-analysis! project-root settings)
  (cli-println "Checking namespaces...")
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
              (cli-println "Cleaned" namespace))))
        (cli-println "Namespace" namespace "not found")))))
