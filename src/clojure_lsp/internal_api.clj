(ns clojure-lsp.internal-api
  (:require
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.rename :as f.rename]
   [clojure-lsp.client :as client]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.interop :as interop]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :refer [>! alts!! chan go timeout]]
   [taoensso.timbre :as log]
   [clojure-lsp.diff :as diff]))

(defn ^:private cli-print [& msg]
  (if (:cli? @db/db)
    (do
      (apply print msg)
      (flush))
    (log/debug msg)))

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

(defn ^:private start-analysis! [{:keys [project-root settings log-path]}]
  (print-with-time
    "Analyzing project..."
    (let [project-uri (shared/filename->uri (.getCanonicalPath project-root))]
      (crawler/initialize-project
        project-uri
        {:workspace {:workspace-edit {:document-changes true}}}
        (interop/clean-client-settings {})
        (merge (shared/assoc-some
                 {:lint-project-files-after-startup? false
                  :text-document-sync-kind :full}
                 :log-path log-path)
               settings)))))

(defn ^:private ns->uri [namespace]
  (let [source-paths (-> @db/db :settings :source-paths)]
    (when-let [filename (:filename (q/find-namespace-definition-by-namespace (:analysis @db/db) namespace))]
      (shared/namespace->uri (name namespace) source-paths filename))))

(defn ^:private open-file! [uri]
  (handlers/did-open {:textDocument {:uri uri :text (slurp uri)}}))

(defn ^:private process-dry-run
  [diffs]
  (when (seq diffs)
    (mapv (comp cli-print diff/colorize-diff) diffs)
    (throw (ex-info "Code not clean" {:message diffs}))))

(defn clean-ns! [{:keys [namespace dry-run?] :as options}]
  (start-analysis! options)
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
            (when-let [uris-or-diffs (seq (client/apply-workspace-edits edits (:dry-run? options)))]
              (if dry-run?
                (process-dry-run uris-or-diffs)
                (cli-println "Cleaned" namespace)))))
        (cli-println "Namespace" namespace "not found")))))

(defn rename! [{:keys [from to] :as options}]
  (start-analysis! options)
  (let [from-name (symbol (name from))
        from-ns (symbol (namespace from))
        project-analysis (q/filter-project-analysis (:analysis @db/db))]
    (if-let [from-element (q/find-element-by-full-name project-analysis from-name from-ns)]
      (let [uri (shared/filename->uri (:filename from-element))]
        (open-file! uri)
        (if-let [edits (f.rename/rename uri (str to) (:name-row from-element) (:name-col from-element))]
          (when (seq (client/apply-workspace-edits edits (:dry-run? options)))
            (cli-println "Renamed" from "to" to)
            to)
          (cli-println "Could not rename" from "to" to)))
      (cli-println "Symbol" from "not found in project"))))
