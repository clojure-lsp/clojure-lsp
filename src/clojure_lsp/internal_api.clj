(ns clojure-lsp.internal-api
  (:require
   [clojure-lsp.client :as client]
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
   [clojure-lsp.diff :as diff]
   [clojure-lsp.feature.rename :as f.rename]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.interop :as interop]
   [clojure-lsp.logging :as logging]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :refer [>! alts!! chan go timeout]]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [taoensso.timbre :as log]))

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
         ~'_result-ch (chan)]
     (go
       (try
         ~@body
         (>! ~'_result-ch [:success])
         (catch Exception e#
           (>! ~'_result-ch [:error e#]))))
     (loop []
       (cli-print (str "\r" ~@msg " " (quot (- (System/nanoTime) ~'_time) 1000000) "ms"))
       (let [[~'_result ~'_ex] (first (alts!! [(timeout 100) ~'_result-ch]))]
         (case ~'_result
           :success (cli-println "")
           :error (do (cli-println "")
                      (throw (ex-info "Error during project analysis" {:message ~'_ex})))
           (recur))))))

(defn ^:private start-analysis! [{:keys [project-root settings log-path verbose]}]
  (when verbose
    (logging/set-log-to-stdout))
  (print-with-time
    "Analyzing project..."
    (let [project-root-file (or ^java.io.File project-root (io/file ""))
          project-uri (shared/filename->uri (.getCanonicalPath project-root-file))]
      (crawler/initialize-project
        project-uri
        {:workspace {:workspace-edit {:document-changes true}}}
        (interop/clean-client-settings {})
        (merge (shared/assoc-some
                {:lint-project-files-after-startup? false
                 :text-document-sync-kind :full}
                 :log-path log-path)
               settings)))))

(defn ^:private ns->ns+uri [namespace]
  (if-let [filename (:filename (q/find-namespace-definition-by-namespace (:analysis @db/db) namespace))]
    {:namespace namespace
     :uri (shared/filename->uri filename)}
    {:namespace namespace}))

(defn ^:private uri->ns
  [uri ns+uris]
  (->> ns+uris
       (filter #(= uri (:uri %)))
       first
       :namespace))

(defn ^:private assert-ns-exists-or-drop! [ns+uris]
  (->> ns+uris
       (filter (complement :uri))
       (mapv #(cli-println "Namespace" (:namespace %) "not found")))
  (filter :uri ns+uris))

(defn ^:private open-file! [{:keys [uri] :as ns+uri}]
  (handlers/did-open {:textDocument {:uri uri :text (slurp uri)}})
  ns+uri)

(defn ^:private edits->diff-string [edits]
  (->> edits
       (map (fn [{:keys [uri old-text new-text]}]
              (diff/unified-diff uri old-text new-text)))
       (map diff/colorize-diff)
       (string/join "\n")))

(defn clean-ns! [{:keys [namespace dry?] :as options}]
  (start-analysis! options)
  (cli-println "Checking namespaces...")
  (let [namespaces (or (seq namespace)
                       (->> (:analysis @db/db)
                            q/filter-project-analysis
                            q/find-all-ns-definitions))
        ns+uris (map ns->ns+uri namespaces)
        edits (->> ns+uris
                   assert-ns-exists-or-drop!
                   (map open-file!)
                   (mapcat (comp :document-changes
                                 #(handlers/execute-command {:command "clean-ns"
                                                             :arguments [(:uri %) 0 0]})))
                   (map client/document-change->edit-summary)
                   (remove nil?))]
    (if (seq edits)
      (if dry?
        (throw
          (ex-info "Code not clean"
                   {:message (edits->diff-string edits)}))
        (mapv (comp #(cli-println "Cleaned" (uri->ns (:uri %) ns+uris))
                    client/apply-workspace-edit-summary!) edits))
      (cli-println "Nothing to clear!"))))

(defn format! [{:keys [namespace dry?] :as options}]
  (start-analysis! options)
  (cli-println "Formatting namespaces...")
  (let [namespaces (or (seq namespace)
                       (->> (:analysis @db/db)
                            q/filter-project-analysis
                            q/find-all-ns-definitions))
        ns+uris (map ns->ns+uri namespaces)
        edits (->> ns+uris
                   assert-ns-exists-or-drop!
                   (map open-file!)
                   (mapcat (fn [{:keys [uri]}]
                             (some->> (handlers/formatting {:textDocument uri})
                                      (map #(client/edit->summary uri %)))))
                   (remove nil?))]
    (if (seq edits)
      (if dry?
        (throw
          (ex-info "Code not formatted"
                   {:message (edits->diff-string edits)}))
        (mapv (comp #(cli-println "Formatted" (uri->ns (:uri %) ns+uris))
                    client/apply-workspace-edit-summary!) edits))
      (cli-println "Nothing to format!"))))

(defn rename! [{:keys [from to dry?] :as options}]
  (start-analysis! options)
  (let [from-name (symbol (name from))
        from-ns (symbol (namespace from))
        project-analysis (q/filter-project-analysis (:analysis @db/db))]
    (if-let [from-element (q/find-element-by-full-name project-analysis from-name from-ns)]
      (let [uri (shared/filename->uri (:filename from-element))]
        (open-file! {:uri uri
                     :namespace from-ns})
        (if-let [{:keys [document-changes]} (f.rename/rename uri (str to) (:name-row from-element) (:name-col from-element))]
          (if-let [edits (->> document-changes
                              (map client/document-change->edit-summary)
                              (remove nil?)
                              seq)]
            (if dry?
              (cli-println (edits->diff-string edits))
              (do
                (mapv client/apply-workspace-edit-summary! edits)
                (cli-println "Renamed" from "to" to)
                to))
            (cli-println "Nothing to rename"))
          (cli-println "Could not rename" from "to" to)))
      (cli-println "Symbol" from "not found in project"))))
