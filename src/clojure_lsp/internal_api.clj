(ns clojure-lsp.internal-api
  (:require
   [clojure-lsp.client :as client]
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
   [clojure-lsp.diff :as diff]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.feature.rename :as f.rename]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.interop :as interop]
   [clojure-lsp.logging :as logging]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(defn ^:private cli-print [& msg]
  (apply print msg)
  (flush))

(defn ^:private cli-println [options & msg]
  (when-not (:raw? options)
    (apply cli-print (update-in (vec msg) [(dec (count msg))] str "\n"))))

(defn ^:private show-message-cli [options {:keys [message extra type]}]
  (cli-println options (format "\n[%s] %s" (string/upper-case (name type)) message))
  (when (and (= :error type)
             (settings/get db/db [:api :exit-on-errors?] true))
    (throw (ex-info message {:result-code 1 :message extra}))))

(defmacro ^:private safe-analyze [& body]
  `(try
     ~@body
     (catch clojure.lang.ExceptionInfo e#
       (throw e#))
     (catch Exception e#
       (throw (ex-info "Error during project analysis" {:message e#})))))

(defn ^:private project-root->uri [project-root]
  (-> (or ^java.io.File project-root (io/file ""))
      .getCanonicalPath
      (shared/filename->uri db/db)))

(defn initialize-report-callback [options percentage message _db]
  (when-not (:raw? options)
    (cli-print (format "\r[%3d%s] %-28s" (int percentage) "%" message))
    (when (= 100 percentage)
      (cli-println options ""))))

(defn ^:private setup-analysis! [{:keys [project-root settings log-path verbose] :as options}]
  (swap! db/db assoc
         :api? true
         :messages-fn #(show-message-cli options %))
  (when verbose
    (logging/set-log-to-stdout))
  (when-not (:analysis @db/db)
    (safe-analyze
      (crawler/initialize-project
        (project-root->uri project-root)
        {:workspace {:workspace-edit {:document-changes true}}}
        (interop/clean-client-settings {})
        (merge (shared/assoc-some
                 {:lint-project-files-after-startup? false
                  :text-document-sync-kind :full}
                 :log-path log-path)
               settings)
        (partial initialize-report-callback options)
        db/db)
      true)))

(defn ^:private ns->ns+uri [namespace]
  (if-let [filename (:filename (q/find-namespace-definition-by-namespace (:analysis @db/db) namespace db/db))]
    {:namespace namespace
     :uri (shared/filename->uri filename db/db)}
    {:namespace namespace}))

(defn ^:private uri->ns
  [uri ns+uris]
  (->> ns+uris
       (filter #(= uri (:uri %)))
       first
       :namespace))

(defn ^:private assert-ns-exists-or-drop! [options ns+uris]
  (->> ns+uris
       (filter (complement :uri))
       (mapv #(cli-println options "Namespace" (:namespace %) "not found")))
  (filter :uri ns+uris))

(defn ^:private open-file! [{:keys [uri] :as ns+uri}]
  (handlers/did-open {:textDocument {:uri uri :text (slurp uri)}})
  ns+uri)

(defn ^:private find-new-uri-checking-rename
  [uri edits]
  (or (some->> edits
               (filter #(and (= :rename (:kind %))
                             (= uri (:old-uri %))))
               first
               :new-uri)
      uri))

(defn ^:private edits->diff-string [edits {:keys [raw? project-root]}]
  (->> edits
       (sort-by #(not= :rename (:kind %)))
       (map (fn [{:keys [kind uri old-text new-text old-uri new-uri]}]
              (if (= :rename kind)
                (diff/rename-diff old-uri new-uri (project-root->uri project-root))
                (diff/unified-diff uri (find-new-uri-checking-rename uri edits) old-text new-text (project-root->uri project-root)))))
       (map #(if raw? % (diff/colorize-diff %)))
       (string/join "\n")))

(defn ^:private exclude-ns? [{:keys [ns-exclude-regex]} namespace]
  (and ns-exclude-regex
       (re-matches ns-exclude-regex (str namespace))))

(def analyze-project! setup-analysis!)

(defn clean-ns! [{:keys [namespace dry?] :as options}]
  (setup-analysis! options)
  (cli-println options "Checking namespaces...")
  (let [namespaces (or (seq namespace)
                       (->> (q/filter-project-analysis (:analysis @db/db) db/db)
                            q/find-all-ns-definition-names
                            (remove (partial exclude-ns? options))))
        ns+uris (map ns->ns+uri namespaces)
        edits (->> ns+uris
                   (assert-ns-exists-or-drop! options)
                   (map open-file!)
                   (mapcat (comp :document-changes
                                 #(handlers/execute-command {:command "clean-ns"
                                                             :arguments [(:uri %) 0 0]})))
                   (map #(client/document-change->edit-summary % db/db))
                   (remove nil?))]
    (if (seq edits)
      (if dry?
        {:result-code 1
         :message (edits->diff-string edits options)
         :edits edits}
        (do
          (mapv (comp #(cli-println options "Cleaned" (uri->ns (:uri %) ns+uris))
                      #(client/apply-workspace-edit-summary! % db/db)) edits)
          {:result-code 0
           :edits edits}))
      {:result-code 0 :message "Nothing to clear!"})))

(defn ^:private diagnostics->diagnostic-messages [diagnostics {:keys [project-root output raw?]}]
  (let [project-path (shared/uri->filename (project-root->uri project-root))]
    (mapcat (fn [[uri diags]]
              (let [filename (shared/uri->filename uri)
                    file-output (if (:canonical-paths output)
                                  filename
                                  (shared/relativize-filepath filename project-path))]
                (map (fn [{:keys [message severity range code]}]
                       (cond-> (format "%s:%s:%s: %s: [%s] %s"
                                       file-output
                                       (-> range :start :line)
                                       (-> range :start :character)
                                       (name (f.diagnostic/severity->level severity))
                                       code
                                       message)
                         (not raw?) (diff/colorize (f.diagnostic/severity->color severity))))
                     diags)))
            diagnostics)))

(defn diagnostics [{:keys [namespace] :as options}]
  (setup-analysis! options)
  (cli-println options "Finding diagnostics...")
  (let [namespaces (or (seq namespace)
                       (->> (q/filter-project-analysis (:analysis @db/db) db/db)
                            q/find-all-ns-definition-names
                            (remove (partial exclude-ns? options))))
        diags-by-uri (->> namespaces
                          (map ns->ns+uri)
                          (assert-ns-exists-or-drop! options)
                          (map (fn [{:keys [uri]}]
                                 {:uri uri
                                  :diagnostics (f.diagnostic/find-diagnostics uri db/db)}))
                          (remove (comp empty? :diagnostics))
                          (reduce (fn [a {:keys [uri diagnostics]}]
                                    (assoc a uri diagnostics))
                                  {}))
        diags (mapcat val diags-by-uri)
        errors? (some (comp #(= 1 %) :severity) diags)
        warnings? (some (comp #(= 2 %) :severity) diags)]
    (if (seq diags-by-uri)
      {:result-code (cond errors? 3 warnings? 2 :else 0)
       :message (string/join "\n" (diagnostics->diagnostic-messages diags-by-uri options))
       :diagnostics diags-by-uri}
      {:result-code 0 :message "No diagnostics found!"})))

(defn format! [{:keys [namespace dry?] :as options}]
  (setup-analysis! options)
  (cli-println options "Formatting namespaces...")
  (let [namespaces (or (seq namespace)
                       (->> (q/filter-project-analysis (:analysis @db/db) db/db)
                            q/find-all-ns-definition-names
                            (remove (partial exclude-ns? options))))
        ns+uris (map ns->ns+uri namespaces)
        edits (->> ns+uris
                   (assert-ns-exists-or-drop! options)
                   (map open-file!)
                   (mapcat (fn [{:keys [uri]}]
                             (some->> (handlers/formatting {:textDocument uri})
                                      (map #(client/edit->summary db/db uri %)))))
                   (remove nil?))]
    (if (seq edits)
      (if dry?
        {:result-code 1
         :message (edits->diff-string edits options)
         :edits edits}
        (do
          (mapv (comp #(cli-println options "Formatted" (uri->ns (:uri %) ns+uris))
                      #(client/apply-workspace-edit-summary! % db/db)) edits)
          {:result-code 0
           :edits edits}))
      {:result-code 0 :message "Nothing to format!"})))

(defn rename! [{:keys [from to dry?] :as options}]
  (setup-analysis! options)
  (let [ns-only? (simple-symbol? from)
        from-name (when-not ns-only? (symbol (name from)))
        from-ns (if ns-only?
                  from
                  (symbol (namespace from)))
        project-analysis (q/filter-project-analysis (:analysis @db/db) db/db)]
    (if-let [from-element (if ns-only?
                            (q/find-namespace-definition-by-namespace project-analysis from-ns db/db)
                            (q/find-element-by-full-name project-analysis from-name from-ns db/db))]
      (let [uri (shared/filename->uri (:filename from-element) db/db)]
        (open-file! {:uri uri :namespace from-ns})
        (let [{:keys [error document-changes]} (f.rename/rename uri (str to) (:name-row from-element) (:name-col from-element) db/db)]
          (if document-changes
            (if-let [edits (->> document-changes
                                (map #(client/document-change->edit-summary % db/db))
                                (remove nil?)
                                seq)]
              (if dry?
                {:result-code 0
                 :message (edits->diff-string edits options)
                 :edits edits}
                (do
                  (mapv #(client/apply-workspace-edit-summary! % db/db) edits)
                  {:result-code 0
                   :message (format "Renamed %s to %s" from to)
                   :edits edits}))
              {:result-code 1 :message "Nothing to rename"})
            {:result-code 1 :message (format "Could not rename %s to %s. %s" from to (-> error :message))})))
      {:result-code 1 :message (format "Symbol %s not found in project" from)})))
