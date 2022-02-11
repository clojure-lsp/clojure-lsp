(ns clojure-lsp.internal-api
  (:require
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
   [clojure-lsp.diff :as diff]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.feature.rename :as f.rename]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.logging :as logging]
   [clojure-lsp.producer :as producer]
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

(defrecord APIProducer [options]
  producer/IProducer

  (refresh-code-lens [_this])
  (refresh-test-tree [_this _uris])
  (publish-diagnostic [_this _diagnostic])
  (publish-workspace-edit [_this _edit])

  (publish-progress [_this percentage message _progress-token]
    (when-not (:raw? options)
      (cli-print (format "\r[%3d%s] %-28s" (int percentage) "%" message))
      (when (= 100 percentage)
        (cli-println options ""))))

  (show-document-request [_this _document-request])
  (show-message-request [_this _message _type _actions])

  (show-message [_this message type extra]
    (let [message-content {:message message
                           :type type
                           :extra extra}]
      (show-message-cli options message-content)))
  (register-capability [_this _capability]))

(defn ^:private edit->summary
  ([db uri edit]
   (edit->summary db uri edit nil))
  ([db uri {:keys [range new-text]} old-text]
   (let [old-text (or old-text
                      (get-in @db [:documents uri :text])
                      (slurp uri))
         new-full-text (f.file-management/replace-text
                         old-text
                         new-text
                         (-> range :start :line)
                         (-> range :start :character)
                         (-> range :end :line)
                         (-> range :end :character))]
     (when (not= new-full-text old-text)
       {:kind :change
        :uri uri
        :version (get-in @db [:documents uri :version] 0)
        :old-text old-text
        :new-text new-full-text}))))

(defn ^:private document-change->edit-summary [{:keys [text-document edits kind old-uri new-uri]} db]
  (if (= "rename" kind)
    {:kind :rename
     :new-uri new-uri
     :old-uri old-uri}
    (let [uri (:uri text-document)]
      (loop [edit-summary nil
             i 0]
        (if-let [edit (nth edits i nil)]
          (when-let [edit-summary (edit->summary db uri edit (:new-text edit-summary))]
            (recur edit-summary (inc i)))
          edit-summary)))))

(defn ^:private apply-workspace-change-edit-summary!
  [{:keys [uri new-text version changed?]} db]
  (spit uri new-text)
  (when (and changed?
             (get-in @db [:documents uri :text]))
    (f.file-management/did-change uri new-text (inc version) db)))

(defn ^:private apply-workspace-rename-edit-summary!
  [{:keys [old-uri new-uri]} db]
  (let [old-file (-> old-uri shared/uri->filename io/file)
        new-file (-> new-uri shared/uri->filename io/file)]
    (io/make-parents new-file)
    (io/copy old-file new-file)
    (io/delete-file old-file)
    (f.file-management/did-close old-uri db)
    (f.file-management/did-open new-uri (slurp new-file) db false)))

(defn ^:private apply-workspace-edit-summary!
  [change db]
  (if (= :rename (:kind change))
    (apply-workspace-rename-edit-summary! change db)
    (apply-workspace-change-edit-summary! change db))
  change)

(defn ^:private project-root->uri [project-root]
  (-> (or ^java.io.File project-root (io/file ""))
      .getCanonicalPath
      (shared/filename->uri db/db)))

(defn ^:private setup-api! [{:keys [verbose] :as options}]
  (swap! db/db assoc
         :api? true
         :producer (->APIProducer options))
  (when verbose
    (logging/set-log-to-stdout)))

(defn ^:private analyze!
  [{:keys [project-root settings log-path]}]
  (try
    (crawler/initialize-project
      (project-root->uri project-root)
      {:workspace {:workspace-edit {:document-changes true}}}
      (settings/clean-client-settings {})
      (merge (shared/assoc-some
               {:lint-project-files-after-startup? false
                :text-document-sync-kind :full}
               :log-path log-path)
             settings)
      nil
      db/db)
    true
    (catch clojure.lang.ExceptionInfo e
      (throw e))
    (catch Exception e
      (throw (ex-info "Error during project analysis" {:message e})))))

(defn ^:private setup-project-and-deps-analysis! [options]
  (when (or (not (:analysis @db/db))
            (not= :project-and-deps (:project-analysis-type @db/db)))
    (swap! db/db assoc :project-analysis-type :project-and-deps)
    (analyze! options)))

(defn ^:private setup-project-only-analysis! [options]
  (when (not (:analysis @db/db))
    (swap! db/db assoc :project-analysis-type :project-only)
    (analyze! options)))

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

(defn analyze-project-and-deps! [options]
  (setup-api! options)
  (setup-project-and-deps-analysis! options))

(defn analyze-project-only! [options]
  (setup-api! options)
  (setup-project-only-analysis! options))

(defn clean-ns! [{:keys [namespace dry?] :as options}]
  (setup-api! options)
  (setup-project-only-analysis! options)
  (cli-println options "Checking namespaces...")
  (let [namespaces (or (seq namespace)
                       (->> (q/filter-project-analysis (:analysis @db/db) db/db)
                            q/find-all-ns-definition-names
                            (remove (partial exclude-ns? options))))
        ns+uris (pmap ns->ns+uri namespaces)
        edits (->> ns+uris
                   (assert-ns-exists-or-drop! options)
                   (map open-file!)
                   (pmap (comp :document-changes
                               #(handlers/execute-command {:command "clean-ns"
                                                           :arguments [(:uri %) 0 0]})))
                   (apply concat)
                   (pmap #(document-change->edit-summary % db/db))
                   (remove nil?))]
    (if (seq edits)
      (if dry?
        {:result-code 1
         :message (edits->diff-string edits options)
         :edits edits}
        (do
          (mapv (comp #(cli-println options "Cleaned" (uri->ns (:uri %) ns+uris))
                      #(apply-workspace-edit-summary! % db/db)) edits)
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
  (setup-api! options)
  (setup-project-and-deps-analysis! options)
  (cli-println options "Finding diagnostics...")
  (let [namespaces (or (seq namespace)
                       (->> (q/filter-project-analysis (:analysis @db/db) db/db)
                            q/find-all-ns-definition-names
                            (remove (partial exclude-ns? options))))
        diags-by-uri (->> namespaces
                          (pmap ns->ns+uri)
                          (assert-ns-exists-or-drop! options)
                          (pmap (fn [{:keys [uri]}]
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
  (setup-api! options)
  (setup-project-only-analysis! options)
  (cli-println options "Formatting namespaces...")
  (let [namespaces (or (seq namespace)
                       (->> (q/filter-project-analysis (:analysis @db/db) db/db)
                            q/find-all-ns-definition-names
                            (remove (partial exclude-ns? options))))
        ns+uris (pmap ns->ns+uri namespaces)
        edits (->> ns+uris
                   (assert-ns-exists-or-drop! options)
                   (map open-file!)
                   (pmap (comp (fn [{:keys [uri]}]
                                 (some->> (handlers/formatting {:textDocument uri})
                                          (map #(edit->summary db/db uri %))))))
                   (apply concat)
                   (remove nil?))]
    (if (seq edits)
      (if dry?
        {:result-code 1
         :message (edits->diff-string edits options)
         :edits edits}
        (do
          (mapv (comp #(cli-println options "Formatted" (uri->ns (:uri %) ns+uris))
                      #(apply-workspace-edit-summary! % db/db)) edits)
          {:result-code 0
           :edits edits}))
      {:result-code 0 :message "Nothing to format!"})))

(defn rename! [{:keys [from to dry?] :as options}]
  (setup-api! options)
  (setup-project-only-analysis! options)
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
                                (pmap #(document-change->edit-summary % db/db))
                                (remove nil?)
                                seq)]
              (if dry?
                {:result-code 0
                 :message (edits->diff-string edits options)
                 :edits edits}
                (do
                  (mapv #(apply-workspace-edit-summary! % db/db) edits)
                  {:result-code 0
                   :message (format "Renamed %s to %s" from to)
                   :edits edits}))
              {:result-code 1 :message "Nothing to rename"})
            {:result-code 1 :message (format "Could not rename %s to %s. %s" from to (-> error :message))})))
      {:result-code 1 :message (format "Symbol %s not found in project" from)})))
