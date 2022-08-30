(ns clojure-lsp.internal-api
  (:require
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
   [clojure-lsp.diff :as diff]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.feature.rename :as f.rename]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async :refer [<! go-loop]]
   [clojure.java.io :as io]
   [clojure.string :as string])
  (:import
   [java.io File]))

(set! *warn-on-reflection* true)

(defn ^:private cli-print [& msg]
  (apply print msg)
  (flush))

(defn ^:private cli-println [options & msg]
  (when-not (:raw? options)
    (apply cli-print (update-in (vec msg) [(dec (count msg))] str "\n"))))

(defn ^:private log-print [type {:keys [verbose] :as options} & messages]
  (when verbose
    (apply cli-println options type messages)))

(defrecord ^:private CLILogger [options]
  logger/ILogger
  (setup [this]
    (logger/set-logger! this))
  (set-log-path [_ _])
  (-info [_ _fmeta arg1] (log-print "[INFO]" options arg1))
  (-info [_ _fmeta arg1 arg2] (log-print "[INFO]" options arg1 arg2))
  (-info [_ _fmeta arg1 arg2 arg3] (log-print "[INFO]" options arg1 arg2 arg3))
  (-warn [_ _fmeta arg1] (log-print (shared/colorize "[WARN]" :yellow) options arg1))
  (-warn [_ _fmeta arg1 arg2] (log-print (shared/colorize "[WARN]" :yellow) options arg1 arg2))
  (-warn [_ _fmeta arg1 arg2 arg3] (log-print (shared/colorize "[WARN]" :yellow) options arg1 arg2 arg3))
  (-error [_ _fmeta arg1] (log-print (shared/colorize "[ERROR]" :red) options arg1))
  (-error [_ _fmeta arg1 arg2] (log-print (shared/colorize "[ERROR]" :red) options arg1 arg2))
  (-error [_ _fmeta arg1 arg2 arg3] (log-print (shared/colorize "[ERROR]" :red) options arg1 arg2 arg3))
  (-debug [_ _fmeta arg1] (log-print (shared/colorize "[DEBUG]" :cyan) options arg1))
  (-debug [_ _fmeta arg1 arg2] (log-print (shared/colorize "[DEBUG]" :cyan) options arg1 arg2))
  (-debug [_ _fmeta arg1 arg2 arg3] (log-print (shared/colorize "[DEBUG]" :cyan) options arg1 arg2 arg3)))

(defn ^:private show-message-cli [db* options {:keys [message extra type]}]
  (cli-println options (format "\n[%s] %s" (string/upper-case (name type)) message))
  (when (and (= :error type)
             (settings/get @db* [:api :exit-on-errors?] true))
    (throw (ex-info message {:result-code 1 :message extra}))))

(defrecord APIProducer [db* options]
  producer/IProducer

  (refresh-code-lens [_this])
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
      (show-message-cli db* options message-content)))

  (refresh-test-tree [_this _uris]))

(def db* (atom nil))

(defn clean-db! [env]
  (doto db*
    (reset! (assoc db/initial-db :env env))))

(defn ^:private build-components [options]
  (let [db* (if @db* db* (clean-db! nil))]
    {:db* db*
     :logger (doto (->CLILogger options)
               (logger/setup))
     :producer (->APIProducer db* options)
     :current-changes-chan (async/chan 1)
     :diagnostics-chan (async/chan 1)
     :watched-files-chan (async/chan 1)
     :edits-chan (async/chan 1)}))

(defn ^:private edit->summary
  ([db uri edit]
   (edit->summary db uri edit nil))
  ([db uri {:keys [range new-text]} old-text]
   (when-let [old-text (or old-text
                           (get-in db [:documents uri :text])
                           (shared/slurp-uri uri))]
     (let [new-full-text (f.file-management/replace-text
                           old-text
                           new-text
                           (-> range :start :line)
                           (-> range :start :character)
                           (-> range :end :line)
                           (-> range :end :character))]
       (when (not= new-full-text old-text)
         {:kind :change
          :uri uri
          :version (get-in db [:documents uri :version] 0)
          :old-text old-text
          :new-text new-full-text})))))

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
  [{:keys [uri new-text version changed?]} {:keys [db*] :as components}]
  (spit uri new-text)
  (when (and changed?
             (get-in @db* [:documents uri :text]))
    (f.file-management/did-change uri new-text (inc version) components)))

(defn ^:private apply-workspace-rename-edit-summary!
  [{:keys [old-uri new-uri]} components]
  (let [old-file (-> old-uri shared/uri->filename io/file)
        new-file (-> new-uri shared/uri->filename io/file)]
    (io/make-parents new-file)
    (io/copy old-file new-file)
    (io/delete-file old-file)
    (f.file-management/did-close old-uri components)
    (f.file-management/did-open new-uri (slurp new-file) components false)))

(defn ^:private apply-workspace-edit-summary!
  [change components]
  (if (= :rename (:kind change))
    (apply-workspace-rename-edit-summary! change components)
    (apply-workspace-change-edit-summary! change components))
  change)

(defn ^:private project-root->uri [project-root db]
  (-> (or ^File project-root (io/file ""))
      .getCanonicalPath
      (shared/filename->uri db)))

(defn ^:private setup-api! [{:keys [producer db* diagnostics-chan]}]
  (swap! db* assoc :api? true)
  (go-loop []
    (producer/publish-diagnostic producer (<! diagnostics-chan))
    (recur)))

(defn ^:private analyze!
  [{:keys [project-root settings log-path]}
   {:keys [db*] :as components}]
  (try
    (crawler/initialize-project
      (project-root->uri project-root @db*)
      {:workspace {:workspace-edit {:document-changes true}}}
      (settings/clean-client-settings {})
      (merge (shared/assoc-some
               {:lint-project-files-after-startup? false
                :text-document-sync-kind :full}
               :log-path log-path)
             settings)
      nil
      components)
    true
    (catch clojure.lang.ExceptionInfo e
      (throw e))
    (catch Exception e
      (throw (ex-info "Error during project analysis" {:message e})))))

(defn ^:private setup-project-and-deps-analysis! [options {:keys [db*] :as components}]
  (let [db @db*]
    (when (or (not (:analysis db))
              (not= :project-and-deps (:project-analysis-type db)))
      (swap! db* assoc :project-analysis-type :project-and-deps)
      (analyze! options components))))

(defn ^:private setup-project-only-analysis! [options {:keys [db*] :as components}]
  (when (not (:analysis @db*))
    (swap! db* assoc :project-analysis-type :project-only)
    (analyze! options components)))

(defn ^:private nses->ns+uri [namespaces db]
  (let [found (q/nses-some-internal-uri db namespaces)]
    (map (fn [namespace]
           (if-let [uri (get found namespace)]
             {:namespace namespace
              :uri uri}
             {:namespace namespace}))
         namespaces)))

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

(defn ^:private open-file! [{:keys [uri] :as ns+uri} components]
  (f.file-management/load-document! uri (slurp uri) (:db* components))
  ns+uri)

(defn ^:private find-new-uri-checking-rename
  [uri edits]
  (or (some->> edits
               (filter #(and (= :rename (:kind %))
                             (= uri (:old-uri %))))
               first
               :new-uri)
      uri))

(defn ^:private edits->diff-string [edits {:keys [raw? project-root]} db]
  (->> edits
       (sort-by #(not= :rename (:kind %)))
       (map (fn [{:keys [kind uri old-text new-text old-uri new-uri]}]
              (if (= :rename kind)
                (diff/rename-diff old-uri new-uri (project-root->uri project-root db))
                (diff/unified-diff uri (find-new-uri-checking-rename uri edits) old-text new-text (project-root->uri project-root db)))))
       (map #(if raw? % (diff/colorize-diff %)))
       (string/join "\n")))

(defn ^:private exclude-ns? [{:keys [ns-exclude-regex]} namespace]
  (and ns-exclude-regex
       (re-matches ns-exclude-regex (str namespace))))

(defn ^:private options->namespaces [{:keys [namespace filenames project-root] :as options} db]
  (or (seq namespace)
      (->> filenames
           (map (fn [^File filename-or-dir]
                  (if (.isAbsolute filename-or-dir)
                    (io/file filename-or-dir)
                    (io/file project-root filename-or-dir))))
           (map (fn [^File filename-or-dir]
                  (if (shared/directory? filename-or-dir)
                    (->> filename-or-dir
                         file-seq
                         (remove shared/directory?)
                         (map #(shared/filename->namespace (.getCanonicalPath ^File %) db)))
                    (shared/filename->namespace (.getCanonicalPath filename-or-dir) db))))
           flatten
           (remove nil?)
           (map symbol)
           seq)
      (into #{}
            (remove (partial exclude-ns? options))
            (q/internal-ns-names db))))

(defn ^:private analyze-project-and-deps!* [options components]
  (setup-api! components)
  (setup-project-and-deps-analysis! options components))

(defn ^:private analyze-project-only!* [options components]
  (setup-api! components)
  (setup-project-only-analysis! options components))

(defn ^:private clean-ns!* [{:keys [dry?] :as options} {:keys [db*] :as components}]
  (setup-api! components)
  (setup-project-only-analysis! options components)
  (cli-println options "Checking namespaces...")
  (let [db @db*
        namespaces (options->namespaces options db)
        ns+uris (nses->ns+uri namespaces db)
        edits (->> ns+uris
                   (assert-ns-exists-or-drop! options)
                   (map #(open-file! % components))
                   (pmap (comp :document-changes
                               #(handlers/execute-command components
                                                          {:command "clean-ns"
                                                           :arguments [(:uri %) 0 0]})))
                   (apply concat)
                   (pmap #(document-change->edit-summary % db))
                   (remove nil?))]
    (if (seq edits)
      (if dry?
        {:result-code 1
         :message (edits->diff-string edits options db)
         :edits edits}
        (do
          (mapv (comp #(cli-println options "Cleaned" (uri->ns (:uri %) ns+uris))
                      #(apply-workspace-edit-summary! % components)) edits)
          {:result-code 0
           :edits edits}))
      {:result-code 0 :message "Nothing to clear!"})))

(defn ^:private diagnostics->diagnostic-messages [diagnostics {:keys [project-root output raw?]} db]
  (let [project-path (shared/uri->filename (project-root->uri project-root db))]
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
                         (not raw?) (shared/colorize (f.diagnostic/severity->color severity))))
                     diags)))
            diagnostics)))

(defn ^:private diagnostics* [options {:keys [db*] :as components}]
  (setup-api! components)
  (setup-project-and-deps-analysis! options components)
  (cli-println options "Finding diagnostics...")
  (let [db @db*
        namespaces (options->namespaces options db)
        ns+uris (nses->ns+uri namespaces db)
        diags-by-uri (->> ns+uris
                          (assert-ns-exists-or-drop! options)
                          (pmap (fn [{:keys [uri]}]
                                  {:uri uri
                                   :diagnostics (f.diagnostic/find-diagnostics uri db)}))
                          (remove (comp empty? :diagnostics))
                          (reduce (fn [a {:keys [uri diagnostics]}]
                                    (assoc a uri diagnostics))
                                  {}))
        diags (mapcat val diags-by-uri)
        errors? (some (comp #(= 1 %) :severity) diags)
        warnings? (some (comp #(= 2 %) :severity) diags)]
    (if (seq diags-by-uri)
      {:result-code (cond errors? 3 warnings? 2 :else 0)
       :message (string/join "\n" (diagnostics->diagnostic-messages diags-by-uri options db))
       :diagnostics diags-by-uri}
      {:result-code 0 :message "No diagnostics found!"})))

(defn ^:private format!* [{:keys [dry?] :as options} {:keys [db*] :as components}]
  (setup-api! components)
  (setup-project-only-analysis! options components)
  (cli-println options "Formatting namespaces...")
  (let [db @db*
        namespaces (options->namespaces options db)
        ns+uris (nses->ns+uri namespaces db)
        edits (->> ns+uris
                   (assert-ns-exists-or-drop! options)
                   (map #(open-file! % components))
                   (pmap (comp (fn [{:keys [uri]}]
                                 (some->> (handlers/formatting components {:text-document {:uri uri}})
                                          (map #(edit->summary @db* uri %))))))
                   (apply concat)
                   (remove nil?))]
    (if (seq edits)
      (if dry?
        {:result-code 1
         :message (edits->diff-string edits options @db*)
         :edits edits}
        (do
          (mapv (comp #(cli-println options "Formatted" (uri->ns (:uri %) ns+uris))
                      #(apply-workspace-edit-summary! % components)) edits)
          {:result-code 0
           :edits edits}))
      {:result-code 0 :message "Nothing to format!"})))

(defn ^:private rename!* [{:keys [from to dry?] :as options} {:keys [db*] :as components}]
  (setup-api! components)
  (setup-project-only-analysis! options components)
  (let [db @db*
        ns-only? (simple-symbol? from)
        from-name (when-not ns-only? (symbol (name from)))
        from-ns (if ns-only?
                  from
                  (symbol (namespace from)))]
    (if-let [from-element (q/find-element-for-rename db from-ns from-name)]
      (let [uri (shared/filename->uri (:filename from-element) db)]
        (open-file! {:uri uri :namespace from-ns} components)
        (let [{:keys [error document-changes]} (f.rename/rename-from-position uri (str to) (:name-row from-element) (:name-col from-element) db)]
          (if document-changes
            (if-let [edits (->> document-changes
                                (pmap #(document-change->edit-summary % db))
                                (remove nil?)
                                seq)]
              (if dry?
                {:result-code 0
                 :message (edits->diff-string edits options db)
                 :edits edits}
                (do
                  (mapv #(apply-workspace-edit-summary! % components) edits)
                  {:result-code 0
                   :message (format "Renamed %s to %s" from to)
                   :edits edits}))
              {:result-code 1 :message "Nothing to rename"})
            {:result-code 1 :message (format "Could not rename %s to %s. %s" from to (-> error :message))})))
      {:result-code 1 :message (format "Symbol %s not found in project" from)})))

(defn analyze-project-and-deps! [options]
  (analyze-project-and-deps!* options (build-components options)))

(defn analyze-project-only! [options]
  (analyze-project-only!* options (build-components options)))

(defn clean-ns! [options]
  (clean-ns!* options (build-components options)))

(defn diagnostics [options]
  (diagnostics* options (build-components options)))

(defn format! [options]
  (format!* options (build-components options)))

(defn rename! [options]
  (rename!* options (build-components options)))
