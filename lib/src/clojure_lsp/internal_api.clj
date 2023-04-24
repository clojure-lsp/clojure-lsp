(ns clojure-lsp.internal-api
  (:require
   [cheshire.core :as json]
   [clojure-lsp.db :as db]
   [clojure-lsp.dep-graph :as dep-graph]
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
   [clojure-lsp.startup :as startup]
   [clojure.core.async :as async :refer [<! go-loop]]
   [clojure.java.io :as io]
   [clojure.string :as string])
  (:import
   [java.io File]))

(set! *warn-on-reflection* true)

(defn ^:private cli-print [& msg]
  (binding [*out* *err*]
    (apply print msg)
    (flush)))

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
    (throw (ex-info message {:result-code 1 :message-fn (constantly extra)}))))

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
    (apply-workspace-change-edit-summary! change components)))

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
    (startup/initialize-project
      (project-root->uri project-root @db*)
      {:workspace {:workspace-edit {:document-changes true}}}
      (settings/clean-client-settings {})
      (merge (shared/assoc-some
               {:lint-project-files-after-startup? false
                :text-document-sync-kind :full}
               :log-path log-path)
             settings)
      "clojure-lsp-api"
      components)
    true
    (catch clojure.lang.ExceptionInfo e
      (throw e))
    (catch Exception e
      (throw (ex-info "Error during project analysis" {:message e})))))

(defn ^:private setup-project-and-full-deps-analysis! [options {:keys [db*] :as components}]
  (let [db @db*]
    (when (or (not (:analysis db))
              (not= :project-and-full-dependencies (:project-analysis-type db)))
      (swap! db* assoc :project-analysis-type :project-and-full-dependencies)
      (analyze! options components))))

(defn ^:private setup-project-and-clojure-only-deps-analysis! [options {:keys [db*] :as components}]
  (let [db @db*]
    (when (or (not (:analysis db))
              (not= :project-and-clojure-only-dependencies (:project-analysis-type db)))
      (swap! db* assoc :project-analysis-type :project-and-clojure-only-dependencies)
      (analyze! options components))))

(defn ^:private setup-project-only-analysis! [options {:keys [db*] :as components}]
  (when (not (:analysis @db*))
    (swap! db* assoc :project-analysis-type :project-only)
    (analyze! options components)))

(defn ^:private dynamic-setup-project-analysis! [options components]
  (case (get-in options [:analysis :type] :project-only)
    :project-and-full-dependencies (setup-project-and-full-deps-analysis! options components)
    :project-only (setup-project-only-analysis! options components)))

(defn ^:private open-file! [uri components]
  (f.file-management/load-document! uri (slurp uri) (:db* components))
  uri)

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

(defn ^:private options->uris [{:keys [namespace filenames project-root] :as options} db]
  (cond
    (seq namespace)
    (->> namespace
         (mapcat (fn [namespace]
                   (let [uris (dep-graph/ns-internal-uris db namespace)]
                     (when-not (seq uris)
                       (cli-println options "Namespace" namespace "not found"))
                     uris))))
    (seq filenames)
    (->> filenames
         (map (fn [^File filename-or-dir]
                (if (.isAbsolute filename-or-dir)
                  (io/file filename-or-dir)
                  (io/file project-root filename-or-dir))))
         (mapcat (fn [^File filename-or-dir]
                   (if (shared/directory? filename-or-dir)
                     (->> filename-or-dir
                          file-seq
                          (remove shared/directory?)
                          (map #(.getCanonicalPath ^File %)))
                     [(.getCanonicalPath filename-or-dir)])))
         (map #(shared/filename->uri % db))
         seq)
    :else
    (into #{}
          (comp
            (filter #(contains? shared/valid-langs (shared/uri->file-type %)))
            (remove #(exclude-ns? options %)))
          (dep-graph/internal-uris db))))

(defn ^:private analyze-project-and-deps!* [options components]
  (setup-api! components)
  (setup-project-and-full-deps-analysis! options components))

(defn ^:private analyze-project-only!* [options components]
  (setup-api! components)
  (setup-project-only-analysis! options components))

(defn ^:private clean-ns!* [{:keys [dry?] :as options} {:keys [db*] :as components}]
  (setup-api! components)
  (setup-project-only-analysis! options components)
  (cli-println options "Checking namespaces...")
  (let [db @db*
        edits (->> (options->uris options db)
                   (map #(open-file! % components))
                   (pmap (comp :document-changes
                               #(handlers/execute-command components
                                                          {:command "clean-ns"
                                                           :arguments [% 0 0]})))
                   (apply concat)
                   (pmap #(document-change->edit-summary % db))
                   (remove nil?))]
    (if (seq edits)
      (if dry?
        {:result-code 1
         :message-fn (fn [] (edits->diff-string edits options db))
         :edits edits}
        (do
          (run! #(apply-workspace-edit-summary! % components) edits)
          (->> edits
               (mapcat #(dep-graph/ns-names-for-uri db (:uri %)))
               (run! #(cli-println options "Cleaned" %)))
          {:result-code 0
           :message-fn (constantly (format "Cleaned %s namespaces" (count edits)))
           :edits edits}))
      {:result-code 0 :message-fn (constantly "Nothing to clear!")})))

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
                                       (-> range :start :line inc)
                                       (-> range :start :character inc)
                                       (name (f.diagnostic/severity->level severity))
                                       code
                                       message)
                         (not raw?) (shared/colorize (f.diagnostic/severity->color severity))))
                     diags)))
            diagnostics)))

(defn ^:private diagnostics* [{{:keys [format]} :output :as options} {:keys [db*] :as components}]
  (setup-api! components)
  (setup-project-and-clojure-only-deps-analysis! options components)
  (cli-println options "Finding diagnostics...")
  (let [db @db*
        diags-by-uri (->> (options->uris options db)
                          (pmap (fn [uri]
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
       :message-fn (fn []
                     (case format
                       :edn (with-out-str (pr diags-by-uri))
                       :json (json/generate-string diags-by-uri)
                       (string/join "\n" (diagnostics->diagnostic-messages diags-by-uri options db))))
       :diagnostics diags-by-uri}
      {:result-code 0 :message-fn (constantly "No diagnostics found!")})))

(defn ^:private format!* [{:keys [dry?] :as options} {:keys [db*] :as components}]
  (setup-api! components)
  (setup-project-only-analysis! options components)
  (cli-println options "Formatting namespaces...")
  (let [db @db*
        edits (->> (options->uris options db)
                   (map #(open-file! % components))
                   (pmap (fn [uri]
                           (some->> (handlers/formatting components {:text-document {:uri uri}})
                                    (map #(edit->summary @db* uri %)))))
                   (apply concat)
                   (remove nil?))]
    (if (seq edits)
      (if dry?
        {:result-code 1
         :message-fn (fn [] (edits->diff-string edits options @db*))
         :edits edits}
        (do
          (run! #(apply-workspace-edit-summary! % components) edits)
          (->> edits
               (mapcat #(dep-graph/ns-names-for-uri db (:uri %)))
               (run! #(cli-println options "Formatted" %)))
          {:result-code 0
           :message-fn (constantly (format "Formatted %s namespaces" (count edits)))
           :edits edits}))
      {:result-code 0 :message-fn (constantly "Nothing to format!")})))

(defn ^:private rename!* [{:keys [from to dry?] :as options} {:keys [db*] :as components}]
  (setup-api! components)
  (setup-project-only-analysis! options components)
  (let [db @db*
        ns-only? (simple-symbol? from)
        from-name (when-not ns-only? (symbol (name from)))
        from-ns (if ns-only?
                  from
                  (symbol (namespace from)))]
    (if-let [{:keys [uri name-row name-col]} (q/find-element-for-rename db from-ns from-name)]
      (let [{:keys [error document-changes]} (-> uri
                                                 (open-file! components)
                                                 (f.rename/rename-from-position (str to) name-row name-col db))]
        (if document-changes
          (if-let [edits (->> document-changes
                              (pmap #(document-change->edit-summary % db))
                              (remove nil?)
                              seq)]
            (if dry?
              {:result-code 0
               :message-fn (fn [] (edits->diff-string edits options db))
               :edits edits}
              (do
                (run! #(apply-workspace-edit-summary! % components) edits)
                {:result-code 0
                 :message-fn (constantly (format "Renamed %s to %s" from to))
                 :edits edits}))
            {:result-code 1 :message-fn (constantly "Nothing to rename")})
          {:result-code 1 :message-fn (constantly (format "Could not rename %s to %s. %s" from to (-> error :message)))}))
      {:result-code 1 :message-fn (constantly (format "Symbol %s not found in project" from))})))

(defn ^:private db->dump-data [db filter-keys]
  (as-> db $
    (select-keys $ [:classpath :analysis :dep-graph :findings :settings])
    (assoc $
           :project-root (shared/uri->filename (:project-root-uri db))
           :source-paths (-> db :settings :source-paths))
    (select-keys $
                 (if (not (coll? filter-keys))
                   (keys $)
                   (for [[k _] $
                         :when (contains? (set filter-keys) k)]
                     k)))))

(defn ^:private dump* [{{:keys [format filter-keys] :or {format :edn}} :output :as options} {:keys [db*] :as components}]
  (setup-api! components)
  (dynamic-setup-project-analysis! options components)
  (let [db @db*
        dump-data (db->dump-data db filter-keys)]
    (case format
      :edn {:result-code 0
            :result dump-data
            :message-fn (fn [] (with-out-str (pr dump-data)))}
      :json (let [json-string (json/generate-string dump-data)]
              {:result-code 0
               :result json-string
               :message-fn (constantly json-string)})
      {:result-code 1
       :message-fn (constantly (clojure.core/format "Output format %s not supported" format))})))

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

(defn dump [options]
  (dump* options (build-components options)))
