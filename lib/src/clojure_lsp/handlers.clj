(ns clojure-lsp.handlers
  (:require
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.feature.call-hierarchy :as f.call-hierarchy]
   [clojure-lsp.feature.clojuredocs :as f.clojuredocs]
   [clojure-lsp.feature.code-actions :as f.code-actions]
   [clojure-lsp.feature.code-lens :as f.code-lens]
   [clojure-lsp.feature.completion :as f.completion]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.feature.document-symbol :as f.document-symbol]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.feature.format :as f.format]
   [clojure-lsp.feature.hover :as f.hover]
   [clojure-lsp.feature.java-interop :as f.java-interop]
   [clojure-lsp.feature.linked-editing-range :as f.linked-editing-range]
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.feature.rename :as f.rename]
   [clojure-lsp.feature.semantic-tokens :as f.semantic-tokens]
   [clojure-lsp.feature.signature-help :as f.signature-help]
   [clojure-lsp.feature.stubs :as stubs]
   [clojure-lsp.feature.workspace-symbols :as f.workspace-symbols]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.startup :as startup]
   [clojure.core.async :as async]
   [clojure.pprint :as pprint]))

(set! *warn-on-reflection* true)

(def backoff-start 5)
(def backoff-mult 1.2)
(def backoff-max 200)

(comment
  (->> backoff-start
       (iterate #(int (min backoff-max (* backoff-mult %))))
       (reductions (fn [[t _] b]
                     [(+ t b) b])
                   [0 0])
       rest
       (cons [:total :backoff])
       (take 20))
  #_{})

(defn ^:private caught-up? [db uri desired-version]
  (let [analysed-version (get-in db [:documents uri :analyzed-version])]
    (or
      ;; The doc was never opened, or was closed. Whatever analysis we have has to be good enough.
      (not analysed-version)
      ;; The analysis has caught up.
      (>= analysed-version desired-version))))

(defn ^:private wait-for-changes [{:keys [db* :lsp4clj.server/req-cancelled?]} uris]
  (let [delay-start (System/nanoTime)
        db @db*]
    (loop [immediate? true
           backoff backoff-start
           ;; At the time of the request, what was the client looking at?
           desired-versions (map (fn [uri]
                                   [uri (get-in db [:documents uri :v] 0)])
                                 uris)]
      (let [now (System/nanoTime)]
        (cond
          (some-> req-cancelled? deref)
          {:delay/outcome :cancelled
           :delay/start delay-start
           :delay/end now}
          (> (quot (- now delay-start) 1000000) 60000) ; one minute timeout
          {:delay/outcome :timed-out
           :delay/timeout-uris (map first desired-versions)}
          :else
          ;; At the current time, do we have analysis for what the client was looking at?
          (let [db @db*]
            (if-let [processing-versions (->> desired-versions
                                              (remove (fn [[uri desired-version]]
                                                        (caught-up? db uri desired-version)))
                                              seq)]
              (do
                (Thread/sleep (long backoff))
                (recur false (min backoff-max (* backoff-mult backoff)) processing-versions))
              (if immediate?
                {:delay/outcome :immediate
                 :delay/start delay-start}
                {:delay/outcome :waited
                 :delay/start delay-start
                 :delay/end now}))))))))

(defmacro logging-delayed-task [delay-data task-id & body]
  (let [cancelled-msg (str task-id " cancelled - waited %s")
        timed-out-msg (format "Timeout in %s waiting for changes to %%s" task-id)
        immediate-msg (str task-id " %s")
        waited-msg (str immediate-msg " - waited %s")
        msg-sym (gensym "log-message")]
    `(let [delay-data# ~delay-data
           delay-outcome# (:delay/outcome delay-data#)]
       (case delay-outcome#
         :cancelled
         (let [~msg-sym (format ~cancelled-msg (shared/format-time-delta-ms (:delay/start delay-data#) (:delay/end delay-data#)))]
           ~(with-meta `(logger/debug ~msg-sym) (meta &form))
           {:error {:code :request-cancelled
                    :message "Request cancelled by client."}})
         :timed-out
         (let [~msg-sym (format ~timed-out-msg (first (:delay/timeout-uris delay-data#)))]
           ~(with-meta `(logger/warn ~msg-sym) (meta &form))
           {:error {:code :request-failed
                    :message "Timed out waiting for analysis."}})
         (let [result# (do ~@body)
               ~msg-sym (case delay-outcome#
                          :immediate
                          (format ~immediate-msg
                                  (shared/start-time->end-time-ms (:delay/start delay-data#)))
                          :waited
                          (format ~waited-msg
                                  (shared/start-time->end-time-ms (:delay/end delay-data#))
                                  (shared/format-time-delta-ms (:delay/start delay-data#) (:delay/end delay-data#))))]
           ~(with-meta `(logger/info ~msg-sym) (meta &form))
           result#)))))

(defmacro process-after-all-changes
  [components uris task & body]
  (with-meta
    `(logging-delayed-task
       (wait-for-changes ~components ~uris)
       ~task
       ~@body)
    (meta &form)))

(defmacro process-after-changes
  [components uri task & body]
  (with-meta
    `(process-after-all-changes ~components [~uri] ~task ~@body)
    (meta &form)))

(defn ^:private element->location [db producer element]
  {:uri (f.java-interop/uri->translated-uri (:uri element) db producer)
   :range (shared/->range element)})

(defn initialize
  [{:keys [db* producer] :as components}
   project-root-uri
   client-capabilities
   client-settings
   work-done-token]
  (shared/logging-task
    :initialize
    (swap! db* assoc :project-analysis-type :project-and-full-dependencies)
    (if project-root-uri
      (do
        (startup/initialize-project
          project-root-uri
          client-capabilities
          client-settings
          {}
          work-done-token
          components)
        (let [db @db*
              internal-uris (dep-graph/internal-uris db)]
          (when (settings/get db [:lint-project-files-after-startup?] true)
            (f.diagnostic/publish-all-diagnostics! internal-uris components))
          (async/go
            (f.clojuredocs/refresh-cache! db*))
          (async/go
            (let [settings (:settings db)]
              (when (stubs/check-stubs? settings)
                (stubs/generate-and-analyze-stubs! settings db*))))
          (logger/info startup/startup-logger-tag "Analyzing test paths for project root" project-root-uri)
          (producer/refresh-test-tree producer internal-uris)
          (when (settings/get db [:java] true)
            (async/go
              (f.java-interop/retrieve-jdk-source-and-analyze! db*)))))
      (producer/show-message producer "No project-root-uri was specified, some features may not work properly." :warning nil))))

(defn did-open [{:keys [producer] :as components} {:keys [text-document]}]
  (shared/logging-task
    :did-open
    (let [uri (:uri text-document)
          text (:text text-document)]
      (f.file-management/did-open uri text components true)
      (producer/refresh-test-tree producer [uri])))
  nil)

(defn did-save [components {:keys [text-document]}]
  (shared/logging-task
    :did-save
    (f.file-management/did-save (:uri text-document) components)))

;; TODO implement it, do we need to do anything?
#_(defn did-delete-files [{:keys [db*]} {:keys [text-document]}]
    (when (get-in @db [:documents (:uri text-document) :saved-on-disk])
      (swap! db #(update % :documents dissoc (:uri text-document)))))

(defn did-change [components {:keys [text-document content-changes]}]
  (f.file-management/did-change (:uri text-document) content-changes (:version text-document) components))

(defn did-close [components {:keys [text-document]}]
  (shared/logging-task
    :did-close
    (f.file-management/did-close (:uri text-document) components)))

(defn did-change-watched-files [components {:keys [changes]}]
  (f.file-management/did-change-watched-files changes components))

(defn completion [{:keys [db*] :as components} {:keys [text-document position]}]
  (letfn [(completion-fn []
            (shared/logging-results
              (str :completion " %s - total items: %s")
              count
              (let [db @db*
                    [row col] (shared/position->row-col position)]
                (f.completion/completion (:uri text-document) row col db))))]
    (if (identical? :slow-but-accurate (get-in @db* [:settings :completion :analysis-type] :fast-but-stale))
      (process-after-changes
        components (:uri text-document)
        :completion
        (completion-fn))
      (completion-fn))))

(defn references [{:keys [db* producer]} {:keys [text-document position context]}]
  (shared/logging-task
    :references
    (let [db @db*
          [row col] (shared/position->row-col position)]
      (mapv #(element->location db producer %1)
            (q/find-references-from-cursor db (:uri text-document) row col (:include-declaration context))))))

(defn completion-resolve-item [{:keys [db*]} item]
  (shared/logging-task
    :resolve-completion-item
    (f.completion/resolve-item item db*)))

(defn prepare-rename [{:keys [db*] :as components} {:keys [text-document position]}]
  (process-after-changes
    components (:uri text-document)
    :prepare-rename
    (let [[row col] (shared/position->row-col position)]
      (f.rename/prepare-rename (:uri text-document) row col @db*))))

(defn rename [{:keys [db*]} {:keys [text-document position new-name]}]
  (shared/logging-task
    :rename
    (let [[row col] (shared/position->row-col position)]
      (f.rename/rename-from-position (:uri text-document) new-name row col @db*))))

(defn definition [{:keys [db* producer]} {:keys [text-document position]}]
  (shared/logging-task
    :definition
    (let [db @db*
          [row col] (shared/position->row-col position)]
      (when-let [definition (q/find-definition-from-cursor db (:uri text-document) row col)]
        (element->location db producer definition)))))

(defn declaration [{:keys [db* producer]} {:keys [text-document position]}]
  (shared/logging-task
    :declaration
    (let [db @db*
          [row col] (shared/position->row-col position)]
      (when-let [declaration (q/find-declaration-from-cursor db (:uri text-document) row col)]
        (element->location db producer declaration)))))

(defn implementation [{:keys [db* producer]} {:keys [text-document position]}]
  (shared/logging-task
    :implementation
    (let [db @db*
          [row col] (shared/position->row-col position)]
      (mapv #(element->location db producer %)
            (q/find-implementations-from-cursor db (:uri text-document) row col)))))

(defn document-symbol [{:keys [db*] :as components} {:keys [text-document]}]
  (process-after-changes
    components (:uri text-document)
    :document-symbol
    (let [db @db*]
      (f.document-symbol/document-symbols db (:uri text-document)))))

(defn document-highlight [{:keys [db*] :as components} {:keys [text-document position]}]
  (process-after-changes
    components (:uri text-document)
    :document-highlight
    (let [db @db*
          [row col] (shared/position->row-col position)
          uri (:uri text-document)
          local-db (update db :analysis select-keys [uri])
          references (q/find-references-from-cursor local-db uri row col true)]
      (mapv (fn [reference]
              {:range (shared/->range reference)})
            references))))

(defn workspace-symbols [{:keys [db*]} {:keys [query]}]
  (shared/logging-task
    :workspace-symbol
    (f.workspace-symbols/workspace-symbols query @db*)))

(defn ^:private server-info [{:keys [db*]}]
  (let [db-value @db*]
    {:project-root-uri (:project-root-uri db-value)
     :project-settings (:project-settings db-value)
     :classpath-settings (:classpath-settings db-value)
     :classpath (:classpath db-value)
     :client-settings (:client-settings db-value)
     :final-settings (settings/all db-value)
     :cljfmt-raw (binding [*print-meta* true]
                   (pr-str (f.format/resolve-user-cljfmt-config db-value)))
     :port (or (:port db-value)
               "NREPL only available on :debug profile (`bb debug-cli`)")
     :server-version (shared/clojure-lsp-version)
     :clj-kondo-version (lsp.kondo/clj-kondo-version)
     :log-path (:log-path db-value)}))

(defn server-info-log [{:keys [producer] :as components}]
  (shared/logging-task
    :server-info-log
    (producer/show-message
      producer
      (with-out-str (pprint/pprint (server-info components)))
      :info
      nil)))

(defn server-info-raw [components]
  (shared/preserve-kebab-case (server-info components)))

(defn ^:private cursor-info [{:keys [db*]} [uri line character]]
  (let [db @db*
        elements (q/find-all-elements-under-cursor db uri (inc line) (inc character))]
    (shared/assoc-some {}
                       :elements (mapv (fn [e]
                                         (shared/assoc-some
                                           {:element e}
                                           :definition (q/find-definition db e)
                                           :semantic-tokens (f.semantic-tokens/element->token-type e)))
                                       elements))))

(defn cursor-info-log [{:keys [producer] :as components} {:keys [text-document position]}]
  (shared/logging-task
    :cursor-info-log
    (producer/show-message
      producer
      (with-out-str (pprint/pprint (cursor-info components
                                                [(:uri text-document) (:line position) (:character position)])))
      :info
      nil)))

(defn cursor-info-raw [components {:keys [text-document position]}]
  (shared/logging-task
    :cursor-info-raw
    (-> components
        (cursor-info [(:uri text-document) (:line position) (:character position)])
        (shared/preserve-kebab-case))))

(defn clojuredocs-raw [{:keys [db*]} {:keys [sym-name sym-ns]}]
  (shared/logging-task
    :clojuredocs-raw
    (f.clojuredocs/find-docs-for sym-name sym-ns db*)))

(defn ^:private refactor [{:keys [db*] :as components} refactoring [uri line character & args]]
  (let [db @db*
        row (inc (int line))
        col (inc (int character))
        ;; TODO Instead of v=0 should I send a change AND a document change
        v (get-in db [:documents uri :v] 0)
        loc (some-> (parser/zloc-of-file db uri)
                    (parser/to-pos row col))]
    (f.refactor/call-refactor {:refactoring (keyword refactoring)
                               :db          db
                               :loc         loc
                               :uri         uri
                               :row         row
                               :col         col
                               :args        args
                               :version     v}
                              components)))

(defn execute-command [{:keys [producer] :as components} {:keys [command arguments]}]
  (cond
    (= command "server-info")
    (server-info-log components)

    (= command "cursor-info")
    (cursor-info-log components
                     {:text-document (nth arguments 0)
                      :position {:line (nth arguments 1)
                                 :character (nth arguments 2)}})

    (some #(= % command) f.refactor/available-refactors)
    (shared/logging-task
      :execute-command
      ;; TODO move components upper to a common place
      (when-let [{:keys [edit show-document-after-edit error] :as result} (refactor components command arguments)]
        (if error
          result
          (do
            ;; waits for client to apply edit before showing doc/moving cursor
            (producer/publish-workspace-edit producer edit)
            (when show-document-after-edit
              (->> (update show-document-after-edit :range #(or (some-> % shared/->range)
                                                                shared/full-file-range))
                   (producer/show-document-request producer)))
            edit))))))

(defn hover [components {:keys [text-document position]}]
  (shared/logging-task
    :hover
    (let [[row col] (shared/position->row-col position)]
      (f.hover/hover (:uri text-document) row col components))))

(defn signature-help [components {:keys [text-document position _context]}]
  (shared/logging-task
    :signature-help
    (let [[row col] (shared/position->row-col position)]
      (f.signature-help/signature-help (:uri text-document) row col components))))

(defn formatting [components {:keys [text-document]}]
  (shared/logging-task
    :formatting
    (f.format/formatting (:uri text-document) components)))

(defn range-formatting [{:keys [db*] :as components} {:keys [text-document range]}]
  (process-after-changes
    components (:uri text-document)
    :range-formatting
    (let [db @db*
          [row col] (shared/position->row-col (:start range))
          [end-row end-col] (shared/position->row-col (:end range))
          format-pos {:row row
                      :col col
                      :end-row end-row
                      :end-col end-col}]
      (f.format/range-formatting (:uri text-document) format-pos db))))

(defn dependency-contents [{:keys [db* producer]} {:keys [uri]}]
  (shared/logging-task
    :dependency-contents
    (f.java-interop/read-content! uri @db* producer)))

(defn code-actions
  [{:keys [db*] :as components} {:keys [range context text-document]}]
  (process-after-changes
    components (:uri text-document)
    :code-actions
    (let [db @db*
          diagnostics (-> context :diagnostics)
          [row col] (shared/position->row-col (:start range))
          root-zloc (parser/safe-zloc-of-file db (:uri text-document))
          client-capabilities (get db :client-capabilities)]
      (f.code-actions/all root-zloc (:uri text-document) row col diagnostics client-capabilities db))))

(defn code-lens
  [{:keys [db*] :as components} {:keys [text-document]}]
  (process-after-changes
    components (:uri text-document)
    :code-lens
    (f.code-lens/reference-code-lens (:uri text-document) @db*)))

(defn code-lens-resolve
  [{:keys [db*]} {[uri row col] :data range :range}]
  (shared/logging-task
    :resolve-code-lens
    (f.code-lens/resolve-code-lens uri row col range @db*)))

(defn semantic-tokens-full
  [{:keys [db*] :as components} {:keys [text-document]}]
  (process-after-changes
    components (:uri text-document)
    :semantic-tokens-full
    (let [data (f.semantic-tokens/full-tokens (:uri text-document) @db*)]
      {:data data})))

(defn semantic-tokens-range
  [{:keys [db*] :as components} {:keys [text-document] {:keys [start end]} :range}]
  (process-after-changes
    components (:uri text-document)
    :semantic-tokens-range
    (let [db @db*
          [row col] (shared/position->row-col start)
          [end-row end-col] (shared/position->row-col end)
          range {:name-row row
                 :name-col col
                 :name-end-row end-row
                 :name-end-col end-col}
          data (f.semantic-tokens/range-tokens (:uri text-document) range db)]
      {:data data})))

(defn prepare-call-hierarchy
  [{:keys [db*]} {:keys [text-document position]}]
  (shared/logging-task
    :prepare-call-hierarchy
    (let [[row col] (shared/position->row-col position)]
      (f.call-hierarchy/prepare (:uri text-document) row col @db*))))

(defn call-hierarchy-incoming
  [components {{:keys [uri range]} :item}]
  (shared/logging-task
    :call-hierarchy-incoming-calls
    (let [[row col] (shared/position->row-col (:start range))]
      (f.call-hierarchy/incoming uri row col components))))

(defn call-hierarchy-outgoing
  [components {{:keys [uri range]} :item}]
  (shared/logging-task
    :call-hierarchy-outgoing-calls
    (let [[row col] (shared/position->row-col (:start range))]
      (f.call-hierarchy/outgoing uri row col components))))

(defn linked-editing-ranges
  [{:keys [db*]} {:keys [text-document position]}]
  (shared/logging-task
    :linked-editing-range
    (let [db @db*
          [row col] (shared/position->row-col position)]
      (f.linked-editing-range/ranges (:uri text-document) row col db))))

(defn will-rename-files [{:keys [db*] :as components} {:keys [files]}]
  (process-after-all-changes
    components (map :old-uri files)
    :will-rename-files
    (f.file-management/will-rename-files files @db*)))
