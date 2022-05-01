(ns clojure-lsp.handlers
  (:require
   [clojure-lsp.clojure-feature :as clojure-feature]
   [clojure-lsp.clojure-producer :as clojure-producer]
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
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
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.pprint :as pprint]
   [lsp4clj.protocols.feature-handler :as feature-handler]
   [lsp4clj.protocols.logger :as logger]
   [lsp4clj.protocols.producer :as producer]))

(set! *warn-on-reflection* true)

;; e.g. 2^0, 2^1, ..., up to 200ms
(def backoff-start 5)
(def backoff-mult 1.2)
(def backoff-max 200)
(comment
  (->> backoff-start
       (iterate #(min backoff-max (Math/ceil (* backoff-mult %))))
       (reductions +)
       (take 15)))

(defmacro process-after-changes [task-id uri & body]
  (let [waiting-start-sym (gensym "waiting-start-time")
        start-sym (gensym "start-time")
        backoff-sym (gensym "backoff")
        process-msg (str task-id " %s")
        wait-and-process-msg (str task-id " %s - waited %s")]
    `(let [~waiting-start-sym (System/nanoTime)]
       (loop [~backoff-sym backoff-start]
         (if (> (quot (- (System/nanoTime) ~waiting-start-sym) 1000000) 60000) ; one minute timeout
           ~(with-meta
              `(logger/warn (format "Timeout in %s waiting for changes to %s" ~task-id ~uri))
              (meta &form))
           (if (contains? (:processing-changes @db/db*) ~uri)
             (do
               (Thread/sleep ~backoff-sym)
               (recur (min backoff-max (* backoff-mult ~backoff-sym))))
             (let [~start-sym (System/nanoTime)
                   result# (do ~@body)]
               ~(with-meta
                  `(logger/info
                     (if (= backoff-start ~backoff-sym)
                       (format ~process-msg (shared/start-time->end-time-ms ~waiting-start-sym))
                       (format ~wait-and-process-msg
                               (shared/start-time->end-time-ms ~start-sym)
                               (shared/format-time-delta-ms ~waiting-start-sym ~start-sym))))
                  (meta &form))
               result#)))))))

(defn ^:private analyze-test-paths! [{:keys [db* producer]}]
  (let [db @db*
        project-files (into #{}
                            (comp
                              q/filter-project-analysis-xf
                              (map key))
                            (:analysis db))]
    (->> project-files
         (map #(shared/filename->uri % db))
         (clojure-producer/refresh-test-tree producer))))

(defn initialize
  [project-root-uri
   client-capabilities
   client-settings
   work-done-token
   {:keys [db* producer] :as components}]
  (shared/logging-task
    :initialize
    (swap! db* assoc :project-analysis-type :project-and-deps)
    (if project-root-uri
      (do
        (crawler/initialize-project
          project-root-uri
          client-capabilities
          client-settings
          {}
          work-done-token
          components)
        (let [db @db*]
          (when (settings/get db [:lint-project-files-after-startup?] true)
            (async/go
              (f.diagnostic/publish-all-diagnostics! (-> db :settings :source-paths) db)))
          (async/go
            (f.clojuredocs/refresh-cache! db*))
          (async/go
            (let [settings (:settings db)]
              (when (stubs/check-stubs? settings)
                (stubs/generate-and-analyze-stubs! settings db*))))
          (async/go
            (logger/info crawler/startup-logger-tag "Analyzing test paths for project root" project-root-uri)
            (analyze-test-paths! components))
          (when (settings/get db [:java] true)
            (async/go
              (f.java-interop/retrieve-jdk-source-and-analyze! db*)))))
      (producer/show-message producer "No project-root-uri was specified, some features may not work properly." :warning nil))))

(defn did-open [{:keys [textDocument]} {:keys [producer db*]}]
  (shared/logging-task
    :did-open
    (let [uri (:uri textDocument)
          text (:text textDocument)]
      (f.file-management/did-open uri text db* true)
      (clojure-producer/refresh-test-tree producer [uri])))
  nil)

(defn did-save [{:keys [textDocument]}]
  (shared/logging-task
    :did-save
    (f.file-management/did-save textDocument db/db*)))

;; TODO implement it, do we need to do anything?
#_(defn did-delete-files [{:keys [textDocument]}]
    (when (get-in @db/db [:documents textDocument :saved-on-disk])
      (swap! db/db #(update % :documents dissoc textDocument))))

(defn did-change [{:keys [textDocument contentChanges]}]
  (f.file-management/did-change (:uri textDocument) contentChanges (:version textDocument) db/db*))

(defn did-close [{:keys [textDocument]}]
  (shared/logging-task
    :did-close
    (f.file-management/did-close textDocument db/db*)))

(defn did-change-watched-files [{:keys [changes]}]
  (f.file-management/did-change-watched-files changes db/db*))

(defn completion [{:keys [textDocument position]}]
  (shared/logging-results
    (str :completion " %s - total items: %s")
    count
    (let [db @db/db*
          row (-> position :line inc)
          col (-> position :character inc)]
      (f.completion/completion textDocument row col db))))

(defn references [{:keys [textDocument position context]} {:keys [db*]}]
  (shared/logging-task
    :references
    (let [db @db*
          row (-> position :line inc)
          col (-> position :character inc)]
      (mapv (fn [reference]
              {:uri (-> (:filename reference)
                        (shared/filename->uri db)
                        (f.java-interop/uri->translated-uri db))
               :range (shared/->range reference)})
            (q/find-references-from-cursor (:analysis db) (shared/uri->filename textDocument) row col (:includeDeclaration context))))))

(defn completion-resolve-item [item {:keys [db*]}]
  (shared/logging-task
    :resolve-completion-item
    (f.completion/resolve-item item db*)))

(defn prepare-rename [{:keys [textDocument position]}]
  (shared/logging-task
    :prepare-rename
    (let [[row col] (shared/position->line-column position)]
      (f.rename/prepare-rename textDocument row col @db/db*))))

(defn rename [{:keys [textDocument position newName]}]
  (shared/logging-task
    :rename
    (let [[row col] (shared/position->line-column position)]
      (f.rename/rename-from-position textDocument newName row col db/db*))))

(defn definition [{:keys [textDocument position]} {:keys [db*]}]
  (shared/logging-task
    :definition
    (let [db @db*
          [line column] (shared/position->line-column position)]
      (when-let [definition (q/find-definition-from-cursor (:analysis db) (shared/uri->filename textDocument) line column)]
        {:uri (-> (:filename definition)
                  (shared/filename->uri db)
                  (f.java-interop/uri->translated-uri db))
         :range (shared/->range definition)}))))

(defn declaration [{:keys [textDocument position]} {:keys [db*]}]
  (shared/logging-task
    :declaration
    (let [db @db*
          [line column] (shared/position->line-column position)]
      (when-let [declaration (q/find-declaration-from-cursor (:analysis db) (shared/uri->filename textDocument) line column)]
        {:uri (-> (:filename declaration)
                  (shared/filename->uri db)
                  (f.java-interop/uri->translated-uri db))
         :range (shared/->range declaration)}))))

(defn implementation [{:keys [textDocument position]} {:keys [db*]}]
  (shared/logging-task
    :implementation
    (let [db @db*
          [row col] (shared/position->line-column position)]
      (mapv (fn [implementation]
              {:uri (-> (:filename implementation)
                        (shared/filename->uri db)
                        (f.java-interop/uri->translated-uri db))
               :range (shared/->range implementation)})
            (q/find-implementations-from-cursor (:analysis db) (shared/uri->filename textDocument) row col)))))

(defn document-symbol [{:keys [textDocument]}]
  (shared/logging-task
    :document-symbol
    (let [db @db/db*
          filename (shared/uri->filename textDocument)
          analysis (:analysis db)
          namespace-definition (->> (get analysis filename)
                                    (q/find-first (comp #{:namespace-definitions} :bucket)))]
      [{:name (or (some-> namespace-definition :name name)
                  filename)
        :kind (f.document-symbol/element->symbol-kind namespace-definition)
        :range shared/full-file-range
        :selection-range (if namespace-definition
                           (shared/->scope-range namespace-definition)
                           shared/full-file-range)
        :children (->> (q/find-var-definitions analysis filename true)
                       (mapv (fn [e]
                               (shared/assoc-some
                                 {:name (-> e :name name)
                                  :kind (f.document-symbol/element->symbol-kind e)
                                  :range (shared/->scope-range e)
                                  :selection-range (shared/->range e)
                                  :tags (cond-> []
                                          (:deprecated e) (conj 1))}
                                 :detail (when (:private e)
                                           "private")))))}])))

(defn document-highlight [{:keys [textDocument position]}]
  (process-after-changes
    :document-highlight textDocument
    (let [db @db/db*
          line (-> position :line inc)
          column (-> position :character inc)
          filename (shared/uri->filename textDocument)
          scoped-analysis (select-keys (:analysis db) [filename])
          references (q/find-references-from-cursor scoped-analysis filename line column true)]
      (mapv (fn [reference]
              {:range (shared/->range reference)})
            references))))

(defn workspace-symbols [{:keys [query]}]
  (shared/logging-task
    :workspace-symbol
    (f.workspace-symbols/workspace-symbols query @db/db*)))

(defn ^:private server-info []
  (let [db-value @db/db*]
    {:project-root-uri (:project-root-uri db-value)
     :project-settings (:project-settings db-value)
     :classpath-settings (:classpath-settings db-value)
     :classpath (:classpath db-value)
     :client-settings (:client-settings db-value)
     :final-settings (settings/all db-value)
     :cljfmt-raw (binding [*print-meta* true]
                   (with-out-str (pr (f.format/resolve-user-cljfmt-config db-value))))
     :port (or (:port db-value)
               "NREPL only available on :debug profile (`make debug-cli`)")
     :server-version (shared/clojure-lsp-version)
     :clj-kondo-version (lsp.kondo/clj-kondo-version)
     :log-path (:log-path db-value)}))

(defn server-info-log [{:keys [producer]}]
  (shared/logging-task
    :server-info-log
    (producer/show-message
      producer
      (with-out-str (pprint/pprint (server-info)))
      :info
      nil)))

(def server-info-raw #'server-info)

(defn ^:private cursor-info [[doc-id line character]]
  (let [db @db/db*
        analysis (:analysis db)
        elements (q/find-all-elements-under-cursor analysis (shared/uri->filename doc-id) (inc line) (inc character))]
    (shared/assoc-some {}
                       :elements (mapv (fn [e]
                                         (shared/assoc-some
                                           {:element e}
                                           :definition (q/find-definition analysis e)
                                           :semantic-tokens (f.semantic-tokens/element->token-type e)))
                                       elements))))

(defn cursor-info-log [{:keys [textDocument position]} {:keys [producer]}]
  (shared/logging-task
    :cursor-info-log
    (producer/show-message
      producer
      (with-out-str (pprint/pprint (cursor-info [textDocument (:line position) (:character position)])))
      :info
      nil)))

(defn cursor-info-raw [{:keys [textDocument position]}]
  (shared/logging-task
    :cursor-info-raw
    (cursor-info [textDocument (:line position) (:character position)])))

(defn clojuredocs-raw [{:keys [symName symNs]} {:keys [db*]}]
  (shared/logging-task
    :clojuredocs-raw
    (f.clojuredocs/find-docs-for symName symNs db*)))

(defn ^:private refactor [refactoring [doc-id line character & args] {:keys [db*] :as components}]
  (let [db @db*
        row (inc (int line))
        col (inc (int character))
        ;; TODO Instead of v=0 should I send a change AND a document change
        v (get-in db [:documents doc-id :v] 0)
        loc (some-> (parser/zloc-of-file db doc-id)
                    (parser/to-pos row col))]
    (f.refactor/call-refactor {:refactoring (keyword refactoring)
                               :db          db
                               :loc         loc
                               :uri         doc-id
                               :row         row
                               :col         col
                               :args        args
                               :version     v}
                              components)))

(defn execute-command [{:keys [command arguments]} {:keys [producer] :as components}]
  (cond
    (= command "server-info")
    (server-info-log components)

    (= command "cursor-info")
    (cursor-info-log {:textDocument (nth arguments 0)
                      :position {:line (nth arguments 1)
                                 :character (nth arguments 2)}}
                     components)

    (some #(= % command) f.refactor/available-refactors)
    (shared/logging-task
      :execute-command
      ;; TODO move components upper to a common place
      (when-let [{:keys [edit show-document-after-edit]} (refactor command arguments components)]
        ;; waits for client to apply edit before showing doc/moving cursor
        (producer/publish-workspace-edit producer edit)
        (when show-document-after-edit
          (->> (update show-document-after-edit :range #(or (some-> % shared/->range)
                                                            shared/full-file-range))
               (producer/show-document-request producer)))
        edit))))

(defn hover [{:keys [textDocument position]} {:keys [db*]}]
  (shared/logging-task
    :hover
    (let [[line column] (shared/position->line-column position)
          filename (shared/uri->filename textDocument)]
      (f.hover/hover filename line column db*))))

(defn signature-help [{:keys [textDocument position _context]}]
  (shared/logging-task
    :signature-help
    (let [[line column] (shared/position->line-column position)]
      (f.signature-help/signature-help textDocument line column db/db*))))

(defn formatting [{:keys [textDocument]}]
  (shared/logging-task
    :formatting
    (f.format/formatting textDocument @db/db*)))

(defn range-formatting [{:keys [textDocument range]}]
  (process-after-changes
    :range-formatting textDocument
    (let [db @db/db*
          start (:start range)
          end (:end range)
          format-pos {:row (inc (:line start))
                      :col (inc (:character start))
                      :end-row (inc (:line end))
                      :end-col (inc (:character end))}]
      (f.format/range-formatting textDocument format-pos db))))

(defn dependency-contents [doc-id {:keys [db*]}]
  (shared/logging-task
    :dependency-contents
    (f.java-interop/read-content! doc-id @db*)))

(defn code-actions
  [{:keys [range context textDocument]}]
  (process-after-changes
    :code-actions textDocument
    (let [db @db/db*
          diagnostics (-> context :diagnostics)
          line (-> range :start :line)
          character (-> range :start :character)
          row (inc line)
          col (inc character)
          root-zloc (parser/safe-zloc-of-file db textDocument)
          client-capabilities (get db :client-capabilities)]
      (f.code-actions/all root-zloc textDocument row col diagnostics client-capabilities db))))

(defn code-lens
  [{:keys [textDocument]}]
  (process-after-changes
    :code-lens textDocument
    (f.code-lens/reference-code-lens textDocument @db/db*)))

(defn code-lens-resolve
  [{[text-document row col] :data range :range}]
  (shared/logging-task
    :resolve-code-lens
    (f.code-lens/resolve-code-lens text-document row col range @db/db*)))

(defn semantic-tokens-full
  [{:keys [textDocument]}]
  (process-after-changes
    :semantic-tokens-full textDocument
    (let [data (f.semantic-tokens/full-tokens textDocument @db/db*)]
      {:data data})))

(defn semantic-tokens-range
  [{:keys [textDocument] {:keys [start end]} :range}]
  (process-after-changes
    :semantic-tokens-range textDocument
    (let [db @db/db*
          range {:name-row (inc (:line start))
                 :name-col (inc (:character start))
                 :name-end-row (inc (:line end))
                 :name-end-col (inc (:character end))}
          data (f.semantic-tokens/range-tokens textDocument range db)]
      {:data data})))

(defn prepare-call-hierarchy
  [{:keys [textDocument position]}]
  (shared/logging-task
    :prepare-call-hierarchy
    (f.call-hierarchy/prepare textDocument
                              (inc (:line position))
                              (inc (:character position)) db/db*)))

(defn call-hierarchy-incoming
  [{:keys [item]}]
  (shared/logging-task
    :call-hierarchy-incoming-calls
    (let [uri (:uri item)
          row (inc (-> item :range :start :line))
          col (inc (-> item :range :start :character))]
      (f.call-hierarchy/incoming uri row col db/db*))))

(defn call-hierarchy-outgoing
  [{:keys [item]}]
  (shared/logging-task
    :call-hierarchy-outgoing-calls
    (let [uri (:uri item)
          row (inc (-> item :range :start :line))
          col (inc (-> item :range :start :character))]
      (f.call-hierarchy/outgoing uri row col db/db*))))

(defn linked-editing-ranges
  [{:keys [textDocument position]}]
  (shared/logging-task
    :linked-editing-range
    (let [db @db/db*
          row (-> position :line inc)
          col (-> position :character inc)]
      (f.linked-editing-range/ranges textDocument row col db))))

(defn will-rename-files [{:keys [files]} {:keys [db*]}]
  (shared/logging-task
    :will-rename-files
    (f.file-management/will-rename-files files db*)))

(defrecord ClojureLSPFeatureHandler [components*]
  feature-handler/ILSPFeatureHandler
  (initialize [_ project-root-uri client-capabilities client-settings work-done-token]
    (initialize project-root-uri client-capabilities client-settings work-done-token @components*))
  (did-open [_ doc]
    (did-open doc @components*))
  (did-change [_ doc]
    (did-change doc))
  (did-save [_ doc]
    (did-save doc))
  (execute-command [_ doc]
    (execute-command doc @components*))
  (did-close [_ doc]
    (did-close doc))
  (did-change-watched-files [_ doc]
    (did-change-watched-files doc))
  (references [_ doc]
    (references doc @components*))
  (completion [_ doc]
    (completion doc))
  (completion-resolve-item [_ doc]
    (completion-resolve-item doc @components*))
  (prepare-rename [_ doc]
    (prepare-rename doc))
  (rename [_ doc]
    (rename doc))
  (hover [_ doc]
    (hover doc @components*))
  (signature-help [_ doc]
    (signature-help doc))
  (formatting [_ doc]
    (formatting doc))
  (code-actions [_ doc]
    (code-actions doc))
  (code-lens [_ doc]
    (code-lens doc))
  (code-lens-resolve [_ doc]
    (code-lens-resolve doc))
  (definition [_ doc]
    (definition doc @components*))
  (declaration [_ doc]
    (declaration doc @components*))
  (implementation [_ doc]
    (implementation doc @components*))
  (document-symbol [_ doc]
    (document-symbol doc))
  (document-highlight [_ doc]
    (document-highlight doc))
  (semantic-tokens-full [_ doc]
    (semantic-tokens-full doc))
  (semantic-tokens-range [_ doc]
    (semantic-tokens-range doc))
  (prepare-call-hierarchy [_ doc]
    (prepare-call-hierarchy doc))
  (call-hierarchy-incoming [_ doc]
    (call-hierarchy-incoming doc))
  (call-hierarchy-outgoing [_ doc]
    (call-hierarchy-outgoing doc))
  (linked-editing-ranges [_ doc]
    (linked-editing-ranges doc))
  (workspace-symbols [_ doc]
    (workspace-symbols doc))
  (will-rename-files [_ rename-files]
    (will-rename-files rename-files @components*))
  (range-formatting [_ doc]
    (range-formatting doc))
  ;; (did-delete-files [_ doc]
  ;;   (did-delete-files doc))
  clojure-feature/IClojureLSPFeature
  (cursor-info-log [_ doc]
    (cursor-info-log doc @components*))
  (cursor-info-raw [_ doc]
    (cursor-info-raw doc))
  (server-info-raw [_]
    (server-info-raw))
  (clojuredocs-raw [_ doc]
    (clojuredocs-raw doc @components*))
  (server-info-log [_]
    (server-info-log @components*))
  (dependency-contents [_ doc-id]
    (dependency-contents doc-id @components*)))
