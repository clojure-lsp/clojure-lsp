(ns clojure-lsp.handlers
  (:require
    [cljfmt.core :as cljfmt]
    [clojure-lsp.code-lens :as f.code-lens]
    [clojure-lsp.config :as config]
    [clojure-lsp.crawler :as crawler]
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.call-hierarchy :as f.call-hierarchy]
    [clojure-lsp.feature.code-actions :as f.code-actions]
    [clojure-lsp.feature.completion :as f.completion]
    [clojure-lsp.feature.document-symbol :as f.document-symbol]
    [clojure-lsp.feature.file-management :as f.file-management]
    [clojure-lsp.feature.hover :as f.hover]
    [clojure-lsp.feature.refactor :as f.refactor]
    [clojure-lsp.feature.rename :as f.rename]
    [clojure-lsp.feature.resolve-macro :as f.resolve-macro]
    [clojure-lsp.feature.semantic-tokens :as f.semantic-tokens]
    [clojure-lsp.feature.signature-help :as f.signature-help]
    [clojure-lsp.feature.workspace-symbols :as f.workspace-symbols]
    [clojure-lsp.interop :as interop]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.producer :as producer]
    [clojure-lsp.queries :as q]
    [clojure-lsp.shared :as shared]
    [clojure.pprint :as pprint]
    [medley.core :as medley]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]
    [taoensso.timbre :as log])
  (:import
    [java.net
     URL
     JarURLConnection]))

(def ^:private full-file-range
  (shared/->range {:row 1 :col 1 :end-row 1000000 :end-col 1000000}))

(defmacro process-after-changes [& body]
  `(let [~'_time (System/nanoTime)]
     (loop []
       (if (> (quot (- (System/nanoTime) ~'_time) 1000000) 60000) ; one minute timeout
         (log/warn "Timeout waiting for changes for body")
         (if (:processing-changes @db/db)
           (recur)
           ~@body)))))

(defn initialize [project-root-uri client-capabilities client-settings]
  (when project-root-uri
    (crawler/initialize-project project-root-uri client-capabilities client-settings {})))

(defn did-open [{:keys [textDocument]}]
  (let [uri (:uri textDocument)
        text (:text textDocument)]
    (f.file-management/did-open uri text))
  nil)

(defn did-save [{:keys [textDocument]}]
  (swap! db/db #(assoc-in % [:documents textDocument :saved-on-disk] true)))

;; TODO wait for lsp4j release
#_(defn did-delete-files [{:keys [textDocument]}]
    (when (get-in @db/db [:documents textDocument :saved-on-disk])
      (swap! db/db #(update % :documents dissoc textDocument))))

(defn did-change [{:keys [textDocument contentChanges]}]
  (f.file-management/did-change (:uri textDocument) contentChanges (:version textDocument)))

(defn did-change-watched-files [changes]
  (let [uris (map :uri (filter (comp #{:deleted} :type) changes))]
    (swap! db/db (fn [db]
                   (-> db
                       (update :documents #(apply dissoc % uris))
                       (update :file-envs #(apply dissoc % uris)))))))

(defn completion [{:keys [textDocument position]}]
  (let [row (-> position :line inc)
        col (-> position :character inc)]
    (f.completion/completion textDocument row col)))

(defn references [{:keys [textDocument position context]}]
  (let [row (-> position :line inc)
        col (-> position :character inc)]
    (mapv (fn [reference]
            {:uri (shared/filename->uri (:filename reference))
             :range (shared/->range reference)})
          (q/find-references-from-cursor (:analysis @db/db) (shared/uri->filename textDocument) row col (:includeDeclaration context)))))

(defn completion-resolve-item [item]
  (f.completion/resolve-item item))

(defn rename [{:keys [textDocument position newName]}]
  (let [[row col] (shared/position->line-column position)]
    (f.rename/rename textDocument newName row col)))

(defn definition [{:keys [textDocument position]}]
  (let [[line column] (shared/position->line-column position)]
    (when-let [d (q/find-definition-from-cursor (:analysis @db/db) (shared/uri->filename textDocument) line column)]
      {:uri (shared/filename->uri (:filename d))
       :range (shared/->range d)})))

(defn document-symbol [{:keys [textDocument]}]
  (let [filename (shared/uri->filename textDocument)
        local-analysis (get-in @db/db [:analysis filename])
        namespace-definition (q/find-first (comp #{:namespace-definitions} :bucket) local-analysis)]
    [{:name (or (some-> namespace-definition :name name)
                filename)
      :kind (f.document-symbol/element->symbol-kind namespace-definition)
      :range full-file-range
      :selection-range (if namespace-definition
                         (shared/->scope-range namespace-definition)
                         full-file-range)
      :children (->> local-analysis
                     (filter (comp #{:var-definitions} :bucket))
                     (medley/distinct-by (juxt :filename :name :name-row :name-col))
                     (mapv (fn [e]
                             {:name            (-> e :name name)
                              :kind            (f.document-symbol/element->symbol-kind e)
                              :range           (shared/->scope-range e)
                              :selection-range (shared/->range e)})))}]))

(defn document-highlight [{:keys [textDocument position]}]
  (process-after-changes
    (let [line (-> position :line inc)
          column (-> position :character inc)
          filename (shared/uri->filename textDocument)
          scoped-analysis (select-keys (:analysis @db/db) [filename])
          references (q/find-references-from-cursor scoped-analysis filename line column true)]
      (mapv (fn [reference]
              {:range (shared/->range reference)})
            references))))

(defn workspace-symbols [{:keys [query]}]
  (f.workspace-symbols/workspace-symbols query))

(defn ^:private server-info []
  (let [db @db/db]
    {:project-root-uri (:project-root-uri db)
     :project-settings (:project-settings db)
     :client-settings (:client-settings db)
     :final-settings (:settings db)
     :port (or (:port db)
               "NREPL only available on :debug profile (`make debug-bin`)")
     :server-version config/clojure-lsp-version
     :clj-kondo-version config/clj-kondo-version
     :log-path (:log-path db)}))

(defn server-info-log []
  (producer/window-show-message
    (with-out-str (pprint/pprint (server-info)))
    :info))

(def server-info-raw server-info)

(defn ^:private cursor-info [[doc-id line character]]
  (let [analysis (:analysis @db/db)
        element (q/find-element-under-cursor analysis (shared/uri->filename doc-id) (inc line) (inc character))
        definition (when element (q/find-definition analysis element))]
    {:type    :info
     :message (with-out-str (pprint/pprint {:element element
                                            :definition definition}))}))

(defn cursor-info-log [{:keys [textDocument position]}]
  (producer/window-show-message (cursor-info [textDocument (:line position) (:character position)])))

(defn ^:private refactor [refactoring [doc-id line character args]]
  (let [row                        (inc (int line))
        col                        (inc (int character))
        ;; TODO Instead of v=0 should I send a change AND a document change
        {:keys [v text] :or {v 0}} (get-in @db/db [:documents doc-id])
        loc                        (parser/loc-at-pos text row col)]
    (f.refactor/call-refactor {:refactoring (keyword refactoring)
                               :loc         loc
                               :uri         doc-id
                               :row         row
                               :col         col
                               :args        args
                               :version     v})))

(defn execute-command [{:keys [command arguments]}]
  (cond
    (= command "server-info")
    (server-info-log)

    (= command "cursor-info")
    (cursor-info-log {:textDocument (nth arguments 0)
                      :position {:line (nth arguments 1)
                                 :character (nth arguments 2)}})

    (= command "resolve-macro-as")
    (apply f.resolve-macro/resolve-macro-as! arguments)

    (some #(= % command) f.refactor/available-refactors)
    (when-let [result (refactor command arguments)]
      (if (:client @db/db)
        (producer/workspace-apply-edit result)
        result))))

(defn hover [{:keys [textDocument position]}]
  (let [[line column] (shared/position->line-column position)
        filename (shared/uri->filename textDocument)]
    (f.hover/hover filename line column)))

(defn signature-help [{:keys [textDocument position _context]}]
  (let [[line column] (shared/position->line-column position)]
    (f.signature-help/signature-help textDocument line column)))

(defn formatting [{:keys [textDocument]}]
  (let [{:keys [text]} (get-in @db/db [:documents textDocument])
        new-text (cljfmt/reformat-string
                   text
                   (get-in @db/db [:settings :cljfmt]))]
    (if (= new-text text)
      []
      [{:range full-file-range
        :new-text new-text}])))

(defn range-formatting [doc-id format-pos]
  (process-after-changes
    (let [{:keys [text]} (get-in @db/db [:documents doc-id])
          cljfmt-settings (get-in @db/db [:settings :cljfmt])
          forms (parser/find-top-forms-in-range text format-pos)]
      (mapv (fn [form-loc]
              {:range (shared/->range (-> form-loc z/node meta))
               :new-text (n/string (cljfmt/reformat-form (z/node form-loc) cljfmt-settings))})
            forms))))

(defmulti extension (fn [method _] method))

(defmethod extension "dependencyContents"
  [_ doc]
  (let [doc-id (interop/java->clj doc)
        url (URL. doc-id)
        connection ^JarURLConnection (.openConnection url)
        jar (.getJarFile connection)
        entry (.getJarEntry connection)]
    (with-open [stream (.getInputStream jar entry)]
      (slurp stream))))

(defn code-actions
  [{:keys [range context textDocument]}]
  (process-after-changes
    (let [db @db/db
          diagnostics (-> context :diagnostics)
          line (-> range :start :line)
          character (-> range :start :character)
          row (inc line)
          col (inc character)
          zloc (parser/safe-cursor-loc textDocument line character)
          client-capabilities (get db :client-capabilities)]
      (f.code-actions/all zloc textDocument row col diagnostics client-capabilities))))

(defn resolve-code-action [{{:keys [uri line character]} :data :as action}]
  (let [zloc (parser/safe-cursor-loc uri line character)]
    (f.code-actions/resolve-code-action action zloc)))

(defn code-lens
  [{:keys [textDocument]}]
  (process-after-changes
    (f.code-lens/reference-code-lens textDocument)))

(defn code-lens-resolve
  [{[text-document row col] :data range :range}]
  (f.code-lens/resolve-code-lens text-document row col range))

(defn semantic-tokens-full
  [{:keys [textDocument]}]
  (process-after-changes
    (let [data (f.semantic-tokens/full-tokens textDocument)]
      {:data data})))

(defn semantic-tokens-range
  [{:keys [textDocument] {:keys [start end]} :range}]
  (process-after-changes
    (let [range {:name-row (inc (:line start))
                 :name-col (inc (:character start))
                 :name-end-row (inc (:line end))
                 :name-end-col (inc (:character end))}
          data (f.semantic-tokens/range-tokens textDocument range)]
      {:data data})))

(defn prepare-call-hierarchy
  [{:keys [textDocument position]}]
  (let [project-root-uri (:project-root-uri @db/db)]
    (f.call-hierarchy/prepare textDocument
                              (inc (:line position))
                              (inc (:character position))
                              project-root-uri)))

(defn call-hierarchy-incoming
  [{:keys [item]}]
  (let [uri (:uri item)
        row (inc (-> item :range :start :line))
        col (inc (-> item :range :start :character))
        project-root-uri (:project-root-uri @db/db)]
    (f.call-hierarchy/incoming uri row col project-root-uri)))

(defn call-hierarchy-outgoing
  [{:keys [item]}]
  (let [uri (:uri item)
        row (inc (-> item :range :start :line))
        col (inc (-> item :range :start :character))
        project-root-uri (:project-root-uri @db/db)]
    (f.call-hierarchy/outgoing uri row col project-root-uri)))
