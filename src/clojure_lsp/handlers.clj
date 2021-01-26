(ns clojure-lsp.handlers
  (:require
    [cljfmt.core :as cljfmt]
    [clojure-lsp.crawler :as crawler]
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.call-hierarchy :as f.call-hierarchy]
    [clojure-lsp.feature.code-actions :as f.code-actions]
    [clojure-lsp.feature.completion :as f.completion]
    [clojure-lsp.feature.diagnostics :as f.diagnostic]
    [clojure-lsp.feature.document-symbol :as f.document-symbol]
    [clojure-lsp.feature.hover :as f.hover]
    [clojure-lsp.feature.refactor :as f.refactor]
    [clojure-lsp.feature.semantic-tokens :as f.semantic-tokens]
    [clojure-lsp.interop :as interop]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.producer :as producer]
    [clojure-lsp.queries :as q]
    [taoensso.timbre :as log]
    [clojure-lsp.shared :as shared]
    [clojure.core.async :as async]
    [clojure.pprint :as pprint]
    [clojure.set :as set]
    [clojure.string :as string]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]
    [trptcolin.versioneer.core :as version])
  (:import
   [java.net URL
             URLDecoder
             JarURLConnection]))

(def full-file-range
  (shared/->range {:row 1 :col 1 :end-row 1000000 :end-col 1000000}))

(defn ^:private uri->namespace [uri]
  (let [project-root (:project-root @db/db)
        source-paths (get-in @db/db [:settings :source-paths])
        in-project? (string/starts-with? uri project-root)
        file-type (shared/uri->file-type uri)
        filename (shared/uri->filename uri)]
    (when (and in-project? (not= :unknown file-type))
      (->> source-paths
           (some (fn [source-path]
                   (when (string/starts-with? filename source-path)
                     (some-> filename
                             (subs 0 (dec (- (count filename) (count (name file-type)))))
                             (subs (inc (count source-path)))
                             (string/replace #"/" ".")
                             (string/replace #"_" "-")))))))))

(defn ^:private namespace->uri [namespace source-paths filename]
  (let [file-type (shared/uri->file-type filename)]
    (shared/filename->uri
      (str (first (filter #(string/starts-with? filename %) source-paths))
           "/"
           (-> namespace
               (string/replace "." "/")
               (string/replace "-" "_"))
           "."
           (name file-type)))))

(defn did-open [{:keys [textDocument] :as params}]
  (log/info "params-parsed->" params)
  (let [uri (-> textDocument :uri URLDecoder/decode)
        text (:text textDocument)]
    (when-let [new-ns (and (string/blank? text)
                           (uri->namespace uri))]
      (when (get-in @db/db [:settings :auto-add-ns-to-new-files?] true)
        (let [new-text (format "(ns %s)" new-ns)
              changes [{:text-document {:version (get-in @db/db [:documents uri :v] 0) :uri uri}
                        :edits [{:range (shared/->range {:row 1 :end-row 1 :col 1 :end-col 1})
                                 :new-text new-text}]}]]
          (async/put! db/edits-chan (f.refactor/client-changes changes)))))
    (when-let [result (crawler/run-kondo-on-text! text uri)]
      (swap! db/db (fn [state-db]
                     (-> state-db
                         (assoc-in [:documents uri] {:v 0 :text text :saved-on-disk false})
                         (crawler/update-analysis uri (:analysis result))
                         (crawler/update-findings uri (:findings result)))))
      (f.diagnostic/notify uri result)))
  nil)

(defn did-save [{:keys [textDocument]}]
  (swap! db/db #(assoc-in % [:documents textDocument :saved-on-disk] true)))

;; TODO wait for lsp4j release
#_(defn did-delete-files [{:keys [textDocument]}]
  (when (get-in @db/db [:documents textDocument :saved-on-disk])
    (swap! db/db #(update % :documents dissoc textDocument))))

(defn did-change [uri text version]
  ;; Ensure we are only accepting newer changes
  (loop [state-db @db/db]
    (when (> version (get-in state-db [:documents uri :v] -1))
      (when-let [result (crawler/run-kondo-on-text! text uri)]

        (if (compare-and-set! db/db state-db (-> state-db
                                                 (assoc-in [:documents uri] {:v version :text text})
                                                 (crawler/update-analysis uri (:analysis result))
                                                 (crawler/update-findings uri (:findings result))))
          (f.diagnostic/notify uri result)
          (recur @db/db))))))

(defn initialize [project-root client-capabilities client-settings]
  (when project-root
    (crawler/initialize-project project-root client-capabilities client-settings)
    nil))

(defn completion [{:keys [textDocument position]}]
  (let [row (-> position :line inc)
        col (-> position :character inc)]
    (f.completion/completion textDocument row col)))

(defn resolve-completion-item [item]
  (f.completion/resolve-item item))

(defn references [{:keys [textDocument position context]}]
  (let [row (-> position :line inc)
        col (-> position :character inc)]
    (mapv (fn [reference]
            {:uri (shared/filename->uri (:filename reference))
             :range (shared/->range reference)})
          (q/find-references-from-cursor (:analysis @db/db) (shared/uri->filename textDocument) row col (:includeDeclaration context)))))

(defn ^:private rename-alias [references replacement]
  (mapv
    (fn [element]
      (let [alias? (= :namespace-alias (:bucket element))
            ref-doc-id (shared/filename->uri (:filename element))
            [u-prefix _ u-name] (when-not alias?
                                  (parser/ident-split (:name element)))
            version (get-in @db/db [:documents ref-doc-id :v] 0)]
        {:range (shared/->range element)
         :new-text (if alias? replacement (str u-prefix replacement "/" u-name))
         :text-document {:version version :uri ref-doc-id}}))
    references))

;; TODO use after kondo supports keyword analysis
#_(defn ^:private rename-name [file-envs cursor-sym replacement]
  (for [[doc-id usages] file-envs
        :let [version (get-in @db/db [:documents doc-id :v] 0)]
        {u-sym :sym u-str :str :as usage} usages
        :when (= u-sym cursor-sym)
        :let [[u-prefix u-ns _] (parser/ident-split u-str)]]
    {:range (shared/->range usage)
     :new-text (str u-prefix u-ns (when u-ns "/") replacement)
     :text-document {:version version :uri doc-id}}))

(defn rename [{:keys [textDocument position newName]}]
  (let [[row col] (shared/position->line-column position)
        filename (shared/uri->filename textDocument)
        references (q/find-references-from-cursor (:analysis @db/db) filename row col true)
        definition (first (filter (comp #{:locals :var-definitions :namespace-definitions :namespace-alias} :bucket) references))
        source-paths (get-in @db/db [:settings :source-paths])
        can-rename? (or (not (seq source-paths))
                        (some #(string/starts-with? (:filename definition) %) source-paths))]
    (when (and (seq references) can-rename?)
      (let [replacement (string/replace newName #".*/([^/]*)$" "$1")
            changes (if (= :namespace-alias (:bucket definition))
                      (rename-alias references replacement)
                      (mapv
                        (fn [r]
                          (let [name-start (- (:name-end-col r) (count (name (:name r))))
                                ref-doc-id (shared/filename->uri (:filename r))
                                version (get-in @db/db [:documents ref-doc-id :v] 0)]
                            {:range (shared/->range (assoc r :name-col name-start))
                             :new-text replacement
                             :text-document {:version version :uri ref-doc-id}}))
                        references))
            doc-changes (->> changes
                             (group-by :text-document)
                             (remove (comp empty? val))
                             (map (fn [[text-document edits]]
                                    {:text-document text-document
                                     :edits (mapv #(dissoc % :text-document) edits)})))]
        (if (and (= (:bucket definition) :namespace-definitions)
                 (get-in @db/db [:client-capabilities :workspace :workspace-edit :document-changes]))
          (let [new-uri (namespace->uri replacement source-paths (:filename definition))]
            (swap! db/db (fn [db] (-> db
                                      (update :documents #(set/rename-keys % {filename (shared/uri->filename new-uri)}))
                                      (update :analysis #(set/rename-keys % {filename (shared/uri->filename new-uri)})))))
            (f.refactor/client-changes (concat doc-changes
                                               [{:kind "rename"
                                                 :old-uri textDocument
                                                 :new-uri new-uri}])))
          (f.refactor/client-changes doc-changes))))))

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
                     (mapv (fn [e]
                             {:name            (-> e :name name)
                              :kind            (f.document-symbol/element->symbol-kind e)
                              :range           (shared/->scope-range e)
                              :selection-range (shared/->range e)})))}]))

(defn document-highlight [{:keys [textDocument position]}]
  (let [line (-> position :line inc)
        column (-> position :character inc)
        filename (shared/uri->filename textDocument)
        scoped-analysis (select-keys (:analysis @db/db) [filename])
        references (q/find-references-from-cursor scoped-analysis filename line column true)]
    (mapv (fn [reference]
            {:range (shared/->range reference)})
          references)))

(defn workspace-symbols [{:keys [_query]}]
  (->> (get-in @db/db [:analysis])
       vals
       flatten
       (filter #(and (string/starts-with? (shared/filename->uri (:filename %)) "file://")
                     (f.document-symbol/declaration? %)))
       (mapv (fn [element]
               {:name (-> element :name name)
                :kind (f.document-symbol/element->symbol-kind element)
                :location {:uri (shared/filename->uri (:filename element))
                           :range (shared/->scope-range element)}}))))

(defn server-info []
  (let [db @db/db
        server-version (version/get-version "clojure-lsp" "clojure-lsp")]
    {:type :info
     :message (with-out-str (pprint/pprint {:project-root (:project-root db)
                                            :project-settings (:project-settings db)
                                            :client-settings (:client-settings db)
                                            :port (:port db)
                                            :version server-version}))}))

(defn ^:private cursor-info [[doc-id line character]]
  (let [analysis (:analysis @db/db)
        element (q/find-element-under-cursor analysis (shared/uri->filename doc-id) (inc line) (inc character))
        definition (when element (q/find-definition analysis element))]
    {:type    :info
     :message (with-out-str (pprint/pprint {:element element
                                            :definition definition}))}))

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
    (producer/window-show-message (server-info))

    (= command "cursor-info")
    (producer/window-show-message (cursor-info arguments))

    (some #(= % command) f.refactor/available-refactors)
    (when-let [result (refactor command arguments)]
      (producer/workspace-apply-edit result))))

(defn hover [{:keys [textDocument position]}]
  (let [[line column] (shared/position->line-column position)
        filename (shared/uri->filename textDocument)
        analysis (:analysis @db/db)
        element (q/find-element-under-cursor analysis filename line column)
        definition (when element (q/find-definition analysis element))]
    (cond
      definition
      {:range (shared/->range element)
       :contents (f.hover/hover-documentation definition)}

      element
      {:range (shared/->range element)
       :contents (f.hover/hover-documentation element)}

      :else
      {:contents []})))

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
  (let [{:keys [text]} (get-in @db/db [:documents doc-id])
        forms (parser/find-top-forms-in-range text format-pos)]
    (mapv (fn [form-loc]
            {:range (shared/->range (-> form-loc z/node meta))
             :new-text (n/string
                         (cljfmt/reformat-form
                           (z/node form-loc)
                           (get-in @db/db [:settings :cljfmt])))})
          forms)))

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

(defn did-change-watched-files [changes]
  (let [uris (map :uri (filter (comp #{:deleted} :type) changes))]
    (swap! db/db (fn [db]
                   (-> db
                       (update :documents #(apply dissoc % uris))
                       (update :file-envs #(apply dissoc % uris)))))))

(defn code-actions
  [{:keys [range context textDocument]}]
  (let [db @db/db
        diagnostics (-> context :diagnostics)
        line (-> range :start :line)
        character (-> range :start :character)
        row (inc line)
        col (inc character)
        zloc (parser/safe-cursor-loc textDocument line character)
        client-capabilities (get db :client-capabilities)]
    (f.code-actions/all zloc textDocument row col diagnostics client-capabilities)))

(defn resolve-code-action [{{:keys [uri line character]} :data :as action}]
  (let [zloc (parser/safe-cursor-loc uri line character)]
    (f.code-actions/resolve-code-action action zloc)))

(defn code-lens
  [{:keys [textDocument]}]
  (let [analysis (get @db/db :analysis)]
    (->> (q/find-vars analysis (shared/uri->filename textDocument) true)
         (filter #(not= 'clojure.test/deftest (:defined-by %)))
         (map (fn [var]
                {:range (shared/->range var)
                 :data  [textDocument (:name-row var) (:name-col var)]})))))

(defn code-lens-resolve
  [{[text-document row col] :data range :range}]
  (let [references (q/find-references-from-cursor (:analysis @db/db) (shared/uri->filename text-document) row col false)]
    {:range range
     :command {:title (-> references count (str " references"))
               :command "code-lens-references"
               :arguments [text-document row col]}}))

(defn semantic-tokens-full
  [{:keys [textDocument]}]
  (let [data (f.semantic-tokens/full-tokens textDocument)]
    {:data data}))

(defn semantic-tokens-range
  [{:keys [textDocument] {:keys [start end]} :range}]
  (let [range {:name-row (inc (:line start))
               :name-col (inc (:character start))
               :name-end-row (inc (:line end))
               :name-end-col (inc (:character end))}
        data (f.semantic-tokens/range-tokens textDocument range)]
    {:data data}))

(defn prepare-call-hierarchy
  [{:keys [textDocument position]}]
  (let [project-root (:project-root @db/db)]
    (f.call-hierarchy/prepare textDocument
                              (inc (:line position))
                              (inc (:character position))
                              project-root)))

(defn call-hierarchy-incoming
  [{:keys [item]}]
  (let [uri (:uri item)
        row (inc (-> item :range :start :line))
        col (inc (-> item :range :start :character))
        project-root (:project-root @db/db)]
    (f.call-hierarchy/incoming uri row col project-root)))
