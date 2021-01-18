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
    [clojure-lsp.feature.references :as f.references]
    [clojure-lsp.feature.semantic-tokens :as f.semantic-tokens]
    [clojure-lsp.interop :as interop]
    [clojure-lsp.queries :as q]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.producer :as producer]
    [clojure-lsp.shared :as shared]
    [clojure.core.async :as async]
    [clojure.pprint :as pprint]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [medley.core :as medley]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]
    [trptcolin.versioneer.core :as version])
  (:import
   [java.net URL
             URLDecoder
             JarURLConnection]))

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

(defn ^:private source-path-from-uri [uri source-paths project-root]
  (->> source-paths
       (some (fn [source-path]
               (when (string/starts-with? (string/replace-first uri (shared/uri->filename project-root) "") (str "/" source-path))
                 source-path)))))

(defn ^:private namespace->uri [namespace project-root source-paths filename]
  (let [file-type (shared/uri->file-type filename)]
    (str project-root
         "/"
         (source-path-from-uri filename source-paths project-root)
         "/"
         (-> namespace
             (string/replace "." "/")
             (string/replace "-" "_"))
         "."
         (name file-type))))


(defn did-open [{:keys [textDocument]}]
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
                         (assoc-in [:documents uri] {:v 0 :text text})
                         (crawler/update-analysis uri (:analysis result))
                         (crawler/update-findings uri (:findings result)))))
      (f.diagnostic/notify uri result)))
  nil)

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
        col (-> position :character inc)
        {:keys [text]} (get-in @db/db [:documents textDocument])
        file-envs (:file-envs @db/db)
        local-env (get file-envs textDocument)
        remote-envs (dissoc file-envs textDocument)
        cursor-loc (try
                     (parser/loc-at-pos text row (dec col))
                     (catch Exception e
                       (log/error (.getMessage e))))
        cursor-usage (loop [try-column col]
                       (if-let [usage (f.references/find-under-cursor row try-column local-env (shared/uri->file-type textDocument))]
                         usage
                         (when (pos? try-column)
                           (recur (dec try-column)))))]
    (f.completion/completion textDocument row col file-envs remote-envs cursor-loc cursor-usage)))

(defn resolve-completion-item
  [{:keys [label data]}]
  (let [file-envs (:file-envs @db/db)]
    (f.completion/resolve-item label data file-envs)))

(defn references [{:keys [textDocument position context]}]
  (let [row (-> position :line inc)
        col (-> position :character inc)]
    (mapv (fn [reference]
            {:uri (shared/filename->uri (:filename reference))
             :range (shared/->range reference)})
          (q/find-references-from-cursor (:analysis @db/db) (shared/uri->filename textDocument) row col (:includeDeclaration context)))))

(defn did-close [{:keys [textDocument]}]
  (swap! db/db #(update % :documents dissoc textDocument)))

(defn ^:private rename-alias [doc-id local-env cursor-usage cursor-name replacement]
  (for [{u-str :str :as usage} local-env
        :let [version (get-in @db/db [:documents doc-id :v] 0)
              [u-prefix u-ns u-name] (parser/ident-split u-str)
              alias? (= usage cursor-usage)]
        :when (and (#{"::" ""} u-prefix)
                   (or (= u-ns cursor-name) alias?))]
    {:range (shared/->range usage)
     :new-text (if alias? replacement (str u-prefix replacement "/" u-name))
     :text-document {:version version :uri doc-id}}))

(defn ^:private rename-name [file-envs cursor-sym replacement]
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
        project-root (:project-root @db/db)
        filename (shared/uri->filename textDocument)
        references (q/find-references-from-cursor (:analysis @db/db) filename row col true)
        definition (first (filter (comp #{:locals :var-definitions :namespace-definitions} :bucket) references))
        source-paths (get-in @db/db [:settings :source-paths])
        can-rename? (or (not (seq source-paths))
                        (some #(string/starts-with? (string/replace-first (:filename definition) (shared/uri->filename project-root) "")
                                                    (str "/" %)) source-paths))]
    (when (and (seq references) can-rename?)
      (let [replacement (string/replace newName #".*/([^/]*)$" "$1")
            changes (mapv
                      (fn [r]
                        (let [name-start (- (:name-end-col r) (count (name (:name r))))
                              ref-doc-id (shared/filename->uri (:filename r))
                              version (get-in @db/db [:documents ref-doc-id :v] 0)]
                          {:range (shared/->range (assoc r :name-col name-start))
                           :new-text replacement
                           :text-document {:version version :uri ref-doc-id}}))
                      references)
            doc-changes (->> changes
                             (group-by :text-document)
                             (remove (comp empty? val))
                             (map (fn [[text-document edits]]
                                    {:text-document text-document
                                     :edits (mapv #(dissoc % :text-document) edits)})))]
        (if (and (= (:bucket definition) :namespace-definitions)
                 (get-in @db/db [:client-capabilities :workspace :workspace-edit :document-changes]))
          (let [new-uri (namespace->uri replacement project-root source-paths (:filename definition))]
            (swap! db/db #(-> %
                              (update :documents dissoc filename)
                              (update :analysis dissoc filename)))
            (f.refactor/client-changes (concat doc-changes
                                               [{:kind "rename"
                                                 :old-uri textDocument
                                                 :new-uri new-uri}])))
          (f.refactor/client-changes doc-changes)))))
  #_
  (let [file-envs (:file-envs @db/db)
        project-root (:project-root @db/db)
        local-env (get file-envs textDocument)
        file-type (shared/uri->file-type textDocument)
        source-path (source-path-from-uri textDocument)
        {cursor-sym :sym cursor-str :str tags :tags :as cursor-usage} (f.references/find-under-cursor row col local-env file-type)]
    (when (and cursor-usage (not (simple-keyword? cursor-sym)) (not (contains? tags :norename)))
      (let [[_ cursor-ns cursor-name] (parser/ident-split cursor-str)
            replacement (if cursor-ns
                          (string/replace newName (re-pattern (str "^:{0,2}" cursor-ns "/")) "")
                          (string/replace newName #"^:{0,2}" ""))
            changes (if (contains? tags :alias)
                      (rename-alias textDocument local-env cursor-usage cursor-name replacement)
                      (rename-name file-envs cursor-sym replacement))
            doc-changes (->> changes
                             (group-by :text-document)
                             (remove (comp empty? val))
                             (map (fn [[text-document edits]]
                                    {:text-document text-document
                                     :edits edits})))]
        (if (and (contains? tags :ns)
                 (not= (compare cursor-name replacement) 0)
                 (get-in @db/db [:client-capabilities :workspace :workspace-edit :document-changes]))
          (let [new-uri (namespace->uri replacement project-root source-path file-type)]
            (swap! db/db #(-> %
                              (update :documents dissoc textDocument)
                              (update :file-envs dissoc textDocument)))
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
  (let [local-analysis (get-in @db/db [:analysis (shared/uri->filename textDocument)])]
    ;; TODO what is children? why group by namespace before?
    (->> local-analysis
         (filter (every-pred (complement :private)
                             (comp #{:namespace-definitions} :bucket)))
         (mapv (fn [e]
                 {:name (:name e)
                  :kind :declaration
                  :range (shared/->range e)
                  :selection-range (shared/->range e)})))))

(defn document-highlight [{:keys [textDocument position]}]
  (let [line (-> position :line inc)
        column (-> position :character inc)
        filename (shared/uri->filename textDocument)
        scoped-analysis (select-keys (:analysis @db/db) [filename])
        references (q/find-references-from-cursor scoped-analysis filename line column true)]
    (mapv (fn [reference]
            {:range (shared/->range reference)})
          references)))

(defn file-env-entry->document-symbol [[e kind]]
  (let [{n :str :keys [row col end-row end-col sym]} e
        symbol-kind (f.document-symbol/entry-kind->symbol-kind kind)
        r {:start {:line (dec row) :character (dec col)}
           :end {:line (dec end-row) :character (dec end-col)}}]
    {:name n
     :kind symbol-kind
     :range r
     :selection-range r
     :namespace (namespace sym)}))

(defn file-env-entry->document-highlight [{:keys [row end-row col end-col]}]
  (let [r {:start {:line (dec row) :character (dec col)}
           :end {:line (dec end-row) :character (dec end-col)}}]
    {:range r}))

(defn file-env-entry->workspace-symbol [uri [e kind]]
  (let [{:keys [row col end-row end-col sym]} e
        symbol-kind (f.document-symbol/entry-kind->symbol-kind kind)
        r {:start {:line (dec row) :character (dec col)}
           :end {:line (dec end-row) :character (dec end-col)}}]
    {:name (str sym)
     :kind symbol-kind
     :location {:uri uri :range r}}))

(defn workspace-symbols [{:keys [query]}]
  (if (seq query)
    (let [file-envs (:file-envs @db/db)]
      (->> file-envs
           (mapcat (fn [[uri env]]
                     (->> env
                          (keep #(cond (:kind %) [% (:kind %)]
                                       (f.document-symbol/is-declaration? %) [% :declaration]
                                       :else nil))
                          (filter #(.contains (str (:sym (first %))) query))
                          (map (partial file-env-entry->workspace-symbol uri)))))
           (sort-by :name)))
    []))

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
      {:range (shared/->range definition)
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
      [{:range (shared/->range {:row 1 :col 1 :end-row 1000000 :end-col 1000000})
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
  (let [{doc-id :uri} (interop/java->clj doc)
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
        zloc (parser/cursor-zloc textDocument line character)
        client-capabilities (get db :client-capabilities)]
    (f.code-actions/all zloc textDocument row col diagnostics client-capabilities)))

(defn resolve-code-action [{{:keys [uri line character]} :data :as action}]
  (let [zloc (parser/cursor-zloc uri line character)]
    (f.code-actions/resolve-code-action action zloc)))

(defn code-lens
  [{:keys [textDocument]}]
  (let [analysis (get @db/db :analysis)]
    (->> (q/find-vars analysis (shared/uri->filename textDocument) true)
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
  (let [usages (get-in @db/db [:analysis (shared/uri->filename textDocument)])
        data (f.semantic-tokens/full-tokens usages)]
    {:data data}))

(defn semantic-tokens-range
  [{:keys [textDocument] {:keys [start end]} :range}]
  (let [usages (get-in @db/db [:analysis (shared/uri->filename textDocument)])
        range {:name-row (inc (:line start))
               :name-col (inc (:character start))
               :name-end-row (inc (:line end))
               :name-end-col (inc (:character end))}
        data (f.semantic-tokens/range-tokens usages range)]
    {:data data}))

(defn prepare-call-hierarchy
  [{:keys [textDocument position]}]
  (let [{:keys [project-root file-envs]} @db/db
        local-env (get file-envs textDocument)]
    (f.call-hierarchy/prepare textDocument
                              (inc (:line position))
                              (inc (:character position))
                              local-env
                              project-root)))

(defn call-hierarchy-incoming
  [{:keys [item]}]
  (let [uri (:uri item)
        row (inc (-> item :range :start :line))
        col (inc (-> item :range :start :character))
        project-root (:project-root @db/db)]
    (f.call-hierarchy/incoming uri row col project-root)))
