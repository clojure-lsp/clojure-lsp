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
    [java.net URL JarURLConnection]))

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

(defn ^:private source-path-from-uri [uri]
  (let [source-paths (get-in @db/db [:settings :source-paths])]
    (->> source-paths
         (some (fn [source-path]
                 (when (string/starts-with? uri source-path)
                   source-path))))))

(defn ^:private namespace->uri [namespace source-path file-type]
  (str source-path
       (-> namespace
           (string/replace "." "/")
           (string/replace "-" "_"))
       "."
       (name file-type)))

(defn did-open [uri text]
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
                       (crawler/update-analysis uri (:analysis result)))))
    (f.diagnostic/notify uri result))
  nil)

(defn did-change [uri text version]
  ;; Ensure we are only accepting newer changes
  (loop [state-db @db/db]
    (when (> version (get-in state-db [:documents uri :v] -1))
      (when-let [result (crawler/run-kondo-on-text! text uri)]

        (if (compare-and-set! db/db state-db (-> state-db
                                                 (assoc-in [:documents uri] {:v version :text text})
                                                 (crawler/update-analysis uri (:analysis result))))
          (f.diagnostic/notify uri result)
          (recur @db/db))))))

(defn initialize [project-root client-capabilities client-settings]
  (when project-root
    (crawler/initialize-project project-root client-capabilities client-settings)
    nil))

(defn completion [doc-id line column]
  (let [{:keys [text]} (get-in @db/db [:documents doc-id])
        file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        remote-envs (dissoc file-envs doc-id)
        cursor-loc (try
                     (parser/loc-at-pos text line (dec column))
                     (catch Exception e
                       (log/error (.getMessage e))))
        cursor-usage (loop [try-column column]
                       (if-let [usage (f.references/find-under-cursor line try-column local-env (shared/uri->file-type doc-id))]
                         usage
                         (when (pos? try-column)
                           (recur (dec try-column)))))]
    (f.completion/completion doc-id line column file-envs remote-envs cursor-loc cursor-usage)))

(defn resolve-completion-item [label sym-wanted]
  (let [file-envs (:file-envs @db/db)]
    (f.completion/resolve-item label sym-wanted file-envs)))

(defn references [doc-id line column]
  (mapv (fn [reference]
          {:uri (shared/filename->uri (:filename reference)) :range (shared/->range reference)})
        (q/find-references-from-cursor (:analysis @db/db) (shared/uri->filename doc-id) line column true)))

(defn did-close [uri]
  (swap! db/db #(update % :documents dissoc uri)))

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

(defn rename [doc-id line column new-name]
  (let [references (q/find-references-from-cursor (:analysis @db/db) (shared/uri->filename doc-id) line column true)
        definition (first (filter (comp #{:locals :var-definitions :namespace-definitions} :bucket) references))
        can-rename? (and (not= :namespace-definitions (:bucket definition))
                         (string/starts-with? (:filename definition) (get-in @db/db [:project-settings :source-paths])))]
    (when (and (seq references) can-rename?)
      (let [changes (mapv
                      (fn [r]
                        (let [name-start (- (:name-end-col r) (count (name (:name r))))
                              ref-doc-id (shared/filename->uri (:filename r))
                              version (get-in @db/db [:documents ref-doc-id :v] 0)]
                          {:range (shared/->range (assoc r :name-col name-start))
                           :new-text new-name
                           :text-document {:version version :uri ref-doc-id}}))
                      references)
            doc-changes (->> changes
                             (group-by :text-document)
                             (remove (comp empty? val))
                             (map (fn [[text-document edits]]
                                    {:text-document text-document
                                     :edits edits})))]
        ;; TODO rename documents for namespace rename
        (f.refactor/client-changes doc-changes))))
  #_
  (let [file-envs (:file-envs @db/db)
        project-root (:project-root @db/db)
        local-env (get file-envs doc-id)
        file-type (shared/uri->file-type doc-id)
        source-path (source-path-from-uri doc-id)
        {cursor-sym :sym cursor-str :str tags :tags :as cursor-usage} (f.references/find-under-cursor line column local-env file-type)]
    (when (and cursor-usage (not (simple-keyword? cursor-sym)) (not (contains? tags :norename)))
      (let [[_ cursor-ns cursor-name] (parser/ident-split cursor-str)
            replacement (if cursor-ns
                          (string/replace new-name (re-pattern (str "^:{0,2}" cursor-ns "/")) "")
                          (string/replace new-name #"^:{0,2}" ""))
            changes (if (contains? tags :alias)
                      (rename-alias doc-id local-env cursor-usage cursor-name replacement)
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
                              (update :documents dissoc doc-id)
                              (update :file-envs dissoc doc-id)))
            (f.refactor/client-changes (concat doc-changes
                                    [{:kind "rename"
                                      :old-uri doc-id
                                      :new-uri new-uri}])))
          (f.refactor/client-changes doc-changes))))))

(defn definition [doc-id line column]
  (when-let [d (q/find-definition-from-cursor (:analysis @db/db) (shared/uri->filename doc-id) line column)]
    {:uri (shared/filename->uri (:filename d)) :range (shared/->range d) :str (:str d)}))

(defn document-symbol [doc-id]
  (let [local-analysis (get-in @db/db [:analysis (shared/uri->filename doc-id)])]
    ;; TODO what is children? why group by namespace before?
    (->> local-analysis
         (filter (every-pred (complement :private)
                             (comp #{:namespace-definitions} :bucket)))
         (mapv (fn [e]
                 {:name (:name e)
                  :kind :declaration
                  :range (shared/->range e)
                  :select-range (shared/->range e)})))))

(defn document-highlight [doc-id line column]
  (let [filename (shared/uri->filename doc-id)
        scoped-analysis (select-keys (:analysis @db/db) [filename])
        references (q/find-references-from-cursor scoped-analysis filename line column true)]
    (mapv (fn [reference]
            {:range (shared/->range reference)})
          references)))

(defn file-env-entry->workspace-symbol [uri [e kind]]
  (let [{:keys [row col end-row end-col sym]} e
        symbol-kind (f.document-symbol/entry-kind->symbol-kind kind)
        r {:start {:line (dec row) :character (dec col)}
           :end {:line (dec end-row) :character (dec end-col)}}]
    {:name (str sym)
     :kind symbol-kind
     :location {:uri uri :range r}}))

(defn workspace-symbols [query]
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

(defn refactor [doc-id line column refactoring args]
  (let [;; TODO Instead of v=0 should I send a change AND a document change
        {:keys [v text] :or {v 0}} (get-in @db/db [:documents doc-id])
        loc (parser/loc-at-pos text line column)]
    (f.refactor/call-refactor {:refactoring (keyword refactoring)
                               :loc loc
                               :uri doc-id
                               :row line
                               :col column
                               :args args
                               :version v})))

(defn server-info []
  (let [db @db/db
        server-version (version/get-version "clojure-lsp" "clojure-lsp")]
    {:type :info
     :message (with-out-str (pprint/pprint {:project-root (:project-root db)
                                            :project-settings (:project-settings db)
                                            :client-settings (:client-settings db)
                                            :port (:port db)
                                            :version server-version}))}))

(defn hover [doc-id line column]
  (let [filename (shared/uri->filename doc-id)
        ana (:analysis @db/db)
        element (q/find-element-under-cursor ana filename line column)
        definition (when element (q/find-definition ana element))]
    (cond
      definition
      {:contents (f.hover/hover-documentation definition)}

      element
      {:contents (f.hover/hover-documentation element)}

      :else
      {:contents []})))

(defn formatting [doc-id]
  (let [{:keys [text]} (get-in @db/db [:documents doc-id])
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
  (let [{doc-id :uri} (interop/debeaner doc)
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
  [doc-id diagnostics line character]
  []
  (let [db @db/db
        row (inc (int line))
        col (inc (int character))
        zloc (-> db
                 (get-in [:documents doc-id])
                 :text
                 (parser/loc-at-pos row col))
        client-capabilities (get db :client-capabilities)]
    (f.code-actions/all zloc doc-id row col diagnostics client-capabilities)))

(defn code-lens
  [doc-id]
  (let [usages (get-in @db/db [:file-envs doc-id])]
    (->> usages
         (filter (fn [{:keys [tags]}]
                   (and (contains? tags :declare)
                        (not (contains? tags :alias))
                        (not (contains? tags :param)))))
         (map (fn [usage]
                {:range (shared/->range usage)
                 :data [doc-id (:row usage) (:col usage)]})))))

(defn code-lens-resolve
  [range [doc-id row col]]
  (let [references (q/find-references-from-cursor (:analysis @db/db) (shared/uri->filename doc-id) row col false)]
    {:range range
     :command {:title (-> references
                        count
                        (str " references"))
               :command "code-lens-references"
               :arguments [doc-id row col]}}))

(defn semantic-tokens-full
  [doc-id]
  (let [db @db/db
        usages (get-in db [:file-envs doc-id])
        data (f.semantic-tokens/full-tokens usages)]
    {:data data}))

(defn semantic-tokens-range
  [doc-id range]
  (let [db @db/db
        usages (get-in db [:file-envs doc-id])
        data (f.semantic-tokens/range-tokens usages range)]
    {:data data}))

(defn prepare-call-hierarchy
  [doc-id row col]
  (let [{:keys [project-root file-envs]} @db/db
        local-env (get file-envs doc-id)]
    (f.call-hierarchy/prepare doc-id row col local-env project-root)))

(defn call-hierarchy-incoming
  [item]
  (let [uri (.getUri item)
        row (inc (-> item .getRange .getStart .getLine))
        col (inc (-> item .getRange .getStart .getCharacter))
        project-root (:project-root @db/db)]
    (f.call-hierarchy/incoming uri row col project-root)))
