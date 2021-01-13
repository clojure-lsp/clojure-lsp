(ns clojure-lsp.handlers
  (:require
    [cljfmt.core :as cljfmt]
    [cljfmt.main :as cljfmt.main]
    [clojure-lsp.crawler :as crawler]
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.call-hierarchy :as f.call-hierarchy]
    [clojure-lsp.feature.code-actions :as f.code-actions]
    [clojure-lsp.feature.completion :as f.completion]
    [clojure-lsp.feature.definition :as f.definition]
    [clojure-lsp.feature.document-symbol :as f.document-symbol]
    [clojure-lsp.feature.hover :as f.hover]
    [clojure-lsp.feature.refactor :as f.refactor]
    [clojure-lsp.feature.references :as f.references]
    [clojure-lsp.feature.semantic-tokens :as f.semantic-tokens]
    [clojure-lsp.interop :as interop]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.shared :as shared]
    [clojure.core.async :as async]
    [clojure.pprint :as pprint]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]
    [trptcolin.versioneer.core :as version])
  (:import
    [java.net URL
              JarURLConnection]))

(defn ^:private uri->namespace [uri]
  (let [project-root (:project-root @db/db)
        source-paths (get-in @db/db [:settings :source-paths])
        in-project? (string/starts-with? uri project-root)
        file-type (shared/uri->file-type uri)]
    (when (and in-project? (not= :unknown file-type))
      (->> source-paths
           (map #(str project-root "/" % "/"))
           (some (fn [source-path]
                   (when (string/starts-with? uri source-path)
                     (some-> uri
                             (subs 0 (dec (- (count uri) (count (name file-type)))))
                             (subs (count source-path))
                             (string/replace #"/" ".")
                             (string/replace #"_" "-")))))))))

(defn ^:private source-path-from-uri [uri]
  (let [project-root (:project-root @db/db)
        source-paths (get-in @db/db [:settings :source-paths])]
    (->> source-paths
         (some (fn [source-path]
                 (when (string/starts-with? uri (str project-root "/" source-path "/"))
                   source-path))))))

(defn ^:private namespace->uri [namespace project-root source-path file-type]
  (str project-root
       "/"
       source-path
       "/"
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

  (when-let [references (f.references/safe-find-references uri text)]
    (swap! db/db (fn [state-db]
                   (-> state-db
                       (assoc-in [:documents uri] {:v 0 :text text})
                       (assoc-in [:file-envs uri] references)))))
  text)

(defn initialize [project-root client-capabilities client-settings]
  (when project-root
    (let [project-settings (crawler/find-project-settings project-root)]
      (swap! db/db assoc
             :project-root project-root
             :project-settings project-settings
             :client-settings client-settings
             :settings (-> (merge client-settings project-settings)
                           (update :cljfmt cljfmt.main/merge-default-options))
             :client-capabilities client-capabilities))
    (let [file-envs (crawler/determine-dependencies project-root)]
      (swap! db/db assoc
             :file-envs file-envs
             :project-aliases (apply merge (map (comp :aliases val) file-envs))))))

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
  (mapv (fn [{:keys [uri usage]}]
          {:uri uri :range (shared/->range usage)})
        (f.references/reference-usages doc-id line column)))

(defn did-close [uri]
  (swap! db/db #(-> %
                    (update :documents dissoc uri))))

(defn did-change [uri text version]
  ;; Ensure we are only accepting newer changes
  (loop [state-db @db/db]
    (when (> version (get-in state-db [:documents uri :v] -1))
      (when-let [references (f.references/safe-find-references uri text)]
        (when-not (compare-and-set! db/db state-db (-> state-db
                                                       (assoc-in [:documents uri] {:v version :text text})
                                                       (assoc-in [:file-envs uri] references)))
          (recur @db/db))))))

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
  (let [[cursor {:keys [uri usage]}] (f.definition/definition-usage doc-id line column)]
    (log/info "Finding definition" doc-id "row" line "col" column "cursor" (:sym cursor))
    (if (:sym cursor)
      (if usage
        {:uri uri :range (shared/->range usage) :str (:str usage)}
        (log/warn "Could not find definition for element under cursor, I think your cursor is:" (pr-str (:str cursor)) "qualified as:" (pr-str (:sym cursor))))
      (let [file-envs (:file-envs @db/db)
            local-env (get file-envs doc-id)
            file-type (shared/uri->file-type doc-id)]
        (if-let [next-stuff (f.references/find-after-cursor line column local-env file-type)]
          (log/warn "Could not find element under cursor, next three known elements are:" (string/join ", " (map (comp pr-str :str) next-stuff)))
          (log/warn "Could not find element under cursor, there are no known elements after this position."))))))

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

(defn document-symbol [doc-id]
  (let [local-env (get-in @db/db [:file-envs doc-id])
        symbol-parent-map (->> local-env
                               (keep #(cond (:kind %) [% (:kind %)]
                                            (f.document-symbol/is-declaration? %) [% :declaration]
                                            :else nil))
                               (map file-env-entry->document-symbol)
                               (group-by :namespace))]
    (->> (symbol-parent-map nil)
         (map (fn [e]
                (if-let [children (symbol-parent-map (:name e))]
                  (assoc e :children children)
                  e)))
         (into []))))

(defn file-env-entry->document-highlight [{:keys [row end-row col end-col]}]
  (let [r {:start {:line (dec row) :character (dec col)}
           :end {:line (dec end-row) :character (dec end-col)}}]
    {:range r}))

(defn document-highlight [doc-id line column]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        cursor (f.references/find-under-cursor line column local-env (shared/uri->file-type doc-id))
        sym (:sym cursor)]
    (into [] (comp (filter #(= (:sym %) sym))
                   (map file-env-entry->document-highlight)) local-env)))

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
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        cursor (f.references/find-under-cursor line column local-env (shared/uri->file-type doc-id))
        [content-format] (get-in @db/db [:client-capabilities :text-document :hover :content-format])
        docs (f.hover/hover-documentation (-> cursor :sym str) file-envs)]
    (if cursor
      {:range (shared/->range cursor)
       :contents (if (= content-format "markdown")
                   docs
                   [docs])}
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
  [{:keys [range context textDocument]}]
  (let [db @db/db
        diagnostics (-> context :diagnostics)
        row (-> range :start :line inc)
        col (-> range :start :character inc)
        zloc (-> db
                 (get-in [:documents textDocument])
                 :text
                 (parser/loc-at-pos row col))
        client-capabilities (get db :client-capabilities)]
    (f.code-actions/all zloc textDocument row col diagnostics client-capabilities)))

(defn resolve-code-action [{{:keys [uri line character]} :data :as action}]
  (let [zloc (-> @db/db
                 (get-in [:documents uri])
                 :text
                 (parser/loc-at-pos (inc line) (inc character)))]
    (f.code-actions/resolve-code-action action zloc)))

(defn code-lens
  [doc-id]
  (let [db     @db/db
        usages (get-in db [:file-envs doc-id])]
    (->> usages
         (filter (fn [{:keys [tags]}]
                   (and (contains? tags :declare)
                        (not (contains? tags :alias))
                        (not (contains? tags :param)))))
         (map (fn [usage]
                {:range (shared/->range usage)
                 :data  [doc-id (:row usage) (:col usage)]})))))

(defn code-lens-resolve
  [range [doc-id row col]]
  {:range   range
   :command {:title   (-> doc-id
                          (f.references/reference-usages row col)
                          count
                          (str " references"))
             :command "code-lens-references"
             :arguments [doc-id row col]}})

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
