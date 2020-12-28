(ns clojure-lsp.handlers
  (:require
    [cljfmt.core :as cljfmt]
    [cljfmt.main :as cljfmt.main]
    [clojure-lsp.clojure-core :as cc]
    [clojure-lsp.crawler :as crawler]
    [clojure-lsp.db :as db]
    [clojure-lsp.features.definition :as f.definition]
    [clojure-lsp.features.refactor :as f.refactor]
    [clojure-lsp.features.references :as f.references]
    [clojure-lsp.interop :as interop]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.refactor.transform :as r.transform]
    [clojure-lsp.features.semantic-tokens :as semantic-tokens]
    [clojure-lsp.shared :as shared]
    [clojure.core.async :as async]
    [clojure.pprint :as pprint]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z])
  (:import
    [java.net URL JarURLConnection]))

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

(defn- client-changes [changes]
  (if (get-in @db/db [:client-capabilities :workspace :workspace-edit :document-changes])
    {:document-changes changes}
    {:changes (into {} (map (fn [{:keys [text-document edits]}]
                              [(:uri text-document) edits])
                            changes))}))

(defn- drop-whitespace [n s]
  (if (> n (count s))
    s
    (let [fully-trimmed (string/triml s)
          dropped (subs s n)]
      (last (sort-by count [fully-trimmed dropped])))))

(defn- count-whitespace [s]
  (- (count s) (count (string/triml s))))

(def line-break "\n----\n")
(def opening-code "```clojure\n")
(def closing-code "\n```\n")

(defn- docstring->formatted-markdown [doc]
  (let [lines       (string/split-lines doc)
        other-lines (filter (comp not string/blank?) (rest lines))
        multi-line? (> (count other-lines) 0)]
    (str opening-code
         (if-not multi-line?
           doc
           (let [indentation      (apply min (map count-whitespace other-lines))
                 unindented-lines (cons (first lines)
                                        (map #(drop-whitespace indentation %) (rest lines)))]
             (string/join "\n" unindented-lines)))
         closing-code)))

(defn- generate-docs [content-format usage show-docs-arity-on-same-line?]
  (let [{:keys [sym signatures doc]} usage
        signatures (some->> signatures
                            (:strings)
                            (string/join "\n"))
        signatures (if (and show-docs-arity-on-same-line? signatures)
                     (-> signatures
                         (clojure.string/replace #"\n" ",")
                         (clojure.string/replace #"  +" " "))
                     signatures)]
    (case content-format
      "markdown" {:kind "markdown"
                  :value (cond-> (str opening-code sym " " (when show-docs-arity-on-same-line? signatures) closing-code)
                           (and (not show-docs-arity-on-same-line?) signatures) (str opening-code signatures closing-code)
                           (seq doc) (str line-break (docstring->formatted-markdown doc)))}
      ;; Default to plaintext
      (cond-> (str sym " " (when show-docs-arity-on-same-line? signatures) "\n")
        (and (not show-docs-arity-on-same-line?) signatures) (str signatures "\n")
        (seq doc) (str line-break doc "\n")))))

(defn did-open [uri text]
  (when-let [new-ns (and (string/blank? text)
                         (uri->namespace uri))]
    (when (get-in @db/db [:settings :auto-add-ns-to-new-files?] true)
      (let [new-text (format "(ns %s)" new-ns)
            changes [{:text-document {:version (get-in @db/db [:documents uri :v] 0) :uri uri}
                      :edits [{:range (shared/->range {:row 1 :end-row 1 :col 1 :end-col 1})
                               :new-text new-text}]}]]
        (async/put! db/edits-chan (client-changes changes)))))

  (when-let [references (crawler/safe-find-references uri text)]
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

(defn- matches-cursor? [cursor-value s]
  (when (and s (string/starts-with? s cursor-value))
    s))

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
                           (recur (dec try-column)))))
        {cursor-value :str cursor-file-type :file-type} cursor-usage
        [cursor-ns _] (if-let [idx (some-> cursor-value (string/index-of "/"))]
                        [(subs cursor-value 0 idx) (subs cursor-value (inc idx))]
                        [cursor-value nil])
        matches? (partial matches-cursor? cursor-value)
        namespaces-and-aliases (->> file-envs
                                    (mapcat val)
                                    (filter (fn [{:keys [file-type tags] :as _usage}]
                                              (and
                                                (= cursor-file-type file-type)
                                                (or
                                                  (set/subset? #{:public :ns} tags)
                                                  (:alias tags)))))
                                    (mapv (fn [{:keys [sym] alias-str :str alias-ns :ns :as _usage}]
                                            [alias-str {:label (name sym)
                                                        :detail (if alias-ns
                                                                  (str alias-ns)
                                                                  (name sym))
                                                        :alias-str alias-str
                                                        :alias-ns alias-ns}]))
                                    (distinct)
                                    (reduce (fn [m [k v]]
                                              (update m k (fnil conj []) v))
                                            {}))
        remotes-by-ns (->> (for [[_ usages] remote-envs
                                 usage usages
                                 :when (and (set/subset? #{:ns :public} (:tags usage))
                                            (= cursor-file-type (:file-type usage)))]
                             [(:sym usage) usages])
                           (into {}))]
    (when cursor-value
      (if (contains? (:tags cursor-usage) :refer)
        ;; If the cursor is within a :refer then the sole
        ;; completions should be symbols within the namespace
        ;; that's being referred
        (->> (get remotes-by-ns (:ns cursor-usage))
             (filter #(every? (:tags %) [:declare :public]))
             (filter (comp matches? :str))
             (map (fn [candidate]
                    {:label (:str candidate)
                     :detail (str (:sym candidate))
                     :data (str (:sym candidate))})))
        ;; Otherwise, completions could come from various sources
        (concat
          (->> local-env
               (filter (comp :declare :tags))
               (filter (comp matches? :str))
               (remove (fn [usage]
                         (when-let [scope-bounds (:scope-bounds usage)]
                           (not= :within (shared/check-bounds line column scope-bounds)))))
               (mapv (fn [{:keys [sym kind]}]
                       (cond-> {:label (name sym)
                                :data (str sym)}
                         kind (assoc :kind kind))))
               (sort-by :label))
          (->> namespaces-and-aliases
               (filter (comp matches? key))
               (mapcat val)
               (mapv (fn [{:keys [alias-str alias-ns] :as info}]
                       (let [require-edit (some-> cursor-loc
                                                  (r.transform/add-known-libspec (symbol alias-str) alias-ns)
                                                  (r.transform/result))]
                         (cond-> (dissoc info :alias-ns :alias-str)
                           require-edit (assoc :additional-text-edits (mapv #(update % :range shared/->range) require-edit))))))
               (sort-by :label))
          (->> (for [[alias-str matches] namespaces-and-aliases
                     :when (= alias-str cursor-ns)
                     {:keys [alias-ns]} matches
                     :let [usages (get remotes-by-ns alias-ns)]
                     usage usages
                     :when (and (get-in usage [:tags :public])
                                (not (get-in usage [:tags :ns]))
                                (= cursor-file-type (:file-type usage)))
                     :let [require-edit (some-> cursor-loc
                                                (r.transform/add-known-libspec (symbol alias-str) alias-ns)
                                                (r.transform/result))]]
                 (cond-> {:label (str alias-str "/" (name (:sym usage)))
                          :detail (name alias-ns)
                          :data (str (:sym usage))}
                   require-edit (assoc :additional-text-edits (mapv #(update % :range shared/->range) require-edit))))
               (sort-by :label))
          (->> cc/core-syms
               (filter (comp matches? str))
               (map (fn [sym] {:label (str sym)
                               :data (str "clojure.core/" sym)}))
               (sort-by :label))
          (when (or (contains? cursor-file-type :cljc)
                    (contains? cursor-file-type :cljs))
            (->> cc/cljs-syms
                 (filter (comp matches? str))
                 (map (fn [sym] {:label (str sym)
                                 :data (str "cljs.core/" sym)}))
                 (sort-by :label)))
          (when (or (contains? cursor-file-type :cljc)
                    (contains? cursor-file-type :clj))
            (->> cc/java-lang-syms
                 (filter (comp matches? str))
                 (map (fn [sym] {:label (str sym)
                                 :data (str "java.lang." sym)}))
                 (sort-by :label))))))))

(defn resolve-completion-item [label sym-wanted]
  (let [file-envs (:file-envs @db/db)
        usage (first
                (for [[_ usages] file-envs
                      {:keys [sym tags] :as usage} usages
                      :when (and (= (str sym) sym-wanted)
                                 (:declare tags))]
                  usage))
        [content-format] (get-in @db/db [:client-capabilities :text-document :completion :completion-item :documentation-format])
        show-docs-arity-on-same-line? (get-in @db/db [:settings :show-docs-arity-on-same-line?])]
    {:label label
     :data sym-wanted
     :documentation (generate-docs content-format usage show-docs-arity-on-same-line?)}))

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
      (when-let [references (crawler/safe-find-references uri text)]
        (when-not (compare-and-set! db/db state-db (-> state-db
                                                       (assoc-in [:documents uri] {:v version :text text})
                                                       (assoc-in [:file-envs uri] references)))
          (recur @db/db))))))

(defn- rename-alias [doc-id local-env cursor-usage cursor-name replacement]
  (for [{u-str :str :as usage} local-env
        :let [version (get-in @db/db [:documents doc-id :v] 0)
              [u-prefix u-ns u-name] (parser/ident-split u-str)
              alias? (= usage cursor-usage)]
        :when (and (#{"::" ""} u-prefix)
                   (or (= u-ns cursor-name) alias?))]
    {:range (shared/->range usage)
     :new-text (if alias? replacement (str u-prefix replacement "/" u-name))
     :text-document {:version version :uri doc-id}}))

(defn- rename-name [file-envs cursor-sym replacement]
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
            (client-changes (concat doc-changes
                                    [{:kind "rename"
                                      :old-uri doc-id
                                      :new-uri new-uri}])))
          (client-changes doc-changes))))))

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

(defn entry-kind->symbol-kind [k]
  (case k
    :module :namespace
    :function :function
    :declaration :variable
    :null))

(defn file-env-entry->document-symbol [[e kind]]
  (let [{n :str :keys [row col end-row end-col sym]} e
        symbol-kind (entry-kind->symbol-kind kind)
        r {:start {:line (dec row) :character (dec col)}
           :end {:line (dec end-row) :character (dec end-col)}}]
    {:name n
     :kind symbol-kind
     :range r
     :selection-range r
     :namespace (namespace sym)}))

(defn is-declaration? [e]
  (and (get-in e [:tags :declare])
       (or (get-in e [:tags :local])
         (get-in e [:tags :public]))))

(defn document-symbol [doc-id]
  (let [local-env (get-in @db/db [:file-envs doc-id])
        symbol-parent-map (->> local-env
                               (keep #(cond (:kind %) [% (:kind %)]
                                            (is-declaration? %) [% :declaration]
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
        symbol-kind (entry-kind->symbol-kind kind)
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
                                       (is-declaration? %) [% :declaration]
                                       :else nil))
                          (filter #(.contains (str (:sym (first %))) query))
                          (map (partial file-env-entry->workspace-symbol uri)))))
           (sort-by :name)))
    []))

(defn refactor [doc-id line column refactoring args]
  (let [;; TODO Instead of v=0 should I send a change AND a document change
        {:keys [v text] :or {v 0}} (get-in @db/db [:documents doc-id])
        loc (parser/loc-at-pos text line column)
        result (f.refactor/refactor {:refactoring (keyword refactoring)
                                       :loc loc
                                       :uri doc-id
                                       :row line
                                       :col column
                                       :args args})]
    (if (or loc
            (= "clean-ns" refactoring))
      (cond
        (map? result)
        (let [changes (vec (for [[doc-id sub-results] result]
                             {:text-document {:uri doc-id :version v}
                              :edits (mapv #(update % :range shared/->range) (r.transform/result sub-results))}))]
          (client-changes changes))

        (seq result)
        (let [changes [{:text-document {:uri doc-id :version v}
                        :edits (mapv #(update % :range shared/->range) (r.transform/result result))}]]
          (client-changes changes))

        (empty? result)
        (log/warn refactoring "made no changes" (z/string loc))

        :else
        (log/warn "Could not apply" refactoring "to form: " (z/string loc)))
      (log/warn "Could not find a form at this location. row" line "col" column "file" doc-id))))

(defn server-info []
  (let [db @db/db]
    {:type :info
     :message (with-out-str (pprint/pprint {:project-root (:project-root db)
                                            :project-settings (:project-settings db)
                                            :client-settings (:client-settings db)
                                            :port (:port db)}))}))

(defn hover [doc-id line column]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        cursor (f.references/find-under-cursor line column local-env (shared/uri->file-type doc-id))
        usage (first
                (for [[_ usages] file-envs
                      {:keys [sym tags] :as usage} usages
                      :when (and (= sym (:sym cursor))
                                 (:declare tags))]
                  usage))
        [content-format] (get-in @db/db [:client-capabilities :text-document :hover :content-format])
        show-docs-arity-on-same-line? (get-in @db/db [:settings :show-docs-arity-on-same-line?])
        docs (generate-docs content-format usage show-docs-arity-on-same-line?)]
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
  [doc-id diagnostics line character]
  (let [db @db/db
        row (inc (int line))
        col (inc (int character))
        has-unknow-ns? (some #(= (compare "unresolved-namespace" (some-> % .getCode .get)) 0) diagnostics)
        missing-ns (when has-unknow-ns?
                     (refactor doc-id row col "add-missing-libspec" []))
        zloc (-> db
                 (get-in [:documents doc-id])
                 :text
                 (parser/loc-at-pos row col))
        inside-function? (r.transform/inside-function? zloc)
        [_ {def-uri :uri
            definition :usage}] (f.definition/definition-usage doc-id row col)
        inline-symbol? (r.transform/inline-symbol? def-uri definition)
        workspace-edit-capability? (get-in db [:client-capabilities :workspace :workspace-edit])]
    (cond-> []

      (and has-unknow-ns? missing-ns)
      (conj {:title          "Add missing namespace"
             :kind           :quick-fix
             :preferred?     true
             :workspace-edit missing-ns})

      inline-symbol?
      (conj {:title   "Inline symbol"
             :kind    :refactor-inline
             :command {:title     "Inline symbol"
                       :command   "inline-symbol"
                       :arguments [doc-id line character]}})

      inside-function?
      (conj {:title   "Cycle privacy"
             :kind    :refactor-rewrite
             :command {:title     "Cycle privacy"
                       :command   "cycle-privacy"
                       :arguments [doc-id line character]}}
            {:title   "Extract function"
             :kind    :refactor-extract
             :command {:title     "Extract function"
                       :command   "extract-function"
                       :arguments [doc-id line character "new-function"]}})

      workspace-edit-capability?
      (conj {:title   "Clean namespace"
             :kind    :source-organize-imports
             :command {:title     "Clean namespace"
                       :command   "clean-ns"
                       :arguments [doc-id line character]}}))))
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
        data (semantic-tokens/full usages)]
    {:data data}))
