(ns clojure-lsp.handlers
  (:require
    [cljfmt.core :as cljfmt]
    [clojure-lsp.clojure-core :as cc]
    [clojure-lsp.crawler :as crawler]
    [clojure-lsp.db :as db]
    [clojure-lsp.interop :as interop]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.refactor.transform :as refactor]
    [clojure-lsp.shared :as shared]
    [clojure.core.async :as async]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z])
  (:import
    [java.net URI URL JarURLConnection]
    [java.util.jar JarFile]
    [java.nio.file Paths]))

(defn check-bounds [line column {:keys [row end-row col end-col] :as _usage}]
  (cond
    (< line row) :before
    (and (= line row) (< column col)) :before
    (< line end-row) :within
    (and (= end-row line) (>= end-col column)) :within
    :else :after))

(defn- find-references-after-cursor [line column env file-type]
  (let [file-types (if (= :cljc file-type)
                     #{:clj :cljs}
                     #{file-type})]
    (->> env
         (filter (comp file-types :file-type))
         (partition-all 3 1)
         (filter (comp #{:within :before} (partial check-bounds line column) first))
         first)))

(defn find-reference-under-cursor [line column env file-type]
  (let [file-types (if (= :cljc file-type)
                     #{:clj :cljs}
                     #{file-type})]
    (->> env
         (filter (comp file-types :file-type))
         (filter (comp #{:within} (partial check-bounds line column)))
         first)))

(defn ^:private uri->namespace [uri]
  (let [project-root (:project-root @db/db)
        source-paths (get-in @db/db [:client-settings "source-paths"])
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
(defn- client-changes [changes]
  (if (get-in @db/db [:client-capabilities :workspace :workspace-edit :document-changes])
    {:document-changes changes}
    {:changes (into {} (map (fn [{:keys [text-document edits]}]
                              [(:uri text-document) edits])
                            changes))}))

(defn did-open [uri text]
  (when-let [new-ns (and (string/blank? text) (uri->namespace uri))]
    (let [new-text (format "(ns %s)" new-ns)
          changes [{:text-document {:version (get-in @db/db [:documents uri :v] 0) :uri uri}
                    :edits [{:range (shared/->range {:row 1 :end-row 1 :col 1 :end-col 1})
                             :new-text new-text}]}]]
      (async/put! db/edits-chan (client-changes changes))))

  (when-let [references (crawler/safe-find-references uri text)]
    (swap! db/db (fn [state-db]
                   (-> state-db
                       (assoc-in [:documents uri] {:v 0 :text text})
                       (assoc-in [:file-envs uri] references)))))
  text)

(defn initialize [project-root client-capabilities client-settings]
  (when project-root
    (swap! db/db assoc
           :project-root project-root
           :client-settings client-settings
           :client-capabilities client-capabilities)
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
                       (if-let [usage (find-reference-under-cursor line try-column local-env (shared/uri->file-type doc-id))]
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
      (concat
        (->> local-env
             (filter (comp :declare :tags))
             (filter (comp matches? :str))
             (remove (fn [usage]
                       (when-let [scope-bounds (:scope-bounds usage)]
                         (not= :within (check-bounds line column scope-bounds)))))
             (mapv (fn [{:keys [sym kind]}]
                     (cond-> {:label (name sym)}
                       kind (assoc :kind kind))))
             (sort-by :label))
        (->> namespaces-and-aliases
             (filter (comp matches? key))
             (mapcat val)
             (mapv (fn [{:keys [alias-str alias-ns] :as info}]
                     (let [require-edit (some-> cursor-loc
                                                (refactor/add-known-libspec (symbol alias-str) alias-ns)
                                                (refactor/result))]
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
                                              (refactor/add-known-libspec (symbol alias-str) alias-ns)
                                              (refactor/result))]]
               (cond-> {:label (str alias-str "/" (name (:sym usage)))
                        :detail (name alias-ns)}
                 require-edit (assoc :additional-text-edits (mapv #(update % :range shared/->range) require-edit))))
             (sort-by :label))
        (->> cc/core-syms
             (filter (comp matches? str))
             (map (fn [sym] {:label (str sym)}))
             (sort-by :label))
        (when (contains? #{:cljc :cljs} cursor-file-type)
          (->> cc/cljs-syms
               (filter (comp matches? str))
               (map (fn [sym] {:label (str sym)}))
               (sort-by :label)))
        (when (contains? #{:cljc :clj} cursor-file-type)
          (->> cc/java-lang-syms
               (filter (comp matches? str))
               (map (fn [sym] {:label (str sym)}))
               (sort-by :label)))))))

(defn references [doc-id line column]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        cursor-sym (:sym (find-reference-under-cursor line column local-env (shared/uri->file-type doc-id)))]
    (log/warn "references" doc-id line column cursor-sym)
    (into []
          (for [[uri usages] (:file-envs @db/db)
                {:keys [sym] :as usage} usages
                :when (= sym cursor-sym)]
            {:uri uri
             :range (shared/->range usage)}))))

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
     :text-document {:version version :uri doc-id}}) )

(defn rename [doc-id line column new-name]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        {cursor-sym :sym cursor-str :str tags :tags :as cursor-usage} (find-reference-under-cursor line column local-env (shared/uri->file-type doc-id))]
    (when-not (contains? tags :norename)
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
        (client-changes doc-changes)))))

(defn definition [doc-id line column]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        file-type (shared/uri->file-type doc-id)
        cursor (find-reference-under-cursor line column local-env file-type)
        cursor-sym (:sym cursor)]
    (log/warn "Finding definition" doc-id "row" line "col" column "cursor" cursor-sym)
    (if cursor-sym
      (if-let [result (first
                        (for [[env-doc-id usages] file-envs
                              {:keys [sym tags] :as usage} usages
                              :when (= sym cursor-sym)
                              :when (and (or (= doc-id env-doc-id) (:public tags))
                                      (:declare tags))]
                          {:uri env-doc-id :range (shared/->range usage)}))]
        result
        (log/warn "Could not find definition for element under cursor, I think your cursor is:" (pr-str (:str cursor)) "qualified as:" (pr-str cursor-sym)))
      (if-let [next-stuff (find-references-after-cursor line column local-env file-type)]
        (log/warn "Could not find element under cursor, next three known elements are:" (string/join ", " (map (comp pr-str :str) next-stuff)))
        (log/warn "Could not find element under cursor, there are no known elements after this position.")))))

(def refactorings
  {"cycle-coll" #'refactor/cycle-coll
   "thread-first" #'refactor/thread-first
   "thread-first-all" #'refactor/thread-first-all
   "thread-last" #'refactor/thread-last
   "thread-last-all" #'refactor/thread-last-all
   "move-to-let" #'refactor/move-to-let
   "introduce-let" #'refactor/introduce-let
   "expand-let" #'refactor/expand-let
   "clean-ns" #'refactor/clean-ns
   "add-missing-libspec" #'refactor/add-missing-libspec})

(defn refactor [doc-id line column refactoring args]
  (let [;; TODO Instead of v=0 should I send a change AND a document change
        {:keys [v text] :or {v 0}} (get-in @db/db [:documents doc-id])
        loc (parser/loc-at-pos text line column)
        result (apply (get refactorings refactoring) loc doc-id args)]
    (if loc
      (cond
        (seq result)
        (let [changes [{:text-document {:uri doc-id :version v}
                        :edits (mapv #(update % :range shared/->range) (refactor/result result))}]]
          (client-changes changes))

        (empty? result)
        (log/warn refactoring "made no changes" (z/string loc))

        :else
        (log/warn "Could not apply" refactoring "to form: " (z/string loc)))
      (log/warn "Could not find a form at this location. row" line "col" column "file" doc-id))))

(defn hover [doc-id line column]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        cursor (find-reference-under-cursor line column local-env (shared/uri->file-type doc-id))
        {:keys [signatures doc]} (first
                                   (for [[_ usages] file-envs
                                         {:keys [sym tags] :as usage} usages
                                         :when (and (= sym (:sym cursor))
                                                    (:declare tags))]
                                     usage))
        [content-format] (get-in @db/db [:client-capabilities :text-document :hover :content-format])]
    (if cursor
      {:range (shared/->range cursor)
       :contents (case content-format
                   "markdown" (let [{:keys [sym tags]} cursor
                                    signatures (some->> signatures (string/join "\n"))
                                    tags (string/join " " tags)]
                                {:kind "markdown"
                                 :value (cond-> (str "```\n" sym "\n```\n")
                                          signatures (str "```\n" signatures "\n```\n")
                                          (seq doc) (str doc "\n")
                                          (seq tags) (str "\n----\n" "lsp: " tags))})

                   ;; default to plaintext
                   [(cond-> (select-keys cursor [:sym :tags])
                      (seq signatures) (assoc :signatures signatures)
                      :always (pr-str))])}
      {:contents []})))

(defn formatting [doc-id]
  (let [{:keys [text]} (get-in @db/db [:documents doc-id])
        new-text (cljfmt/reformat-string
                   text
                   (get-in @db/db [:client-settings "cljfmt"]))]
    (when-not (= new-text text)
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
                           (get-in @db/db [:client-settings "cljfmt"])))})
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
