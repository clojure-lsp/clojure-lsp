(ns clojure-lsp.handlers
  (:require
    [cljfmt.core :as cljfmt]
    [cljfmt.main :as cljfmt.main]
    [clojure-lsp.clojure-core :as cc]
    [clojure-lsp.db :as db]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.refactor.transform :as refactor]
    [clojure.core.async :as async]
    [clojure.java.io :as io]
    [clojure.java.shell :as shell]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [digest :as digest]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z])
  (:import
    [java.net URI]
    [java.util.jar JarFile]
    [java.nio.file Paths]))

(defonce diagnostics-chan (async/chan 1))
(defonce edits-chan (async/chan 1))

(defn- file->uri [file]
  (str (.toUri (.toPath file))))

(defn- to-file [path child]
  (.toFile (.resolve path child)))

(defn- uri->path [uri]
  (Paths/get (URI. uri)))

(defn- uri->file-type [uri]
  (cond
    (string/ends-with? uri ".cljs") :cljs
    (string/ends-with? uri ".cljc") :cljc
    (string/ends-with? uri ".clj") :clj
    :else :unknown))

(defn- ->range [{:keys [row end-row col end-col]}]
  {:start {:line (dec row) :character (dec col)}
   :end {:line (dec end-row) :character (dec end-col)}})

(defn check-bounds [line column {:keys [row end-row col end-col] :as _usage}]
  (cond
    (< line row) :before
    (and (= line row) (< column col)) :before
    (< line end-row) :within
    (and (= end-row line) (>= end-col column)) :within
    :else :after))

(defn find-reference-under-cursor [line column env file-type]
  (let [file-types (if (= :cljc file-type)
                     #{:clj :cljs}
                     #{file-type})]
    (->> env
         (filter (comp file-types :file-type))
         (filter (comp #{:within} (partial check-bounds line column)))
         first)))

(defn ^:private diagnose-unknown [project-aliases usages]
  (let [unknown-usages (seq (filter (fn [usage] (contains? (:tags usage) :unknown))
                                    usages))
        aliases (set/map-invert project-aliases)]
    (for [usage unknown-usages
          :let [known-alias? (some-> (:unkown-ns usage)
                                     aliases)
                problem (if known-alias?
                          :require
                          :unknown)]]
      {:range (->range usage)
       :code problem
       :message (case problem
                  :unknown "Unknown symbol"
                  :require "Needs Require")
       :severity 1})))

(defn ^:private diagnose-unused-references [declared-references all-envs]
  (let [references (->> all-envs
                        (mapcat (comp val))
                        (remove (comp #(contains? % :declare) :tags))
                        (map :sym)
                        set)
        unused-syms (set/difference (set (map :sym declared-references)) references)]
    (for [usage (filter (comp unused-syms :sym) declared-references)]
      {:range (->range usage)
       :code :unused
       :message (str "Unused declaration: " (:str usage))
       :severity 1})))

(defn ^:private diagnose-unused-aliases [declared-aliases usages]
  (let [references (->> usages
                        (remove (comp #(contains? % :declare) :tags))
                        (map #(some-> % :sym namespace symbol))
                        set)
        unused-aliases (set/difference (set (map :ns declared-aliases)) references)]
    (for [usage (filter (comp unused-aliases :ns) declared-aliases)]
      {:range (->range usage)
       :code :unused
       :message (str "Unused alias: " (:str usage))
       :severity 1})))

(defn ^:private diagnose-unused [uri usages]
  (let [all-envs (assoc (:file-envs @db/db) uri usages)
        declarations (->> usages
                          (filter (comp #(and (contains? % :declare) (not (contains? % :unused))) :tags))
                          (remove (comp #(string/starts-with? % "_") name :sym)))
        declared-references (remove (comp #(contains? % :alias) :tags) declarations)
        declared-aliases (filter (comp #(contains? % :alias) :tags) declarations)]
    (concat (diagnose-unused-aliases declared-aliases usages)
            (diagnose-unused-references declared-references all-envs))))

(defn find-diagnostics [project-aliases uri usages]
  (let [unknown (diagnose-unknown project-aliases usages)
        unused (diagnose-unused uri usages)
        result (concat unknown unused)]
    result))

(defn safe-find-references
  ([uri text]
   (safe-find-references uri text true false))
  ([uri text diagnose? remove-private?]
   (try
     #_(log/warn "trying" uri (get-in @db/db [:documents uri :v]))
     (let [file-type (uri->file-type uri)
           macro-defs (get-in @db/db [:client-settings "macro-defs"])
           references (cond->> (parser/find-usages text file-type macro-defs)
                        remove-private? (filter (fn [{:keys [tags]}] (and (:public tags) (:declare tags)))))]
       (when diagnose?
         (async/put! diagnostics-chan
                     {:uri uri
                      :diagnostics (find-diagnostics (:project-aliases @db/db) uri references)}))
       references)
     (catch Throwable e
       (log/warn e "Ignoring: " uri (.getMessage e))
       ;; On purpose
       nil))))

(defn ^:private uri->namespace [uri]
  (let [project-root (:project-root @db/db)
        source-paths (get-in @db/db [:client-settings "source-paths"])
        in-project? (string/starts-with? uri project-root)
        file-type (uri->file-type uri)]
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

(defn did-open [uri text]
  (when-let [new-ns (and (empty? text) (uri->namespace uri))]
    (let [new-text (format "(ns %s)" new-ns)
          changes [{:text-document {:version (get-in @db/db [:documents uri :v] 0) :uri uri}
                    :edits [{:range (->range {:row 1 :end-row 1 :col 1 :end-col 1})
                             :new-text new-text}]}]]
      (async/put!
        edits-chan
        (if (get-in @db/db [:client-capabilities :workspace :workspace-edit :document-changes])
          {:document-changes changes}
          {:changes (into {} (map (fn [{:keys [text-document edits]}]
                                    [(:uri text-document) edits])
                                  changes))}))))

  (when-let [references (safe-find-references uri text)]
    (swap! db/db (fn [state-db]
                   (-> state-db
                       (assoc-in [:documents uri] {:v 0 :text text})
                       (assoc-in [:file-envs uri] references)))))
  text)

(defn crawl-jars [jars]
  (let [xf (comp
             (mapcat (fn [jar-file]
                       (let [jar (JarFile. jar-file)]
                         (->> jar
                              (.entries)
                              (enumeration-seq)
                              (remove #(.isDirectory %))
                              (map (fn [entry]
                                     [(str "zipfile://" jar-file "::" (.getName entry))
                                      entry
                                      jar]))))))
             (filter (fn [[uri _ _]]
                       (or (string/ends-with? uri ".clj")
                           (string/ends-with? uri ".cljc")
                           (string/ends-with? uri ".cljs"))))
             (map (fn [[uri entry jar]]
                    (let [text (with-open [stream (.getInputStream jar entry)]
                                 (slurp stream))]
                      [uri (safe-find-references uri text false true)])))
             (remove (comp nil? second)))
        output-chan (async/chan 5)]
    (async/pipeline-blocking 5 output-chan xf (async/to-chan jars) true (fn [e] (log/warn e "hello")))
    (async/<!! (async/into {} output-chan))))

(defn crawl-source-dirs [dirs]
  (let [xf (comp
            (mapcat file-seq)
            (filter #(.isFile %))
            (map file->uri)
            (filter (fn [uri]
                      (or (string/ends-with? uri ".clj")
                          (string/ends-with? uri ".cljc")
                          (string/ends-with? uri ".cljs"))))
            (map (juxt identity (fn [uri]
                                  (safe-find-references uri (slurp uri) false false))))
            (remove (comp nil? second)))
        output-chan (async/chan)]
    (async/pipeline-blocking 5 output-chan xf (async/to-chan dirs) true (fn [e] (log/warn e "hello")))
    (async/<!! (async/into {} output-chan))))

(defn lookup-classpath [root-path command-args]
  (try
    (let [sep (re-pattern (System/getProperty "path.separator"))]
      (-> (apply shell/sh (into command-args [:dir (str root-path)]))
          (:out)
          (string/trim-newline)
          (string/split sep)))
    (catch Exception e
      (log/warn e "Error while looking up classpath info in" (str root-path) (.getMessage e))
      [])))

(defn try-project [root-path project-path command-args]
  (let [project-file (to-file root-path project-path)]
    (when (.exists project-file)
      (let [file-hash (digest/md5 project-file)
            classpath (lookup-classpath root-path command-args)]
        {:project-hash file-hash :classpath classpath}))))

(def project-specs
  [{:project-path "project.clj"
    :classpath-cmd ["lein" "classpath"]}
   {:project-path "build.boot"
    :classpath-cmd ["boot" "show" "--fake-classpath"]}])

(defn get-project-from [root-path]
  (reduce
    (fn [project {:keys [project-path classpath-cmd]}]
      (if-let [subproject (try-project root-path project-path classpath-cmd)]
        (-> project
            (update :project-hash (fnil str "") (:project-hash subproject))
            (update :classpath (fnil into []) (:classpath subproject)))
        project))
    {}
    project-specs))

(defn determine-dependencies [project-root]
  (let [root-path (uri->path project-root)
        source-paths (mapv #(to-file root-path %)
                           (get @db/db [:client-settings "source-paths"]))
        project (get-project-from root-path)]
    (if (some? project)
      (let [project-hash (:project-hash project)
            loaded (db/read-deps root-path)
            use-cp-cache (= (:project-hash loaded) project-hash)
            classpath (if use-cp-cache
                        (:classpath loaded)
                        (:classpath project))
            jars (filter #(.isFile %) (map io/file (reverse classpath)))
            jar-envs (if use-cp-cache
                       (:jar-envs loaded)
                       (crawl-jars jars))
            file-envs (crawl-source-dirs source-paths)]
        (db/save-deps root-path project-hash classpath jar-envs)
        (merge file-envs jar-envs))
      (crawl-source-dirs source-paths))))

(defn initialize [project-root client-capabilities client-settings]
  (when project-root
    (swap! db/db assoc
           :project-root project-root
           :client-settings client-settings
           :client-capabilities client-capabilities)
    (let [file-envs (determine-dependencies project-root)]
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
                       (if-let [usage (find-reference-under-cursor line try-column local-env (uri->file-type doc-id))]
                         usage
                         (when (pos? try-column)
                           (recur (dec try-column)))))
        {cursor-value :str cursor-file-type :file-type} cursor-usage
        [cursor-ns cursor-name] (if-let [idx (some-> cursor-value (string/index-of "/"))]
                                  [(subs cursor-value 0 idx) (subs cursor-value (inc idx))]
                                  [cursor-value nil])
        matches? (partial matches-cursor? cursor-value)
        namespaces-and-aliases (->> file-envs
                                    (mapcat val)
                                    (filter (fn [{:keys [file-type tags] :as usage}]
                                              (and
                                                (= cursor-file-type file-type)
                                                (or
                                                  (set/subset? #{:public :ns} tags)
                                                  (:alias tags)))))
                                    (mapv (fn [{:keys [sym tags] alias-str :str alias-ns :ns :as usage}]
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
                         require-edit (assoc :additional-text-edits (mapv #(update % :range ->range) require-edit))))))
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
                 require-edit (assoc :additional-text-edits (mapv #(update % :range ->range) require-edit))))
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
        cursor-sym (:sym (find-reference-under-cursor line column local-env (uri->file-type doc-id)))]
    (log/warn "references" doc-id line column cursor-sym)
    (into []
          (for [[uri usages] (:file-envs @db/db)
                {:keys [sym] :as usage} usages
                :when (= sym cursor-sym)]
            {:uri uri
             :range (->range usage)}))))

(defn did-change [uri text version]
  ;; Ensure we are only accepting newer changes
  (loop [state-db @db/db]
    (when (> version (get-in state-db [:documents uri :v] -1))
      (when-let [references (safe-find-references uri text)]
        (when-not (compare-and-set! db/db state-db (-> state-db
                                                       (assoc-in [:documents uri] {:v version :text text})
                                                       (assoc-in [:file-envs uri] references)))
          (recur @db/db))))))

(defn rename [doc-id line column new-name]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        {cursor-sym :sym cursor-str :str tags :tags} (find-reference-under-cursor line column local-env (uri->file-type doc-id))]
    (when-not (contains? tags :norename)
      (let [[_ cursor-ns _] (parser/ident-split cursor-str)
            replacement (if cursor-ns
                          (string/replace new-name (re-pattern (str "^:{0,2}" cursor-ns "/")) "")
                          (string/replace new-name #"^:{0,2}" ""))
            changes (->> (for [[doc-id usages] file-envs
                               :let [version (get-in @db/db [:documents doc-id :v] 0)]
                               {u-sym :sym u-str :str :as usage} usages
                               :when (= u-sym cursor-sym)
                               :let [[u-prefix u-ns _] (parser/ident-split u-str)]]
                           {:range (->range usage)
                            :new-text (str u-prefix u-ns (when u-ns "/") replacement)
                            :text-document {:version version :uri doc-id}})
                         (group-by :text-document)
                         (remove (comp empty? val))
                         (map (fn [[text-document edits]]
                                {:text-document text-document
                                 :edits edits})))]
        (if (get-in @db/db [:client-capabilities :workspace :workspace-edit :document-changes])
          {:document-changes changes}
          {:changes (into {} (map (fn [{:keys [text-document edits]}]
                                    [(:uri text-document) edits])
                                  changes))})))))

(defn definition [doc-id line column]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        cursor-sym (:sym (find-reference-under-cursor line column local-env (uri->file-type doc-id)))]
    (log/warn "definition" doc-id line column cursor-sym)
    (when cursor-sym
      (first
        (for [[env-doc-id usages] file-envs
              {:keys [sym tags] :as usage} usages
              :when (= sym cursor-sym)
              :when (and (or (= doc-id env-doc-id) (:public tags))
                         (:declare tags))]
          {:uri env-doc-id :range (->range usage)})))))

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
        result (apply (get refactorings refactoring) (parser/loc-at-pos text line column) doc-id args)]
    (when result
      (let [changes [{:text-document {:uri doc-id :version v}
                      :edits (mapv #(update % :range ->range) (refactor/result result))}]]
        (if (get-in @db/db [:client-capabilities :workspace :workspace-edit :document-changes])
          {:document-changes changes}
          {:changes (into {} (map (fn [{:keys [text-document edits]}]
                                    [(:uri text-document) edits])
                                  changes))})))))

(defn hover [doc-id line column]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        cursor (find-reference-under-cursor line column local-env (uri->file-type doc-id))
        {:keys [signatures doc]} (first
                                   (for [[_ usages] file-envs
                                         {:keys [sym tags] :as usage} usages
                                         :when (and (= sym (:sym cursor))
                                                    (:declare tags))]
                                     usage))
        [content-format] (get-in @db/db [:client-capabilities :text-document :hover :content-format])]
    (if cursor
      {:range (->range cursor)
       :contents (case content-format
                   "markdown" (let [{:keys [sym]} cursor
                                    signatures (string/join "\n" signatures)]
                                {:kind "markdown"
                                 :value (str "```\n" sym "\n"
                                             signatures "\n```\n"
                                             doc)})

                   ;; default to plaintext
                   [(cond-> (select-keys cursor [:sym :tags])
                      (seq signatures) (assoc :signatures signatures)
                      :always (pr-str))])}
      {:contents []})))

(defn formatting [doc-id]
  (let [{:keys [text]} (get-in @db/db [:documents doc-id])
        new-text (cljfmt/reformat-string
                   text
                   (cljfmt.main/merge-default-options
                     (get-in @db/db [:client-settings "cljfmt"])))]
    (when-not (= new-text text)
      [{:range (->range {:row 1 :col 1 :end-row 1000000 :end-col 1000000})
        :new-text new-text}])))

(defn range-formatting [doc-id format-pos]
  (let [{:keys [text]} (get-in @db/db [:documents doc-id])
        forms (parser/find-top-forms-in-range text format-pos)]
    (mapv (fn [form-loc]
            {:range (->range (-> form-loc z/node meta))
             :new-text (n/string
                         (cljfmt/reformat-form
                           (z/node form-loc)
                           (cljfmt.main/merge-default-options
                             (get-in (deref db/db) [:client-settings "cljfmt"]))))})
          forms)))
