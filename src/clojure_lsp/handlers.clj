(ns clojure-lsp.handlers
  (:require
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
   [java.util.jar JarFile]))

(defonce diagnostics-chan (async/chan 1))
(defonce edits-chan (async/chan 1))

(defn- uri->path [uri]
  (string/replace uri #"^file:///" "/"))

(defn- ->range [{:keys [row end-row col end-col]}]
  {:start {:line (dec row) :character (dec col)}
   :end {:line (dec end-row) :character (dec end-col)}})

(defn check-bounds [line column {:keys [row end-row col end-col]}]
  (cond
    (< line row) :before
    (and (= line row) (< column col)) :before
    (< line end-row) :within
    (and (= end-row line) (>= end-col column)) :within
    :else :after))

(defn find-reference-under-cursor [line column env]
  (first (filter (comp #{:within} (partial check-bounds line column)) (:usages env))))

(defn add-to-ns [requires-to-add uri env]
  (let [ns->alias (:project-aliases @db/db)
        version (get-in @db/db [:documents uri :v] 0)
        {:keys [add-require? row col]} (:require-pos env)
        loc (parser/loc-at-pos (get-in @db/db [:documents uri :text]) row col)
        requires-node (z/node (z/up (reduce
                                     (fn [loc ns-to-add]
                                       (-> loc (z/insert-right [ns-to-add :as (ns->alias ns-to-add)])))
                                     (if add-require?
                                       (-> loc (z/insert-right '(:require)) (z/right) (z/down))
                                       loc)
                                     requires-to-add)))
        require-sexprs (n/list-node (interpose (n/spaces 4)
                                               (interpose (n/newlines 1)
                                                          (cons :require (sort-by pr-str (rest (n/child-sexprs requires-node)))))))
        requires (n/string require-sexprs)
        changes [{:text-document {:uri uri :version version}
                  :edits [{:range (->range (meta requires-node))
                           :new-text requires}]}]]
    (log/warn "applyEdit" uri " and " version)
    (if (:supports-document-changes @db/db)
      {:document-changes changes}
      {:changes (into {} (map (fn [{:keys [text-document edits]}]
                                [(:uri text-document) edits])
                              changes))})))

(defn send-notifications [uri env]
  (let [unknowns (seq (filter (fn [reference] (contains? (:tags reference) :unknown))
                              (:usages env)))
        aliases (set/map-invert (:project-aliases @db/db))
        groups (group-by (fn [usage]
                           usage
                           (if-let [usage-ns (some-> (:sexpr usage) namespace symbol aliases)]
                             usage-ns
                             nil))
                         unknowns)
        requires-to-add (remove nil? (keys groups))
        other-unknowns (get groups nil)]
    ;; todo alias should be in local-env
    ;; needs to go off config value maybe just a refactoring
    (when false #_(seq requires-to-add)
      (async/put! edits-chan (add-to-ns requires-to-add uri env)))

    (async/put! diagnostics-chan {:uri uri :diagnostics (for [unknown other-unknowns]
                                                         {:range (->range unknown)
                                                          :message "Unknown symbol"
                                                          :severity 1})})))

(defn safe-find-references
  ([uri text]
   (safe-find-references uri text true))
  ([uri text diagnose?]
   (try
     #_(log/warn "trying" uri (get-in @db/db [:documents uri :v]))
     (let [references (parser/find-references text)]
       (when diagnose?
         (send-notifications uri references))
       references)
     (catch Throwable e
       (log/warn "Ignoring: " uri (.getMessage e))
       ;; On purpose
       nil))))

(defn did-open [uri text]
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
                     [uri (safe-find-references uri text false)])))
            (remove (comp nil? second)))
        output-chan (async/chan)]
    (async/pipeline-blocking 5 output-chan xf (async/to-chan jars) true (fn [e] (log/warn e "hello")))
    (async/<!! (async/into {} output-chan))))

(defn crawl-source-dirs [dirs]
  (let [xf (comp
            (mapcat file-seq)
            (filter #(.isFile %))
            (map #(str "file://" (.getAbsolutePath %)))
            (filter (fn [uri]
                      (or (string/ends-with? uri ".clj")
                          (string/ends-with? uri ".cljc")
                          (string/ends-with? uri ".cljs"))))
            (map (juxt identity (fn [uri] (safe-find-references uri (slurp uri) false))))
            (remove (comp nil? second)))
        output-chan (async/chan)]
    (async/pipeline-blocking 5 output-chan xf (async/to-chan dirs) true (fn [e] (log/warn e "hello")))
    (async/<!! (async/into {} output-chan))))

(defn lookup-classpath [project-root]
  (try
    (let [root-path (uri->path project-root)
          sep (re-pattern (System/getProperty "path.separator"))]
      (-> (shell/sh "lein" "classpath" :dir root-path)
          (:out)
          (string/trim-newline)
          (string/split sep)))
    (catch Exception e
      (log/warn "Could not run lein in" project-root (.getMessage e)))))

(defn determine-dependencies [project-root]
  (let [root-path (uri->path project-root)
        project-file (io/file root-path "project.clj")]
    (if (.exists project-file)
      (let [project-hash (digest/md5 project-file)
            loaded (db/read-deps root-path)
            use-cp-cache (= (:project-hash loaded) project-hash)
            classpath (if use-cp-cache
                        (:classpath loaded)
                        (lookup-classpath project-root))
            {jars true dirs false} (group-by #(.isFile %) (map io/file (reverse classpath)))
            jar-envs (if use-cp-cache
                       (:jar-envs loaded)
                       (crawl-jars jars))
            file-envs (crawl-source-dirs dirs)]
        (db/save-deps root-path project-hash classpath jar-envs)
        (merge file-envs jar-envs))
      (crawl-source-dirs [(io/file root-path "src")]))))

(defn initialize [project-root supports-document-changes]
  (when project-root
    (let [file-envs (determine-dependencies project-root)]
      (swap! db/db assoc
             :supports-document-changes supports-document-changes
             :project-root project-root
             :file-envs file-envs
             :project-aliases (apply merge (map (comp :aliases val) file-envs))))))

(defn namespaces-and-aliases [local-aliases project-aliases remote-envs]
  (map (fn [[doc-id remote-env]]
         (let [ns-sym (:ns remote-env)
               project-alias (get project-aliases ns-sym)
               as-alias (cond-> ""
                          project-alias (str " :as " (name project-alias)))
               local-alias (get local-aliases ns-sym) ]
           {:ns ns-sym
            :local-alias local-alias
            :project-alias project-alias
            :as-alias as-alias
            :ref (or local-alias project-alias ns-sym)
            :usages (:usages remote-env)}))
       remote-envs))

(defn completion [doc-id line column]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        remote-envs (dissoc file-envs doc-id)
        {:keys [add-require? row col]} (:require-pos local-env)
        cursor-value (some-> (find-reference-under-cursor line column local-env) :sexpr str)
        matches-cursor? #(some-> % (string/starts-with? cursor-value))
        matches-ns? (fn [ns-sym]
                      (and (or (= cursor-value (str ns-sym))
                               (string/starts-with? cursor-value (str ns-sym "/")))
                           ns-sym))
        remotes (group-by
                 (fn [remote]
                   (or (matches-ns? (:local-alias remote))
                       (matches-ns? (:project-alias remote))
                       (matches-ns? (:ns remote))))
                 (namespaces-and-aliases (:aliases local-env) (:project-aliases @db/db) remote-envs))
        unmatched-remotes (get remotes false)
        matched-remotes (dissoc remotes false)]
    (concat
     (->> (:usages local-env)
          (filter (comp :declare :tags))
          (filter (comp matches-cursor? str :sexpr))
          (remove (fn [usage]
                    (when-let [scope-bounds (:scope-bounds usage)]
                      (not= :within (check-bounds line column scope-bounds)))))
          (map (fn [{:keys [sym]}] {:label (name sym)}))
          (sort-by :label))
     (->> (get remotes false)
          (filter (fn [remote]
                    (->> [:project-alias :ns :local-alias]
                         (select-keys remote)
                         (vals)
                         (some matches-cursor?))))
          (map (fn [match]
                 {:label (str (:ref match))}))
          (sort-by :label))
     (->>
      (for [[matched-ns remotes] matched-remotes
            remote remotes
            usage (:usages remote)
            :let [label (format "%s/%s" (name (:ref remote)) (name (:sym usage)))]
            :when (contains? (:tags usage) :public)
            :when (matches-cursor? (format "%s/%s" (name matched-ns) (name (:sym usage))))]
        (cond-> {:label label}
          (not (contains? (:requires local-env) (:ns remote)))
          (assoc :additional-text-edits [{:range (->range {:row row :col col :end-row row :end-col col})
                                          :new-text (if add-require?
                                                      (format "\n  (:require\n   [%s%s])" (name (:ns remote)) (:as-alias remote))
                                                      (format "\n   [%s%s]" (name (:ns remote)) (:as-alias remote)))}])))
      (sort-by :label))
     (->> cc/core-syms
          (filter (comp matches-cursor? str))
          (map (fn [sym] {:label (str sym)}))
          (sort-by :label))
     (->> cc/java-lang-syms
          (filter (comp matches-cursor? str))
          (map (fn [sym] {:label (str sym)}))
          (sort-by :label)))))

(defn references [doc-id line column]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        cursor-sym (:sym (find-reference-under-cursor line column local-env))]
    (log/warn "references" doc-id line column)
    (into []
          (for [[uri {:keys [usages]}] (:file-envs @db/db)
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
        {cursor-sym :sym cursor-sexpr :sexpr tags :tags} (find-reference-under-cursor line column local-env)]
    (when-not (contains? tags :norename)
      (let [replacement (if-let [cursor-ns (namespace cursor-sexpr)]
                          (string/replace new-name (re-pattern (str "^" cursor-ns "/")) "")
                          new-name)
            changes (->> (for [[doc-id {:keys [usages]}] file-envs
                               :let [version (get-in @db/db [:documents doc-id :v] 0)]
                               {:keys [sym sexpr] :as usage} usages
                               :when (= sym cursor-sym)
                               :let [sym-ns (namespace sexpr)]]
                           {:range (->range usage)
                            :new-text (if sym-ns
                                        (str sym-ns "/" replacement)
                                        replacement)
                            :text-document {:version version :uri doc-id}})
                         (group-by :text-document)
                         (remove (comp empty? val))
                         (map (fn [[text-document edits]]
                                {:text-document text-document
                                 :edits edits})))]
        (if (:supports-document-changes @db/db)
          {:document-changes changes}
          {:changes (into {} (map (fn [{:keys [text-document edits]}]
                                    [(:uri text-document) edits])
                                  changes))})))))

(defn definition [doc-id line column]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        cursor-sym (:sym (find-reference-under-cursor line column local-env))]
    (log/warn "definition" doc-id line column)
    (first
     (for [[doc-id {:keys [usages]}] file-envs
            {:keys [sym tags] :as usage} usages
            :when (= sym cursor-sym)
            :when (and (:public tags) (:declare tags))]
        {:uri doc-id :range (->range usage)}))))

(def refactorings
  {"cycle-coll" #'refactor/cycle-coll
   "thread-first" #'refactor/thread-first
   "thread-first-all" #'refactor/thread-first-all
   "thread-last" #'refactor/thread-last
   "thread-last-all" #'refactor/thread-last-all
   "move-to-let" #'refactor/move-to-let
   "introduce-let" #'refactor/introduce-let
   "expand-let" #'refactor/expand-let})

(defn refactor [doc-id line column refactoring args]
  (try
    (let [ ;; TODO Instead of v=0 should I send a change AND a document change
          {:keys [v text] :or {v 0}} (get-in @db/db [:documents doc-id])
          result (apply (get refactorings refactoring) (parser/loc-at-pos text line column) args)
          changes [{:text-document {:uri doc-id :version v}
                    :edits (mapv #(update % :range ->range) (refactor/result result))}]]
      (if (:supports-document-changes @db/db)
        {:document-changes changes}
        {:changes (into {} (map (fn [{:keys [text-document edits]}]
                                  [(:uri text-document) edits])
                                changes))}))
    (catch Exception e
      (log/error e "could not refactor" (.getMessage e)))))

(defn hover [doc-id line column]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        cursor (find-reference-under-cursor line column local-env)
        signature (first
                    (for [[_ {:keys [usages]}] file-envs
                          {:keys [sym tags] :as usage} usages
                          :when (and (= sym (:sym cursor)) (:declare tags))]
                      (:signature usage)))]
    (if cursor
      {:range (->range cursor)
       :contents [(-> cursor
                      (select-keys [:sym :tags])
                      (assoc :signature signature)
                      (pr-str))]}
      {:contents []})))
