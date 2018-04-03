(ns clojure-lsp.handlers
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.parser :as parser]
    [clojure.core.async :as async]
    [clojure.java.io :as io]
    [clojure.string :as string]
    [clojure.tools.logging :as log]))

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

(defn safe-find-references [uri text]
  (try
    (parser/find-references text)
    (catch Exception e
      (log/warn "Ignoring: " uri (.getMessage e))
      ;; On purpose
      nil)))

(defn crawl-files [files]
  (let [xf (comp (filter #(.isFile %))
                 (map #(.getAbsolutePath %))
                 (filter (fn [path]
                           (or (string/ends-with? path ".clj")
                               (string/ends-with? path ".cljc"))))
                 (map (juxt identity (fn [path] (safe-find-references path (slurp path)))))
                 (remove (comp nil? second)))
        output-chan (async/chan)]
    (async/pipeline-blocking 5 output-chan xf (async/to-chan files))
    (async/<!! (async/into {} output-chan))))

(defn did-open [uri text]
  (when-let [references (safe-find-references uri text)]
    (swap! db/db (fn [state-db]
                   (-> state-db
                       (assoc-in [:documents uri] {:v 0 :text text})
                       (assoc-in [:file-envs (uri->path uri)] references)))))
  text)

(defn initialize [project-root supports-document-changes]
  (when project-root
    (let [root-file (io/file (uri->path project-root) "src")
          file-envs (->> (file-seq root-file)
                         (crawl-files))]
      (swap! db/db assoc
             :supports-document-changes supports-document-changes
             :project-root project-root
             :file-envs file-envs
             :project-aliases (apply merge (map (comp :aliases val) file-envs))))))

(defn completion [doc-id line column]
  (let [path (uri->path doc-id)
        file-envs (:file-envs @db/db)
        local-env (get file-envs path)
        remote-envs (dissoc file-envs path)
        {:keys [add-require? row col]} (:require-pos local-env)]
    (log/warn "completion" doc-id line column)
    (into
      (->> (:usages local-env)
           (filter (comp :declare :tags))
           (remove (fn [usage]
                     (when-let [scope-bounds (:scope-bounds usage)]
                       (not= :within (check-bounds line column scope-bounds)))))
           (map :sym)
           (mapv (fn [sym] {:label (name sym)}))
           (set))
      (for [[doc-id remote-env] remote-envs
            :let [ns-sym (:ns remote-env)
                  local-alias (get-in local-env [:aliases ns-sym])
                  alias (get-in @db/db [:project-aliases ns-sym])
                  as-alias (cond-> ""
                             alias (str " :as " (name alias)))
                  ref (or local-alias alias ns-sym)]
            usage (filter (comp :public :tags) (:usages remote-env))]

        (cond-> {:label (format "%s/%s" (name ref) (name (:sym usage)))}
          (not (contains? (:requires local-env) ns-sym))
          (assoc :additional-text-edits [{:range (->range {:row row :col col :end-row row :end-col col})
                                          :new-text (if add-require?
                                                      (format "\n  (:require\n   [%s%s])" (name ns-sym) as-alias)
                                                      (format "\n   [%s%s]" (name ns-sym) as-alias))}]))))))

(defn references [doc-id line column]
  (let [path (uri->path doc-id)
        file-envs (:file-envs @db/db)
        local-env (get file-envs path)
        cursor-sym (:sym (find-reference-under-cursor line column local-env))]
    (log/warn "references" doc-id line column)
    (into []
          (for [[path {:keys [usages]}] (:file-envs @db/db)
                {:keys [sym] :as usage} usages
                :when (= sym cursor-sym)]
            {:uri (str "file://" path)
             :range (->range usage)}))))

(defn did-change [uri text version]
  ;; Ensure we are only accepting newer changes
  (loop [state-db @db/db]
    (when (> version (get-in state-db [:documents uri :v] -1))
      (when-let [references (safe-find-references uri text)]
        (when-not (compare-and-set! db/db state-db (-> state-db
                                                       (assoc-in [:documents uri] {:v version :text text})
                                                       (assoc-in [:file-envs (string/replace uri #"^file:///" "/")] references)))
          (recur @db/db))))))

(comment
  (do (did-change "foo" "foo" 1)
      @db/db))

(defn rename [doc-id line column new-name]
  (let [path (uri->path doc-id)
        file-envs (:file-envs @db/db)
        local-env (get file-envs path)
        cursor-sym (:sym (find-reference-under-cursor line column local-env))
        changes (->> (for [[path {:keys [usages]}] file-envs
                           :let [doc-id (str "file://" path)
                                 version (get-in @db/db [:documents doc-id :v] 0)]
                           {:keys [sym sexpr] :as usage} usages
                           :when (= sym cursor-sym)
                           :let [sym-ns (namespace sexpr)]]
                       {:range (->range usage)
                        :new-text (if sym-ns
                                    (str sym-ns "/" new-name)
                                    new-name)
                        :text-document {:version version :uri doc-id}})
                     (group-by :text-document)
                     (remove (comp empty? val))
                     (map (fn [[text-document edits]]
                            {:text-document text-document
                             :edits edits})))]
    (log/warn "rename" doc-id line column)
    (if (:supports-document-changes @db/db)
        {:document-changes changes}
        {:changes (into {} (map (fn [{:keys [text-document edits]}]
                                  [(:uri text-document) edits])
                                changes))})))

(defn definition [doc-id line column]
  (let [path (uri->path doc-id)
        file-envs (:file-envs @db/db)
        local-env (get file-envs path)
        cursor-sym (:sym (find-reference-under-cursor line column local-env))]
    (log/warn "definition" doc-id line column)
    (first
      (for [[path {:keys [usages]}] file-envs
            :let [doc-id (str "file://" path)]
            {:keys [sym tags] :as usage} usages
            :when (and (= sym cursor-sym) (:declare tags))]
        {:uri doc-id :range (->range usage)}))))
