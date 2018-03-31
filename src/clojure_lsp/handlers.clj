(ns clojure-lsp.handlers
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.parser :as parser]
    [clojure.core.async :as async]
    [clojure.java.io :as io]
    [clojure.string :as string]
    [clojure.tools.logging :as log])
  (:import
    (org.eclipse.lsp4j CompletionItem Position Range TextEdit Location)))

(defn- uri->path [uri]
  (string/replace uri #"^file:///" "/"))

(defn- check-bounds [line column {:keys [row end-row col end-col]}]
  (cond
    (< line row) :before
    (and (= line row) (< column col)) :before
    (< line end-row) :within
    (and (= end-row line) (>= end-col column)) :within
    :else :after))

(defn safe-find-references [text]
  (try
    (parser/find-references text)
    (catch Exception e
      (log/error "Cannot parse `" (subs text 0 100) "`" (.getMessage e)))))

(defn crawl-files [files]
  (let [xf (comp (filter #(.isFile %))
                 (map #(.getAbsolutePath %))
                 (filter (fn [path]
                           (or (string/ends-with? path ".clj")
                               (string/ends-with? path ".cljc"))))
                 (map (juxt identity (comp safe-find-references slurp)))
                 (remove (comp nil? second)))
        output-chan (async/chan)]
    (async/pipeline-blocking 5 output-chan xf (async/to-chan files))
    (async/<!! (async/into {} output-chan))))

(defn did-open [uri text]
  (when-let [references (safe-find-references text)]
    (swap! db/db (fn [state-db]
                   (-> state-db
                       (assoc-in [:documents uri] {:v 0 :text text})
                       (assoc-in [:file-envs (uri->path uri)] references)))))
  text)

(defn initialize [project-root]
  (when project-root
    (let [root-file (io/file (uri->path project-root) "src")
          file-envs (->> (file-seq root-file)
                         (crawl-files))]
      (swap! db/db assoc
             :project-root project-root
             :file-envs file-envs
             :project-aliases (apply merge (map (comp :aliases val) file-envs))))))

(defn completion [doc-id line column]
  (let [path (uri->path doc-id)
        file-envs (:file-envs @db/db)
        local-env (get file-envs path)
        remote-envs (dissoc file-envs path)
        {:keys [add-require? row col]} (:require-pos local-env)]
    (into
      (->> (:usages local-env)
           (filter (comp :declare :tags))
           (remove (fn [usage]
                     (when-let [scope-bounds (:scope-bounds usage)]
                       (not= :within (check-bounds line column scope-bounds)))))
           (map :sym)
           (mapv (fn [sym] (CompletionItem. (name sym))))
           (set))
      (for [[doc-id remote-env] remote-envs
            :let [ns-sym (:ns remote-env)
                  local-alias (get-in local-env [:aliases ns-sym])
                  alias (get-in @db/db [:project-aliases ns-sym])
                  as-alias (cond-> ""
                             alias (str " :as " (name alias)))
                  ref (or local-alias alias ns-sym)]
            usage (filter (comp :public :tags) (:usages remote-env))]
        (doto (CompletionItem. (format "%s/%s" (name ref) (name (:sym usage))))
          (.setAdditionalTextEdits
            (cond-> []
              (not (contains? (:requires local-env) ns-sym))
              (conj (TextEdit. (Range. (Position. (dec row) (dec col))
                                       (Position. (dec row) (dec col)))
                               (if add-require?
                                 (format "\n  (:require\n   [%s%s])" (name ns-sym) as-alias)
                                 (format "\n   [%s%s]" (name ns-sym) as-alias)))))))))))

(defn references [doc-id line column]
  (let [path (uri->path doc-id)
        file-envs (:file-envs @db/db)
        local-env (get file-envs path)
        cursor-sym (:sym (first (filter (comp #{:within} (partial check-bounds line column)) (:usages local-env))))]
    (into []
          (for [[path {:keys [usages]}] (:file-envs @db/db)
                {:keys [sym row end-row col end-col]} usages
                :when (= sym cursor-sym)]
            (Location. (str "file://" path)
                       (Range. (Position. (dec row) (dec col))
                               (Position. (dec end-row) (dec end-col))))))))

(defn did-change [uri text version]
  ;; Ensure we are only accepting newer changes
  (loop [state-db @db/db]
    (when (> version (get-in state-db [:documents uri :v] -1))
      (when-let [references (safe-find-references text)]
        (when-not (compare-and-set! db/db state-db (-> state-db
                                                       (assoc-in [:documents uri] {:v version :text text})
                                                       (assoc-in [:file-envs (string/replace uri #"^file:///" "/")] references)))
          (recur @db/db))))))

(comment
  (do (did-change "foo" "foo" 1)
      @db/db)
  )
