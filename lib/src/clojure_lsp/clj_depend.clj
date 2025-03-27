(ns clojure-lsp.clj-depend
  (:require
   [clj-depend.api :as clj-depend]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]))

(set! *warn-on-reflection* true)

(def logger-tag "[clj-depend]")

(defn ^:private violations-grouped-by-namespace
  [{:keys [violations]}]
  {:violations (group-by :namespace violations)})

(defn ^:private analyze
  [options]
  (logger/debug logger-tag "Running clj-depend analysis with the following options:" options)
  (try
    (clj-depend/analyze options)
    (catch Exception e
      (logger/error logger-tag "Error while running clj-depend analysis" e))))

(defn ^:private with-files
  [options paths]
  (assoc options :files (set (map io/file paths))))

(defn ^:private with-namespaces
  [options namespace]
  (assoc options :namespaces #{namespace}))

(defn ^:private relative-project-source-paths
  [project-root db]
  (set (map #(shared/relativize-filepath % project-root)
            (settings/get db [:source-paths]))))

(defn ^:private with-source-paths
  [options project-root db]
  (if (-> options :config :source-paths not-empty)
    options
    (assoc-in options [:config :source-paths] (relative-project-source-paths project-root db))))

(defn ^:private with-project-root
  [options project-root]
  (assoc options :project-root (io/file project-root)))

(defn ^:private new-options
  "This function maintains backwards compatibility with the old and new configuration format"
  [{:keys [snapshot?] :as config}]
  (if (:config config)
    config
    (shared/assoc-some {:config (dissoc config :project-root :files :namespaces :snapshot?)}
                       :snapshot? snapshot?)))

(defn ^:private configured?
  [project-root]
  (try
    (clj-depend/configured? (io/file project-root))
    (catch AssertionError e
      (logger/error e "Error checking if clj-depend is configured on" project-root)
      false)))

(defn ^:private should-run?
  [config project-root]
  (when project-root
    (or config
        (configured? project-root))))

(defn analyze-uri! [uri db]
  (let [project-root (some-> db :project-root-uri shared/uri->filename)
        config (not-empty (settings/get db [:clj-depend]))]
    (when (should-run? config project-root)
      ;; NOTE probably can't use dep-graph to find nses of uri, because
      ;; dep-graph won't exist until after kondo analysis is done, which is
      ;; run in parallel to clj-depend.
      (when-let [namespace (some-> uri (shared/uri->namespace db) symbol)]
        (-> (analyze (-> (new-options (or config {}))
                         (with-project-root project-root)
                         (with-source-paths project-root db)
                         (with-namespaces namespace)))
            (violations-grouped-by-namespace))))))

(defn analyze-paths! [paths db]
  (let [project-root (some-> db :project-root-uri shared/uri->filename)
        config (not-empty (settings/get db [:clj-depend]))]
    (when (should-run? config project-root)
      (-> (analyze (-> (new-options (or config {}))
                       (with-project-root project-root)
                       (with-source-paths project-root db)
                       (with-files paths)))
          (violations-grouped-by-namespace)))))

(defn db-with-results
  "Update `db` with clj-depend result."
  [db {:keys [violations]}]
  (update db :clj-depend-violations merge violations))
