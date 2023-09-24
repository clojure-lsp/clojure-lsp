(ns clojure-lsp.clj-depend
  (:require
   [clj-depend.api :as clj-depend]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [medley.core :as medley]))

(set! *warn-on-reflection* true)

(defn relative-project-source-paths
  [project-root db]
  (map #(shared/relativize-filepath % project-root)
       (settings/get db [:source-paths])))

(defn config-with-source-paths
  [config source-paths]
  (if (not-empty (:source-paths config))
    config
    (assoc config :source-paths source-paths)))

(defn ^:private configured?
  [config project-root]
  (or (seq config)
      (clj-depend/configured? (io/file project-root))))

(defn analyze-uri! [uri db]
  (when-let [project-root (some-> db :project-root-uri shared/uri->filename)]
    (let [config (settings/get db [:clj-depend] {})]
      (when (configured? config project-root)
        ;; NOTE probably can't use dep-graph to find nses of uri, because
        ;; dep-graph won't exist until after kondo analysis is done, which is
        ;; run in parallel to clj-depend.
        (when-let [namespace (some-> uri (shared/uri->namespace db) symbol)]
          (-> {:violations {namespace []}}
              (medley/deep-merge
                (-> (clj-depend/analyze {:project-root (io/file project-root)
                                         :config (config-with-source-paths config (relative-project-source-paths project-root db))
                                         :namespaces #{namespace}})
                    (update :violations #(group-by :namespace %))))))))))

(defn analyze-paths! [paths db]
  (when-let [project-root (some-> db :project-root-uri shared/uri->filename)]
    (let [config (settings/get db [:clj-depend] {})]
      (when (configured? config project-root)
        (-> (clj-depend/analyze {:project-root (io/file project-root)
                                 :config (config-with-source-paths config (relative-project-source-paths project-root db))
                                 :files (set (map io/file paths))})
            (update :violations #(group-by :namespace %)))))))

(defn db-with-results
  "Update `db` with clj-depend result."
  [db {:keys [violations]}]
  (update db :clj-depend-violations merge violations))
