(ns clojure-lsp.clj-depend
  (:require
   [clj-depend.api :as clj-depend]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [medley.core :as medley]))

(set! *warn-on-reflection* true)

(defn resolve-user-clj-depend-config [project-root db]
  (let [clj-depend-config-file (io/file project-root ".clj-depend" "config.edn")]
    (medley/deep-merge
      (settings/get db [:clj-depend] {})
      (when (shared/file-exists? clj-depend-config-file)
        (edn/read-string {} (slurp clj-depend-config-file))))))

(defn analyze-filename! [filename db]
  (when-let [project-root (some-> db :project-root-uri shared/uri->filename)]
    (let [config (resolve-user-clj-depend-config project-root db)]
      (when (seq (seq config))
        (when-let [namespace (some-> filename (shared/filename->namespace db) symbol)]
          (-> {:violations {namespace []}}
              (medley/deep-merge
                (-> (clj-depend/analyze {:project-root (io/file project-root)
                                         :config (assoc config :source-paths (map #(shared/relativize-filepath % project-root)
                                                                                  (settings/get db [:source-paths])))
                                         :namespaces #{namespace}})
                    (update :violations #(group-by :namespace %))))))))))

(defn db-with-results
  "Update `db` with clj-depend result."
  [db {:keys [violations]}]
  (update db :clj-depend-violations merge violations))
