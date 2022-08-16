(ns clojure-lsp.feature.format
  (:require
   [cljfmt.core :as cljfmt]
   [cljfmt.main :as cljfmt.main]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.memoize :as memoize]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [medley.core :as medley]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defn resolve-user-cljfmt-config [db]
  (when-let [project-root (some-> db :project-root-uri shared/uri->filename)]
    (let [config-path (settings/get db [:cljfmt-config-path] ".cljfmt.edn")
          cljfmt-config-file (if (string/starts-with? config-path "/")
                               (io/file config-path)
                               (io/file project-root config-path))]
      (medley/deep-merge
        (settings/get db [:cljfmt] {})
        (when (shared/file-exists? cljfmt-config-file)
          (if (string/ends-with? cljfmt-config-file ".clj")
            (binding [*read-eval* false]
              (read-string (slurp cljfmt-config-file)))
            (edn/read-string {:readers {'re re-pattern}} (slurp cljfmt-config-file))))))))

(defn ^:private resolve-cljfmt-config [db]
  (cljfmt.main/merge-default-options
    (resolve-user-cljfmt-config db)))

(def memoize-ttl-threshold-milis 3000)

(def cljfmt-config
  (memoize/ttl resolve-cljfmt-config :ttl/threshold memoize-ttl-threshold-milis))

(defn formatting [uri db*]
  (if-let [text (f.file-management/force-get-document-text uri db*)]
    (let [cljfmt-settings (cljfmt-config @db*)
          new-text (cljfmt/reformat-string text cljfmt-settings)]
      (if (= new-text text)
        []
        [{:range shared/full-file-range
          :new-text new-text}]))
    []))

(defn range-formatting [doc-id format-pos db]
  (let [cljfmt-settings (cljfmt-config db)
        root-loc (parser/zloc-of-file db doc-id)
        start-loc (or (parser/to-pos root-loc (:row format-pos) (:col format-pos))
                      (z/leftmost* root-loc))
        start-top-loc (edit/to-top start-loc)
        end-loc (or (parser/to-pos start-top-loc (:end-row format-pos) (:end-col format-pos))
                    (z/rightmost* root-loc))
        end-top-loc (edit/to-top end-loc)

        forms (->> start-top-loc
                   (iterate z/right*) ;; maintain comments and whitespace between nodes
                   (take-while (complement z/end?))
                   (medley/take-upto #(= % end-top-loc)))
        span (merge (-> start-top-loc z/node meta (select-keys [:row :col]))
                    (-> end-top-loc z/node meta (select-keys [:end-row :end-col])))]
    [{:range (shared/->range span)
      :new-text (-> (map z/node forms)
                    n/forms-node
                    (cljfmt/reformat-form cljfmt-settings)
                    n/string)}]))
