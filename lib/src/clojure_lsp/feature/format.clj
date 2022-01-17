(ns clojure-lsp.feature.format
  (:require
   [cljfmt.core :as cljfmt]
   [cljfmt.main :as cljfmt.main]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [medley.core :as medley]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(defn ^:private resolve-cljfmt-config [db]
  (let [config-path (settings/get db [:cljfmt-config-path] ".cljfmt.edn")
        project-root (shared/uri->filename (:project-root-uri @db))
        cljfmt-config-file (if (string/starts-with? config-path "/")
                             (io/file config-path)
                             (io/file project-root config-path))]
    (cljfmt.main/merge-default-options
      (medley/deep-merge
        (settings/get db [:cljfmt] {})
        (when (shared/file-exists? cljfmt-config-file)
          (edn/read-string {:readers {'re re-pattern}} (slurp cljfmt-config-file)))))))

(defn formatting [uri db]
  (let [{:keys [text]} (get-in @db [:documents uri])
        cljfmt-settings (resolve-cljfmt-config db)
        new-text (cljfmt/reformat-string text cljfmt-settings)]
    (if (= new-text text)
      []
      [{:range (shared/full-file-range)
        :new-text new-text}])))

(defn range-formatting [doc-id format-pos db]
  (let [{:keys [text]} (get-in @db [:documents doc-id])
        cljfmt-settings (resolve-cljfmt-config db)
        forms (parser/find-top-forms-in-range text format-pos)]
    (mapv (fn [form-loc]
            {:range (shared/->range (-> form-loc z/node meta))
             :new-text (n/string (cljfmt/reformat-form (z/node form-loc) cljfmt-settings))})
          forms)))
