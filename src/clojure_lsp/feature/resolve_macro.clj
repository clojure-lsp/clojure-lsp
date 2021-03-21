(ns clojure-lsp.feature.resolve-macro
  (:require
   [borkdude.rewrite-edn :as r]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]))

(defn ^:private find-function-name-position [uri row col]
  (some-> (get-in @db/db [:documents uri :text])
          (parser/loc-at-pos (inc row) (inc col))
          edit/find-function-usage-name
          z/node
          meta))

(defn ^:private full-symbol-to-resolve [uri row col]
  (let [filename (shared/uri->filename uri)
        analysis (:analysis @db/db)
        {macro-name-row :row macro-name-col :col} (find-function-name-position uri row col)
        element (q/find-element-under-cursor analysis filename macro-name-row macro-name-col)
        definition (q/find-definition analysis element)]
    (when (:macro definition)
      (symbol (-> definition :ns name) (-> definition :name name)))))

(defn ^:private update-macro-resolve-for-config
  [resolved-full-symbol full-symbol config-loc]
  (-> config-loc
      z/node
      (r/assoc-in [:lint-as full-symbol] resolved-full-symbol)
      (str "\n")))

(defn resolve-macro-as [uri row col resolved-full-symbol-str kondo-config-path]
  (let [full-symbol (full-symbol-to-resolve uri row col)
        kondo-config-file (io/file kondo-config-path)
        document (get-in @db/db [:documents uri])]
    (if (.exists kondo-config-file)
      (->> (z/of-file kondo-config-file)
           (update-macro-resolve-for-config (symbol resolved-full-symbol-str) full-symbol)
           (spit kondo-config-path))
      (do
        (io/make-parents kondo-config-path)
        (->> (z/of-string "{}")
             (update-macro-resolve-for-config (symbol resolved-full-symbol-str) full-symbol)
             (spit kondo-config-path))))
    (f.file-management/analyze-changes {:uri uri
                                        :version (:v document)
                                        :text (:text document)})))
