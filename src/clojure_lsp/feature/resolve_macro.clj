(ns clojure-lsp.feature.resolve-macro
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [rewrite-clj.node :as n]
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

(defn ^:private add-lint-as-config [config-loc resolved-full-symbol full-symbol]
  (let [spaces-count (some-> config-loc
                             z/down
                             z/node
                             meta
                             :col
                             dec)]
    (-> config-loc
        (z/edit->
         ((fn [loc] (cond-> loc
                      spaces-count
                      (-> z/down
                          z/rightmost
                          (z/insert-right (n/whitespace-node (apply str (repeat spaces-count " "))))
                          (z/insert-right (n/newlines 1))
                          z/up)))))
        (z/assoc :lint-as {full-symbol resolved-full-symbol})
        z/string
        (str "\n"))))

(defn ^:private update-lint-as-config [config-loc spaces-node resolved-full-symbol full-symbol]
  (-> config-loc
      (z/edit->
       z/down
       (z/find-next-value :lint-as)
       z/next
       ((fn [loc] (cond-> loc
                    spaces-node
                    (-> z/down
                        z/rightmost
                        (z/insert-right spaces-node)
                        (z/insert-right (n/newlines 1))
                        z/up))))
       (z/assoc full-symbol resolved-full-symbol))
      z/string
      (str "\n")))

(defn ^:private update-macro-resolve-for-config [resolved-full-symbol full-symbol config-loc]
  (let [lint-as-exists? (-> config-loc
                            z/down
                            (z/find-next-value :lint-as)
                            boolean)
        add-spaces? (and (-> config-loc
                             z/down
                             (z/find-next-value :lint-as)
                             z/next
                             z/sexpr
                             seq)
                         (-> config-loc
                             z/down
                             (z/find-next-value :lint-as)
                             z/next
                             z/down
                             (z/find-next-value full-symbol)
                             boolean
                             not))
        spaces-count (when add-spaces?
                       (-> config-loc
                           z/down
                           (z/find-next-value :lint-as)
                           z/next
                           z/node
                           meta
                           :col
                           dec))
        spaces-node (when add-spaces?
                      (n/whitespace-node (apply str (repeat spaces-count " "))))]
    (if lint-as-exists?
      (update-lint-as-config config-loc spaces-node resolved-full-symbol full-symbol)
      (add-lint-as-config config-loc resolved-full-symbol full-symbol))))

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
