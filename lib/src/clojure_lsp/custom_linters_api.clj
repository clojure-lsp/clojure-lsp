(ns clojure-lsp.custom-linters-api
  "Public API used by custom linters, avoid breaking changes."
  (:require
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]))

(defn external-analysis
  "Filter only dependencies analysis, that are not related to project code."
  [db]
  (q/external-analysis db))

(defn internal-analysis
  "Filter only project related analysis, that are not related to dependencies code."
  [db]
  (q/internal-analysis db))

(defn db-with-internal-analysis
  "Return a db with only internal analysis."
  [db]
  (q/db-with-internal-analysis db))

(defn db-with-uris-analysis
  "Return a db with only analysis for uris.
   Good for performance since the db is reduced to see only those uris."
  [db uris]
  (q/db-with-uris-analysis db uris))

(defn find-definition
  "Find the analysis element definition."
  [db element]
  (q/find-definition db element))

(defn find-declaration
  "Find the analysis element declaration."
  [db element]
  (q/find-declaration db element))

(defn find-implementations
  "Find the analysis element implementations."
  [db element]
  (q/find-implementations db element))

(defn find-references
  "Find the analysis element references."
  [db element include-definition?]
  (q/find-references db element include-definition?))

(defn find-element
  "Find the analysis element in given position."
  [db uri row col]
  (q/find-element-under-cursor db uri row col))

(defn find-var-definitions
  "Find all var-definitions of a uri."
  [db uri include-private?]
  (q/find-var-definitions db uri include-private?))

(defn find-all-var-definitions
  "Find all var-definitions of db."
  [db include-private?]
  (q/find-all-var-definitions db include-private?))

(defn find-element-from-sym
  "Find element from a full qualified namespaced symbol."
  [db ns-sym name-sym]
  (q/find-element-from-sym db ns-sym name-sym))

(defn find-nodes
  "Find all rewrite-clj zipper nodes from elements filtering by filter-fn function.

   ```clojure
   (find-nodes
     db
     (find-all-var-definitions db true)
     #(.equals 'clojure.core/defn (:defined-by %)))
   ```"
  [db elements filter-fn]
  (into
    []
    (comp
      (filter filter-fn)
      (keep (fn [var-def]
              (when-let [zloc (parser/safe-zloc-of-file db (:uri var-def))]
                (edit/find-at-element zloc var-def)))))
    elements))

(defn find-nodes-from-sym
  "Find the rewrite-clj zipper node from a fully qualified symbol."
  [db ns-sym name-sym]
  (when-let [e (q/find-element-from-sym db ns-sym name-sym)]
    (find-nodes db [e] identity)))

(defn find-nodes-from-defined-bys
  "Find the rewrite-clj zipper node from all var-definitions defined-by one of `defined-bys`."
  [db defined-bys]
  (let [defined-bys (set defined-bys)]
    (find-nodes
      db
      (q/find-all-var-definitions db true)
      #(contains? defined-bys (:defined-by %)))))

(defn filename->uri
  "Convert a absolute filename path to a uri."
  [filename db]
  (shared/filename->uri filename db))

(defn uri->filename
  "Convert a uri to an absolute filename path."
  [uri]
  (shared/uri->filename uri))

(defn dir-uris->file-uris
  "Convert a coll of uris that may represent dirs to its
   respective file uris if present on db.

  **Example**

   ```clojure
   (dir-uris->file-uris [\"file:///project/src\"] db)
   => [\"file:///project/src/foo.clj\"
       \"file:///project/src/bar.clj\"]

   ```"
  [uris _db]
  (shared/dir-uris->file-uris uris))

(defn inside?
  "Checks if element `a` is inside element `b` scope."
  [a b]
  (shared/inside? a b))

(defn fast=
  "Fast `=` comparassion, useful for string, sym and keyword comparassions."
  [a b]
  (shared/fast= a b))

(def api-fns
  {'external-analysis external-analysis
   'internal-analysis internal-analysis
   'db-with-internal-analysis db-with-internal-analysis
   'db-with-uris-analysis db-with-uris-analysis
   'find-definition find-definition
   'find-declaration find-declaration
   'find-implementations find-implementations
   'find-references find-references
   'find-element find-element
   'find-var-definitions find-var-definitions
   'find-all-var-definitions find-all-var-definitions
   'find-element-from-sym find-element-from-sym
   'find-nodes find-nodes
   'find-nodes-from-sym find-nodes-from-sym
   'find-nodes-from-defined-bys find-nodes-from-defined-bys
   'filename->uri filename->uri
   'uri->filename uri->filename
   'dir-uris->file-uris dir-uris->file-uris
   'inside? inside?
   'fast= fast=})
