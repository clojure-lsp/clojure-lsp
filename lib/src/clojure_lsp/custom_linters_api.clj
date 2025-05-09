(ns clojure-lsp.custom-linters-api
  "Public API used by custom linters, avoid breaking changes."
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]))

(defn external-analysis
  "Filter only dependencies analysis, that are not related to project code."
  [db]
  (q/external-analysis db))

(defn internal-analysis
  "Filter only project related analysis, that are not related to dependencies code."
  [db]
  (q/internal-analysis db))

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

(defn find-element-from-sym
  "Find element from a full qualified namespaced symbol."
  [db ns-sym name-sym]
  (q/find-element-from-sym db ns-sym name-sym))

(defn filename->uri
  "Convert a absolute filename path to a uri."
  [filename db]
  (shared/filename->uri filename db))

(defn uri->filename
  "Convert a uri to an absolute filename path."
  [uri]
  (shared/uri->filename uri))

(def api-fns
  {'external-analysis external-analysis
   'internal-analysis internal-analysis
   'find-definition find-definition
   'find-declaration find-declaration
   'find-implementations find-implementations
   'find-references find-references
   'find-element find-element
   'find-var-definitions find-var-definitions
   'find-element-from-sym find-element-from-sym
   'filename->uri filename->uri
   'uri->filename uri->filename})
