(ns clojure-lsp.test-helper
  "Entrypoint for clojure-lsp test helper"
  (:require
   [clojure-lsp.feature.diagnostics.custom :as f.diagnostics.custom]
   [clojure-lsp.test-helper.internal :as h.internal]))

(set! *warn-on-reflection* true)

(def default-uri h.internal/default-uri)

(def components* h.internal/components*)

(defn db*
  "The in-memory atom clojure-lsp db, can be changed."
  []
  (h.internal/db*))

(defn db
  "Get current value of in-memory clojure-lsp db."
  []
  (h.internal/db))

(defn reset-components!
  "Resets in-memory clojure-lsp components to a empty state,
   removing any previously loaded code."
  []
  (h.internal/reset-components!))

(defn code
  "Return a string of concatenated strings separated by \n."
  [& strings]
  (apply h.internal/code strings))

(defn file-uri
  "Conform uri to a file uri considering Windows and unix paths."
  [^String uri]
  (h.internal/file-uri uri))

(defn file-path
  "Conform path to a file path considering Windows and unix paths."
  [^String path]
  (h.internal/file-path path))

(defn load-code!
  "Simulates code load by clojure-lsp, analyzing it."
  [{:keys [code uri components]
    :or {uri h.internal/default-uri
         components (h.internal/components)}}]
  (h.internal/load-code code (h.internal/file-uri uri) components))

(defn set-db!
  "Changes in-memory db merging specified db."
  [db]
  (swap! (h.internal/db*) merge db))

(defn custom-lint!
  "Custom lint uris using previously loaded code and settings."
  [{:keys [uris]}]
  (f.diagnostics.custom/analyze-uris! uris (h.internal/db*)))
