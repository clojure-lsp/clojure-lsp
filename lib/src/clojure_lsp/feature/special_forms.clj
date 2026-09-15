(ns clojure-lsp.feature.special-forms
  (:require
   [clojure.repl]))

(set! *warn-on-reflection* true)

(def ^:private special-doc-map
  (if-let [special-doc-map-var (ns-resolve 'clojure.repl 'special-doc-map)]
    (var-get special-doc-map-var)
    {}))

(defn special-form-doc
  "Return the built-in Clojure documentation for SYM-NAME, if any.

  Clojure exposes special-form documentation through `clojure.repl/doc`, but
  not as a public data API. Reuse its source map so hover stays aligned with
  the Clojure runtime instead of duplicating the documentation here."
  [sym-name]
  (when sym-name
    (get special-doc-map (symbol sym-name))))
