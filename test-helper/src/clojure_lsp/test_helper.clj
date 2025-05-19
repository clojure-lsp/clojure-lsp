(ns clojure-lsp.test-helper
  "Entrypoint for clojure-lsp test helper"
  (:require
   [clojure-lsp.test-helper.internal :as h.internal]))

(set! *warn-on-reflection* true)

(defn load-code
  "Simulates code load by clojure-lsp, analyzing it."
  [{:keys [code uri components]
    :or {uri h.internal/default-uri
         components h.internal/components}}]
  (h.internal/load-code code (h.internal/file-uri uri) components))
