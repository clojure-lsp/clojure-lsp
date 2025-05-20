(ns clojure-lsp.test-helper
  "Entrypoint for clojure-lsp test helper"
  (:require
   [clojure-lsp.test-helper.internal :as h.internal]))

(set! *warn-on-reflection* true)

(def default-uri h.internal/default-uri)

(def components* h.internal/components*)

(defn db*
  "The in-memory clojure-lsp db."
  []
  (h.internal/db*))

(defn reset-components!
  "Resets in-memory clojure-lsp components to a empty state,
   removing any previously loaded code."
  []
  (h.internal/reset-components!))

(defn code
  "Return a string of concatenated strings separated by \n."
  [& strings]
  (apply h.internal/code strings))

(defn load-code!
  "Simulates code load by clojure-lsp, analyzing it."
  [{:keys [code uri components]
    :or {uri h.internal/default-uri
         components h.internal/components}}]
  (h.internal/load-code code (h.internal/file-uri uri) components))

(defn set-settings!
  "Changes db adding specified settings."
  [settings]
  (swap! (h.internal/db*) assoc :settings settings))
