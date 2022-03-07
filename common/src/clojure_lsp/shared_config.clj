(ns clojure-lsp.shared-config
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(def diagnostics-debounce-ms 100)
(def change-debounce-ms 300)
(def created-watched-files-debounce-ms 500)

(defn clojure-lsp-version []
  (string/trim (slurp (io/resource "CLOJURE_LSP_VERSION"))))
