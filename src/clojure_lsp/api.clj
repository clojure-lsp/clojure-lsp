(ns clojure-lsp.api
  "Entrypoint for main clojure-lsp features"
  (:require
   [clojure-lsp.internal-api :as internal-api])
  (:import
   [java.io File]))

(defn clean-ns!
  "Organize ns form, removing unused requires/refers/imports and sorting
  alphabetically.
  Options should be passed as keyword arguments and it accepts:

  `:project-root` a java.io.File representing the project root.

  `:namespaces` a coll of symbols representing the namespaces which should be cleaned,
  if empty all project namespaces will be considered.

  `settings` map of settings following https://clojure-lsp.github.io/clojure-lsp/settings/"
  [& {:keys [project-root settings] :as options}]
  {:pre [(instance? File project-root)
         (.exists project-root)
         (or (nil? settings)
             (map? settings))]}
  (internal-api/clean-ns! options))
