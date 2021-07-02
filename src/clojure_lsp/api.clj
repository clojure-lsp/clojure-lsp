(ns clojure-lsp.api
  "Entrypoint for main clojure-lsp features"
  (:require
   [clojure-lsp.internal-api :as internal-api])
  (:import
   [java.io File]))

(defn clean-ns!
  "Organize ns form, removing unused requires/refers/imports and sorting
  alphabetically.
  Options:

  `:project-root` a java.io.File representing the project root.

  `:namespaces` a coll of symbols representing the namespaces which should be cleaned,
  if empty all project namespaces will be considered.

  `dry-run` make no changes to files, only report.

  `settings` map of settings following https://clojure-lsp.github.io/clojure-lsp/settings/"
  [& {:keys [project-root settings namespace] :as options}]
  {:pre [(instance? File project-root)
         (.exists project-root)
         (or (nil? settings)
             (map? settings))
         (or (nil? namespace)
             (coll? namespace))]}
  (internal-api/clean-ns! options))

(defn rename!
  "Rename a symbol and its definitions across the project.
  Options:

  `:project-root` a java.io.File representing the project root.

  `:from` the full qualified symbol origin name that should be renamed. e.g. my-project/my-var

  `:to` the full qualified symbol that will replace the original symbol. e.g. my-project/my-var-2

  `dry-run` make no changes to files, only report.

  `settings` map of settings following https://clojure-lsp.github.io/clojure-lsp/settings/"
  [& {:keys [project-root settings from to] :as options}]
  {:pre [(instance? File project-root)
         (.exists project-root)
         (or (nil? settings)
             (map? settings))
         (and (symbol? from)
              (qualified-ident? from))
         (and (symbol? to)
              (qualified-ident? to))]}
  (internal-api/rename! options))
