(ns clojure-lsp.api
  "Entrypoint for main clojure-lsp features"
  (:require
   [clojure-lsp.api :as api]
   [clojure-lsp.internal-api :as internal-api]
   [clojure-lsp.logging :as logging])
  (:import
   [java.io File]))

(defmacro ^:private safe-parsing-error
  [& body]
  `(try
     ~@body
     (catch clojure.lang.ExceptionInfo e#
       (some-> e# ex-data :message println)
       e#)))

(defn clean-ns!
  "Organize ns form, removing unused requires/refers/imports and sorting
  alphabetically.
  Options:

  `:project-root` a java.io.File representing the project root.

  `:namespace` a coll of symbols representing the namespaces which should be cleaned,
  if empty all project namespaces will be considered.

  `dry?` a boolean, when enabled make no changes to files, only report.

  `settings` map of settings following https://clojure-lsp.github.io/clojure-lsp/settings/"
  [{:keys [project-root settings namespace] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists project-root)))
         (or (nil? settings)
             (map? settings))
         (or (nil? namespace)
             (coll? namespace))]}
  (logging/setup-logging)
  (safe-parsing-error (internal-api/clean-ns! options)))

(defn format!
  "Format one or more namespaces using cljfmt.
  Options:

  `:project-root` a java.io.File representing the project root.

  `:namespace` a coll of symbols representing the namespaces which should be cleaned,
  if empty all project namespaces will be considered.

  `dry?` a boolean, when enabled make no changes to files, only report.

  `settings` map of settings following https://clojure-lsp.github.io/clojure-lsp/settings/"
  [{:keys [project-root settings namespace] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists project-root)))
         (or (nil? settings)
             (map? settings))
         (or (nil? namespace)
             (coll? namespace))]}
  (logging/setup-logging)
  (safe-parsing-error (internal-api/format! options)))

(defn rename!
  "Rename a symbol and its definitions across the project.
  Options:

  `:project-root` a java.io.File representing the project root.

  `:from` the full qualified symbol origin name that should be renamed. e.g. my-project/my-var

  `:to` the full qualified symbol that will replace the original symbol. e.g. my-project/my-var-2

  `dry?` a boolean, when enabled make no changes to files, only report.

  `settings` map of settings following https://clojure-lsp.github.io/clojure-lsp/settings/"
  [{:keys [project-root settings from to] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists project-root)))
         (or (nil? settings)
             (map? settings))
         (and (symbol? from)
              (qualified-ident? from))
         (and (symbol? to)
              (qualified-ident? to))]}
  (logging/setup-logging)
  (safe-parsing-error (internal-api/rename! options)))
