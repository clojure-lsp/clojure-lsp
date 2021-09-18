(ns clojure-lsp.api
  "Entrypoint for main clojure-lsp features"
  (:require
   [clojure-lsp.api :as api]
   [clojure-lsp.db :as db]
   [clojure-lsp.internal-api :as internal-api]
   [clojure-lsp.logging :as logging])
  (:import
   [java.io File]))

(defmacro ^:private safe-process-message
  [options & body]
  `(try
     (let [~'_result ~@body]
       (when-let [~'_message (and (not (:raw? ~options))
                                  (:message ~'_result))]
         (println ~'_message))
       ~'_result)
     (catch clojure.lang.ExceptionInfo e#
       (some-> e# ex-data :message println)
       e#)))

(defn analyze-project!
  "Analyze project caching analysis for future API calls. Useful for REPL usage for example.
  Options:

  `:project-root` a java.io.File representing the project root.

  `settings` map of settings following https://clojure-lsp.io/settings/"
  [{:keys [project-root settings] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists project-root)))
         (or (nil? settings)
             (map? settings))]}
  (logging/setup-logging db/db)
  (safe-process-message
    options
    (internal-api/analyze-project! options)))

(defn clean-ns!
  "Organize ns form, removing unused requires/refers/imports and sorting
  alphabetically.
  Options:

  `:project-root` a java.io.File representing the project root.

  `:namespace` a coll of symbols representing the namespaces which should be cleaned,
  if empty all project namespaces will be considered.

  `:ns-exclude-regex` a string regex representing the namespaces that should be excluded during this call.

  `dry?` a boolean, when enabled make no changes to files, only report.

  `settings` map of settings following https://clojure-lsp.io/settings/"
  [{:keys [project-root settings namespace ns-exclude-regex] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists project-root)))
         (or (nil? settings)
             (map? settings))
         (or (nil? namespace)
             (coll? namespace))
         (or (nil? ns-exclude-regex)
             (instance? java.util.regex.Pattern ns-exclude-regex))]}
  (logging/setup-logging db/db)
  (safe-process-message
    options
    (internal-api/clean-ns! options)))

(defn diagnostics
  "Analyze the project and find all diagnostics (warnings, errors).
  Options:

  `:project-root` a java.io.File representing the project root.

  `:namespace` a coll of symbols representing the namespaces which should be cleaned,
  if empty all project namespaces will be considered.

  `:ns-exclude-regex` a string regex representing the namespaces that should be excluded during this call.

  `:output` a map with options on how the result should be printed, available values are:
    `:canonical-paths` a boolean if the path should be absolute or not, defaults to false.

  `settings` map of settings following https://clojure-lsp.github.io/clojure-lsp/settings/"
  [{:keys [project-root settings] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists project-root)))
         (or (nil? settings)
             (map? settings))]}
  (logging/setup-logging db/db)
  (safe-process-message
    options
    (internal-api/diagnostics options)))

(defn format!
  "Format one or more namespaces using cljfmt.
  Options:

  `:project-root` a java.io.File representing the project root.

  `:namespace` a coll of symbols representing the namespaces which should be cleaned,
  if empty all project namespaces will be considered.

  `dry?` a boolean, when enabled make no changes to files, only report.

  `:ns-exclude-regex` a string regex representing the namespaces that should be excluded during this call.

  `settings` map of settings following https://clojure-lsp.io/settings/"
  [{:keys [project-root settings namespace ns-exclude-regex] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists project-root)))
         (or (nil? settings)
             (map? settings))
         (or (nil? namespace)
             (coll? namespace))
         (or (nil? ns-exclude-regex)
             (instance? java.util.regex.Pattern ns-exclude-regex))]}
  (logging/setup-logging db/db)
  (safe-process-message
    options
    (internal-api/format! options)))

(defn rename!
  "Rename a symbol and its definitions across the project.
  Options:

  `:project-root` a java.io.File representing the project root.

  `:from` the full qualified symbol origin name that should be renamed. e.g. my-project/my-var or my-project.foo for namespaces

  `:to` the full qualified symbol that will replace the original symbol. e.g. my-project/my-var-2 or my-project.bar for namespaces

  `dry?` a boolean, when enabled make no changes to files, only report.

  `settings` map of settings following https://clojure-lsp.io/settings/"
  [{:keys [project-root settings from to] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists project-root)))
         (or (nil? settings)
             (map? settings))
         (symbol? from)
         (symbol? to)
         (or (not (simple-symbol? from))
             (simple-symbol? to))
         (or (not (simple-symbol? to))
             (simple-symbol? from))]}
  (logging/setup-logging db/db)
  (safe-process-message
    options
    (internal-api/rename! options)))
