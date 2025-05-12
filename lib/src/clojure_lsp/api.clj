(ns clojure-lsp.api
  "Entrypoint for main clojure-lsp features"
  (:require
   [clojure-lsp.internal-api :as internal-api])
  (:import
   [java.io File]))

(set! *warn-on-reflection* true)

(defmacro ^:private safe-process-message
  [options & body]
  `(try
     (let [~'_result ~@body]
       (when-let [~'_message-fn (and (not (:raw? ~options))
                                     (:message-fn ~'_result))]
         (println (~'_message-fn)))
       ~'_result)
     (catch clojure.lang.ExceptionInfo e#
       (some-> e# ex-data :message-fn (apply []) println)
       e#)))

(defn analyze-project-and-deps!
  "Analyze whole project and all external dependencies
  caching analysis for future API calls. Useful for REPL
  usage for example.

  This will analyze the whole project and external dependencies with
  clj-kondo caching its analysis for next other API calls.
  All features need analysis and will call this internally if the project
  was not analyzed before.

  **Options**
  All options below are optional, using its default if not provided.

  `:project-root` a java.io.File representing the project root.

  `settings` map of settings following https://clojure-lsp.io/settings/

  **Example**

  ```clojure
  (clojure-lsp.api/analyze-project-and-deps! {:project-root (io/file \".\")
                                              :settings {:classpath-config-paths [\"other-company/other-project\"]}})
  ```"
  [{:keys [project-root settings] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists ^File project-root)))
         (or (nil? settings)
             (map? settings))]}
  (safe-process-message
    options
    (internal-api/analyze-project-and-deps! options)))

(defn analyze-project-only!
  "Analyze whole project only caching analysis for future API calls. Useful for REPL
  usage for example.

  This will analyze the whole project and external dependencies with
  clj-kondo caching its analysis for next other API calls.
  All features need analysis and will call this internally if the project
  was not analyzed before.

  **Options**
  All options below are optional, using its default if not provided.

  `:project-root` a java.io.File representing the project root.

  `settings` map of settings following https://clojure-lsp.io/settings/

  **Example**

  ```clojure
  (clojure-lsp.api/analyze-project-only! {:project-root (io/file \".\")
                                          :settings {:classpath-config-paths [\"other-company/other-project\"]}})
  ```"
  [{:keys [project-root settings] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists ^File project-root)))
         (or (nil? settings)
             (map? settings))]}
  (safe-process-message
    options
    (internal-api/analyze-project-only! options)))

(defn clean-ns!
  "Organize `ns` form, removing unused requires/refers/imports and sorting
  alphabetically by default.

  **Options**
  All options below are optional, using its default if not provided.

  `:project-root` a java.io.File representing the project root.

  `:namespace` a coll of symbols representing the namespaces which should be cleaned,
  if empty all project namespaces will be considered.

  `:filenames` a coll of files representing the files which should be cleaned.

  `:ns-exclude-regex` a string regex representing the namespaces that should be excluded during this call.

  `dry?` a boolean, when enabled make no side-effects (no changes to files), only report.

  `settings` map of settings following https://clojure-lsp.io/settings/

  **Example**

  ```clojure
  (clojure-lsp.api/clean-ns! {:namespace '[my-project.foo my-project.bar]})
  ```"
  [{:keys [project-root settings namespace filenames ns-exclude-regex] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists ^File project-root)))
         (or (nil? settings)
             (map? settings))
         (or (nil? namespace)
             (coll? namespace))
         (or (nil? filenames)
             (coll? filenames))
         (or (nil? ns-exclude-regex)
             (instance? java.util.regex.Pattern ns-exclude-regex))]}
  (safe-process-message
    options
    (internal-api/clean-ns! options)))

(defn diagnostics
  "Find all project diagnostics (warnings, errors and infos).
  Returns all clj-kondo lint plus custom linters configured by clojure-lsp like
  clojure-lsp/unused-public-var for example.

  **Options**
  All options below are optional, using its default if not provided.

  `:project-root` a java.io.File representing the project root.

  `:namespace` a coll of symbols representing the namespaces which should be checked for diagnostics,
  if empty all project namespaces will be considered.

  `:filenames` a coll of files representing the files which should be checked for diagnostics.

  `:ns-exclude-regex` a string regex representing the namespaces that should be excluded during this call.

  `:output` a map with options on how the result should be printed, available values are:
    `:canonical-paths` a boolean if the path should be absolute or not, defaults to false.

  `settings` map of settings following https://clojure-lsp.github.io/clojure-lsp/settings/

  **Example**

  ```clojure
  (clojure-lsp.api/diagnostics {:namespace '[my-project.foo my-project.bar]
                                :output {:canonical-paths true}})
  ```"
  [{:keys [project-root namespace filenames settings] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists ^File project-root)))
         (or (nil? namespace)
             (coll? namespace))
         (or (nil? filenames)
             (coll? filenames))
         (or (nil? settings)
             (map? settings))]}
  (safe-process-message
    options
    (internal-api/diagnostics options)))

(defn format!
  "Format one or more namespaces using cljfmt internally.

  **Options**
  All options below are optional, using its default if not provided.

  `:project-root` a java.io.File representing the project root.

  `:namespace` a coll of symbols representing the namespaces which should be formatted,
  if empty all project namespaces will be considered.

  `:filenames` a coll of files representing the files which should be formatted.

  `dry?` a boolean, when enabled make no side-effects (no changes to files), only report.

  `:ns-exclude-regex` a string regex representing the namespaces that should be excluded during this call.

  `settings` map of settings following https://clojure-lsp.io/settings/

  **Example**

  ```clojure
  (clojure-lsp.api/format! {:namespace '[my-project.foo my-project.bar]})
  ```"
  [{:keys [project-root settings namespace filenames ns-exclude-regex] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists ^File project-root)))
         (or (nil? settings)
             (map? settings))
         (or (nil? namespace)
             (coll? namespace))
         (or (nil? filenames)
             (coll? filenames))
         (or (nil? ns-exclude-regex)
             (instance? java.util.regex.Pattern ns-exclude-regex))]}
  (safe-process-message
    options
    (internal-api/format! options)))

(defn rename!
  "Rename a symbol and its definitions across the project.
  The symbol can be a full qualified symbol or a namespace only.

  **Options**
  All options besides `:from` and `:to` below are optional, using its default if not provided.

  `:project-root` a java.io.File representing the project root.

  `:from` the full qualified symbol origin name that should be renamed. e.g. my-project.foo/my-var or my-project.foo for namespaces

  `:to` the full qualified symbol that will replace the original symbol. e.g. my-project.bar/my-var-2 or my-project.bar for namespaces

  `dry?` a boolean, when enabled make no side-effects (no changes to files), only report.

  `settings` map of settings following https://clojure-lsp.io/settings/

  **Example**

  ```clojure
  (clojure-lsp.api/rename! {:from 'my-project.some/foo
                            :to 'my-project.some/bar})
  ```"
  [{:keys [project-root settings from to] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists ^File project-root)))
         (or (nil? settings)
             (map? settings))
         (symbol? from)
         (symbol? to)
         (or (not (simple-symbol? from))
             (simple-symbol? to))
         (or (not (simple-symbol? to))
             (simple-symbol? from))]}
  (safe-process-message
    options
    (internal-api/rename! options)))

(defn references
  "Find all references of an symbol across the project and dependencies.
  The symbol can be a full qualified symbol or a namespace only.

  **Options**
  All options besides `:from` and `:to` below are optional, using its default if not provided.

  `:project-root` a java.io.File representing the project root.

  `:analysis` a map with options on how clojure-lsp should analyze the project, available values are:
    `:type` keyword values: `project-only`, `project-and-shallow-analysis`, `project-and-full-dependencies`. Default to `project-and-full-dependencies`.

  `:from` the full qualified symbol to find references. e.g. 'my-project.foo/my-var

  `settings` map of settings following https://clojure-lsp.io/settings/

  **Output**

  `result-code` an integer representing whether the action was successful, 0 means ok, 1 means error

  `message-fn` a function of no arity to be called if want the result as a stringfied version.

  `:result`
    `:references` a coll of all reference elements found for the `:from` symbol.

  **Example**

  ```clojure
  (clojure-lsp.api/references {:from 'my-project.some/foo})
  => {:result {:references [{:name ... :uri ...}]}
      :result-code 0}
  ```"
  [{:keys [project-root settings from] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists ^File project-root)))
         (or (nil? settings)
             (map? settings))
         (symbol? from)
         (qualified-symbol? from)]}
  (safe-process-message
    options
    (internal-api/references options)))

(defn dump
  "Dump all project known data including classpath, source-paths, dep-graph
  and clj-kondo analysis data.

  **Options**

  `:project-root` a java.io.File representing the project root.

  `:analysis` a map with options on how clojure-lsp should analyze the project, available values are:
    `:type` keyword values: `project-only`, `project-and-shallow-analysis`, `project-and-full-dependencies`. Default to `project-only`.

  `:output` a map with options on how the result should be printed, available values are:
    `:format` a keyword specifying in which format the data should be returned, defaults to `:edn`.
    `:filter-keys` a list of keywords in case you want only specific fields from output.

  `settings` map of settings following https://clojure-lsp.io/settings/

  **Output**

  `result-code` an integer representing whether the action was successful, 0 means ok, 1 means error

  `message-fn` a function of no arity to be called if want the result as a stringfied version.

  `:result`
    `:project-root` a string path representing the project root.
    `:source-paths` list of source-paths considered by clojure-lsp.
    `:classpath` list of paths of found classpath.
    `:analysis` clj-kondo analysis normalized for clojure-lsp usage.
    `:dep-graph` Dependency graph of namespaces relationship derived from clj-kondo analysis.
    `:diagnostics` diagnostics including all findings sources.
    `:settings` a map with all settings considered by clojure-lsp.
    `:clj-kondo-settings` a map with all clj-kondo config used by clojure-lsp.

  **Example**

  ```clojure
  (clojure-lsp.api/dump {:output {:format :edn
                                  :filter-keys [:source-paths :analysis]}})
  => {:result {:source-paths [...]
               :analysis {...}}
      :result-code 0}
  ```"
  [{:keys [project-root output settings] :as options}]
  {:pre [(or (nil? project-root)
             (and (instance? File project-root)
                  (.exists ^File project-root)))
         (or (nil? (:format output))
             (keyword? (:format output)))
         (or (nil? settings)
             (map? settings))]}
  (safe-process-message
    options
    (internal-api/dump options)))
