(ns clojure-lsp.main
  (:refer-clojure :exclude [run!])
  (:require
   borkdude.dynaload
   [clojure-lsp.config :as config]
   [clojure-lsp.db :as db]
   [clojure-lsp.internal-api :as internal-api]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.logging :as logging]
   [clojure-lsp.server :as server]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.cli :refer [parse-opts]]
   [pod.clojure-lsp.api :as pod]
   [taoensso.timbre :as log])
  (:import
   [clojure_lsp WarningLogDisabler])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn ^:private version []
  (->> [(str "clojure-lsp " (config/clojure-lsp-version))
        (str "clj-kondo " (lsp.kondo/clj-kondo-version))]
       (string/join \newline)))

(defn ^:private help [options-summary]
  (->> ["Clojure development tool implementing LSP"
        ""
        "Usage: clojure-lsp <command> [<options>]"
        ""
        "All options:"
        options-summary
        ""
        "Available commands:"
        "  listen (or empty)    Start clojure-lsp as server, listening to stdin."
        "  clean-ns             Organize ns form, removing unused requires/refers/imports and sorting alphabetically."
        "  diagnostics          Analyze the project and find all diagnostics (warnings, errors)."
        "  format               Format code using cljfmt."
        "  rename               Rename a symbol and all references across the project, use --from and --to options."
        ""
        ;; "Run \"clojure-lsp help <command>\" for more information about a command."
        "See https://clojure-lsp.io/settings/ for detailed documentation."]
       (string/join \newline)))

(defn ^:private cli-options []
  [["-h" "--help" "Print the available commands and its options"]
   [nil "--version" "Print clojure-lsp version"]
   [nil "--verbose" "Use stdout for clojure-lsp logs instead of default log settings"]
   ["-s" "--settings SETTINGS" "Optional settings as edn to use for the specified command. For all available settings, check https://clojure-lsp.io/settings"
    :id :settings
    :validate [#(try (edn/read-string %) true (catch Exception _ false))
               "Invalid --settings EDN"]
    :assoc-fn #(assoc %1 %2 (edn/read-string %3))]
   [nil "--log-path PATH" "Path to use as the log path for clojure-lsp.out, debug purposes only."
    :id :log-path]
   [nil "--dry" "Make no changes to files, only report diffs"
    :id :dry?
    :default false]
   [nil "--raw" "Print only necessary data"
    :id :raw?
    :default false]
   ["-p" "--project-root PATH" "Specify the path to the project root to clojure-lsp consider during analysis startup."
    :id :project-root
    :parse-fn io/file
    :validate [#(-> % io/file .exists) "Specify a valid path after --project-root"]]
   ["-n" "--namespace NS" "Optional namespace to apply the action, all if not supplied. This flag accepts multiple values"
    :id :namespace
    :default []
    :parse-fn symbol
    :multi true
    :update-fn conj]
   [nil "--ns-exclude-regex REGEX" "Optional regex representing the namespaces to be excluded during a command"
    :id :ns-exclude-regex
    :parse-fn re-pattern
    :validate [#(instance? java.util.regex.Pattern %) "Specify a valid string regex after --ns-exclude-regex"]]
   ["-o" "--output EDN" "Optional settings as edn on how the result should be printed. Check `clojure-lsp.api/diagnostics` for all available options to this flag."
    :id :output
    :validate [#(try (edn/read-string %) true (catch Exception _ false))
               "Invalid --output EDN"]
    :assoc-fn #(assoc %1 %2 (edn/read-string %3))]
   [nil "--from FROM" "Full qualified symbol name or ns only, e.g. my-project/my-var. option for rename"
    :id :from
    :parse-fn symbol
    :validate [symbol? "Specify a valid clojure full qualified symbol or the namespace after --from"]]
   [nil "--to TO" "Full qualified symbol name or ns only, e.g. my-project/my-var. option for rename"
    :id :to
    :parse-fn symbol
    :validate [symbol? "Specify a valid clojure full qualified symbol or the namespace after --to"]]])

(defn ^:private error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn ^:private parse [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args (cli-options))
        pod? (= "true" (System/getenv "BABASHKA_POD"))]
    (cond
      pod?
      {:action "pod" :options options}

      (:help options)
      {:exit-message (help summary) :ok? true}

      (:version options)
      {:exit-message (version) :ok? true}

      errors
      {:exit-message (error-msg errors)}

      (= 0 (count arguments))
      {:action "listen" :options options}

      (and (= 1 (count arguments))
           (#{"clean-ns" "diagnostics" "format" "rename" "listen"} (first arguments)))
      {:action (first arguments) :options options}

      :else
      {:exit-message (help summary)})))

(defn ^:private exit [status msg]
  (when msg
    (println msg))
  (System/exit status))

(defn ^:private with-required-options [options required fn]
  (doseq [option required]
    (when-not (get options option)
      (exit 1 (format "Missing required %s option for this command" option))))
  (when (every? options required)
    (apply fn [options])))

(defn ^:private handle-action!
  [action options]
  (if (= "listen" action)
    (do
      (with-out-str @(server/run-server!))
      {:exit-code 0})
    (try
      (case action
        "pod" (pod/run-pod)
        "clean-ns" (internal-api/clean-ns! options)
        "diagnostics" (internal-api/diagnostics options)
        "format" (internal-api/format! options)
        "rename" (with-required-options
                   options
                   [:from :to]
                   internal-api/rename!))
      (catch clojure.lang.ExceptionInfo e
        (ex-data e)))))

(defn ^:private disable-warnings
  "LMDB has a illegal access warning that mess up clojure-lsp output."
  []
  (WarningLogDisabler/disable))

(defn run!
  "Entrypoint for clojure-lsp CLI, Use `clojure-lsp.api` for a better API usage."
  [& args]
  (logging/setup-logging db/db)
  (disable-warnings)
  (let [{:keys [action options exit-message ok?]} (parse args)]
    (if exit-message
      {:result-code (if ok? 0 1)
       :message exit-message}
      (handle-action! action options))))

(defn -main [& args]
  (let [{:keys [result-code message]} (apply run! args)]
    (exit result-code message)))
