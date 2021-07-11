(ns clojure-lsp.main
  (:require
   borkdude.dynaload
   [clojure-lsp.config :as config]
   [clojure-lsp.db :as db]
   [clojure-lsp.internal-api :as internal-api]
   [clojure-lsp.logging :as logging]
   [clojure-lsp.server :as server]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(defn ^:private version []
  (->> [(str "clojure-lsp " config/clojure-lsp-version)
        (str "clj-kondo " config/clj-kondo-version)]
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
        "  format               Format code using cljfmt."
        "  rename               Rename a symbol and all references across the project, use --from and --to options."
        ""
        ;; "Run \"clojure-lsp help <command>\" for more information about a command."
        "See https://clojure-lsp.github.io/clojure-lsp/settings/ for detailed documentation."]
       (string/join \newline)))

(def ^:private cli-options
  [["-h" "--help" "Print the available commands and its options"]
   [nil "--version" "Print clojure-lsp version"]
   [nil "--verbose" "Use stdout for clojure-lsp logs instead of default log settings"]
   ["-s" "--settings SETTINGS" "Optional settings as edn to use for the specified command. For all available settings, check https://clojure-lsp.github.io/clojure-lsp/settings"
    :id :settings
    :validate [#(try (edn/read-string %) true (catch Exception _ false))
               "Invalid --settings EDN"]
    :assoc-fn #(assoc %1 %2 (edn/read-string %3))]
   [nil "--log-path PATH" "Path to use as the log path for clojure-lsp.out, debug purposes only."
    :id :log-path]
   [nil "--dry" "Make no changes to files, only report"
    :id :dry?
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
   [nil "--from FROM" "Full qualified symbol name, e.g. my-project/my-var"
    :id :from
    :parse-fn symbol
    :validate [qualified-ident? "Specify a valid clojure full qualified symbol after --from"]]
   [nil "--to TO" "Full qualified symbol name, e.g. my-project/my-var"
    :id :to
    :parse-fn symbol
    :validate [qualified-ident? "Specify a valid clojure full qualified symbol after --to"]]])

(defn ^:private error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn ^:private parse [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit-message (help summary) :ok? true}

      (:version options)
      {:exit-message (version) :ok? true}

      errors
      {:exit-message (error-msg errors)}

      (= 0 (count arguments))
      {:action "listen" :options options}

      (and (= 1 (count arguments))
           (#{"clean-ns" "format" "rename" "listen"} (first arguments)))
      {:action (first arguments) :options options}

      :else
      {:exit-message (help summary)})))

(defn ^:private exit [status msg]
  (println msg)
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
    (with-out-str (server/run-server!))
    (do
      (swap! db/db assoc :cli? true)
      (let [result
            (case action
              "clean-ns" (internal-api/clean-ns! options)
              "format" (internal-api/format! options)
              "rename" (with-required-options
                         options
                         [:from :to]
                         internal-api/rename!))]
        (exit (:result-code result) (:message result))))))

(defn -main [& args]
  (logging/setup-logging)
  (let [{:keys [action options exit-message ok?]} (parse args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (handle-action! action options))))
