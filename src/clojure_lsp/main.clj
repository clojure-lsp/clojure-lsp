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
        "Options:"
        options-summary
        ""
        "Available commands:"
        "  listen (or empty)    Start clojure-lsp as server, listening to stdin."
        "  clean-ns             Organize ns form, removing unused requires/refers/imports and sorting alphabetically."
        ""
        "Run \"clojure-lsp help <command>\" for more information about a command."
        "See https://clojure-lsp.github.io/clojure-lsp/settings/ for detailed documentation."]
       (string/join \newline)))

(def ^:private cli-options
  [["-h" "--help" "Print the available commands and its options"]
   [nil "--version" "Print clojure-lsp version"]
   ["-s" "--settings SETTINGS" "Optional settings as edn to use for the specified command. For all available settings, check https://clojure-lsp.github.io/clojure-lsp/settings"
    :id :settings
    :validate [#(try (edn/read-string %) true (catch Exception _ false))
               "Invalid --settings EDN"]
    :assoc-fn #(assoc %1 %2 (edn/read-string %3))]
   ["-p" "--project-root PATH" "Specify the path to the project root to clojure-lsp consider during analysis startup."
    :id :project-root
    :default (io/file (System/getProperty "user.dir"))
    :parse-fn io/file
    :validate [#(.exists %) "Specify a valid path after --project-root"]]
   ["-n" "--namespace NS" "Optional namespace to apply the action, all if not supplied. This flag accepts multiple values"
    :id :namespace
    :default []
    :parse-fn symbol
    :multi true
    :update-fn conj]])

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
           (#{"clean-ns" "listen"} (first arguments)))
      {:action (first arguments) :options options}

      :else
      {:exit-message (help summary)})))

(defn ^:private exit [status msg]
  (println msg)
  (System/exit status))

(defn ^:private handle-action!
  [action options]
  (swap! db/db assoc :cli? true)
  (case action
    "listen" (with-out-str (server/run-server!))
    "clean-ns" (internal-api/clean-ns! options)))

(defn -main [& args]
  (logging/setup-logging)
  (let [{:keys [action options exit-message ok?]} (parse args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (handle-action! action options))))
