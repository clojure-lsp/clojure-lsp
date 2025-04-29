(ns clojure-lsp.main
  (:refer-clojure :exclude [run!])
  (:require
   [babashka.cli :as cli]
   borkdude.dynaload
   [clojure-lsp.internal-api :as internal-api]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.server :as server]
   [clojure-lsp.shared :as shared]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.cli :as tools.cli]
   [pod.clojure-lsp.api :as pod])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn ^:private version []
  (->> [(str "clojure-lsp " (shared/clojure-lsp-version))
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
        "  references           Find all references of a full qualified symbol across the project and/or dependencies, use --from option."
        "  dump (experimental)  Dump all project known data including classpath, source-paths, dep-graph and clj-kondo analysis data."
        ""
        ;; "Run \"clojure-lsp help <command>\" for more information about a command."
        "See https://clojure-lsp.io/settings/ for detailed documentation."]
       (string/join \newline)))

(def ^:private trace-levels
  #{"off" "messages" "verbose"})

(defn ^:private cli-options []
  [["-h" "--help" "Print the available commands and its options"]
   [nil "--version" "Print clojure-lsp version"]
   [nil "--verbose" "Use stdout for clojure-lsp logs instead of default log settings"]
   [nil "--trace" "Deprecated: use --trace-level instead."]
   [nil "--trace-level LEVEL" "Enable trace logs between client and server, for debugging. Set to 'messages' for basic traces, or 'verbose' for more detailed traces. Defaults to 'off' for no traces."
    :default "off"
    :validate [trace-levels (str "Must be in " trace-levels)]]
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
   [nil "--filenames FILENAMES" "Optional filenames to apply the action. Filenames can be either absolute/relatetive files or directories. This flag accepts filenames separated by comma or double colon."
    :id :filenames
    :validate [#(not (string/includes? (str %) " ")) "Filenames should be separated by comma or double colon."]
    :assoc-fn #(assoc %1 %2 (->> (if (string/includes? %3 ",")
                                   (string/split %3 #",")
                                   (string/split %3 #":"))
                                 (map io/file)))]
   [nil "--ns-exclude-regex REGEX" "Optional regex representing the namespaces to be excluded during a command"
    :id :ns-exclude-regex
    :parse-fn re-pattern
    :validate [#(instance? java.util.regex.Pattern %) "Specify a valid string regex after --ns-exclude-regex"]]
   ["-o" "--output EDN" "Optional settings as edn on how the result should be printed. Check `clojure-lsp.api/diagnostics`/`clojure-lsp.api/dump` for all available options to this flag."
    :id :output
    :validate [#(try (edn/read-string %) true (catch Exception _ false))
               "Invalid --output EDN"]
    :assoc-fn #(assoc %1 %2 (edn/read-string %3))]
   [nil "--from FROM" "Full qualified symbol name or ns only, e.g. my-project/my-var. option for rename/references"
    :id :from
    :parse-fn symbol
    :validate [symbol? "Specify a valid clojure full qualified symbol or the namespace after --from"]]
   [nil "--to TO" "Full qualified symbol name or ns only, e.g. my-project/my-var. option for rename"
    :id :to
    :parse-fn symbol
    :validate [symbol? "Specify a valid clojure full qualified symbol or the namespace after --to"]]
   [nil "--analysis EDN" "Optional settings as edn on how clojure-lsp should consider the analysis. Check `clojure-lsp.api/dump` for all available options to this flag."
    :id :analysis
    :validate [#(try (edn/read-string %) true (catch Exception _ false))
               "Invalid --analysis EDN"]
    :assoc-fn #(assoc %1 %2 (edn/read-string %3))]])

(def cli-spec
  {:order [:help :version :verbose :trace :trace-level :settings :log-path :dry :raw :project-root :namespace :filenames
           :ns-exclude-regex :output :from :to :analysis]
   :spec {:help {:alias :h
                 :desc "Print the available commands and its options"}
          :version {:desc "Print clojure-lsp version"}
          :verbose {:desc "Use stdout for clojure-lsp logs instead of default log settings"}
          :trace {:desc "Deprecated: use --trace-level instead."}
          :trace-level {:ref "<LEVEL>"
                        :desc "Enable trace logs between client and server, for debugging. Set to 'messages' for basic traces, or 'verbose' for more detailed traces. Defaults to 'off' for no traces."
                        :default "off"
                        :validate {:pred trace-levels
                                   :ex-msg (fn [{:keys [_option _value]}]
                                             (format "Must be in %s" trace-levels))}}
          :settings {:alias :s
                     :ref "<EDN>"
                     :desc "Optional settings as EDN to use for the specified command. For all available settings, check https://clojure-lsp.io/settings"
                     :coerce :edn}
          :log-path {:ref "<PATH>"
                     :desc "Path to use as the log path for clojure-lsp.out, debug purposes only."}
          :dry {:desc "Make no changes to files, only report diffs"
                :default false}
          :raw {:desc "Print only necessary data"
                :default false}
          :project-root {:alias :p
                         :ref "<PATH>"
                         :desc "Specify the path to the project root to clojure-lsp consider during analysis startup."
                         :coerce io/file
                         :validate #(-> % io/file .exists)}
          :namespace {:alias :n
                      :ref "<NS>"
                      :desc "Optional namespace to apply the action, all if not supplied. This flag accepts multiple values"
                      :default []
                      :coerce [:symbol]}
          :filenames {:ref "<FILENAMES>"
                      :desc "Optional filenames to apply the action. FILENAMES can be either absolute/relatetive files or directories. This flag accepts filenames separated by comma or double colon."
                      :validate (fn [files] (println (pr-str files) (every? #(.exists ^java.io.File %) files)) (every? #(.exists ^java.io.File %) files))
                      :coerce #(->> (if (string/includes? % ",")
                                      (string/split % #",")
                                      (string/split % #":"))
                                    (mapv io/file))}
          :ns-exclude-regex {:ref "<REGEX>"
                             :desc "Optional regex representing the namespaces to be excluded during a command"
                             :coerce re-pattern
                             :validate #(instance? java.util.regex.Pattern %)}
          :output {:alias :o
                   :ref "<EDN>"
                   :desc "Optional settings as edn on how the result should be printed. Check `clojure-lsp.api/diagnostics`/`clojure-lsp.api/dump` for all available options to this flag."
                   :coerce :edn}
          :from {:ref "<FQNS>"
                 :desc "Full qualified symbol name or ns only, e.g. my-project/my-var. option for rename/references"
                 :coerce :symbol}
          :to {:ref "<FQNS>"
               :desc "Full qualified symbol name or ns only, e.g. my-project/my-var. option for rename"
               :coerce :symbol}
          :analysis {:ref "<EDN>"
                     :desc "Optional settings as edn on how clojure-lsp should consider the analysis. Check `clojure-lsp.api/dump` for all available options to this flag."
                     :coerce :edn}}})

(comment

  (cli/parse-args ["diagnostics"  "--to"] cli-spec)

  (parse-opts ["diagnostics" "--to"])

  (parse ["--settings" "1"])

  (parse ["--analysis" "{:a {:b 1} :c 2"])
  (tools.cli/parse-opts ["--filenames" "deps.edn,cli"] (cli-options))
  (cli/parse-args ["--filenames" "deps.edn,src"] cli-spec)

  (:errors (tools.cli/parse-opts ["dump"] (cli-options))))

(defn ^:private error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn ^:private parse-opts
  [args]
  ;; (tools.cli/parse-opts args (cli-options))
  (let [errors (atom [])
        {:keys [args opts]} (cli/parse-args args (assoc cli-spec
                                                        :error-fn (fn [error] (swap! errors conj error))))]
    {:options (-> opts
                  (assoc :dry? (:dry opts))
                  (dissoc :dry)
                  (assoc :raw? (:raw opts))
                  (dissoc :raw))
     :arguments args
     :errors (when (seq @errors)
               (map (fn [{:keys [spec type cause msg option value] :as data}]
                      (when (= :org.babashka/cli type)
                        (case cause
                          :require
                          (format "Missing required argument: %s\n" option)
                          :validate
                          (format "Failed to validate \"--%s %s\": %s"
                                  (name option)
                                  value
                                  msg))))
                    @errors))
     :summary (cli/format-opts cli-spec)}))

(defn ^:private parse [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args)
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
           (#{"clean-ns" "diagnostics" "format" "rename" "references" "dump" "listen"} (first arguments)))
      {:action (first arguments) :options options}

      :else
      {:exit-message (help summary)})))

(defn ^:private exit [status msg]
  (when msg
    (println msg))
  (System/exit (or status 1)))

(defn ^:private with-required-options [options required fn]
  (doseq [option required]
    (when-not (get options option)
      (exit 1 (format "Missing required %s option for this command" option))))
  (when (every? options required)
    (apply fn [options])))

(defn ^:private handle-action!
  [action options]
  (if (= "listen" action)
    (let [finished @(server/run-lsp-io-server! (:trace-level options) (:log-path options))]
      {:result-code (if (= :done finished) 0 1)})
    (try
      (case action
        "pod" (pod/run-pod)
        "clean-ns" (internal-api/clean-ns! options)
        "diagnostics" (internal-api/diagnostics options)
        "format" (internal-api/format! options)
        "rename" (with-required-options
                   options
                   [:from :to]
                   internal-api/rename!)
        "references" (with-required-options
                       options
                       [:from]
                       internal-api/references)
        "dump" (internal-api/dump options))
      (catch clojure.lang.ExceptionInfo e
        (ex-data e)))))

(defn run!
  "Entrypoint for clojure-lsp CLI, Use `clojure-lsp.api` for a better API usage."
  [& args]
  (let [{:keys [action options exit-message ok?]} (parse args)]
    (if exit-message
      {:result-code (if ok? 0 1)
       :message-fn (constantly  exit-message)}
      (handle-action! action options))))

(defn main [& args]
  (let [{:keys [result-code message-fn]} (apply run! args)]
    (exit result-code (when message-fn (message-fn)))))

(def musl?
  "Captured at compile time, to know if we are running inside a
  statically compiled executable with musl."
  (and (= "true" (System/getenv "CLOJURE_LSP_STATIC"))
       (= "true" (System/getenv "CLOJURE_LSP_MUSL"))))

(defmacro run [args]
  (if musl?
    ;; When running in musl-compiled static executable we lift execution of clojure-lsp
    ;; inside a thread, so we have a larger than default stack size, set by an
    ;; argument to the linker. See https://github.com/oracle/graal/issues/3398
    `(let [v# (volatile! nil)
           f# (fn []
                (vreset! v# (apply main ~args)))]
       (doto (Thread. nil f# "main")
         (.start)
         (.join))
       @v#)
    `(apply main ~args)))

(defn -main
  [& args]
  (run args))
