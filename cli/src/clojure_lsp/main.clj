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

(def cli-spec
  {:order [:help :version :verbose :trace :trace-level :settings :log-path :dry :raw :project-root :namespace :filenames
           :ns-exclude-regex :output :from :to :analysis :diff]
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
                     :coerce #(try (edn/read-string %) (catch Exception _ %))
                     :validate {:pred map?
                                :ex-msg (fn [_] "Invalid --settings EDN")}}
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
                         :validate {:pred #(-> % io/file .exists)
                                    :ex-msg (fn [_] "Specify a valid path after --project-root")}}
          :namespace {:alias :n
                      :ref "<NS>"
                      :desc "Optional namespace to apply the action, all if not supplied. This flag accepts multiple values"
                      :default []
                      :coerce [:symbol]}
          :filenames {:ref "<FILENAMES>"
                      :desc "Optional filenames to apply the action. FILENAMES can be either absolute/relatetive files or directories. This flag accepts filenames separated by comma or double colon."
                      :validate {:pred (fn [files]
                                         (every? #(not (string/includes? (str %) " ")) files))
                                 :ex-msg (fn [_] "Filenames should be separated by comma or double colon.")}
                      :coerce #(mapv io/file
                                     (if (string/includes? % ",")
                                       (string/split % #",")
                                       (string/split % #":")))}
          :ns-exclude-regex {:ref "<REGEX>"
                             :desc "Optional regex representing the namespaces to be excluded during a command"
                             :coerce #(try (re-pattern %) (catch Exception _ %))
                             :validate {:pred #(instance? java.util.regex.Pattern %)
                                        :ex-msg (fn [{:keys [value]}]
                                                  (try (re-pattern value) (catch Exception e (ex-message e))))}}
          :output {:alias :o
                   :ref "<EDN>"
                   :desc "Optional settings as edn on how the result should be printed. Check `clojure-lsp.api/diagnostics`/`clojure-lsp.api/dump` for all available options to this flag."
                   :coerce #(try (edn/read-string %) (catch Exception _ %))
                   :validate {:pred map?
                              :ex-msg (fn [_] "Invalid --output EDN")}}
          :from {:ref "<FQNS>"
                 :desc "Full qualified symbol name or ns only, e.g. my-project/my-var. option for rename/references"
                 :coerce :symbol}
          :to {:ref "<FQNS>"
               :desc "Full qualified symbol name or ns only, e.g. my-project/my-var. option for rename"
               :coerce :symbol}
          :analysis {:ref "<EDN>"
                     :desc "Optional settings as edn on how clojure-lsp should consider the analysis. Check `clojure-lsp.api/dump` for all available options to this flag."
                     :coerce #(try (edn/read-string %) (catch Exception _ %))
                     :validate {:pred map?
                                :ex-msg (fn [_] "Invalid --analysis EDN")}}
          :diff {:ref "[REV_RANGE]"
                 :desc "Enable code diagnostics focused on the changes between revisions. [REV_RANGE] is the git revision range to be used. Defaults to `origin/HEAD`."
                 :alias :d
                 :validate {:pred #(or (true? %)
                                       (re-matches #"^[\w navigating around the git history \-./~^@{}]+(?:(?:\.\.|\.\.\.)[\w navigating around the git history \-./~^@{}]+)?$" %))
                            :ex-msg (fn [{:keys [value]}]
                                      (format "Invalid git revision range: %s" value))}}}})

(defn ^:private error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn ^:private parse-opts
  [args]
  (let [errors (atom [])
        {:keys [args opts]} (cli/parse-args args (assoc cli-spec
                                                        :error-fn (fn [error] (swap! errors conj error))))]
    {:options (-> opts
                  (update :diff #(cond (true? %) "origin/HEAD"
                                       (nil? %) false
                                       :else %))
                  (assoc :dry? (:dry opts))
                  (dissoc :dry)
                  (assoc :raw? (:raw opts))
                  (dissoc :raw))
     :arguments args
     :errors (when (seq @errors)
               (map (fn [{:keys [_spec type cause msg option value] :as _data}]
                      (when (= :org.babashka/cli type)
                        (case cause
                          :require
                          (format "Missing required argument: %s\n" option)
                          :validate
                          (case option
                            :ns-exclude-regex
                            (format "Error while parsing option \"--ns-exclude-regex %s\": %s"
                                    value
                                    msg)
                            :filenames
                            (format "Failed to validate \"--%s %s\": %s"
                                    (name option)
                                    (string/join " " (map str value))
                                    msg)
                            (format "Failed to validate \"--%s %s\": %s"
                                    (name option)
                                    value
                                    msg)))))
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
