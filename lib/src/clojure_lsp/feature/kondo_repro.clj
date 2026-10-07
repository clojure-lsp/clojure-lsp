(ns clojure-lsp.feature.kondo-repro
  "Reproduces with clj-kondo only how clojure-lsp runs clj-kondo, helping to
  find out whether a diagnostic issue comes from clj-kondo or clojure-lsp."
  (:require
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.startup :as startup]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(set! *warn-on-reflection* true)

(def analysis-types
  #{:project-only :project-and-shallow-analysis :project-and-full-dependencies})

(def default-analysis-type :project-and-full-dependencies)

(defn ^:private ignored-filenames [db uris]
  (let [settings (settings/all db)]
    (->> uris
         (map shared/uri->filename)
         (filter #(shared/ignore-path? settings %)))))

(defn ^:private planned-runs
  "The clj-kondo runs of a startup without caches followed by the lint of each
  of `uris` when opened, mirroring `startup/initialize-project` and
  `kondo/run-kondo-on-text!`."
  [db uris]
  (let [settings (settings/all db)
        classpath (seq (:classpath db))
        root-path (shared/uri->path (:project-root-uri db))
        external-paths (when (and classpath (startup/external-classpath-analysis? db))
                         (-> (startup/external-classpath-paths root-path (startup/project-paths-to-analyze db) classpath)
                             (shared/generate-and-update-analysis-checksums nil nil)
                             :paths-not-on-checksum))
        batches (lsp.kondo/path-batches external-paths)
        source-files (vec (startup/project-source-files db))
        ignored (set (ignored-filenames db uris))]
    (concat
      (when (and classpath (startup/copy-kondo-configs? settings db))
        [{:id :copy-configs
          :description "Copy the clj-kondo configs exported by the classpath"
          :paths classpath}])
      (map-indexed (fn [index paths]
                     {:id :external-paths
                      :description (format "Analyze the external classpath, batch %s/%s" (inc index) (count batches))
                      :paths paths})
                   batches)
      (when (seq source-files)
        [{:id :internal-paths
          :description "Analyze and lint the project files"
          :paths source-files}])
      (for [uri uris
            :let [filename (shared/uri->filename uri)]
            :when (not (contains? ignored filename))]
        {:id :single-file
         :description (str "Lint " filename " like the editor does when opening it")
         :uri uri
         :stdin filename}))))

(defn steps
  "The clj-kondo runs, in order and with their `run!` options, of a clojure-lsp
  startup without caches for `analysis-type`, followed by the lint done when
  opening each of `uris` in an editor."
  [db {:keys [analysis-type uris] :or {analysis-type default-analysis-type}}]
  (let [db (-> db
               (assoc :project-analysis-type analysis-type)
               (dissoc :kondo-config))
        kondo-config (delay (lsp.kondo/project-kondo-config db))]
    (:steps
     (reduce (fn [{:keys [step-db] :as acc} {:keys [id paths uri] :as run}]
               (-> acc
                   (update :steps conj (-> run
                                           (dissoc :paths :uri)
                                           (assoc :options (lsp.kondo/run-options id step-db {:paths paths :uri uri}))))
                   ;; clojure-lsp keeps the clj-kondo config returned by the previous run
                   (assoc :step-db (assoc db :kondo-config @kondo-config))))
             {:steps [] :step-db db}
             (planned-runs db uris)))))

(defn repro
  "Data to reproduce the clj-kondo runs from `steps` with the clj-kondo CLI."
  [db {:keys [analysis-type uris] :or {analysis-type default-analysis-type} :as options}]
  (let [home-config (lsp.kondo/home-config-path)
        ignored (ignored-filenames db uris)]
    {:clojure-lsp-version (shared/clojure-lsp-version)
     :clj-kondo-version (lsp.kondo/clj-kondo-version)
     :clj-kondo-coordinate (lsp.kondo/clj-kondo-coordinate)
     :project-root (shared/uri->filename (:project-root-uri db))
     :analysis-type analysis-type
     :notes (cond-> []
              (empty? (:classpath db))
              (conj "No classpath found, so clojure-lsp only analyzes the project files.")

              (shared/file-exists? (io/file home-config))
              (conj (str "The home clj-kondo config " home-config " is used, like in clojure-lsp."))

              (seq ignored)
              (conj (str "Not linted by clojure-lsp as matching :paths-ignore-regex: " (string/join ", " ignored))))
     :steps (mapv (fn [{:keys [options] :as step}]
                    (let [{:keys [args unsupported]} (lsp.kondo/run-options->cli-args options)]
                      (-> step
                          (dissoc :options)
                          (assoc :args args)
                          (shared/assoc-some :unsupported-options (not-empty unsupported)))))
                  (steps db (assoc options :analysis-type analysis-type)))}))

(defn ^:private shell-quote [s]
  (if (re-matches #"[A-Za-z0-9_@%+=:,./-]+" s)
    s
    (str "'" (string/replace s "'" "'\"'\"'") "'")))

(defn ^:private kondo-fn-lines [coordinate]
  ["clj_kondo() {"
   (if coordinate
     (str "  clojure -Sdeps "
          (shell-quote (binding [*print-namespace-maps* false]
                         (pr-str {:deps {'clj-kondo/clj-kondo coordinate}})))
          " -M -m clj-kondo.main \"$@\"")
     "  clj-kondo \"$@\"")
   "}"])

(defn ^:private step-lines [index total {:keys [description args stdin unsupported-options]} show-output?]
  (let [[flags [_ & lint-paths]] (split-with #(not= "--lint" %) args)
        cache-disabled? (some #{["--cache" "false"]} (partition 2 1 args))
        command (string/join " " (concat ["clj_kondo"]
                                         (when-not cache-disabled? ["--cache-dir" "\"$CACHE_DIR\""])
                                         (map shell-quote flags)))]
    (concat
      [(format "# %s/%s %s" (inc index) total description)]
      (when unsupported-options
        [(str "# WARNING: options without clj-kondo CLI equivalent: " (string/join ", " (sort unsupported-options)))])
      [(string/join " \\\n  "
                    (concat
                      [command]
                      (if (= 1 (count lint-paths))
                        [(str "--lint " (shell-quote (first lint-paths)))]
                        (when (seq lint-paths)
                          (cons "--lint" (map shell-quote lint-paths))))
                      (when stdin [(str "< " (shell-quote stdin))])
                      (when-not show-output? ["> /dev/null"])))
       ""])))

(defn ->script
  "A POSIX shell script running the clj-kondo CLI like clojure-lsp, from
  `repro` data."
  [{:keys [clojure-lsp-version clj-kondo-version clj-kondo-coordinate project-root analysis-type notes steps]}]
  (let [output-step-id (if (some #(= :single-file (:id %)) steps) :single-file :internal-paths)
        total (count steps)]
    (->> (concat
           ["#!/bin/sh"
            (format "# Runs clj-kondo %s like clojure-lsp %s does for %s," clj-kondo-version clojure-lsp-version project-root)
            (format "# on a startup without caches (analysis type %s) and when opening files in the editor." analysis-type)
            "# Not reproduced: clojure-lsp linters (clojure-lsp/*), clj-depend, custom linters, [:linters :clj-kondo] settings and JDK/stubs analysis."
            "# On startups with a valid .lsp/.cache, clojure-lsp skips the external classpath analysis, reusing the clj-kondo cache of a previous run."
            "# CACHE_DIR defaults to a new temporary dir, set CACHE_DIR=.clj-kondo/.cache to use the project clj-kondo cache."]
           (map #(str "# Note: " %) notes)
           [""]
           (kondo-fn-lines clj-kondo-coordinate)
           [""
            "CACHE_DIR=\"${CACHE_DIR:-$(mktemp -d)}\""
            (str "cd " (shell-quote project-root) " || exit 1")
            ""]
           (mapcat (fn [index step]
                     (step-lines index total step (= output-step-id (:id step))))
                   (range)
                   steps))
         (string/join "\n"))))
