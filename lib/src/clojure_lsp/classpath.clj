(ns clojure-lsp.classpath
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.source-paths :as source-paths]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.set :as set]
   [clojure.string :as string])
  (:import
   (java.io ByteArrayOutputStream)
   (java.security MessageDigest)))

(set! *warn-on-reflection* true)

(defn ^:private md5 [^java.io.File file]
  (let [bytes'
        (with-open [xin (io/input-stream file)
                    xout (ByteArrayOutputStream.)]
          (io/copy xin xout)
          (.toByteArray xout))
        algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm bytes')]
    (format "%032x" (BigInteger. 1 raw))))

(defn ^:private valid-project-spec? [root-path {:keys [project-path]}]
  (let [project-file (shared/to-file root-path project-path)]
    (shared/file-exists? project-file)))

(defn ^:private project-root->project-dep-files [project-root dep-file-path settings]
  (let [project-dep-file (io/file project-root dep-file-path)]
    (if (string/ends-with? (str project-dep-file) "deps.edn")
      (if-let [local-roots (seq (source-paths/deps-file->local-roots project-dep-file settings))]
        (concat [project-dep-file]
                (->> local-roots
                     (map #(shared/relativize-filepath % project-root))
                     (map #(io/file project-root % "deps.edn"))
                     (filter shared/file-exists?)))
        [project-dep-file])
      [project-dep-file])))

(defn project-specs->hash [root-path settings]
  (->> (:project-specs settings)
       (filter (partial valid-project-spec? root-path))
       (map (fn [{:keys [project-path]}]
              (map md5 (project-root->project-dep-files (str root-path) project-path settings))))
       flatten
       (reduce str)))

(defn ^:private psh-cmd
  "Return the command vector that uses the PowerShell executable PSH to
  invoke CMD-AND-ARGS."
  ([psh & cmd-and-args]
   (into [psh "-NoProfile" "-Command"] cmd-and-args)))

(defn ^:private locate-executable
  "Locate and return the full path to the EXECUTABLE."
  [executable]
  (some-> ^java.nio.file.Path (fs/which executable) .toString))

(defn ^:private shell
  "Execute CMD-AND-ARGS with `clojure.java.shell/sh`, of which see."
  [& cmd-and-args]
  (apply shell/sh cmd-and-args))

(defn ^:private classpath-cmd->normalize
  "Return CLASSPATH-CMD, but with the EXEC expanded to its full path (if found).

  If the EXEC cannot be found, is one of clojure or lein and the
  program is running on MS-Windows, then, if possible, it tries to
  replace it with a PowerShell cmd invocation sequence in the
  following manner, while keeping ARGS the same.

  There could be two PowerShell executable available in the system
  path: powershell.exe (up to version 5.1, comes with windows) and/or
  pwsh.exe (versions 6 and beyond, can be installed on demand).

  If powershell.exe is available, it checks if the EXEC is installed
  in it as a module, and creates an invocation sequence as such to
  replace the EXEC. If not, it tries the same with pwsh.exe."
  [[exec & args :as classpath-cmd]]

  (if (and shared/windows-os?
           (#{"clojure" "lein"} exec)
           (not (locate-executable exec)))
    (if-let [up (some #(when-let [ps (locate-executable %)]
                         (when (= 0 (:exit (apply shell (psh-cmd ps "Get-Command" exec))))
                           (psh-cmd ps exec)))
                      ["powershell" "pwsh"])]
      (into up args)

      classpath-cmd)

    (or (some->> (locate-executable exec)
                 (assoc classpath-cmd 0))
        classpath-cmd)))

(defn ^:private lookup-classpath! [root-path {:keys [classpath-cmd env]}]
  (let [command (string/join " " classpath-cmd)
        env-with-default (merge {} (System/getenv) env)]
    (logger/info (format "Finding classpath via `%s`" command))
    (try
      (let [sep (re-pattern (System/getProperty "path.separator"))
            {:keys [exit out err]} (apply shell (into classpath-cmd
                                                      (cond-> [:dir (str root-path)]
                                                        env (conj :env env-with-default))))]
        (if (= 0 exit)
          (let [paths (-> out
                          string/split-lines
                          last
                          string/trim-newline
                          (string/split sep))]
            (logger/debug "Classpath found, paths: " paths)
            {:command command
             :paths (set paths)})
          {:command command
           :env env-with-default
           :error err}))
      (catch Exception e
        {:command command
         :env env-with-default
         :error (.getMessage e)}))))

(defn ^:private lookup-classpath-handling-error! [project-spec root-path producer]
  (let [{:keys [command error env paths]} (lookup-classpath! root-path project-spec)
        retry-action "Retry"
        ignore-action "Ignore"]
    (if error
      (do
        (logger/error (format "Error while looking up classpath info in %s. Error: %s" (str root-path) error))
        (logger/debug (format "Env used for classpath lookup:\nSHELL: %s\nPATH: %s" (get env "SHELL") (get env "PATH")))
        (if-let [chosen-action (producer/show-message-request
                                 producer
                                 (format (str "LSP classpath lookup failed when running `%s`. Some features may not work properly if ignored.\n\n"
                                              "Error: %s\n\nChoose an option:") command error)
                                 :warning
                                 [{:title retry-action}
                                  {:title ignore-action}])]
          (if (= retry-action chosen-action)
            (recur project-spec root-path producer)
            (do
              (logger/warn (format "Classpath lookup retry skipped by user"))
              paths))
          (producer/show-message producer (format "Classpath lookup failed when running `%s`. Some features may not work properly." command) :error error)))
      paths)))

(defn scan-classpath! [{:keys [db* producer]}]
  (let [db @db*
        root-path (shared/uri->path (:project-root-uri db))]
    (->> (settings/get db [:project-specs])
         (filter (partial valid-project-spec? root-path))
         (mapv #(lookup-classpath-handling-error! % root-path producer))
         (reduce set/union))))

(defn ^:private lein-source-aliases [source-aliases]
  (some->> source-aliases
           (map #(str "+" (name %)))
           seq
           (string/join ",")
           (conj ["with-profile"])))

(defn ^:private kw->str
  "Given a keyword, returns a string with the ':' omitted, nil if `kw` is not a keyword."
  [kw]
  (cond
    (qualified-keyword? kw)
    (str (namespace kw) "/" (name kw))
    (keyword? kw)
    (name kw)
    :else
    nil))

(defn ^:private deps-source-aliases [source-aliases]
  (some->> source-aliases
           (map kw->str)
           seq
           (string/join ":")
           (str "-A:")
           vector))

(defn default-project-specs
  "A col of project-specs, clojure-lsp will use each matched project-spec and
   merge removing duplicates.

  `project-path`: file name which will be used to determine if project spec
   matches and should run `classpath-cmd`.

  `classpath-cmd`: shell command to run that should return a claspath string
  like some/path:/other/path:/path/etc

  `env` optional map of strings which will inject env variables to
  the `classpath-cmd`. E.g `{\"PATH\": \"/some/path\"}`"
  [source-aliases]
  (->> [{:project-path "project.clj"
         :env nil
         :classpath-cmd (->> ["lein" (lein-source-aliases source-aliases) "classpath"]
                             flatten
                             (remove nil?)
                             vec)}
        {:project-path "deps.edn"
         :env nil
         :classpath-cmd (->> ["clojure" (deps-source-aliases source-aliases) "-Spath"]
                             flatten
                             (remove nil?)
                             vec)}
        {:project-path "build.boot"
         :env nil
         :classpath-cmd ["boot" "show" "--fake-classpath"]}
        {:project-path "shadow-cljs.edn"
         :env nil
         :classpath-cmd ["npx" "shadow-cljs" "classpath"]}
        {:project-path "bb.edn"
         :env nil
         :classpath-cmd ["bb" "print-deps" "--format" "classpath"]}]
       (map #(update % :classpath-cmd classpath-cmd->normalize))))
