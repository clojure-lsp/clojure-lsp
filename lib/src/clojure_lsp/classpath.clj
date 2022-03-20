(ns clojure-lsp.classpath
  (:require
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.source-paths :as source-paths]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as string]
   [lsp4clj.protocols :as protocols]
   [lsp4clj.protocols.logger :as logger])
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

(defn ^:private project-root->project-dep-files [project-root dep-file-path settings logger]
  (let [project-dep-file (io/file project-root dep-file-path)]
    (if (string/ends-with? (str project-dep-file) "deps.edn")
      (if-let [local-roots (seq (source-paths/deps-file->local-roots project-dep-file settings logger))]
        (concat [project-dep-file]
                (->> local-roots
                     (map #(shared/relativize-filepath % project-root))
                     (map #(io/file project-root % "deps.edn"))
                     (filter shared/file-exists?)))
        [project-dep-file])
      [project-dep-file])))

(defn project-specs->hash [root-path settings logger]
  (->> (:project-specs settings)
       (filter (partial valid-project-spec? root-path))
       (map (fn [{:keys [project-path]}]
              (map md5 (project-root->project-dep-files (str root-path) project-path settings logger))))
       flatten
       (reduce str)))

(defn ^:private lookup-classpath [root-path {:keys [classpath-cmd env]} db logger]
  (let [command (string/join " " classpath-cmd)]
    (logger/info logger (format "Finding classpath via `%s`" command))
    (try
      (let [sep (re-pattern (System/getProperty "path.separator"))
            {:keys [exit out err]} (apply shell/sh (into classpath-cmd
                                                         (cond-> [:dir (str root-path)]
                                                           env (conj :env (merge {} (System/getenv) env)))))]
        (if (= 0 exit)
          (let [paths (-> out
                          string/split-lines
                          last
                          string/trim-newline
                          (string/split sep))]
            (logger/debug logger "Classpath found, paths: " paths)
            paths)
          (do
            (logger/error logger (format "Error while looking up classpath info in %s. Exit status %s. Error: %s" (str root-path) exit err))
            (protocols/show-message (:producer @db) (format "Classpath lookup failed when running `%s`. Some features may not work properly. Error: %s" command err) :error err)
            [])))
      (catch clojure.lang.ExceptionInfo e
        (throw e))
      (catch Exception e
        (logger/error logger e (format "Error while looking up classpath info in %s" (str root-path)) (.getMessage e))
        (protocols/show-message (:producer @db) (format "Classpath lookup failed when running `%s`. Some features may not work properly. Error: %s" command (.getMessage e)) :error (.getMessage e))
        []))))

(defn scan-classpath! [db logger]
  (let [root-path (shared/uri->path (:project-root-uri @db) db)]
    (->> (settings/get db [:project-specs])
         (filter (partial valid-project-spec? root-path))
         (mapcat #(lookup-classpath root-path % db logger))
         vec
         seq)))

(defn ^:private classpath-cmd->windows-safe-classpath-cmd
  [classpath]
  (if shared/windows-os?
    (into ["powershell.exe" "-NoProfile"] classpath)
    classpath))

(defn ^:private lein-source-aliases [source-aliases]
  (some->> source-aliases
           (map #(str "+" (name %)))
           seq
           (string/join ",")
           (conj ["with-profile"])))

(defn ^:private deps-source-aliases [source-aliases]
  (some->> source-aliases
           (map name)
           seq
           (string/join ":")
           (str "-A:")
           vector))

(defn default-project-specs [source-aliases]
  (->> [{:project-path "project.clj"
         :classpath-cmd (->> ["lein" (lein-source-aliases source-aliases) "classpath"]
                             flatten
                             (remove nil?)
                             vec)}
        {:project-path "deps.edn"
         :classpath-cmd (->> ["clojure" (deps-source-aliases source-aliases) "-Spath"]
                             flatten
                             (remove nil?)
                             vec)}
        {:project-path "build.boot"
         :classpath-cmd ["boot" "show" "--fake-classpath"]}
        {:project-path "shadow-cljs.edn"
         :classpath-cmd ["npx" "shadow-cljs" "classpath"]}
        {:project-path "bb.edn"
         :classpath-cmd ["bb" "print-deps" "--format" "classpath"]}]
       (map #(update % :classpath-cmd classpath-cmd->windows-safe-classpath-cmd))))
