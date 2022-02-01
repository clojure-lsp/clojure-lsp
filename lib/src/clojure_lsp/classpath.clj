(ns clojure-lsp.classpath
  (:require
   [clojure-lsp.producer :as producer]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.source-paths :as source-paths]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as string]
   [taoensso.timbre :as log]))

(defn valid-project-spec? [root-path {:keys [project-path]}]
  (let [project-file (shared/to-file root-path project-path)]
    (shared/file-exists? project-file)))

(defn project-root->project-dep-files [project-root dep-file-path settings]
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

(defn ^:private lookup-classpath [root-path {:keys [classpath-cmd env]} db]
  (let [command (string/join " " classpath-cmd)]
    (log/info (format "Finding classpath via `%s`" command))
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
            (log/debug "Classpath found, paths: " paths)
            paths)
          (do
            (log/error (format "Error while looking up classpath info in %s. Exit status %s. Error: %s" (str root-path) exit err))
            (producer/show-message (:producer @db) (format "Classpath lookup failed when running `%s`. Some features may not work properly. Error: %s" command err) :error err)
            [])))
      (catch clojure.lang.ExceptionInfo e
        (throw e))
      (catch Exception e
        (log/error e (format "Error while looking up classpath info in %s" (str root-path)) (.getMessage e))
        (producer/show-message (:producer @db) (format "Classpath lookup failed when running `%s`. Some features may not work properly. Error: %s" command (.getMessage e)) :error (.getMessage e))
        []))))

(defn scan-classpath! [db]
  (let [root-path (shared/uri->path (:project-root-uri @db))]
    (->> (settings/get db [:project-specs])
         (filter (partial valid-project-spec? root-path))
         (mapcat #(lookup-classpath root-path % db))
         vec
         seq)))
