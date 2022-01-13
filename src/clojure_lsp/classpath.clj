(ns clojure-lsp.classpath
  (:require
   [clojure-lsp.producer :as producer]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.java.shell :as shell]
   [clojure.string :as string]
   [taoensso.timbre :as log]))

(defn valid-project-spec? [root-path {:keys [project-path]}]
  (let [project-file (shared/to-file root-path project-path)]
    (shared/file-exists? project-file)))

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
            (producer/window-show-message (format "Classpath lookup failed when running `%s`. Some features may not work properly. Error: %s" command err) :error err db)
            [])))
      (catch clojure.lang.ExceptionInfo e
        (throw e))
      (catch Exception e
        (log/error e (format "Error while looking up classpath info in %s" (str root-path)) (.getMessage e))
        (producer/window-show-message (format "Classpath lookup failed when running `%s`. Some features may not work properly. Error: %s" command (.getMessage e)) :error (.getMessage e) db)
        []))))

(defn scan-classpath! [db]
  (let [root-path (shared/uri->path (:project-root-uri @db))]
    (->> (settings/get db [:project-specs])
         (filter (partial valid-project-spec? root-path))
         (mapcat #(lookup-classpath root-path % db))
         vec
         seq)))
