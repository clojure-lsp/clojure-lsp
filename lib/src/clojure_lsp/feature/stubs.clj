(ns clojure-lsp.feature.stubs
  (:require
   [clj-easy.stub.core :as stub]
   [clojure-lsp.db :as db]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [lsp4clj.protocols.logger :as logger])
  (:import
   (java.io File)))

(set! *warn-on-reflection* true)

(defn ^:private stubs-output-dir [settings]
  (or (-> settings :stubs :generation :output-dir)
      ".lsp/.cache/stubs"))

(defn ^:private delete-directory-recursive
  "Recursively delete a directory."
  [^java.io.File file]
  (when (.isDirectory file)
    (run! delete-directory-recursive (.listFiles file)))
  (io/delete-file file true))

(defn ^:private generate-stubs! [namespaces settings db logger]
  (try
    (if-let [classpath (string/join ":" (:classpath @db))]
      (let [java-command (or (-> settings :stubs :generation :java-command)
                             "java")
            output-dir ^File (io/file (stubs-output-dir settings))]
        (delete-directory-recursive output-dir)
        (logger/info logger (str  "Generating stubs for analysis for namespaces " namespaces " on " (str output-dir)))
        (shared/logging-time
          logger
          "Stub generation process took %s secs."
          (stub/generate! {:output-dir output-dir
                           :namespaces namespaces
                           :classpath classpath
                           :java-command java-command})))
      {:result-code 2
       :message "Classpath not found."})
    (catch Exception e
      {:result-code 1
       :message (str "Error: " e)})))

(defn ^:private analyze-stubs!
  [dirs db logger]
  (let [result (shared/logging-time
                 logger
                 "Stubs analyzed, took %s secs."
                 (lsp.kondo/run-kondo-on-paths! dirs true db logger))
        kondo-analysis (-> (:analysis result)
                           (dissoc :namespace-usages :var-usages))
        analysis (->> kondo-analysis
                      lsp.kondo/normalize-analysis
                      (group-by :filename))]
    (loop [state-db @db]
      (when-not (compare-and-set! db state-db (update state-db :analysis merge analysis))
        (logger/warn logger "Analyzis divergent from stub analysis, trying again...")
        (recur @db)))
    (-> (shared/uri->path (:project-root-uri @db) db)
        (db/read-cache db logger)
        (update :analysis merge analysis)
        (db/upsert-cache! db logger))))

(defn generate-and-analyze-stubs!
  [settings db logger]
  (let [namespaces (->> settings :stubs :generation :namespaces (map str) set)
        extra-dirs (-> settings :stubs :extra-dirs)]
    (if (and (seq namespaces)
             (or (:full-scan-analysis-startup @db)
                 (not= namespaces (:stubs-generation-namespaces @db))))
      (let [{:keys [result-code message]} (generate-stubs! namespaces settings db logger)]
        (if (= 0 result-code)
          (analyze-stubs! (concat [(stubs-output-dir settings)]
                                  extra-dirs)
                          db
                          logger)
          (logger/error logger (str "Stub generation failed." message))))
      (when (seq extra-dirs)
        (analyze-stubs! extra-dirs db logger)))))

(defn check-stubs? [settings]
  (or (-> settings :stubs :generation :namespaces seq)
      (-> settings :stubs :extra-dirs seq)))
