(ns clojure-lsp.feature.custom-linters
  (:require
   [clojure-lsp.custom-linters-api :as custom-linters-api]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [sci.core :as sci])
  (:import
   [java.util.jar JarFile]))

(def ^:private logger-tag "[custom-linter]")

(def ^:dynamic *reload* false)

(defn ^:private file-content-from-classpath
  [path [head & tail]]
  (when head
    (if (string/ends-with? head ".jar")
      (if-let [content (with-open [jar (JarFile. (io/file head))]
                         (when-let [entry (.getJarEntry jar path)]
                           (slurp (.getInputStream jar entry))))]
        content
        (recur path tail))
      (if (shared/file-exists? (io/file head path))
        (slurp (io/file head path))
        (recur path tail)))))

(def ^:private required-fields #{:uri :range :severity :message :source :code})

(defn ^:private missing-required-fields [diagnostic]
  (seq (remove (set (keys diagnostic)) required-fields)))

(defn ^:private custom-diagnostic->lsp [diagnostic]
  (-> diagnostic
      (dissoc :uri)
      (update :severity #(case %
                           :error 1
                           :warning 2
                           :info 3))))

(defn ^:private analyze [fqns params uris db]
  (sci/create-ns 'clojure-lsp.custom-linters-api nil)
  (let [sci-ctx (sci/init {:namespaces {'clojure-lsp.custom-linters-api custom-linters-api/api-fns}
                           :classes {'java.io.Exception Exception
                                     'java.lang.System System
                                     'java.io.File java.io.File
                                     ;; enable with-in-str:
                                     'java.io.StringReader java.io.StringReader
                                     'clojure.lang.LineNumberingPushbackReader clojure.lang.LineNumberingPushbackReader
                                     ;; enable assert
                                     'java.lang.AssertionError java.lang.AssertionError}
                           :imports {'Exception 'java.io.Exception
                                     'System java.lang.System
                                     'File java.io.File}
                           :load-fn (fn [{:keys [namespace]}]
                                      (let [package (namespace-munge (name namespace))
                                            path (str "clojure-lsp.exports/linters/" (string/replace package "." "/") ".clj")
                                            source-code (file-content-from-classpath path (:classpath db))]
                                        {:file path
                                         :source source-code}))})
        analyzer-fn (try
                      (sci/binding [sci/out *out*
                                    sci/err *err*]
                        (let [code (let [ns (namespace fqns)]
                                     (format "(require '%s %s)\n%s"
                                             ns
                                             (if *reload* :reload "")
                                             fqns))]
                          (sci/eval-string* sci-ctx code)))
                      (catch Exception e
                        (logger/error logger-tag (str "Error requiring custom linter " fqns) e)
                        identity))
        empty-diagnostics (reduce #(assoc %1 %2 []) {} uris)
        diagnostics* (atom empty-diagnostics)
        reg-diagnostic!-fn (fn [diagnostic]
                             (if-let [missing-fields (missing-required-fields diagnostic)]
                               (logger/warn logger-tag (format "Ignoring diagnostic, missing required fields: %s for diagnostic %s" missing-fields diagnostic))
                               (swap! diagnostics* update (:uri diagnostic) (fnil conj []) (custom-diagnostic->lsp diagnostic))))]
    (analyzer-fn {:db db
                  :params params
                  :uris uris
                  :reg-diagnostic! reg-diagnostic!-fn})
    @diagnostics*))

(defn ^:private analyze-uris!
  [uris db]
  (let [custom-linters (settings/get db [:linters :custom] {})
        uri+diagnostics (if (seq custom-linters)
                          (shared/logging-task
                            :internal/all-custom-linters
                            (reduce (fn [all-diags [fqns params]]
                                      (if (contains? #{:error :warning :info} (:severity params))
                                        (shared/deep-merge all-diags
                                                           (shared/logging-task
                                                             (keyword "custom-lint" (-> fqns
                                                                                        str
                                                                                        (string/replace ":" "")
                                                                                        (string/replace "/" "#")))
                                                             (analyze fqns params uris db)))
                                        all-diags))
                                    {}
                                    custom-linters))
                          {})]
    uri+diagnostics))

(defn analyze-uri!
  [uri db]
  (analyze-uris! [uri] db))

(defn analyze-paths!
  [paths db]
  (analyze-uris! (mapv #(shared/filename->uri % db) paths) db))

(defn db-with-results
  "Update `db` with custom linter analyzers result."
  [db analyze-fn]
  (update db :custom-linter-diagnostics merge (analyze-fn db)))
