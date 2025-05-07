(ns clojure-lsp.feature.custom-linters
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [sci.core :as sci])
  (:import
   [java.util.jar JarFile]))

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

(defn ^:private analyze [fqns params uris db]
  (let [sci-ctx (sci/init {:classes {'java.io.Exception Exception
                                     'java.lang.System System
                                     ;; enable with-in-str:
                                     'java.io.StringReader java.io.StringReader
                                     'clojure.lang.LineNumberingPushbackReader clojure.lang.LineNumberingPushbackReader
                                     ;; enable assert
                                     'java.lang.AssertionError java.lang.AssertionError}
                           :imports {'Exception 'java.io.Exception
                                     'System java.lang.System}
                           :load-fn (fn [{:keys [namespace]}]
                                      (let [package (namespace-munge (name namespace))
                                            path (str "clojure-lsp.exports/linters/" (string/replace package "." "/") ".clj")]
                                        {:file path
                                         :source (file-content-from-classpath path (:classpath db))}))})
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
                        (logger/error "Error requiring custom linter" fqns e)
                        identity))
        diagnostics* (atom [])
        reg-diagnostic!-fn (fn [diagnostic]
                             (swap! diagnostics* conj diagnostic))]
    (analyzer-fn {:db db
                  :params params
                  :uris uris
                  :reg-diagnostic! reg-diagnostic!-fn})
    @diagnostics*))

(comment
  (require '[clojure-lsp.db :as db])
  (binding [*reload* true]
    (analyze 'foo.bar/baz {} ["file:///home/greg/dev/clojure-lsp/lib/src/clojure_lsp/feature/custom_linters.clj"] @db/db*)))

(defn analyze-uris!
  [uris db]
  (let [custom-linters (settings/get db [:linters :custom] {})
        uri+diagnostics (if (seq custom-linters)
                          (shared/logging-task
                            :internal/custom-lint-total
                            (reduce (fn [all-diags [fqns params]]
                                      (if (contains? #{:error :warn :info} (:severity params))
                                        (shared/deep-merge all-diags
                                                           (analyze fqns params uris db))
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
