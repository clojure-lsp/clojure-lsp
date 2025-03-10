(ns clojure-lsp.custom-linters
  (:require
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.pprint]
   [clojure.string :as str]
   [sci.core :as sci])
  (:import
   (java.util.jar JarFile)))

(def ^:dynamic *reload* false)

(defn pprint [& args]
  (apply clojure.pprint/pprint args))

(defn ^:private load-file-from-classpath
  [path [head & tail]]
  (when head
    (if (str/ends-with? head ".jar")
      (if-let [content (with-open [jar (JarFile. (io/file head))]
                         (when-let [entry (.getJarEntry jar path)]
                           (slurp (.getInputStream jar entry))))]
        content
        (recur path tail))
      (if (shared/file-exists? (io/file head path))
        (slurp (io/file head path))
        (recur path tail)))))

(defn analyze
  [fqns params uris db]
  (let [sci-ctx (sci/init {:namespaces {'clojure.pprint {'pprint pprint}}
                           :classes {'java.io.Exception Exception
                                     'java.lang.System System
                             ;; enable with-in-str:
                                     'java.io.StringReader java.io.StringReader
                                     'clojure.lang.LineNumberingPushbackReader clojure.lang.LineNumberingPushbackReader
                             ;; enable assert
                                     'java.lang.AssertionError java.lang.AssertionError}
                           :imports {'Exception 'java.io.Exception
                                     'System java.lang.System}
                           :load-fn (fn [{:keys [:namespace]}]
                                      (let [^String ns-str (namespace-munge (name namespace))
                                            path (str "clojure-lsp.exports/" (.replace ns-str "." "/") ".clj")
                                            result (if-let [content (load-file-from-classpath path (:classpath db))]
                                                     {:file path
                                                      :source content}
                                                     (binding [*out* *err*]
                                                       (println "WARNING: file" path "not found in the classpath while loading custom linter analyzer")
                                                       nil))]
                                        result))})
        analyzer-fn (try
                      (sci/binding [sci/out *out*
                                    sci/err *err*]
                        (let [code (let [ns (namespace fqns)]
                                     (format "(require '%s %s)\n%s" ns
                                             (if *reload* :reload "")
                                             fqns))]
                          (sci/eval-string* sci-ctx code)))
                      (catch Exception e
                        (println e)))]
    (analyzer-fn {:db db :params params :uris uris})))

;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticSeverity
(def error 1)
(def warning 2)
(def information 3)
(def hint 4)

(defn analyze-uris!
  [uris db]
  (let [analyzers (-> db settings/all :linters :analyzers)
        uri+diagnostics (if (seq analyzers)
                          (shared/logging-time
                            "Finding custom linter diagnostics took %s"
                            (reduce (fn [acc [fqns params]]
                                      (if (->> params :severity (contains? #{error warning information hint}))
                                        (shared/deep-merge acc (analyze fqns params uris db))
                                        acc))
                                    {}
                                    analyzers))
                          {})]
    uri+diagnostics))

(defn analyze-paths!
  [paths db]
  (analyze-uris! (->> db
                      :analysis
                      keys
                      (filter (fn [uri]
                                (some #(str/includes? uri %) paths))))
                 db))

(defn analyze-uri!
  [uri db]
  (analyze-uris! [uri] db))

(defn db-with-results
  "Update `db` with custom linter analyzers result."
  [db analyze-fn]
  (update db :custom-linter-diagnostics merge (analyze-fn db)))
