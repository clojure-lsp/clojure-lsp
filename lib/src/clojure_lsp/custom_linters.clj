(ns clojure-lsp.custom-linters
  (:require
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.pprint]
   [clojure.string :as str]
   [sci.core :as sci]))

(def ^:dynamic *reload* false)

(defn pprint [& args]
  (apply clojure.pprint/pprint args))

(defn find-file-on-classpath ^java.io.File
  [base-path classpath]
  (some (fn [cp-entry]
          (let [f (io/file cp-entry (str base-path ".clj"))]
            (when (.exists f) f)))
        classpath))

(defn analyze
  [fqns params uris db]
  (let [cp-candidates (remove nil?
                              (map (fn [cp-entry]
                                     (let [f (io/file cp-entry "clojure-lsp.exports")]
                                       (when (.isDirectory f) f)))
                                   (:classpath db)))
        sci-ctx (sci/init {:namespaces {'clojure.pprint {'pprint pprint}}
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
                                            base-path (.replace ns-str "." "/")
                                            result (if-let [f (find-file-on-classpath base-path cp-candidates)]
                                                     {:file (.getAbsolutePath f)
                                                      :source (slurp f)}
                                                     (binding [*out* *err*]
                                                       (println "WARNING: file" base-path "not found while loading hook")
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

(defn analyze-uris!
  [uris db]
  (let [analyzers (-> db settings/all :linters :analyzers)
        uri+diagnostics (if (seq analyzers)
                          (reduce (fn [acc [fqns params]]
                                    (if (->> params :severity (contains? #{1 2 3}))
                                      (shared/deep-merge acc (analyze fqns params uris db))
                                      acc))
                                  {}
                                  analyzers)
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
