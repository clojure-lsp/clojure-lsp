(ns clojure-lsp.feature.diagnostics.custom
  (:require
   [clojure-lsp.custom-linters-api :as custom-linters-api]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [sci.core :as sci])
  (:import
   [java.io StringWriter]
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

(def ^:private required-fields #{:uri :range :level :message :source :code})
(def ^:private required-range-fields #{:row :col :end-row :end-col})

(defn ^:private missing-required-fields [diagnostic]
  (seq (concat (remove (set (keys diagnostic)) required-fields)
               (remove (set (keys (:range diagnostic))) required-range-fields))))

(defn ^:private custom-diagnostic->lsp [{:keys [level range] :as diagnostic}]
  (-> diagnostic
      (dissoc :uri :level)
      (assoc :range {:start {:line (dec (:row range))
                             :character (dec (:col range))}
                     :end {:line (dec (:end-row range))
                           :character (dec (:end-col range))}})
      (assoc :severity (shared/level->severity level))))

(defn ^:private analyze [fqns params uris db]
  (sci/create-ns 'clojure-lsp.custom-linters-api nil)
  (let [sci-ctx (sci/init {:namespaces {'clojure-lsp.custom-linters-api custom-linters-api/api-fns
                                        'clojure.java.io {'file io/file}
                                        'rewrite-clj.zip {'string z/string
                                                          'tag z/tag
                                                          'sexpr z/sexpr
                                                          'child-sexprs z/child-sexprs
                                                          'next z/next
                                                          'prev z/prev
                                                          'right z/right
                                                          'left z/left
                                                          'down z/down
                                                          'up z/up
                                                          'leftmost z/leftmost
                                                          'leftmost? z/leftmost?
                                                          'rightmost z/rightmost
                                                          'rightmost? z/rightmost?
                                                          'find z/find
                                                          'find-next z/find-next
                                                          'node z/node}
                                        'rewrite-clj.node {'map-node n/map-node
                                                           'vector-node n/vector-node
                                                           'list-node n/list-node
                                                           'string-node n/string-node
                                                           'keyword-node n/keyword-node
                                                           'token-node n/token-node
                                                           'children n/children
                                                           'tag n/tag}}
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
        empty-diagnostics (reduce #(assoc %1 %2 []) {} uris)
        diagnostics* (atom empty-diagnostics)
        reg-diagnostic!-fn (fn [diagnostic]
                             (if-let [missing-fields (missing-required-fields diagnostic)]
                               (logger/warn logger-tag (format "[%s] Ignoring diagnostic, missing required fields: %s for diagnostic %s" fqns missing-fields diagnostic))
                               (do
                                 (swap! diagnostics* update (:uri diagnostic) (fnil conj []) (custom-diagnostic->lsp diagnostic))
                                 (logger/info logger-tag (format "[%s] Registered diagnostic %s" fqns (:uri diagnostic))))))
        out (StringWriter.)
        err (StringWriter.)]
    (try
      (sci/binding [sci/out out
                    sci/err err]
        (let [code (let [ns (namespace fqns)]
                     (format "(require '%s %s)\n%s"
                             ns
                             (if *reload* :reload "")
                             fqns))
              lint-fn (sci/eval-string* sci-ctx code)]
          (lint-fn {:db db
                    :params params
                    :uris uris
                    :reg-diagnostic! reg-diagnostic!-fn})))
      (catch Exception e
        (logger/error logger-tag (str "Error requiring custom linter " fqns) e)
        identity))
    (when-let [out (not-empty (string/trim (str out)))]
      (logger/warn logger-tag (format "[%s] stdout: %s" fqns out)))
    (when-let [err (not-empty (string/trim (str err)))]
      (logger/warn logger-tag (format "[%s] stderr: %s" fqns err)))
    @diagnostics*))

(defn analyze-uris!
  [uris db]
  (let [custom-linters (settings/get db [:linters :custom] {})
        levels (set (keys shared/level->severity))
        uri+diagnostics (if (seq custom-linters)
                          (shared/logging-task
                            :internal/all-custom-linters
                            (reduce (fn [all-diags [fqns params]]
                                      (if (contains? levels (:level params))
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
  (let [uris (mapv #(shared/filename->uri % db) paths)]
    (analyze-uris! (shared/dir-uris->file-uris uris db) db)))

(defn db-with-results
  "Update `db` with custom linter analyzers result."
  [db analyze-fn]
  (update-in db [:diagnostics :custom] merge (analyze-fn db)))
