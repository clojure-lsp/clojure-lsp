(ns clojure-lsp.feature.command.custom
  (:require
   [clojure-lsp.custom-linters-api :as custom-linters-api]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [sci.core :as sci])
  (:import
   [java.io StringWriter]
   [java.util.jar JarFile]))

(def ^:private logger-tag "[custom-command]")

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

;; TODO 21 MOve?
;; copy from clojure-lsp.feature.diagnostics.custom with a couple more fns
(defn sci-init-opts [db]
  {:namespaces {'clojure-lsp.custom-linters-api custom-linters-api/api-fns
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
                                  'node z/node
                                  ;; my additions
                                  'find-value z/find-value
                                  'remove* z/remove*
                                  'prev* z/prev*
                                  'of-string z/of-string}
                'rewrite-clj.node {'map-node n/map-node
                                   'vector-node n/vector-node
                                   'list-node n/list-node
                                   'string-node n/string-node
                                   'keyword-node n/keyword-node
                                   'token-node n/token-node
                                   'children n/children
                                   'tag n/tag
                                   ;; my additions
                                   'whitespace? n/whitespace?}}
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

(defn run [{:keys [fqns loc uri db]}]
  (sci/create-ns 'clojure-lsp.custom-linters-api nil)
  (let [sci-ctx (sci/init (sci-init-opts db))
        out (StringWriter.)
        err (StringWriter.)
        sci-res (try
                  (sci/binding [sci/out out
                                sci/err err]
                    (let [code (let [ns (namespace fqns)]
                                 (format "(require '%s %s)\n%s"
                                         ns
                                         (if *reload* :reload "")
                                         fqns))
                          cmd-fn (sci/eval-string* sci-ctx code)]
                      (cmd-fn {:loc loc
                               :uri uri
                               :db db})))
                  (catch Exception e
                    (logger/error logger-tag (str "Error requiring custom command " fqns) e)
                    identity))]
    (when-let [out (not-empty (string/trim (str out)))]
      (logger/warn logger-tag (format "[%s] stdout: %s" fqns out)))
    (when-let [err (not-empty (string/trim (str err)))]
      (logger/warn logger-tag (format "[%s] stderr: %s" fqns err)))
    sci-res))
