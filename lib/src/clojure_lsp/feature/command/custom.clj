(ns clojure-lsp.feature.command.custom
  (:require
   [clj-kondo.hooks-api :as clj-kondo.api]
   [clojure-lsp.custom-linters-api :as custom-linters-api]
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.refactor.transform :as r.transform]
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

(defn sci-init-opts [db]
  {:namespaces {'clojure-lsp.custom-commands-api custom-linters-api/api-fns
                'clojure.java.io {'file io/file}
                'clojure-lsp.refactor.edit {'to-top edit/to-top
                                            'inside-rcf? edit/inside-rcf?}
                'clojure-lsp.feature.diagnostics {'find-diagnostics f.diagnostic/find-diagnostics}
                'clojure-lsp.shared {'->range clojure-lsp.shared/->range}
                'clojure-lsp.refactor.transform {'r.transform r.transform/locs-to-ranges}
                'clojure-lsp.feature.add-missing-libspec {'add-to-namespace* f.add-missing-libspec/add-to-namespace*}
                'clj-kondo.hooks-api {'string-node? clj-kondo.api/string-node?
                                      'quote-node? clj-kondo.api/quote-node?
                                      'vector-node? clj-kondo.api/vector-node?
                                      'list-node? clj-kondo.api/list-node?}
                'rewrite-clj.zip {'child-sexprs z/child-sexprs
                                  'tag z/tag
                                  'insert-space-left z/insert-space-left
                                  'postwalk z/postwalk
                                  'whitespace? z/whitespace?
                                  'of-string* z/of-string*
                                  'seq? z/seq?
                                  'insert-child* z/insert-child*
                                  'vector? z/vector?
                                  'insert-newline-left z/insert-newline-left
                                  'right* z/right*
                                  'insert-left* z/insert-left*
                                  'map z/map
                                  'linebreak? z/linebreak?
                                  'rightmost* z/rightmost*
                                  'rightmost z/rightmost
                                  'insert-child z/insert-child
                                  'skip z/skip
                                  'left z/left
                                  'leftmost z/leftmost
                                  'find z/find
                                  'append-child z/append-child
                                  'find-token z/find-token
                                  'suffix z/suffix
                                  'set? z/set?
                                  'remove z/remove
                                  'down z/down
                                  'find-next-depth-first z/find-next-depth-first
                                  'map? z/map?
                                  'rightmost? z/rightmost?
                                  'root-string z/root-string
                                  'get z/get
                                  'find-next-token z/find-next-token
                                  'edit* z/edit*
                                  'splice z/splice
                                  'replace z/replace
                                  'left* z/left*
                                  'leftmost* z/leftmost*
                                  'end? z/end?
                                  'sexpr-able? z/sexpr-able?
                                  'find-tag-by-pos z/find-tag-by-pos
                                  'prewalk z/prewalk
                                  'find-tag z/find-tag
                                  'sexpr z/sexpr
                                  'of-file z/of-file
                                  'find-value z/find-value
                                  'find-next z/find-next
                                  'up* z/up*
                                  'map-keys z/map-keys
                                  'edit z/edit
                                  'namespaced-map? z/namespaced-map?
                                  ;; couldn't take value of macro
                                  ;; 'subedit-> z/subedit->
                                  ;; 'subedit->> z/subedit->>
                                  ;; 'edit-> z/edit->
                                  ;; 'edit->> z/edit->>
                                  'find-next-tag z/find-next-tag
                                  'replace* z/replace*
                                  'subzip z/subzip
                                  'position-span z/position-span
                                  'skip-whitespace-left z/skip-whitespace-left
                                  'of-node* z/of-node*
                                  'edit-node z/edit-node
                                  'find-last-by-pos z/find-last-by-pos
                                  'print-root z/print-root
                                  'of-file* z/of-file*
                                  'print z/print
                                  'down* z/down*
                                  'of-node z/of-node
                                  'string z/string
                                  'node z/node
                                  'prefix z/prefix
                                  'up z/up
                                  'insert-space-right z/insert-space-right
                                  'insert-right z/insert-right
                                  'skip-whitespace z/skip-whitespace
                                  'find-depth-first z/find-depth-first
                                  'leftmost? z/leftmost?
                                  'remove* z/remove*
                                  'remove-preserve-newline z/remove-preserve-newline
                                  'subedit-node z/subedit-node
                                  'next* z/next*
                                  'root z/root
                                  'map-vals z/map-vals
                                  'next z/next
                                  'find-next-value z/find-next-value
                                  'list? z/list?
                                  'insert-newline-right z/insert-newline-right
                                  'assoc z/assoc
                                  'length z/length
                                  'prev* z/prev*
                                  'insert-left z/insert-left
                                  'prev z/prev
                                  'insert-right* z/insert-right*
                                  'of-string z/of-string
                                  'right z/right
                                  'append-child* z/append-child*
                                  'whitespace-or-comment? z/whitespace-or-comment?
                                  'position z/position
                                  'reapply-context z/reapply-context}
                'rewrite-clj.node {'child-sexprs n/child-sexprs
                                   'tag n/tag
                                   'whitespace? n/whitespace?
                                   'whitespace-node n/whitespace-node
                                   'fn-node n/fn-node
                                   'map-context-clear n/map-context-clear
                                   'linebreak? n/linebreak?
                                   'var-node n/var-node
                                   'vector-node n/vector-node
                                   'eval-node n/eval-node
                                   'deref-node n/deref-node
                                   'integer-node n/integer-node
                                   'comma-separated n/comma-separated
                                   'children n/children
                                   'newlines n/newlines
                                   'sexprs n/sexprs
                                   'inner? n/inner?
                                   'replace-children n/replace-children
                                   'comma? n/comma?
                                   'forms-node n/forms-node
                                   'string-node n/string-node
                                   'meta-node n/meta-node
                                   'whitespace-nodes n/whitespace-nodes
                                   'printable-only? n/printable-only?
                                   'map-qualifier-node n/map-qualifier-node
                                   'leader-length n/leader-length
                                   'sexpr-able? n/sexpr-able?
                                   'regex-node n/regex-node
                                   'list-node n/list-node
                                   'sexpr n/sexpr
                                   'namespaced-map-node n/namespaced-map-node
                                   'syntax-quote-node n/syntax-quote-node
                                   'comma-node n/comma-node
                                   'newline-node n/newline-node
                                   'unquote-splicing-node n/unquote-splicing-node
                                   'map-node n/map-node
                                   'string n/string
                                   'map-context-apply n/map-context-apply
                                   'reader-macro-node n/reader-macro-node
                                   'comment? n/comment?
                                   'spaces n/spaces
                                   'raw-meta-node n/raw-meta-node
                                   'comment-node n/comment-node
                                   'set-node n/set-node
                                   'keyword-node n/keyword-node
                                   'symbol-node? n/symbol-node?
                                   'length n/length
                                   'quote-node n/quote-node
                                   'unquote-node n/unquote-node
                                   'node? n/node?
                                   'uneval-node n/uneval-node
                                   'keyword-node? n/keyword-node?
                                   'line-separated n/line-separated
                                   'token-node n/token-node
                                   'whitespace-or-comment? n/whitespace-or-comment?
                                   'coerce n/coerce}}
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
                    path (str "clojure-lsp.exports/commands/" (string/replace package "." "/") ".clj")
                    source-code (file-content-from-classpath path (:classpath db))]
                {:file path
                 :source source-code}))})

(defn run [{:keys [fqns params loc uri db]}]
  (sci/create-ns 'clojure-lsp.custom-commands-api nil)
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
                               :db db
                               :params params})))
                  (catch Exception e
                    (logger/error logger-tag (str "Error requiring custom command " fqns) e)
                    identity))]
    (when-let [out (not-empty (string/trim (str out)))]
      (logger/warn logger-tag (format "[%s] stdout: %s" fqns out)))
    (when-let [err (not-empty (string/trim (str err)))]
      (logger/warn logger-tag (format "[%s] stderr: %s" fqns err)))
    sci-res))
