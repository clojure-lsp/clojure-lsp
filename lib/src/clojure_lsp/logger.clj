(ns clojure-lsp.logger)

(defprotocol ILogger
  (setup [this])

  (set-log-path [_this log-path])

  (-info [this fmeta arg1] [this fmeta arg1 arg2] [this fmeta arg1 arg2 arg3])
  (-warn [this fmeta arg1] [this fmeta arg1 arg2] [this fmeta arg1 arg2 arg3])
  (-error [this fmeta arg1] [this fmeta arg1 arg2] [this fmeta arg1 arg2 arg3])
  (-debug [this fmeta arg1] [this fmeta arg1 arg2] [this fmeta arg1 arg2 arg3]))

(def ^:dynamic *logger*
  "Optional logger state to avoid having component available everywhere."
  nil)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn set-logger! [logger]
  (alter-var-root #'*logger* (constantly logger)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro info [& args]
  (let [fmeta (assoc (meta &form)
                     :file *file*
                     :ns-str (str *ns*))]
    `(when *logger*
       (-info *logger* ~fmeta ~@args))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro warn [& args]
  (let [fmeta (assoc (meta &form)
                     :file *file*
                     :ns-str (str *ns*))]
    `(when *logger*
       (-warn *logger* ~fmeta ~@args))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro error [& args]
  (let [fmeta (assoc (meta &form)
                     :file *file*
                     :ns-str (str *ns*))]
    `(when *logger*
       (-error *logger* ~fmeta ~@args))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro debug [& args]
  (let [fmeta (assoc (meta &form)
                     :file *file*
                     :ns-str (str *ns*))]
    `(when *logger*
       (-debug *logger* ~fmeta ~@args))))
