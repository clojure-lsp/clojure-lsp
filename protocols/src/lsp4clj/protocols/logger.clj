(ns lsp4clj.protocols.logger)

(defprotocol ILSPLogger
  (setup [this])

  (set-log-path [_this log-path])

  (-info [this fmeta arg1] [this fmeta arg1 arg2] [this fmeta arg1 arg2 arg3])
  (-warn [this fmeta arg1] [this fmeta arg1 arg2] [this fmeta arg1 arg2 arg3])
  (-error [this fmeta arg1] [this fmeta arg1 arg2] [this fmeta arg1 arg2 arg3])
  (-debug [this fmeta arg1] [this fmeta arg1 arg2] [this fmeta arg1 arg2 arg3]))

(def ^:dynamic *logger*
  "Optional logger state to avoid having component available everywhere."
  nil)

(defn set-logger! [logger]
  (alter-var-root #'*logger* (constantly logger)))

(defmacro info [& args]
  (let [fmeta (assoc (meta &form)
                     :file *file*
                     :ns-str (str *ns*))]
    `(when *logger*
       (-info *logger* ~fmeta ~@args))))

(defmacro warn [& args]
  (let [fmeta (assoc (meta &form)
                     :file *file*
                     :ns-str (str *ns*))]
    `(when *logger*
       (apply -warn *logger* ~fmeta ~@args))))

(defmacro error [& args]
  (let [fmeta (assoc (meta &form)
                     :file *file*
                     :ns-str (str *ns*))]
    `(when *logger*
       (-error *logger* ~fmeta ~@args))))

(defmacro debug [& args]
  (let [fmeta (assoc (meta &form)
                     :file *file*
                     :ns-str (str *ns*))]
    `(when *logger*
       (-debug *logger* ~fmeta ~@args))))
