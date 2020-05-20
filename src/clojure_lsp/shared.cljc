(ns clojure-lsp.shared
  #?(:cljs (:require-macros [clojure-lsp.shared]))
  (:require
    [clojure.string :as string]
    #?(:clj [clojure.tools.logging :as log])))

(defn uri->file-type [uri]
  (cond
    (string/ends-with? uri ".cljs") :cljs
    (string/ends-with? uri ".cljc") :cljc
    (string/ends-with? uri ".clj") :clj
    :else :unknown))

(defn ->range [{:keys [row end-row col end-col]}]
  {:start {:line (dec row) :character (dec col)}
   :end {:line (dec end-row) :character (dec end-col)}})

(defmacro log [ & args]
  (if (:ns &env)
    `(apply println "WARN" ~@args)
    `(log/warn ~@args)))
