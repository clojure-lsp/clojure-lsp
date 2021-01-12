(ns clojure-lsp.shared
  (:require
   [clojure.java.shell :as shell]
   [clojure.string :as string]
   [clojure.tools.logging :as log])
  (:import
   [java.net URI]
   [java.nio.file Paths]
   [org.eclipse.lsp4j Range]))

(def windows-os?
  (.contains (System/getProperty "os.name") "Windows"))

(defn windows-process-alive?
  [pid]
  (let [{:keys [out]} (shell/sh "tasklist" "/fi" (format "\"pid eq %s\"" pid))]
    (string/includes? out (str pid))))

(defn unix-process-alive?
  [pid]
  (let [{:keys [exit]} (shell/sh "kill" "-0" (str pid))]
    (zero? exit)))

(defn process-alive?
  [pid]
  (try
    (if windows-os?
      (windows-process-alive? pid)
      (unix-process-alive? pid))
    (catch Exception e
      (log/warn "Checking if process is alive failed." e)
      ;; Return true since the check failed. Assume the process is alive.
      true)))

(defn uri->file-type [uri]
  (cond
    (string/ends-with? uri ".cljs") :cljs
    (string/ends-with? uri ".cljc") :cljc
    (string/ends-with? uri ".clj") :clj
    (string/ends-with? uri ".edn") :edn
    :else :unknown))

(defn uri->path [uri]
  (Paths/get (URI. uri)))

(defn uri->project-related-path [uri project-root]
  (string/replace uri project-root ""))

(defn ->range [{:keys [row end-row col end-col]}]
  {:start {:line (max 0 (dec row)) :character (max 0 (dec col))}
   :end {:line (max 0 (dec end-row)) :character (max 0 (dec end-col))}})

(defn range->clj [^Range range]
  {:start {:line      (.getLine (.getStart range))
           :character (.getCharacter (.getStart range))}
   :end   {:line      (.getLine (.getEnd range))
           :character (.getCharacter (.getEnd range))}})

(defn keywordize-first-depth
  [m]
  (into {}
        (for [[k v] m]
          [(keyword k) v])))

(defn check-bounds
  [line column {:keys [row end-row col end-col] :as _usage}]
  (cond
    (< line row) :before
    (and (= line row) (< column col)) :before
    (< line end-row) :within
    (and (= end-row line) (>= end-col column)) :within
    :else :after))
