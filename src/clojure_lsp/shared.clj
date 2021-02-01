(ns clojure-lsp.shared
  (:require
    [clojure-lsp.db :as db]
    [clojure.java.shell :as shell]
    [clojure.string :as string]
    [taoensso.timbre :as log])
  (:import
   [java.net URI]
   [java.nio.file Paths]))

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

(defn uri->path ^java.nio.file.Path [uri]
  (.toAbsolutePath (Paths/get (URI. uri))))

(defn uri->filename [uri]
  (if-let [[_ jar-file nested-file] (re-find #"^zipfile://(.*\.jar)::(.*)" uri)]
    (str jar-file ":" nested-file)
    (str (string/replace uri #"^[a-z]+://" ""))))

(defn filename->uri [^String filename]
  (let [jar-scheme? (= "jar" (get-in @db/db [:settings :dependency-scheme]))]
    (if-let [[_ jar-file nested-file] (re-find #"^(.*\.jar):(.*)" filename)]
      (if jar-scheme?
        (str "jar:file:///" jar-file "!/" nested-file)
        (str "zipfile://" jar-file "::" nested-file))
      (str "file://" filename))))

(defn uri->project-related-path [uri project-root]
  (string/replace uri project-root ""))

(defn ->range [{:keys [name-row name-end-row name-col name-end-col row end-row col end-col] :as element}]
  (when element
    {:start {:line (max 0 (dec (or name-row row))) :character (max 0 (dec (or name-col col)))}
     :end {:line (max 0 (dec (or name-end-row end-row))) :character (max 0 (dec (or name-end-col end-col)))}}))

(defn ->scope-range [{:keys [name-row name-end-row name-col name-end-col row end-row col end-col] :as element}]
  (when element
    {:start {:line (max 0 (dec (or row name-row))) :character (max 0 (dec (or col name-col)))}
     :end {:line (max 0 (dec (or end-row name-end-row))) :character (max 0 (dec (or end-col name-end-col)))}}))

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

(defn position->line-column [position]
  [(inc (:line position))
   (inc (:character position))])
