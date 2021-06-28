(ns clojure-lsp.shared
  (:require
    [clojure-lsp.db :as db]
    [clojure.core.async :refer [<! >! alts! chan go-loop timeout]]
    [clojure.java.io :as io]
    [clojure.java.shell :as shell]
    [clojure.string :as string]
    [taoensso.timbre :as log])
  (:import
    [java.net URI URL JarURLConnection]
    [java.nio.file Paths]))

(defn assoc-some
  "Assoc[iate] if the value is not nil. "
  ([m k v]
   (if (nil? v) m (assoc m k v)))
  ([m k v & kvs]
   (let [ret (assoc-some m k v)]
     (if kvs
       (if (next kvs)
         (recur ret (first kvs) (second kvs) (nnext kvs))
         (throw (IllegalArgumentException.
                 "assoc-some expects even number of arguments after map/vector, found odd number")))
       ret))))

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

(defn uri->available-langs [uri]
  (cond
    (string/ends-with? uri ".cljs") #{:cljs}
    (string/ends-with? uri ".cljc") #{:clj :cljs}
    (string/ends-with? uri ".clj") #{:clj}
    (string/ends-with? uri ".edn") #{:edn}
    :else #{}))

(defn conform-uri
  ([uri] (conform-uri uri (get-in @db/db [:settings :uri-format])))
  ([uri format-settings]
   (let [[match scheme+auth path] (re-matches #"([a-z:]+//.*?)(/.*)" uri)]
     (when-not match
       (log/error "Found invalid URI:" uri))
     (str scheme+auth
          (-> path
              (string/replace-first #"^/[a-zA-Z](?::|%3A)/"
                                    (if (:upper-case-drive-letter? format-settings)
                                      string/upper-case
                                      string/lower-case))
              (cond-> (:encode-colons-in-path? format-settings)
                (string/replace ":" "%3A")))))))

(defn uri->path ^java.nio.file.Path [uri]
  (-> (conform-uri uri {:upper-case-drive-letter? true})
      URI. Paths/get))

(defn plain-uri? [uri]
  (when uri
    (or (string/starts-with? uri "file:/")
        (string/starts-with? uri "jar:file:/")
        (string/starts-with? uri "zipfile:/"))))

(defn- uri-obj->filepath [uri]
  (-> uri Paths/get .toString
      (string/replace #"^[a-z]:\\" string/upper-case)))

(defn uri->filename
  "Converts a URI string into an absolute file path.

  The output path representation matches that of the operating system."
  [^String uri]
  (if (string/starts-with? uri "jar:")
    (let [conn ^JarURLConnection (.openConnection (URL. uri))
          jar-file (uri-obj->filepath ^URI (.toURI ^URL (.getJarFileURL conn)))]
      (str jar-file ":" (.getEntryName conn)))
    (let [uri-obj (URI. uri)
          [_ jar-uri-path nested-file] (when (= "zipfile" (.getScheme uri-obj))
                                         (re-find #"^(.*\.jar)::(.*)" (.getPath uri-obj)))]
      (if jar-uri-path
        (str (-> jar-uri-path io/file .getCanonicalPath) ":" nested-file)
        (uri-obj->filepath uri-obj)))))

(defn- filepath->uri-obj ^URI [filepath]
  (-> filepath io/file .toPath .toUri))

(defn- uri-encode [scheme path]
  (.toString (URI. scheme "" path nil)))

(defn filename->uri
  "Converts an absolute file path into a file URI string.

  Jar files are given the `jar:file` or `zipfile` scheme depending on the
  `:dependency-scheme` setting."
  [^String filename]
  (let [jar-scheme? (= "jar" (get-in @db/db [:settings :dependency-scheme]))
        [_ jar-filepath nested-file] (re-find #"^(.*\.jar):(.*)" filename)]
    (conform-uri
      (if-let [jar-uri-path (some-> jar-filepath (-> filepath->uri-obj .getPath))]
        (if jar-scheme?
          (uri-encode "jar:file" (str jar-uri-path "!/" nested-file))
          (uri-encode "zipfile" (str jar-uri-path "::" nested-file)))
        (.toString (filepath->uri-obj filename))))))

(defn relativize-filepath
  "Returns absolute `path` (string) as relative file path starting at `root` (string)

  The output representation path matches that of the operating system."
  [path root]
  (.toString (.relativize (-> root io/file .toPath) (-> path io/file .toPath))))

(defn uri->relative-filepath
  "Returns `uri` as relative file path starting at `root` URI

  The output path representation matches that of the operating system."
  [uri root]
  (.toString (.relativize (uri->path root) (uri->path uri))))

(defn join-filepaths
  [& components]
  (.getPath ^java.io.File (apply io/file components)))

(defn namespace->uri [namespace source-paths filename]
  (let [file-type (uri->file-type filename)]
    (filename->uri
      (join-filepaths (first (filter #(string/starts-with? filename %) source-paths))
                             (-> namespace
                                 (string/replace "." (System/getProperty "file.separator"))
                                 (string/replace "-" "_")
                                 (str "." (name file-type)))))))

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

(defn position->line-column [position]
  [(inc (:line position))
   (inc (:character position))])

(defn debounce-by
  "Debounce in channel with ms miliseconds distincting by by-fn."
  [in ms by-fn]
  (let [out (chan)]
    (go-loop [last-val nil]
      (let [val (if (nil? last-val) (<! in) last-val)
            timer (timeout ms)
            [new-val ch] (alts! [in timer])
            different? (and new-val
                            (not (= (by-fn val) (by-fn new-val))))]
        (cond
          different? (do (>! out val)
                         (>! out new-val)
                         (recur nil))
          (= ch timer) (do (>! out val)
                           (recur nil))
          (= ch in) (recur new-val))))
    out))

(defn deep-merge [a b]
  (merge-with (fn [x y]
                (cond (map? y) (deep-merge x y)
                      (vector? y) (concat x y)
                      :else y))
              a b))
