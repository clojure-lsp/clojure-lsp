(ns clojure-lsp.shared
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.logger :as logger]
   [clojure.core.async :refer [<! >! alts! chan go-loop timeout]]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [lsp4clj.lsp.responses :as lsp.responses])
  (:import
   [java.net
    JarURLConnection
    URI
    URL
    URLDecoder]
   [java.nio.charset StandardCharsets]
   [java.nio.file Paths]
   [java.util.regex Matcher]))

(set! *warn-on-reflection* true)

(def line-separator
  "The system's line separator."
  (System/lineSeparator))

(def ^:private ansi-colors
  {:reset "[0m"
   :red   "[31m"
   :green "[32m"
   :yellow "[33m"
   :cyan  "[36m"
   :bright-green "[92m"
   :bright-yellow "[93m"
   :bright-magenta "[35;1m"
   :bright-cyan "[36;1m"
   :bright-white "[37;1m"})

(defn colorize [s color]
  (str \u001b (ansi-colors color) s \u001b (ansi-colors :reset)))

(defn deep-merge
  "Recursively merges maps together.
  Improved version of medley deep-merge concating colls instead of overwriting."
  ([])
  ([a] a)
  ([a b]
   (when (or a b)
     (letfn [(merge-entry [m e]
               (let [k  (key e)
                     v' (val e)]
                 (if (contains? m k)
                   (assoc m k (let [v (get m k)]
                                (cond
                                  (and (map? v) (map? v'))
                                  (deep-merge v v')

                                  (and (set? v) (set? v'))
                                  (set/union v v')

                                  (and (vector? v) (coll? v'))
                                  (vec (concat v v'))

                                  (and (coll? v) (coll? v'))
                                  (concat v v')

                                  :else v')))
                   (assoc m k v'))))]
       (reduce merge-entry (or a {}) (seq b)))))
  ([a b & more]
   (reduce deep-merge (or a {}) (cons b more))))

(defn file-exists? [^java.io.File f]
  (.exists f))

(defn directory? [^java.io.File f]
  (.isDirectory f))

(defn absolute-path? [^String path]
  (.isAbsolute (io/file path)))

(defn slurp-uri
  "Slurp uri, returning nil if anything goes wrong, and in particular when the
  URI does not exist on disk. Also useful in with-redefs."
  [^String uri]
  (try
    (slurp uri)
    (catch Exception _
      (logger/warn "couldn't read" uri))))

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

(defn assoc-in-some
  [m ks v]
  (if (nil? v) m (assoc-in m ks v)))

(defn dissoc-in
  [m key-vec]
  (let [firsts (vec (butlast key-vec))
        node (dissoc (get-in m firsts) (last key-vec))]
    (assoc-in-some m firsts node)))

(def windows-os?
  (.contains (System/getProperty "os.name") "Windows"))

(def valid-langs #{:clj :cljs :cljc :edn})

(defn uri->file-type [uri]
  (cond
    (string/ends-with? uri ".cljs") :cljs
    (string/ends-with? uri ".cljc") :cljc
    (string/ends-with? uri ".clj") :clj
    (string/ends-with? uri ".edn") :edn
    (string/ends-with? uri ".bb") :clj
    (string/ends-with? uri ".cljd") :clj
    (string/ends-with? uri ".clj_kondo") :clj
    :else :unknown))

(defn uri->available-langs [uri]
  (cond
    (string/ends-with? uri ".cljs") #{:cljs}
    (string/ends-with? uri ".cljc") #{:clj :cljs}
    (string/ends-with? uri ".clj") #{:clj}
    (string/ends-with? uri ".edn") #{:edn}
    (string/ends-with? uri ".bb") #{:clj}
    (string/ends-with? uri ".cljd") #{:clj}
    (string/ends-with? uri ".clj_kondo") #{:clj}
    :else #{}))

(defn ^:private conform-uri
  "Changes and returns URI given FORMAT-SETTINGS:

  :encode-colons-in-path? If true, changes all `:` to `%3A`.

  :upper-case-drive-letter? If true (the default) and path begins with
  a windows drive letter, changes it to an upper case, otherwise to a
  lower case letter."
  [uri {:keys [encode-colons-in-path? upper-case-drive-letter?] :or {upper-case-drive-letter? true}
        :as _format-settings}]
  (let [[match scheme+auth path] (re-matches #"([a-z:]+//.*?)(/.*)" uri)]
    (when-not match
      (logger/error "Found invalid URI:" uri))
    (str scheme+auth
         (-> path
             (string/replace-first #"^/[a-zA-Z](?::|%3A)/"
                                   (if upper-case-drive-letter?
                                     string/upper-case
                                     string/lower-case))
             (cond-> encode-colons-in-path?
               (string/replace ":" "%3A"))))))

(defn uri->path ^java.nio.file.Path [uri]
  (-> (conform-uri uri {:upper-case-drive-letter? true})
      URI. Paths/get))

(defn plain-uri? [uri]
  (when uri
    (or (string/starts-with? uri "file:/")
        (string/starts-with? uri "jar:file:/")
        (string/starts-with? uri "zipfile:/"))))

(defn ^:private uri-obj->filepath [uri]
  (-> uri Paths/get .toString
      (string/replace #"^[a-z]:\\" string/upper-case)))

(defn ^:private unescape-uri
  [^String uri]
  (try
    (URLDecoder/decode uri (.name StandardCharsets/UTF_8)) ;; compatible with Java 1.8 too!
    (catch UnsupportedOperationException _
      uri)
    (catch IllegalArgumentException _
      uri)))

(defn ^:private uri->canonical-path
  "Returns the URI's canonical path as a string using
  `java.io.file/getCanonicalPath`, of which see."
  [^java.net.URI uri]

  (let [uri-path (-> (.getPath uri) fs/path)
        canonical (-> uri-path .toString io/file .getCanonicalPath .toString)]
    canonical))

(defn ^:private jar-uri-string->jar-url-connection ^JarURLConnection [uri]
  (.openConnection (URL. (unescape-uri uri))))

(defn uri->filename
  "Converts a URI string into an absolute file path.

  The output path representation matches that of the operating system."
  [^String uri]
  (if (string/starts-with? uri "jar:")
    (let [conn (jar-uri-string->jar-url-connection uri)
          jar-file (uri-obj->filepath ^URI (.toURI ^URL (.getJarFileURL conn)))]
      (str jar-file ":" (.getEntryName conn)))

    (if-let [[_ uri-jar-path nested-file] (and (string/starts-with? uri "file:")
                                               (re-find #"^(.*\.jar):(.*)" uri))]
      (str (uri->canonical-path (URI. uri-jar-path)) ":" nested-file)

      ;; else
      (let [uri-obj (URI. uri)
            [_ jar-uri-path nested-file] (cond (= "zipfile" (.getScheme uri-obj))
                                               (re-find #"^(.*\.jar)::(.*)" (.getPath uri-obj)))]
        (if jar-uri-path
          (str (uri->canonical-path (URI. jar-uri-path)) ":" nested-file)
          (uri-obj->filepath uri-obj))))))

(defn ensure-jarfile [uri db]
  (let [jar-scheme? (= "jar" (get db [:settings :dependency-scheme]))]
    (if (or jar-scheme?
            (string/starts-with? uri "jar:"))
      uri
      (-> uri
          (string/replace "zipfile:" "jar:file:")
          (string/replace "::" "!/")))))

(defn ^:private filepath->uri-obj ^URI [filepath]
  (-> filepath io/file .toPath .toUri))

(defn ^:private uri-encode [scheme path]
  (.toString (URI. scheme "" path nil)))

(defn jar-file? [filename]
  (string/includes? filename ".jar"))

(defn class-file? [uri]
  (string/ends-with? uri ".class"))

(defn valid-url? [^String value]
  (try
    (URL. value)
    true
    (catch Exception _
      false)))

(defn ^:private filename->source-paths [filename source-paths]
  (filter #(string/starts-with? filename %) source-paths))

(defn uri->source-paths [uri source-paths]
  (filename->source-paths (uri->filename uri) source-paths))

(defn uri->source-path [uri source-paths]
  (first (uri->source-paths uri source-paths)))

(defn external-filename? [filename source-paths]
  (boolean
    (and filename
         (or (-> filename name jar-file?)
             (and (seq source-paths)
                  (not (seq (filename->source-paths filename source-paths))))))))

(def ^:private jar-file-with-filename-regex #"^(.*\.jar):(.*)")

(defn filename->uri
  "Converts an absolute file path into a file URI string.

  Jar files are given the `jar:file` or `zipfile` scheme depending on the
  `:dependency-scheme` setting."
  [^String filename db]
  (let [jar-scheme? (= "jar" (get-in db [:settings :dependency-scheme]))
        [_ jar-filepath nested-file] (re-find jar-file-with-filename-regex filename)]
    (conform-uri
      (if-let [jar-uri-path (some-> jar-filepath (-> filepath->uri-obj .getPath))]
        (if jar-scheme?
          (uri-encode "jar:file" (str jar-uri-path "!/" nested-file))
          (uri-encode "zipfile" (str jar-uri-path "::" nested-file)))
        (.toString (filepath->uri-obj filename)))
      (get-in db [:settings :uri-format]))))

(defn relativize-filepath
  "Returns absolute `path` (string) as relative file path starting at `root` (string)

  The output representation path matches that of the operating system."
  [path root]
  (let [path-obj (-> path io/file .toPath)]
    (if (.isAbsolute path-obj)
      (.toString (.relativize (-> root io/file .toPath) path-obj))
      path)))

(defn join-filepaths
  [& components]
  (.getPath ^java.io.File (apply io/file components)))

(defn namespace->uri [namespace source-path file-type db]
  (filename->uri
    (join-filepaths source-path
                    (-> namespace
                        (string/replace "." (System/getProperty "file.separator"))
                        (string/replace "-" "_")
                        (str "." (name file-type))))
    db))

(defn path-separators-to-system
  "Returns PATH with its file separators converted to match the system's
  file separators."
  [path]
  (let [system-sep fs/file-separator
        other-sep (if (= system-sep "/") "\\" "/")]
    (string/replace path
                    (re-pattern (Matcher/quoteReplacement other-sep))
                    (Matcher/quoteReplacement system-sep))))

(defn namespace+source-path->filename
  "Returns the path to the filename implied by NAMESPACE, starting at
  SOURCE-PATH and ending with FILE-TYPE.

  FILE-TYPE must be a keyword which will simply be converted to a
  string."
  [namespace source-path file-type]
  (let [source-path (path-separators-to-system source-path)
        ns-path (-> (string/replace namespace #"-" "_")
                    (string/replace #"\." (Matcher/quoteReplacement (System/getProperty "file.separator")))
                    (str "." (name file-type)))]
    (str (fs/path source-path ns-path))))

(defn ^:private path->folder-with-slash [^String path]
  (if (directory? (io/file path))
    (if (string/ends-with? path (System/getProperty "file.separator"))
      path
      (str path (System/getProperty "file.separator")))
    path))

(defn uri->namespace
  [uri db]
  (let [filename (uri->filename uri)
        project-root-uri (:project-root-uri db)
        source-paths (get-in db [:settings :source-paths])
        in-project? (when project-root-uri
                      (string/starts-with? uri project-root-uri))
        file-type (uri->file-type uri)]
    (when (and in-project? (not= :unknown file-type))
      (->> source-paths
           (map path->folder-with-slash)
           (filename->source-paths filename)
           (keep (fn [source-path]
                   (some-> (relativize-filepath filename source-path)
                           (->> (re-find #"^(.+)\.\S+$"))
                           (nth 1)
                           (string/replace (System/getProperty "file.separator") ".")
                           (string/replace #"_" "-"))))
           (reduce (fn [source-path-a source-path-b]
                     (cond
                       (not source-path-b) source-path-a
                       (not source-path-a) source-path-b
                       :else (if (> (count source-path-a)
                                    (count source-path-b))
                               source-path-b
                               source-path-a))) nil)))))

(defn inside?
  "Checks if element `a` is inside element `b` scope."
  [a b]
  (when (and a b)
    (let [a-name-row (:name-row a)
          a-name-col (:name-col a)
          b-name-row (:name-row b)
          b-name-col (:name-col b)
          b-end-row (or (:scope-end-row b)
                        (:end-row b)
                        (:name-end-row b))
          b-end-col (or (:scope-end-col b)
                        (:end-col b)
                        (:name-end-col b))]
      (and (or (< b-name-row a-name-row)
               (and (= b-name-row a-name-row)
                    (<= b-name-col a-name-col)))
           (or (< a-name-row b-end-row)
               (and (= a-name-row b-end-row)
                    (<= a-name-col b-end-col)))))))

(defn keywordize-first-depth
  [m]
  (into {}
        (for [[k v] m]
          [(keyword k) v])))

(defn position->row-col [position]
  [(inc (:line position))
   (inc (:character position))])

(defn row-col->position [row col]
  {:line (max 0 (dec row))
   :character (max 0 (dec col))})

(defn debounce-all
  "Debounce in channel with ms miliseconds returning all values."
  [in ms]
  (let [out (chan)]
    (go-loop [old-values []
              last-val nil]
      (let [values (if (nil? last-val)
                     [(<! in)]
                     (conj old-values last-val))
            timer (timeout ms)
            [new-val ch] (alts! [in timer])]
        (condp = ch
          timer (do (>! out values) (recur old-values nil))
          in (recur values new-val))))
    out))

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

(defn to-file ^java.io.File
  [^java.nio.file.Path path
   ^String child]
  (.toFile (.resolve path child)))

(defn normalize-file
  ^java.io.File [^java.io.File file]
  (.toFile (.normalize (.toPath file))))

(defn absolute-path [^String path db]
  (let [project-root-uri (get db :project-root-uri)]
    (if-let [^java.nio.file.Path project-root-path (some-> project-root-uri uri->path)]
      (str (.resolve project-root-path path))
      path)))

;; TODO move to a better place
(defn client-changes [changes db]
  (if (get-in db [:client-capabilities :workspace :workspace-edit :document-changes])
    {:document-changes changes}
    {:changes (into {} (map (fn [{:keys [text-document edits]}]
                              [(:uri text-document) edits])
                            changes))}))

(defn clojure-lsp-version []
  (string/trim (slurp (io/resource "CLOJURE_LSP_VERSION"))))

(defn format-time-delta-ms [start-time end-time]
  (format "%.0fms" (float (/ (- end-time start-time) 1000000))))

(defn start-time->end-time-ms [start-time]
  (format-time-delta-ms start-time (System/nanoTime)))

(defmacro logging-time
  "Executes `body` logging `message` formatted with the time spent
  from body."
  [message & body]
  (let [start-sym (gensym "start-time")]
    `(let [~start-sym (System/nanoTime)
           result# (do ~@body)]
       ~(with-meta
          `(logger/info (format ~message (start-time->end-time-ms ~start-sym)))
          (meta &form))
       result#)))

(defmacro logging-results
  "Executes `body`, passing the results to `results-fn`, which should return a
  results message string. Logs `message` formatted with the time spent from body
  and the results message."
  [message results-fn & body]
  (let [start-sym (gensym "start-time")
        results-msg (gensym "results-msg")]
    `(let [~start-sym (System/nanoTime)
           result# (do ~@body)
           ~results-msg (~results-fn result#)]
       ~(with-meta
          `(logger/info (format ~message (start-time->end-time-ms ~start-sym) ~results-msg))
          (meta &form))
       result#)))

(defmacro logging-task [task-id & body]
  (with-meta `(logging-time (str ~task-id " %s") ~@body)
             (meta &form)))

(defn ->range [{:keys [name-row name-end-row name-col name-end-col row end-row col end-col] :as element}]
  (when element
    {:start (row-col->position (or name-row row) (or name-col col))
     :end (row-col->position (or name-end-row end-row) (or name-end-col end-col))}))

(defn ->scope-range [{:keys [name-row name-end-row name-col name-end-col row end-row col end-col] :as element}]
  (when element
    {:start (row-col->position (or row name-row) (or col name-col))
     :end (row-col->position (or end-row name-end-row) (or end-col name-end-col))}))

(def full-file-range
  (->range {:row 1 :col 1 :end-row 1000000 :end-col 1000000}))

(defn ^:private paths->checksums
  "Return a map with file's last modified timestamp by filename."
  [paths]
  (reduce
    (fn [cks path]
      (let [file (io/file path)]
        (if-let [checksum (and (file-exists? file)
                               (.lastModified ^java.io.File file))]
          (assoc cks path checksum)
          cks)))
    {}
    paths))

(defn generate-and-update-analysis-checksums [paths global-db db]
  (let [old-checksums (merge (:analysis-checksums global-db)
                             (:analysis-checksums db))
        new-checksums (paths->checksums paths)
        paths-not-on-checksum (remove #(= (get old-checksums %)
                                          (get new-checksums %))
                                      paths)]
    {:new-checksums new-checksums
     :paths-not-on-checksum paths-not-on-checksum}))

(def preserve-kebab-case
  "Recursively convert map keywords to kebab-case strings, to avoid automatic
  camelCase conversion that happens in lsp4clj. This is useful when the client
  expects Clojure style JSON, or when a map needs to be round-tripped from
  clojure-lsp to the client and back without case changes."
  lsp.responses/preserve-kebab-case)

(defn normalize-uri-from-client [uri]
  ;; Technically jar:file is not a valid URI scheme. It's a Jar URL and must be
  ;; treated as such.
  (if (string/starts-with? uri "jar:")
    (.toString (.getURL (jar-uri-string->jar-url-connection uri)))
    ;; unescape %3a%3a to ::
    (let [uri (URI. uri)]
      ;; normalize scheme:/some/path to scheme:///some/path
      (uri-encode (.getScheme uri) (.getPath uri)))))

(defn sleep [ms]
  (Thread/sleep
   ;; long cast necessary to avoid reflection in JDK 19
   (long ms)))
