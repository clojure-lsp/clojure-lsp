(ns clojure-lsp.test-helper
  (:require
   [clojure-lsp.components :as components]
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.pprint :as pprint]
   [clojure.string :as string]
   [clojure.test :refer [is]]
   [rewrite-clj.zip :as z]))

(def windows? (string/starts-with? (System/getProperty "os.name") "Windows"))

(defn file-path [path]
  (cond-> path windows?
          (-> (string/replace-first #"^/" "C:\\\\")
              (->> (re-matches #"(.+?)(\.jar:.*)?"))
              (update 1 string/replace "/" "\\")
              rest
              (->> (apply str)))))

(defn file-uri [uri]
  (cond-> uri windows?
          (string/replace #"^(file|zipfile|jar:file):///(?!\w:/)" "$1:///c:/")))

(defn code [& strings] (string/join "\n" strings))

(defrecord TestProducer []
  producer/IProducer
  (refresh-code-lens [_this])
  (publish-diagnostic [_this _diagnostic])
  (publish-workspace-edit [_this _edit])
  (publish-progress [_this _percentage _message _progress-token])
  (show-document-request [_this _document-request])
  (show-message-request [_this _message _type _actions])
  (show-message [_this _message _type _extra])
  (refresh-test-tree [_this _uris]))

(defrecord TestLogger []
  logger/ILogger
  (setup [_])

  (set-log-path [_this _log-path])

  (-info [_this _fmeta _arg1])
  (-info [_this _fmeta _arg1 _arg2])
  (-info [_this _fmeta _arg1 _arg2 _arg3])
  (-warn [_this _fmeta _arg1])
  (-warn [_this _fmeta _arg1 _arg2])
  (-warn [_this _fmeta _arg1 _arg2 _arg3])
  (-error [_this _fmeta _arg1])
  (-error [_this _fmeta _arg1 _arg2])
  (-error [_this _fmeta _arg1 _arg2 _arg3])
  (-debug [_this _fmeta _arg1])
  (-debug [_this _fmeta _arg1 _arg2])
  (-debug [_this _fmeta _arg1 _arg2 _arg3]))

(def snapc components/snapc)

(defn make-components
  ([] (make-components {}))
  ([db-override]
   (snapc
     {:db* (atom (shared/deep-merge db/initial-db db-override))
      :logger (->TestLogger)
      :producer (->TestProducer)
      :current-changes-chan (async/chan 1)
      :diagnostics-chan (async/chan 1)
      :watched-files-chan (async/chan 1)
      :edits-chan (async/chan 1)})))

(defn db [components]
  (:db (snapc components)))

(defmacro with-db [components temp-config & body]
  `(let [components# ~components
         db-before# (db components#)]
     (try
       (swap! (:db* components#) shared/deep-merge ~temp-config)
       ~@body
       (finally
         (reset! (:db* components#) db-before#)))))

(defn take-or-timeout [c timeout-ms]
  (let [timeout (async/timeout timeout-ms)
        [val port] (async/alts!! [c timeout])]
    (is (= port c) "timeout waiting for message to be put on chan")
    val))

(defn assert-no-take [c timeout-ms]
  (let [timeout (async/timeout timeout-ms)
        [val port] (async/alts!! [c timeout])]
    (is (= port timeout) "received message on chan, but expected none")
    (is (nil? val))
    val))

(defn submap? [smaller larger]
  (every? (fn [[smaller-k smaller-v]]
            (let [larger-v (get larger smaller-k)]
              (if (map? smaller-v)
                (submap? smaller-v larger-v)
                (= smaller-v larger-v))))
          smaller))

(defn assert-submap [expected actual]
  (is (submap? expected actual)
      (str "Actual:\n\n" (pr-str actual) "\nExpected:\n\n" (pr-str expected))))

(defmacro assert-submaps
  "Asserts that maps are submaps of result in corresponding order and
  that the number of maps corresponds to the number of
  results. Returns true if all assertions passed (useful for REPL).

   taken from kondo"
  [maps result]
  `(let [maps# ~maps
         res# ~result]
     (and
       (is (= (count maps#) (count res#))
           (format "Expected %s results, but got: %s \n--\n%s--"
                   (count maps#) (count res#) (with-out-str (pprint/pprint res#))))
       (doseq [[r# m#] (map vector res# maps#)]
         (assert-submap m# r#)))))

(defmacro assert-contains-submaps
  "Asserts that maps are contained submaps of result in results. "
  [maps result]
  (let [r (gensym)]
    `(let [~r ~result]
       ~@(map (fn [m]
                `(is (some #(submap? ~m %) ~r)))
              maps))))

(defn positions-from-text
  "Takes text with a pipe `|` as a placeholder for cursor positions and returns the text without
   the pipes alone with a vector of [row col] pairs representing the cursor positions (1-based)"
  [text]
  (let [[_ _ text positions] (reduce
                               (fn [[row col text positions] ch]
                                 (case ch
                                   \|
                                   [row col text (conj positions [row col])]

                                   \newline
                                   [(inc row) 1 (.append text ch) positions]

                                   [row (inc col) (.append text ch) positions]))
                               [1 1 (java.lang.StringBuilder.) []]
                               text)]

    [(str text) positions]))

(def default-uri (file-uri "file:///a.clj"))

(defn load-code
  [code uri components]
  (let [[text positions] (positions-from-text code)]
    (handlers/did-open components {:text-document {:uri uri :text text}})
    positions))

(defn ->position [[row col]]
  {:line (dec row) :character (dec col)})

(defn ->range [start end]
  {:start (->position start)
   :end (->position end)})

(defn load-code-into-zloc-and-position
  "Load a `code` block into the kondo db at the provided `uri` and return a map
  containing the `:zloc` parsed from the code and the `:position` of the cursor.
  Useful for refactorings that consult both the rewrite-clj parse and the
  clj-kondo analysis."
  [code uri components]
  (let [[[row col] :as positions] (load-code code uri components)]
    (let [position-count (count positions)]
      (assert (= 1 position-count) (format "Expected one cursor, got %s" position-count)))
    {:position {:row row :col col}
     :zloc (-> (parser/zloc-of-file (db components) uri)
               (parser/to-pos row col))}))

(defn load-code-and-zloc
  "Load a `code` block into the kondo db at the provided `uri` and return a
  zloc parsed from the code. Useful for refactorings that consult both the
  rewrite-clj parse and the clj-kondo analysis."
  [code uri components]
  (:zloc (load-code-into-zloc-and-position code uri components)))

(defn zloc-from-code
  "Parse a zloc from a `code` block. Useful for refactorings that consult the
  rewrite-clj parse but not the clj-kondo analysis."
  [code]
  (let [[text [[row col] :as positions]] (positions-from-text code)]
    (let [position-count (count positions)]
      (assert (= 1 position-count) (format "Expected one cursor, got %s" position-count)))
    (let [zloc (parser/safe-zloc-of-string text)]
      (assert zloc "Unable to parse code")
      (parser/to-pos zloc row col))))

(defn- results->doc
  "Should mimic an LSP client processing results on a document"
  [doc doc-results]
  (let [lines->count (->> doc
                          string/split-lines
                          (map-indexed vector)
                          (map (juxt first (comp inc count second)))
                          (into {}))
        char-results (->> doc-results
                          (reduce
                            (fn [accum {:keys [loc] {:keys [row col end-row end-col]} :range}]
                              (let [start-char (apply +
                                                      (dec col)
                                                      (map lines->count
                                                           (range (dec row))))
                                    end-char (apply +
                                                    (dec end-col)
                                                    (map lines->count
                                                         (range (dec end-row))))]
                                (conj
                                  accum
                                  {:start start-char :end end-char :loc loc})))

                            [])
                          (sort-by :start)
                          (reduce
                            (fn [{:keys [char-delta] :as accum} {:keys [loc start end]}]
                              (-> accum
                                  (update :char-delta + (- (count loc) (- end start)))
                                  (update :doc (fn [d]
                                                 (str
                                                   (subs d 0 (+ char-delta start))
                                                   loc
                                                   (subs d (+ char-delta end)))))))

                            {:char-delta 0 :doc doc}))]
    (:doc char-results)))

(defn with-strings [results]
  (map #(update % :loc z/string) results))

(defn changes->code
  ([changes db]
   (changes->code changes default-uri db))
  ([changes uri db]
   (let [doc (get-in db [:documents uri :text])]
     (results->doc doc (vec (with-strings changes))))))

(defn changes-by-uri->code [changes-by-uri uri db]
  (changes->code (get changes-by-uri uri) uri db))
