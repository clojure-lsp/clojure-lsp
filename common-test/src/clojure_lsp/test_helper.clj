(ns clojure-lsp.test-helper
  (:require
   [clojure-lsp.clojure-producer :as clojure-producer]
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure.core.async :as async]
   [clojure.pprint :as pprint]
   [clojure.string :as string]
   [clojure.test :refer [is use-fixtures]]
   [lsp4clj.components :as components]
   [lsp4clj.protocols.logger :as logger]
   [lsp4clj.protocols.producer :as producer]
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
  producer/ILSPProducer
  (refresh-code-lens [_this])
  (publish-diagnostic [_this _diagnostic])
  (publish-workspace-edit [_this _edit])
  (publish-progress [_this _percentage _message _progress-token])
  (show-document-request [_this _document-request])
  (show-message-request [_this _message _type _actions])
  (show-message [_this _message _type _extra])
  (register-capability [_this _capability])
  clojure-producer/IClojureProducer
  (refresh-test-tree [_this _uris]))

(defrecord TestLogger []
  logger/ILSPLogger
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

(def components
  (components/->components
    db/db*
    (->TestLogger)
    (->TestProducer)))

(defn clean-db!
  ([]
   (clean-db! :unit-test))
  ([env]
   (reset! db/db* (assoc db/initial-db
                         :env env
                         :producer (:producer components)
                         #_#_:settings {:experimental {:dep-graph-queries true}}))
   (alter-var-root #'db/current-changes-chan (constantly (async/chan 1)))
   (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
   (alter-var-root #'db/created-watched-files-chan (constantly (async/chan 1)))
   (alter-var-root #'db/edits-chan (constantly (async/chan 1)))))

(defn reset-db-after-test
  ([]
   (reset-db-after-test :unit-test))
  ([env]
   (use-fixtures
     :each
     (fn [f]
       (clean-db! env)
       (f)))))

(defmacro let-mock-chans [bindings & body]
  (assert (even? (count bindings)))
  (let [bs (partition 2 bindings)
        let-bindings (mapcat (fn [[binding _]]
                               `[~binding (async/chan 1)])
                             bs)
        alter-vars (map (fn [[binding chan-var]]
                          `(alter-var-root ~chan-var (constantly ~binding)))
                        bs)]
    `(let [~@let-bindings]
       ~@alter-vars
       ~@body)))

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
   the pipes alone with a vector of [line column] pairs representing the cursor positions (1-based)"
  [text]
  (let [[_ _ text positions] (reduce
                               (fn [[row column text positions] ch]
                                 (cond
                                   (= \| ch)
                                   [row column text (conj positions [row column])]

                                   (= \newline ch)
                                   [(inc row) 1 (str text ch) positions]

                                   :else
                                   [row (inc column) (str text ch) positions]))
                               [1 1 "" []]
                               text)]

    [text positions]))

(def default-uri (file-uri "file:///a.clj"))

(defn load-code-and-locs [code & [uri]]
  (let [[code positions] (positions-from-text code)
        uri (or uri default-uri)]
    (handlers/did-open {:textDocument {:uri uri :text code}} components)
    positions))

(defn ->position [[row col]]
  {:line (dec row) :character (dec col)})

(defn ->range [[row col] [end-row end-col]]
  {:start {:line (dec row) :character (dec col)}
   :end {:line (dec end-row) :character (dec end-col)}})

(defn load-code-into-zloc-and-position
  "Load a `code` block into the kondo db at the provided `uri` and return a map
  containing the `:zloc` parsed from the code and the `:position` of the cursor.
  Useful for refactorings that consult the kondo db."
  ([code] (load-code-into-zloc-and-position code default-uri))
  ([code uri]
   (let [[[row col] :as positions] (load-code-and-locs code uri)]
     (let [position-count (count positions)]
       (assert (= 1 position-count) (format "Expected one cursor, got %s" position-count)))
     {:position {:row row :col col}
      :zloc (-> (parser/zloc-of-file @db/db* uri)
                (parser/to-pos row col))})))

(defn load-code-and-zloc
  "Load a `code` block into the kondo db at the provided `uri` and return a
  zloc parsed from the code. Useful for refactorings that consult the kondo db."
  ([code] (load-code-and-zloc code default-uri))
  ([code uri]
   (:zloc (load-code-into-zloc-and-position code uri))))

(defn zloc-from-code
  "Parse a zloc from a `code` block. Useful for refactorings that do not consult
  the kondo db."
  [code]
  (let [[code [[row col] :as positions]] (positions-from-text code)]
    (let [position-count (count positions)]
      (assert (= 1 position-count) (format "Expected one cursor, got %s" position-count)))
    (let [zloc (parser/safe-zloc-of-string code)]
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

(defn use-dep-graph? [db]
  (#'q/use-dep-graph? db))
