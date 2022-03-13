(ns clojure-lsp.test-helper
  (:require
   [clojure-lsp.clojure-producer :as clojure-producer]
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.parser :as parser]
   [clojure.core.async :as async]
   [clojure.pprint :as pprint]
   [clojure.string :as string]
   [clojure.test :refer [is use-fixtures]]
   [lsp4clj.protocols :as protocols]
   [taoensso.timbre :as log]))

(def mock-diagnostics (atom {}))

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
  protocols/IProducer
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

(defn clean-db!
  ([]
   (clean-db! :unit-test))
  ([env]
   (reset! db/db (assoc db/initial-db
                        :env env
                        :producer (->TestProducer)))
   (reset! mock-diagnostics {})
   (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
   (alter-var-root #'db/current-changes-chan (constantly (async/chan 1)))
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
    (handlers/did-open {:textDocument {:uri uri :text code}})
    positions))

(defmacro with-mock-diagnostics [& body]
  `(do
     (reset! mock-diagnostics {})
     (with-redefs [async/put! #(swap! mock-diagnostics assoc (:uri %2) (:diagnostics %2))]
       ~@body)))

(defn edits [after-fn]
  (async/take! db/edits-chan after-fn))

(defn ->position [[row col]]
  {:line (dec row) :character (dec col)})

(defn ->range [[row col] [end-row end-col]]
  {:start {:line (dec row) :character (dec col)}
   :end {:line (dec end-row) :character (dec end-col)}})

(defn load-code-and-zloc
  "Load a `code` block into the kondo db at the provided `uri` and return a
  zloc parsed from the code. Useful for refactorings that consult the kondo db."
  ([code] (load-code-and-zloc code default-uri))
  ([code uri]
   (let [[[row col] :as positions] (load-code-and-locs code uri)]
     (let [position-count (count positions)]
       (assert (= 1 position-count) (format "Expected one cursor, got %s" position-count)))
     (-> (parser/zloc-of-file @db/db uri)
         (parser/to-pos row col)))))

(defn zloc-from-code
  "Parse a zloc from a `code` block. Useful for refactorings that do not consult
  the kondo db."
  [code]
  (let [[code [[row col] :as positions]] (positions-from-text code)]
    (let [position-count (count positions)]
      (assert (= 1 position-count) (format "Expected one cursor, got %s" position-count)))
    (-> (parser/zloc-of-string code)
        (parser/to-pos row col))))
