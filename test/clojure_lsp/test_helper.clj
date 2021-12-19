(ns clojure-lsp.test-helper
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]
   [clojure.core.async :as async]
   [clojure.pprint :as pprint]
   [clojure.string :as string]
   [clojure.test :refer [is use-fixtures]]
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

(defn clean-db!
  ([]
   (clean-db! :unit-test))
  ([env]
   (reset! db/db {:env env})
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

(defn assert-submap [expected actual]
  (is (= expected
         (some-> actual (select-keys (keys expected))))
      (str "No superset of " (pr-str actual) " found")))

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

(defn load-code-and-locs [code & [filename]]
  (let [[code positions] (positions-from-text code)
        filename (or filename (file-uri "file:///a.clj"))]
    (handlers/did-open {:textDocument {:uri filename :text code}})
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
