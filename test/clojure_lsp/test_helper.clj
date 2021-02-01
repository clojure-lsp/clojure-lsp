(ns clojure-lsp.test-helper
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.handlers :as handlers]
    [clojure.core.async :as async]
    [clojure.pprint :as pprint]
    [clojure.test :refer [is use-fixtures]]
    [taoensso.timbre :as log]))

(defn reset-db-after-test []
  (use-fixtures
    :each
    (fn [f]
      (reset! db/db {})
      (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1))
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
        filename (or filename "file:///a.clj")]
    (handlers/did-open {:textDocument {:uri filename :text code}})
    positions))

(defn ->position [[row col]]
  {:line (dec row) :character (dec col)})

(defn ->range [[row col] [end-row end-col]]
  {:start {:line (dec row) :character (dec col)}
   :end {:line (dec end-row) :character (dec end-col)}})
