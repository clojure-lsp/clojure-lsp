(ns clojure-lsp.code-actions-perf-test
  (:require
   [clojure-lsp.debug :as debug]
   [clojure-lsp.feature.code-actions :as code-actions]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

;; TODO: these tests take somewhere around 5 minutes, should they be only be run in a "performance" test suite?

(h/reset-components-before-test)

;; an failure will occur if the mean runtime is higher than 
;; this number of milliseconds; in my testing runtime is rarely larger than
;; 1 millisecond.  It's placed high in case there's a noisy neighbor slowing it down.
(def ^:private max-runtime-ms 500)

;; measure time to move cursor (executed and measured multiple times, by criterium)
(defn ^:private invoke-all-at [s row col]
  (h/reset-components!)
  (let [file-uri (h/file-uri "file:///a.clj")
        positioned-code (h/put-cursor-at s row col col)
        zloc (h/load-code-and-zloc positioned-code)
        diagnostics []
        client-capabilities {:workspace {:workspace-edit false}}
        mean (debug/estimate-runtime-ms-quick-bench
               (code-actions/all zloc file-uri row col diagnostics client-capabilities (h/db)))]
    mean))

;; Return the mean time for invoking body for all positions in the source file s. Only
;; one measurement is taken.
;;
;; the code and zlocs are computed in advance so computation isn't counted in the
;; time to execute the execution-body.  In my experience this number is actually low, 
;; in the 1 to 10 milliseconds for the whole run.
(defn ^:private invoke-all-mult-naive [s body]
  (h/reset-components!)
  (let [file-uri (h/file-uri "file:///a.clj")
        diagnostics []
        client-capabilities {:workspace {:workspace-edit false}}
        precreated-code (for [[row cols] (map-indexed #(vector %1 (count %2))
                                                      (string/split s #"\n"))
                              :let [r (inc row)]
                              c (range 1 (inc cols))
                              :let [positioned-code (h/put-cursor-at s r c c)
                                    zloc (h/load-code-and-zloc positioned-code)
                                    root (edit/to-root zloc)]]
                          {:root root
                           :code positioned-code
                           :row r
                           :col c})]
    (debug/estimate-runtime-ms-naive
      (doseq [{r :row c :col code :code root :root} precreated-code]
        (body root file-uri r c diagnostics client-capabilities (h/db) code)))))

(def test-code "
(ns exercises.lsp-features
  (:require [clojure.string :as str]
            [clojure.set :as set]))    ;; clean namespace

;; extract function
(defn simple-math [x y]
  (let [result (+ x y)]                   
    (println result)))

;; inline symbol
(defn inline-example []
  (let [unnecessary-var 10]
    (+ unnecessary-var 5)))

;; move to let
(defn move-to-let-example [s]
  (let [prefix \" ID- \"]
    (str prefix (str/upper-case s))))

;; change collection to list
(def my-list [1 2 3])

;; change collection to vector
(def my-coll {:a 1 :b 2})

;; convert cond to if
(defn cond-to-if-example [x]
  (cond
  (> x 10)
  \"big\"

  :else
  \"small\"))

;; convert if to cond
(defn if-to-cond-example [x]
  (if (> x 10)
    \"big\"
    \"small\"))

;; change to thread first
(defn threading-example [s]
  (str/upper-case (str/trim s)))

;; unwind thread
(defn unwind-example [data]
  (-> s
    str/trim
    str/upper-case))

;; cycle privacy
(defn private-test [a]
  (* a 2))

;; introduce let
(defn intro-let []
  (println (+ 1 2 3)))

;; destructure keys (on m)
(defn destructure-example [m]
  (let [a (:a m)
        b (:b m)]
    (+ a b)))

;; sort map
(def my-map {:z 1 :a 2 :m 3})
")

;; Note: really we'd want to measure results with (+ mean (* 2 stddev)) but that 
;; isn't directly exposed.  Using only mean is good enough for getting started.
(deftest ^:perf measure-code-actions-all-performance
  ;; This takes about 20 seconds
  (let [code (str "(ns a)\n"
                  "(fn [] 5)\n")]
    (testing "minimal tests"
      (is (> max-runtime-ms (invoke-all-at code 1 2)))
      (is (> max-runtime-ms (invoke-all-at code 2 7)))))
  ;; This takes about 10 seconds
  (testing "bigger test"
    (is (> max-runtime-ms (invoke-all-at test-code 8 16))))

  ;; This takes about 3 minutes to run
  (testing "sequentially hit all positions in a small file, measuring multiple times for each position"
    (let [code "(defn a [b]\n (let [c 1] (b c)))"]
      (doseq [[row cols] (map-indexed #(vector (inc %1) (inc (count %2)))
                                      (string/split code #"\n"))]
        (doseq [col (range 1 cols)]
          (is (> max-runtime-ms (invoke-all-at code row col)))))))

  ;; TODO: should I remove this atom?  It's only used for debugging, but it's not slowing down the test
  ;; much.  Alternatively, I could move it into a let.
  ;;
  ;; the tests below invoke code-actions/all multiple times, the results are placed in this atom
  ;; for inspection during debugging
  #_{:clj-kondo/ignore [:inline-def]}
  (def all-answers* (atom []))

  ;; takes around 60 seconds to run
  (testing "measure the time it takes to invoke all as we walk through every position in the file sequentially"
    (reset! all-answers* [])
    (let [position-count (reduce #(+ %1 (count %2)) 0 (string/split test-code #"\n"))
          measure-with-all-fn (fn [root file-uri r c diagnostics client-capabilities db code]
                                (let [menu-results (code-actions/all root file-uri r c diagnostics client-capabilities db)]
                                  (swap! all-answers* conj {:row r :col c :code code :runtime-ms menu-results})))
          measure-without-all-fn (fn [_root _file-uri _r _c _diagnostics _client-capabilities _db _code])
          without-all-runtime (invoke-all-mult-naive test-code measure-without-all-fn)
          with-all-runtime (invoke-all-mult-naive test-code measure-with-all-fn)
          ms-inside-all (- with-all-runtime without-all-runtime)
          per-loc-runtime (/ ms-inside-all position-count)]
      (is (> max-runtime-ms per-loc-runtime))))

  ;; takes around 60 seconds to run
  (testing "measure the time it takes to invoke all as we walk through every position in the file, jumping from 1,1 each time"
    (reset! all-answers* [])
    (let [position-count (reduce #(+ %1 (count %2)) 0 (string/split test-code #"\n"))
          measure-with-all-fn (fn [root file-uri r c diagnostics client-capabilities db code]
                                (code-actions/all root file-uri 1 1 diagnostics client-capabilities db)
                                (let [menu-results (code-actions/all root file-uri r c diagnostics client-capabilities db)]
                                  (swap! all-answers* conj {:row r :col c :code code :runtime-ms menu-results})))
          measure-without-all-fn (fn [_root _file-uri _r _c _diagnostics _client-capabilities _db _code])
          without-all-runtime (invoke-all-mult-naive test-code measure-without-all-fn)
          with-all-runtime (invoke-all-mult-naive test-code measure-with-all-fn)
          ms-inside-all (- with-all-runtime without-all-runtime)
          per-loc-runtime (/ ms-inside-all position-count)]
      (is (> max-runtime-ms per-loc-runtime)))))
