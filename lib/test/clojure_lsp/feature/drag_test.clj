(ns clojure-lsp.feature.drag-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.drag :as f.drag]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(h/reset-db-after-test)

(defn can-drag-zloc-backward? [zloc]
  (f.drag/can-drag-backward? zloc h/default-uri @db/db*))

(defn can-drag-zloc-forward? [zloc]
  (f.drag/can-drag-forward? zloc h/default-uri @db/db*))

(defn can-drag-code-backward? [code]
  (can-drag-zloc-backward? (h/load-code-and-zloc code)))

(defn can-drag-code-forward? [code]
  (can-drag-zloc-forward? (h/load-code-and-zloc code)))

;; These are only the negative cases, proving when drag-backward is NOT offered
;; in the actions menu. The positive cases are all tested indirectly via
;; assert-drag-backward, since if a movement happens, it implicitly passed
;; can-drag-backward?
(deftest can-drag-backward?
  (testing "unbalanced pairs"
    (is (not (can-drag-code-backward? "{1 2 |3}")))
    (is (not (can-drag-code-backward? "(let [1 2 |3])"))))
  (testing "outside collection"
    (is (not (can-drag-code-backward? "|")))
    (is (not (can-drag-code-backward? "|[]")))
    (is (not (can-drag-code-backward? "|{:a :b :c :d}")))
    (is (not (can-drag-code-backward? "|#{:a :b :c :d}")))
    (is (not (can-drag-code-backward? "|[:a :b :c :d]")))
    (is (not (can-drag-code-backward? "|'(:a :b :c :d)"))))
  (testing "on first entry"
    (is (not (can-drag-code-backward? "[|1]")))
    (is (not (can-drag-code-backward? "#{|1 2 3}")))
    (is (not (can-drag-code-backward? "[|1 2 3]")))
    (is (not (can-drag-code-backward? "'(|1 2 3)")))
    (is (not (can-drag-code-backward? "{|1 2}")))
    (is (not (can-drag-code-backward? "{1 |2}")))
    (is (not (can-drag-code-backward? "{|:a :b :c :d}")))
    (is (not (can-drag-code-backward? "{:a |:b :c :d}")))
    (is (not (can-drag-code-backward? "(let [|a 1 c 2])")))
    (is (not (can-drag-code-backward? "(let [a |1 c 2])")))
    (is (not (can-drag-code-backward? "|(def a) (def b)")))
    (testing "of special functions"
      ;; from rind of cond
      (is (not (can-drag-code-backward? "(|cond a 1 b 2)")))
      ;; from first pair of cond
      (is (not (can-drag-code-backward? "(cond |a 1 b 2)")))
      (is (not (can-drag-code-backward? "(cond a |1 b 2)")))
      ;; from rind of assoc
      (is (not (can-drag-code-backward? "(|assoc x :a 1 :b 2)")))
      (is (not (can-drag-code-backward? "(assoc |x :a 1 :b 2)")))
      ;; from first pair of assoc
      (is (not (can-drag-code-backward? "(assoc x |:a 1 :b 2)")))
      (is (not (can-drag-code-backward? "(assoc x :a |1 :b 2)")))
      ;; from rind of assoc!
      (is (not (can-drag-code-backward? "(|assoc! x :a 1 :b 2)")))
      (is (not (can-drag-code-backward? "(assoc! |x :a 1 :b 2)")))
      ;; from first pair of assoc!
      (is (not (can-drag-code-backward? "(assoc! x |:a 1 :b 2)")))
      (is (not (can-drag-code-backward? "(assoc! x :a |1 :b 2)")))
      ;; from rind of cond->
      (is (not (can-drag-code-backward? "(|cond-> x a inc b dec)")))
      (is (not (can-drag-code-backward? "(cond-> |x a inc b dec)")))
      ;; from first pair of cond->
      (is (not (can-drag-code-backward? "(cond-> x |a inc b dec)")))
      (is (not (can-drag-code-backward? "(cond-> x a |inc b dec)")))
      ;; from rind of cond->>
      (is (not (can-drag-code-backward? "(|cond->> x a inc b dec)")))
      (is (not (can-drag-code-backward? "(cond->> |x a inc b dec)")))
      ;; from first pair of cond->>
      (is (not (can-drag-code-backward? "(cond->> x |a inc b dec)")))
      (is (not (can-drag-code-backward? "(cond->> x a |inc b dec)")))
      ;; from rind of case
      (is (not (can-drag-code-backward? "(|case a 1 :first 2 :second)")))
      (is (not (can-drag-code-backward? "(case |a 1 :first 2 :second)")))
      (is (not (can-drag-code-backward? "(case a 1 :first |:else)")))
      (is (not (can-drag-code-backward? "(case a |:else)")))
      ;; from first pair of case
      (is (not (can-drag-code-backward? "(case a |1 :first)")))
      (is (not (can-drag-code-backward? "(case a 1 |:first)")))
      (is (not (can-drag-code-backward? "(case a |1 :first 2 :second)")))
      (is (not (can-drag-code-backward? "(case a 1 |:first 2 :second)")))
      (is (not (can-drag-code-backward? "(case a |1 :first :else)")))
      (is (not (can-drag-code-backward? "(case a 1 |:first :else)")))
      ;; from rind of condp
      (is (not (can-drag-code-backward? "(|condp = x a 1 b 2)")))
      (is (not (can-drag-code-backward? "(condp |= x a 1 b 2)")))
      (is (not (can-drag-code-backward? "(condp = |x a 1 b 2)")))
      (is (not (can-drag-code-backward? "(condp = x a 1 |:else)")))
      (is (not (can-drag-code-backward? (h/code "(condp some [1 2 3 4]"
                                                "  #{0 6 7} :>> inc"
                                                "  #{4 5 9} :>> dec"
                                                "  |:else)"))))
      ;; from first pair of condp
      (is (not (can-drag-code-backward? "(condp = |x a 1 b 2)")))
      (is (not (can-drag-code-backward? "(condp = x |a 1 b 2)")))
      (is (not (can-drag-code-backward? (h/code "(condp some [1 2 3 4]"
                                                "  #{0 6 7} :>> |inc)"))))
      ;; in invalid ternary form of condp
      (is (not (can-drag-code-backward? (h/code "(condp some [1 2 3 4]"
                                                "  #{0 6 7} :>> inc"
                                                "  |#{4 5 9} :>> dec"
                                                "  :else :invalid)"))))
      ;; from rind of are
      (is (not (can-drag-code-backward? (h/code "(|are [x] (= x 1) 1)"))))
      (is (not (can-drag-code-backward? (h/code "(are |[x] (= x 1) 1)"))))
      (is (not (can-drag-code-backward? (h/code "(are [x] |(= x 1) 1)"))))
      ;; from first group of are
      (is (not (can-drag-code-backward? (h/code "(are [expected x] (= expected x) |1 2 3 4)"))))
      (is (not (can-drag-code-backward? (h/code "(are [expected x] (= expected x) 1 |2 3 4)"))))
      ;; from unbalanced are
      (is (not (can-drag-code-backward? (h/code "(are [expected x] (= expected x) 1 2 |3 4 5)"))))
      ;; from invalid are
      (is (not (can-drag-code-backward? (h/code "(are [] (= 1 1) 1 |2)")))))))

;; These are only the negative cases, proving when drag-forward is NOT offered in
;; the actions menu. The positive cases are all tested indirectly via
;; assert-drag-forward, since if a movement happens, it implicitly passed
;; can-drag-forward?
(deftest can-drag-forward?
  (testing "unbalanced pairs"
    (is (not (can-drag-code-forward? "{|1 2 3}")))
    (is (not (can-drag-code-forward? "(let [|1 2 3])"))))
  (testing "outside collection"
    (is (not (can-drag-code-forward? "|[]")))
    (is (not (can-drag-code-forward? "|{:a :b :c :d}")))
    (is (not (can-drag-code-forward? "|#{:a :b :c :d}")))
    (is (not (can-drag-code-forward? "|[:a :b :c :d]")))
    (is (not (can-drag-code-forward? "|'(:a :b :c :d)"))))
  (testing "on last entry"
    (is (not (can-drag-code-forward? "[|1]")))
    (is (not (can-drag-code-forward? "#{1 2 |3}")))
    (is (not (can-drag-code-forward? "[1 2 |3]")))
    (is (not (can-drag-code-forward? "'(1 2 |3)")))
    (is (not (can-drag-code-forward? "{|1 2}")))
    (is (not (can-drag-code-forward? "{1 |2}")))
    (is (not (can-drag-code-forward? "{:a :b |:c :d}")))
    (is (not (can-drag-code-forward? "{:a :b :c |:d}")))
    (is (not (can-drag-code-forward? "(let [a 1 |c 2])")))
    (is (not (can-drag-code-forward? "(let [a 1 c |2])")))
    (is (not (can-drag-code-forward? "(def a) |(def b)")))
    (testing "of special functions"
      ;; from rind of cond
      (is (not (can-drag-code-forward? "(|cond a 1 b 2)")))
      ;; from last pair of cond
      (is (not (can-drag-code-forward? "(cond a 1 |b 2)")))
      (is (not (can-drag-code-forward? "(cond a 1 b |2)")))
      ;; from rind of cond->
      (is (not (can-drag-code-forward? "(|cond-> x a inc b dec)")))
      (is (not (can-drag-code-forward? "(cond-> |x a inc b dec)")))
      ;; from last pair of cond->
      (is (not (can-drag-code-forward? "(cond-> x a inc |b dec)")))
      (is (not (can-drag-code-forward? "(cond-> x a inc b |dec)")))
      ;; from rind of assoc
      (is (not (can-drag-code-forward? "(|assoc x :a 1 :b 2)")))
      (is (not (can-drag-code-forward? "(assoc |x :a 1 :b 2)")))
      ;; from last pair of assoc
      (is (not (can-drag-code-forward? "(assoc x :a 1 |:b 2)")))
      (is (not (can-drag-code-forward? "(assoc x :a 1 :b |2)")))
      ;; from rind of assoc!
      (is (not (can-drag-code-forward? "(|assoc! x :a 1 :b 2)")))
      (is (not (can-drag-code-forward? "(assoc! |x :a 1 :b 2)")))
      ;; from last pair of assoc!
      (is (not (can-drag-code-forward? "(assoc! x :a 1 |:b 2)")))
      (is (not (can-drag-code-forward? "(assoc! x :a 1 :b |2)")))
      ;; from rind of cond->>
      (is (not (can-drag-code-forward? "(|cond->> x a inc b dec)")))
      (is (not (can-drag-code-forward? "(cond->> |x a inc b dec)")))
      ;; from last pair of cond->>
      (is (not (can-drag-code-forward? "(cond->> x a inc |b dec)")))
      (is (not (can-drag-code-forward? "(cond->> x a inc b |dec)")))
      ;; from rind of case
      (is (not (can-drag-code-forward? "(|case a 1 :first 2 :second)")))
      (is (not (can-drag-code-forward? "(case |a 1 :first 2 :second)")))
      (is (not (can-drag-code-forward? "(case a 1 :first |:else)")))
      (is (not (can-drag-code-forward? "(case a |:else)")))
      ;; from last pair of case
      (is (not (can-drag-code-forward? "(case a 1 :first |2 :second)")))
      (is (not (can-drag-code-forward? "(case a 1 :first 2 |:second)")))
      (is (not (can-drag-code-forward? "(case a 1 :first |2 :second :else)")))
      (is (not (can-drag-code-forward? "(case a 1 :first 2 |:second :else)")))
      ;; from rind of condp
      (is (not (can-drag-code-forward? "(|condp = x a 1 b 2)")))
      (is (not (can-drag-code-forward? "(condp |= x a 1 b 2)")))
      (is (not (can-drag-code-forward? "(condp = |x a 1 b 2)")))
      (is (not (can-drag-code-forward? "(condp = x a 1 |:else)")))
      ;; from last pair of condp
      (is (not (can-drag-code-forward? "(condp = x a 1 |b 2)")))
      (is (not (can-drag-code-forward? "(condp = x a 1 b |2)")))
      (is (not (can-drag-code-forward? (h/code "(condp some [1 2 3 4]"
                                               "  |#{4 5 9} :>> dec"
                                               "  :else)"))))
      ;; from rind of are
      (is (not (can-drag-code-forward? (h/code "(|are [x] (= x 1) 1)"))))
      (is (not (can-drag-code-forward? (h/code "(are |[x] (= x 1) 1)"))))
      (is (not (can-drag-code-forward? (h/code "(are [x] |(= x 1) 1)"))))
      ;; from last group of are
      (is (not (can-drag-code-forward? (h/code "(are [expected x] (= expected x) 1 2 |3 4)"))))
      (is (not (can-drag-code-forward? (h/code "(are [expected x] (= expected x) 1 2 3 |4)"))))
      ;; from unbalanced are
      (is (not (can-drag-code-forward? (h/code "(are [expected x] (= expected x) |1 2 3 4 5)"))))
      ;; from invalid are
      (is (not (can-drag-code-forward? (h/code "(are [] (= 1 1) |1 2)")))))))

(defn drag-zloc-backward [zloc]
  (f.drag/drag-backward zloc h/default-uri @db/db*))

(defn drag-zloc-forward [zloc]
  (f.drag/drag-forward zloc h/default-uri @db/db*))

(defn drag-code-backward [code]
  (drag-zloc-backward (h/load-code-and-zloc code)))

(defn drag-code-forward [code]
  (drag-zloc-forward (h/load-code-and-zloc code)))

(defn- as-string [change]
  (some-> change
          :changes-by-uri
          (get h/default-uri)
          (h/changes->code @db/db*)))

(defn- as-range [change]
  (some-> change
          :changes-by-uri
          (get h/default-uri)
          first
          :range))

(defn- as-position [change]
  (when-let [{:keys [row col end-row end-col]} (some-> change
                                                       :show-document-after-edit
                                                       :range)]
    [[row col] [end-row end-col]]))

;; These are macros so test failures have the right line numbers
(defmacro assert-drag-backward [expected code]
  `(let [moved#         (drag-code-backward ~code)
         [text# [pos#]] (h/positions-from-text ~expected)]
     (is (= text# (as-string moved#)))
     ;; Range should only be single char
     (is (= [pos# pos#] (as-position moved#)))))
(defmacro assert-drag-forward [expected code]
  `(let [moved#         (drag-code-forward ~code)
         [text# [pos#]] (h/positions-from-text ~expected)]
     (is (= text# (as-string moved#)))
     (is (= [pos# pos#] (as-position moved#)))))

(deftest drag-backward
  (testing "common cases"
    (assert-drag-backward (h/code "{|3 4 1 2}")
                          (h/code "{1 2 |3 4}"))
    (assert-drag-backward (h/code "{|3 4, 1 2}")
                          (h/code "{1 2, |3 4}"))
    (assert-drag-backward (h/code "{|3 4"
                                  " 1 2}")
                          (h/code "{1 2"
                                  " |3 4}"))
    (assert-drag-backward (h/code "{|3 4"
                                  " 1 2}")
                          (h/code "{1 2"
                                  " 3 |4}"))
    (assert-drag-backward (h/code "{|3 4,"
                                  " 1 2}")
                          (h/code "{1 2,"
                                  " |3 4}"))
    (assert-drag-backward (h/code "{|:b (+ 1 1)"
                                  " :a 1"
                                  " :c 3}")
                          (h/code "{:a 1"
                                  " |:b (+ 1 1)"
                                  " :c 3}"))
    (assert-drag-backward (h/code "[|2 1]")
                          (h/code "[1 |2]"))
    (assert-drag-backward (h/code "[|:b"
                                  " :a]")
                          (h/code "[:a"
                                  " |:b]"))
    (assert-drag-backward (h/code "[1 |3 2]")
                          (h/code "[1 2 |3]"))
    (assert-drag-backward (h/code "(def a 1)"
                                  "[|2 a]")
                          (h/code "(def a 1)"
                                  "[a |2]"))
    (assert-drag-backward (h/code "(let [a [1 2]"
                                  "      |c 3"
                                  "      b 2])")
                          (h/code "(let [a [1 2]"
                                  "      b 2"
                                  "      |c 3])"))
    (assert-drag-backward (h/code "(binding [|b 2"
                                  "          a 1]"
                                  "  (+ a b))")
                          (h/code "(binding [a 1"
                                  "          |b 2]"
                                  "  (+ a b))"))
    (assert-drag-backward (h/code "(for [|b [3 4]"
                                  "      a [1 2]"
                                  "      :let [c (inc a)]]"
                                  "  (+ a b c))")
                          (h/code "(for [a [1 2]"
                                  "      |b [3 4]"
                                  "      :let [c (inc a)]]"
                                  "  (+ a b c))"))
    (assert-drag-backward (h/code "(for [a [1 2]"
                                  "      :let [|c (inc b)"
                                  "            b (inc a)]]"
                                  "  (+ a b c))")
                          (h/code "(for [a [1 2]"
                                  "      :let [b (inc a)"
                                  "            |c (inc b)]]"
                                  "  (+ a b c))"))
    (assert-drag-backward (h/code "(ns foo"
                                  "  {:clj-kondo/config '{:lint-as {foo/foo clojure.core/let}}})"
                                  "(defmacro foo [bs & form]"
                                  "  `(let [~@bs]"
                                  "     ~@form))"
                                  "(foo [|b 2"
                                  "      a 1]"
                                  "  (inc a))")
                          (h/code "(ns foo"
                                  "  {:clj-kondo/config '{:lint-as {foo/foo clojure.core/let}}})"
                                  "(defmacro foo [bs & form]"
                                  "  `(let [~@bs]"
                                  "     ~@form))"
                                  "(foo [a 1"
                                  "      |b 2]"
                                  "  (inc a))"))
    (assert-drag-backward (h/code "|(def b) (def a)")
                          (h/code "(def a) |(def b)"))
    (assert-drag-backward (h/code "|(def b) (def a) (def c)")
                          (h/code "(def a) |(def b) (def c)"))
    (is (= {:row 1 :col 1
            :end-row 2 :end-col 8}
           (as-range
             (drag-code-backward (h/code "(def a)"
                                         "|(def b)"))))))
  (testing "within special functions"
    (assert-drag-backward (h/code "(cond |b 2 a 1)")
                          (h/code "(cond a 1 |b 2)"))
    (assert-drag-backward (h/code "(cond |b 2 a 1)")
                          (h/code "(cond a 1 b |2)"))
    (assert-drag-backward (h/code "(assoc x |:b 1 :a 2)")
                          (h/code "(assoc x :a 2 |:b 1)"))
    (assert-drag-backward (h/code "(assoc x |:b 1 :a 2)")
                          (h/code "(assoc x :a 2 :b |1)"))
    (assert-drag-backward (h/code "#(assoc % |:b 2 :a 1)")
                          (h/code "#(assoc % :a 1 |:b 2)"))
    (assert-drag-backward (h/code "(-> {}"
                                  "    (assoc |:b 1 :a 2))")
                          (h/code "(-> {}"
                                  "    (assoc :a 2 |:b 1))"))
    (assert-drag-backward (h/code "(some-> {}"
                                  "        (assoc |:b 1 :a 2))")
                          (h/code "(some-> {}"
                                  "        (assoc :a 2 |:b 1))"))
    (assert-drag-backward (h/code "(cond-> {}"
                                  "  true (assoc |:b 1 :a 2))")
                          (h/code "(cond-> {}"
                                  "  true (assoc :a 2 |:b 1))"))
    (assert-drag-backward (h/code "(assoc! x |:b 1 :a 2)")
                          (h/code "(assoc! x :a 2 |:b 1)"))
    (assert-drag-backward (h/code "(assoc! x |:b 1 :a 2)")
                          (h/code "(assoc! x :a 2 :b |1)"))
    (assert-drag-backward (h/code "(-> {}"
                                  "    (assoc! |:b 1 :a 2))")
                          (h/code "(-> {}"
                                  "    (assoc! :a 2 |:b 1))"))
    (assert-drag-backward (h/code "(some-> {}"
                                  "        (assoc! |:b 1 :a 2))")
                          (h/code "(some-> {}"
                                  "        (assoc! :a 2 |:b 1))"))
    (assert-drag-backward (h/code "(cond-> {}"
                                  "  true (assoc! |:b 1 :a 2))")
                          (h/code "(cond-> {}"
                                  "  true (assoc! :a 2 |:b 1))"))
    (assert-drag-backward (h/code "(cond-> x |b dec a inc)")
                          (h/code "(cond-> x a inc |b dec)"))
    (assert-drag-backward (h/code "(cond-> x |b dec a inc)")
                          (h/code "(cond-> x a inc b |dec)"))
    (assert-drag-backward (h/code "(-> 1"
                                  "    (cond-> |b dec a inc))")
                          (h/code "(-> 1"
                                  "    (cond-> a inc |b dec))"))
    (assert-drag-backward (h/code "(some-> 1"
                                  "        (cond-> |b dec a inc))")
                          (h/code "(some-> 1"
                                  "        (cond-> a inc |b dec))"))
    (assert-drag-backward (h/code "(cond-> 1"
                                  "  true (cond-> |b dec a inc))")
                          (h/code "(cond-> 1"
                                  "  true (cond-> a inc |b dec))"))
    (assert-drag-backward (h/code "(cond->> x |b dec a inc)")
                          (h/code "(cond->> x a inc |b dec)"))
    (assert-drag-backward (h/code "(cond->> x |b dec a inc)")
                          (h/code "(cond->> x a inc b |dec)"))
    (assert-drag-backward (h/code "(-> 1"
                                  "    (cond->> |b dec a inc))")
                          (h/code "(-> 1"
                                  "    (cond->> a inc |b dec))"))
    (assert-drag-backward (h/code "(some-> 1"
                                  "        (cond->> |b dec a inc))")
                          (h/code "(some-> 1"
                                  "        (cond->> a inc |b dec))"))
    (assert-drag-backward (h/code "(cond-> 1"
                                  "  true (cond->> |b dec a inc))")
                          (h/code "(cond-> 1"
                                  "  true (cond->> a inc |b dec))"))
    (assert-drag-backward (h/code "(case a |2 :second 1 :first)")
                          (h/code "(case a 1 :first |2 :second)"))
    (assert-drag-backward (h/code "(case a |2 :second 1 :first)")
                          (h/code "(case a 1 :first 2 |:second)"))
    (assert-drag-backward (h/code "(case a |2 :second 1 :first :else)")
                          (h/code "(case a 1 :first |2 :second :else)"))
    (assert-drag-backward (h/code "(case a |2 :second 1 :first :else)")
                          (h/code "(case a 1 :first 2 |:second :else)"))
    (assert-drag-backward (h/code "(-> 1"
                                  "    (case |2 :second 1 :first))")
                          (h/code "(-> 1"
                                  "    (case 1 :first |2 :second))"))
    (assert-drag-backward (h/code "(some-> 1"
                                  "        (case |2 :second 1 :first))")
                          (h/code "(some-> 1"
                                  "        (case 1 :first |2 :second))"))
    (assert-drag-backward (h/code "(cond-> 1"
                                  "  true (case |2 :second 1 :first))")
                          (h/code "(cond-> 1"
                                  "  true (case 1 :first |2 :second))"))
    (assert-drag-backward (h/code "(condp = x |b 2 a 1)")
                          (h/code "(condp = x a 1 |b 2)"))
    (assert-drag-backward (h/code "(condp = x |b 2 a 1)")
                          (h/code "(condp = x a 1 b |2)"))
    (assert-drag-backward (h/code "(condp = x |b 2 a 1 :else)")
                          (h/code "(condp = x a 1 |b 2 :else)"))
    (assert-drag-backward (h/code "(condp = x |b 2 a 1 :else)")
                          (h/code "(condp = x a 1 b |2 :else)"))
    (assert-drag-backward (h/code "(condp some [1 2 3 4]"
                                  "  |#{4 5 9} :>> dec"
                                  "  #{0 6 7} :>> inc)")
                          (h/code "(condp some [1 2 3 4]"
                                  "  #{0 6 7} :>> inc"
                                  "  |#{4 5 9} :>> dec)"))
    (assert-drag-backward (h/code "(condp some [1 2 3 4]"
                                  "  |#{4 5 9} :>> dec"
                                  "  #{0 6 7} :>> inc)")
                          (h/code "(condp some [1 2 3 4]"
                                  "  #{0 6 7} :>> inc"
                                  "  #{4 5 9} |:>> dec)"))
    (assert-drag-backward (h/code "(condp some [1 2 3 4]"
                                  "  |#{4 5 9} :>> dec"
                                  "  #{0 6 7} :>> inc)")
                          (h/code "(condp some [1 2 3 4]"
                                  "  #{0 6 7} :>> inc"
                                  "  #{4 5 9} :>> |dec)"))
    (assert-drag-backward (h/code "(condp some [1 2 3 4]"
                                  "  |#{4 5 9} :>> dec"
                                  "  #{0 6 7} :>> inc"
                                  "  :else)")
                          (h/code "(condp some [1 2 3 4]"
                                  "  #{0 6 7} :>> inc"
                                  "  #{4 5 9} :>> |dec"
                                  "  :else)"))
    (assert-drag-backward (h/code "(are [x] (= x 1) |2 1)")
                          (h/code "(are [x] (= x 1) 1 |2)"))
    (assert-drag-backward (h/code "(are [expected x] (= expected x) |3 4 1 2)")
                          (h/code "(are [expected x] (= expected x) 1 2 |3 4)"))
    (assert-drag-backward (h/code "(are [expected x] (= expected x) |3 4 1 2)")
                          (h/code "(are [expected x] (= expected x) 1 2 3 |4)")))
  (testing "with destructuring"
    (assert-drag-backward (h/code "(let [{:keys [a |c b d]} x])")
                          (h/code "(let [{:keys [a b |c d]} x])"))
    (assert-drag-backward (h/code "(let [[a b] [1 2]"
                                  "      |e 2"
                                  "      {:keys [c d]} {:c 1 :d 2}])")
                          (h/code "(let [[a b] [1 2]"
                                  "      {:keys [c d]} {:c 1 :d 2}"
                                  "      |e 2])"))
    (assert-drag-backward (h/code "(let [|{:keys [c d]} {:c 1 :d 2}"
                                  "      [a b] [1 2]"
                                  "      e 2])")
                          (h/code "(let [[a b] [1 2]"
                                  "      |{:keys [c d]} {:c 1 :d 2}"
                                  "      e 2])"))
    (assert-drag-backward (h/code "(for [a [1 2]"
                                  "      :let [|{:keys [c]} {:c 1}"
                                  "            {:keys [b]} {:b 1}]])")
                          (h/code "(for [a [1 2]"
                                  "      :let [{:keys [b]} {:b 1}"
                                  "            |{:keys [c]} {:c 1}]])")))
  (testing "blank lines"
    (assert-drag-backward (h/code "{|:b 2"
                                  ""
                                  " :a 1}")
                          (h/code "{:a 1"
                                  "|"
                                  " :b 2}"))
    (assert-drag-backward (h/code "{:a 1"
                                  ""
                                  " |:c 2"
                                  ""
                                  " :b 2}")
                          (h/code "{:a 1"
                                  ""
                                  " :b 2"
                                  "|"
                                  " :c 2}")))

  (testing "comments"
    (assert-drag-backward (h/code "{|:b (+ 1 1) ;; two comment"
                                  " :a 1 ;; one comment"
                                  " :c 3} ;; three comment")
                          (h/code "{:a 1 ;; one comment"
                                  " |:b (+ 1 1) ;; two comment"
                                  " :c 3} ;; three comment"))
    (assert-drag-backward (h/code ";; main comment"
                                  "{|;; b comment"
                                  " :b (+ 1 1) ;; two comment"
                                  " :a 1 ;; one comment"
                                  " ;; c comment"
                                  " :c 3} ;; three comment")
                          (h/code ";; main comment"
                                  "{:a 1 ;; one comment"
                                  " ;; b comment"
                                  " |:b (+ 1 1) ;; two comment"
                                  " ;; c comment"
                                  " :c 3} ;; three comment"))
    (assert-drag-backward (h/code "{|:b 2"
                                  " :a 1"
                                  " ;; trailing comment"
                                  " }")
                          (h/code "{:a 1"
                                  " |:b 2"
                                  " ;; trailing comment"
                                  " }"))
    (assert-drag-backward (h/code "{;; a"
                                  " a 1"
                                  ""
                                  " |;; c"
                                  " c 3"
                                  ""
                                  " ;; b"
                                  " b 2}")
                          (h/code "{;; a"
                                  " a 1"
                                  ""
                                  " ;; b"
                                  " b 2"
                                  ""
                                  " ;; c"
                                  " |c 3}"))
    (assert-drag-backward (h/code "{|;; b"
                                  " b 2"
                                  ""
                                  " ;; a"
                                  " a 1"
                                  ""
                                  " ;; c"
                                  " c 3}")
                          (h/code "{;; a"
                                  " a 1"
                                  ""
                                  " ;; b"
                                  " |b 2"
                                  ""
                                  " ;; c"
                                  " c 3}"))
    ;; avoids commenting out closing bracket
    (assert-drag-backward (h/code "[|b"
                                  " a ;; a comment"
                                  " ]")
                          (h/code "[a ;; a comment"
                                  " |b]"))
    (assert-drag-backward (h/code "[|b"
                                  "    a ;; a comment"
                                  " ]")
                          (h/code "[a ;; a comment"
                                  "    |b]"))
    (assert-drag-backward (h/code "{|:b 2"
                                  " :a 1 ;; one comment"
                                  " }")
                          (h/code "{:a 1 ;; one comment"
                                  " |:b 2}"))
    (assert-drag-backward (h/code "{|:b 2"
                                  "    :a 1 ;; one comment"
                                  " }")
                          (h/code "{:a 1 ;; one comment"
                                  "    |:b 2}"))
    ;; from leading comment
    (assert-drag-backward (h/code "{|;; b comment"
                                  " :b 2 ;; two comment"
                                  " :a 1 ;; one comment"
                                  " :c 3}")
                          (h/code "{:a 1 ;; one comment"
                                  " ;; |b comment"
                                  " :b 2 ;; two comment"
                                  " :c 3}"))
    ;; from trailing comment
    (assert-drag-backward (h/code "{|:b 2 ;; two comment"
                                  " :a 1 ;; one comment"
                                  " :c 3}")
                          (h/code "{:a 1 ;; one comment"
                                  " :b 2 ;; |two comment"
                                  " :c 3}"))
    ;; treats uneval as comment
    (assert-drag-backward (h/code "(case 1"
                                  "  |2"
                                  "  #_=> :two"
                                  "  1"
                                  "  #_=> :one)")
                          (h/code "(case 1"
                                  "  1"
                                  "  #_=> :one"
                                  "  |2"
                                  "  #_=> :two)")))
  (testing "multi-line elements"
    (assert-drag-backward (h/code "[|:c"
                                  " [:a"
                                  "  :b]]")
                          (h/code "[[:a"
                                  "  :b]"
                                  " |:c]"))
    (assert-drag-backward (h/code "[|[:a"
                                  "  :b]"
                                  " :c]")
                          (h/code "[:c"
                                  " |[:a"
                                  "  :b]]"))
    (assert-drag-backward (h/code "{|:c 3"
                                  " :a {:a/a 1"
                                  "     :a/b 2}}")
                          (h/code "{:a {:a/a 1"
                                  "     :a/b 2}"
                                  " |:c 3}"))
    (assert-drag-backward (h/code "{|:a {:a/a 1"
                                  "     :a/b 2}"
                                  " :c 3}")
                          (h/code "{:c 3"
                                  " |:a {:a/a 1"
                                  "     :a/b 2}}"))))

(deftest drag-forward
  (testing "common cases"
    (assert-drag-forward (h/code "{3 4 |1 2}")
                         (h/code "{|1 2 3 4}"))
    (assert-drag-forward (h/code "{3 4, |1 2}")
                         (h/code "{|1 2, 3 4}"))
    (assert-drag-forward (h/code "{3 4"
                                 " |1 2}")
                         (h/code "{|1 2"
                                 " 3 4}"))
    (assert-drag-forward (h/code "{3 4"
                                 " |1 2}")
                         (h/code "{1 |2"
                                 " 3 4}"))
    (assert-drag-forward (h/code "{3 4,"
                                 " |1 2}")
                         (h/code "{|1 2,"
                                 " 3 4}"))
    (assert-drag-forward (h/code "{:b (+ 1 1)"
                                 " |:a 1"
                                 " :c 3}")
                         (h/code "{|:a 1"
                                 " :b (+ 1 1)"
                                 " :c 3}"))
    (assert-drag-forward (h/code "[2 |1]")
                         (h/code "[|1 2]"))
    (assert-drag-forward (h/code "[2 |1 3]")
                         (h/code "[|1 2 3]"))
    (assert-drag-forward (h/code "[:b"
                                 " |:a]")
                         (h/code "[|:a"
                                 " :b]"))
    (assert-drag-forward (h/code "(def a 1)"
                                 "[2 |a]")
                         (h/code "(def a 1)"
                                 "[|a 2]"))
    (assert-drag-forward (h/code "(let [a [1 2]"
                                 "      c 3"
                                 "      |b 2])")
                         (h/code "(let [a [1 2]"
                                 "      |b 2"
                                 "      c 3])"))
    (assert-drag-forward (h/code "(binding [b 2"
                                 "          |a 1]"
                                 "  (+ a b))")
                         (h/code "(binding [|a 1"
                                 "          b 2]"
                                 "  (+ a b))"))
    (assert-drag-forward (h/code "(for [b [3 4]"
                                 "      |a [1 2]"
                                 "      :let [c (inc a)]]"
                                 "  (+ a b c))")
                         (h/code "(for [|a [1 2]"
                                 "      b [3 4]"
                                 "      :let [c (inc a)]]"
                                 "  (+ a b c))"))
    (assert-drag-forward (h/code "(for [a [1 2]"
                                 "      :let [c (inc b)"
                                 "            |b (inc a)]]"
                                 "  (+ a b c))")
                         (h/code "(for [a [1 2]"
                                 "      :let [|b (inc a)"
                                 "            c (inc b)]]"
                                 "  (+ a b c))"))
    (assert-drag-forward (h/code "(ns foo"
                                 "  {:clj-kondo/config '{:lint-as {foo/foo clojure.core/let}}})"
                                 "(defmacro foo [bs & form]"
                                 "  `(let [~@bs]"
                                 "     ~@form))"
                                 "(foo [b 2"
                                 "      |a 1]"
                                 "  (inc a))")
                         (h/code "(ns foo"
                                 "  {:clj-kondo/config '{:lint-as {foo/foo clojure.core/let}}})"
                                 "(defmacro foo [bs & form]"
                                 "  `(let [~@bs]"
                                 "     ~@form))"
                                 "(foo [|a 1"
                                 "      b 2]"
                                 "  (inc a))"))
    (assert-drag-forward (h/code "(def b) |(def a)")
                         (h/code "|(def a) (def b)"))
    (assert-drag-forward (h/code "(def a) (def c) |(def b)")
                         (h/code "(def a) |(def b) (def c)"))
    (is (= {:row 1 :col 1
            :end-row 2 :end-col 8}
           (as-range
             (drag-code-forward (h/code "|(def a)"
                                        "(def b)"))))))
  (testing "within special functions"
    (assert-drag-forward (h/code "(cond b 2 |a 1)")
                         (h/code "(cond |a 1 b 2)"))
    (assert-drag-forward (h/code "(cond b 2 |a 1)")
                         (h/code "(cond a |1 b 2)"))
    (assert-drag-forward (h/code "(assoc x :b 1 |:a 2)")
                         (h/code "(assoc x |:a 2 :b 1)"))
    (assert-drag-forward (h/code "(assoc x :b 1 |:a 2)")
                         (h/code "(assoc x :a |2 :b 1)"))
    (assert-drag-forward (h/code "(-> {}"
                                 "    (assoc :b 1 |:a 2))")
                         (h/code "(-> {}"
                                 "    (assoc |:a 2 :b 1))"))
    (assert-drag-forward (h/code "(some-> {}"
                                 "        (assoc :b 1 |:a 2))")
                         (h/code "(some-> {}"
                                 "        (assoc |:a 2 :b 1))"))
    (assert-drag-forward (h/code "(cond-> {}"
                                 "  true (assoc :b 1 |:a 2))")
                         (h/code "(cond-> {}"
                                 "  true (assoc |:a 2 :b 1))"))
    (assert-drag-forward (h/code "(assoc! x :b 1 |:a 2)")
                         (h/code "(assoc! x |:a 2 :b 1)"))
    (assert-drag-forward (h/code "(assoc! x :b 1 |:a 2)")
                         (h/code "(assoc! x :a |2 :b 1)"))
    (assert-drag-forward (h/code "(-> {}"
                                 "    (assoc! :b 1 |:a 2))")
                         (h/code "(-> {}"
                                 "    (assoc! |:a 2 :b 1))"))
    (assert-drag-forward (h/code "(some-> {}"
                                 "        (assoc! :b 1 |:a 2))")
                         (h/code "(some-> {}"
                                 "        (assoc! |:a 2 :b 1))"))
    (assert-drag-forward (h/code "(cond-> {}"
                                 "  true (assoc! :b 1 |:a 2))")
                         (h/code "(cond-> {}"
                                 "  true (assoc! |:a 2 :b 1))"))
    (assert-drag-forward (h/code "(cond-> x b dec |a inc)")
                         (h/code "(cond-> x |a inc b dec)"))
    (assert-drag-forward (h/code "(cond-> x b dec |a inc)")
                         (h/code "(cond-> x a |inc b dec)"))
    (assert-drag-forward (h/code "(-> 1"
                                 "    (cond-> b dec |a inc))")
                         (h/code "(-> 1"
                                 "    (cond-> |a inc b dec))"))
    (assert-drag-forward (h/code "(some-> 1"
                                 "        (cond-> b dec |a inc))")
                         (h/code "(some-> 1"
                                 "        (cond-> |a inc b dec))"))
    (assert-drag-forward (h/code "(cond-> 1"
                                 "  true (cond-> b dec |a inc))")
                         (h/code "(cond-> 1"
                                 "  true (cond-> |a inc b dec))"))
    (assert-drag-forward (h/code "(cond->> x b dec |a inc)")
                         (h/code "(cond->> x |a inc b dec)"))
    (assert-drag-forward (h/code "(cond->> x b dec |a inc)")
                         (h/code "(cond->> x a |inc b dec)"))
    (assert-drag-forward (h/code "(-> 1"
                                 "    (cond->> b dec |a inc))")
                         (h/code "(-> 1"
                                 "    (cond->> |a inc b dec))"))
    (assert-drag-forward (h/code "(some-> 1"
                                 "        (cond->> b dec |a inc))")
                         (h/code "(some-> 1"
                                 "        (cond->> |a inc b dec))"))
    (assert-drag-forward (h/code "(cond-> 1"
                                 "  true (cond->> b dec |a inc))")
                         (h/code "(cond-> 1"
                                 "  true (cond->> |a inc b dec))"))
    (assert-drag-forward (h/code "(case a 2 :second |1 :first)")
                         (h/code "(case a |1 :first 2 :second)"))
    (assert-drag-forward (h/code "(case a 2 :second |1 :first)")
                         (h/code "(case a 1 |:first 2 :second)"))
    (assert-drag-forward (h/code "(case a 2 :second |1 :first :else)")
                         (h/code "(case a |1 :first 2 :second :else)"))
    (assert-drag-forward (h/code "(case a 2 :second |1 :first :else)")
                         (h/code "(case a 1 |:first 2 :second :else)"))
    (assert-drag-forward (h/code "(-> 1"
                                 "    (case 2 :second |1 :first))")
                         (h/code "(-> 1"
                                 "    (case |1 :first 2 :second))"))
    (assert-drag-forward (h/code "(some-> 1"
                                 "        (case 2 :second |1 :first))")
                         (h/code "(some-> 1"
                                 "        (case |1 :first 2 :second))"))
    (assert-drag-forward (h/code "(cond-> 1"
                                 "  true (case 2 :second |1 :first))")
                         (h/code "(cond-> 1"
                                 "  true (case |1 :first 2 :second))"))
    (assert-drag-forward (h/code "(condp = x b 2 |a 1)")
                         (h/code "(condp = x |a 1 b 2)"))
    (assert-drag-forward (h/code "(condp = x b 2 |a 1)")
                         (h/code "(condp = x a |1 b 2)"))
    (assert-drag-forward (h/code "(condp = x b 2 |a 1 :else)")
                         (h/code "(condp = x |a 1 b 2 :else)"))
    (assert-drag-forward (h/code "(condp = x b 2 |a 1 :else)")
                         (h/code "(condp = x a |1 b 2 :else)"))
    (assert-drag-forward (h/code "(condp some [1 2 3 4]"
                                 "  #{4 5 9} :>> dec"
                                 "  |#{0 6 7} :>> inc)")
                         (h/code "(condp some [1 2 3 4]"
                                 "  |#{0 6 7} :>> inc"
                                 "  #{4 5 9} :>> dec)"))
    (assert-drag-forward (h/code "(condp some [1 2 3 4]"
                                 "  #{4 5 9} :>> dec"
                                 "  |#{0 6 7} :>> inc)")
                         (h/code "(condp some [1 2 3 4]"
                                 "  #{0 6 7} |:>> inc"
                                 "  #{4 5 9} :>> dec)"))
    (assert-drag-forward (h/code "(condp some [1 2 3 4]"
                                 "  #{4 5 9} :>> dec"
                                 "  |#{0 6 7} :>> inc)")
                         (h/code "(condp some [1 2 3 4]"
                                 "  #{0 6 7} :>> |inc"
                                 "  #{4 5 9} :>> dec)"))
    (assert-drag-forward (h/code "(condp some [1 2 3 4]"
                                 "  #{4 5 9} :>> dec"
                                 "  |#{0 6 7} :>> inc"
                                 "  :else)")
                         (h/code "(condp some [1 2 3 4]"
                                 "  #{0 6 7} :>> |inc"
                                 "  #{4 5 9} :>> dec"
                                 "  :else)"))
    (assert-drag-forward (h/code "(are [x] (= x 1) 2 |1)")
                         (h/code "(are [x] (= x 1) |1 2)"))
    (assert-drag-forward (h/code "(are [expected x] (= expected x) 3 4 |1 2)")
                         (h/code "(are [expected x] (= expected x) |1 2 3 4)"))
    (assert-drag-forward (h/code "(are [expected x] (= expected x) 3 4 |1 2)")
                         (h/code "(are [expected x] (= expected x) 1 |2 3 4)")))
  (testing "with destructuring"
    (assert-drag-forward (h/code "(let [{:keys [a c |b d]} x])")
                         (h/code "(let [{:keys [a |b c d]} x])"))
    (assert-drag-forward (h/code "(let [[a b] [1 2]"
                                 "      {:keys [c d]} {:c 1 :d 2}"
                                 "      |e 2])")
                         (h/code "(let [[a b] [1 2]"
                                 "      |e 2"
                                 "      {:keys [c d]} {:c 1 :d 2}])"))
    (assert-drag-forward (h/code "(let [[a b] [1 2]"
                                 "      e 2"
                                 "      |{:keys [c d]} {:c 1 :d 2}])")
                         (h/code "(let [[a b] [1 2]"
                                 "      |{:keys [c d]} {:c 1 :d 2}"
                                 "      e 2])"))
    (assert-drag-forward (h/code "(for [a [1 2]"
                                 "      :let [{:keys [c]} {:c 1}"
                                 "            |{:keys [b]} {:b 1}]])")
                         (h/code "(for [a [1 2]"
                                 "      :let [|{:keys [b]} {:b 1}"
                                 "            {:keys [c]} {:c 1}]])")))
  (testing "blank lines"
    (assert-drag-forward (h/code "{"
                                 " :b 2"
                                 ""
                                 " |:a 1}")
                         (h/code "{|"
                                 " :a 1"
                                 ""
                                 " :b 2}"))
    (assert-drag-forward (h/code "{:a 1"
                                 ""
                                 " :c 2"
                                 ""
                                 " |:b 2}")
                         (h/code "{:a 1"
                                 "|"
                                 " :b 2"
                                 ""
                                 " :c 2}")))
  (testing "comments"
    (assert-drag-forward (h/code "{:b (+ 1 1) ;; two comment"
                                 " |:a 1 ;; one comment"
                                 " :c 3} ;; three comment")
                         (h/code "{|:a 1 ;; one comment"
                                 " :b (+ 1 1) ;; two comment"
                                 " :c 3} ;; three comment"))
    (assert-drag-forward (h/code ";; main comment"
                                 "{;; b comment"
                                 " :b (+ 1 1) ;; two comment"
                                 " |:a 1 ;; one comment"
                                 " ;; c comment"
                                 " :c 3} ;; three comment")
                         (h/code ";; main comment"
                                 "{|:a 1 ;; one comment"
                                 " ;; b comment"
                                 " :b (+ 1 1) ;; two comment"
                                 " ;; c comment"
                                 " :c 3} ;; three comment"))
    (assert-drag-forward (h/code "{:b 2"
                                 " |:a 1"
                                 " ;; trailing comment"
                                 " }")
                         (h/code "{|:a 1"
                                 " :b 2"
                                 " ;; trailing comment"
                                 " }"))
    (assert-drag-forward (h/code "{;; a"
                                 " a 1"
                                 ""
                                 " ;; c"
                                 " c 3"
                                 ""
                                 " |;; b"
                                 " b 2}")
                         (h/code "{;; a"
                                 " a 1"
                                 ""
                                 " ;; b"
                                 " |b 2"
                                 ""
                                 " ;; c"
                                 " c 3}"))
    (assert-drag-forward (h/code "{;; b"
                                 " b 2"
                                 ""
                                 " |;; a"
                                 " a 1"
                                 ""
                                 " ;; c"
                                 " c 3}")
                         (h/code "{;; a"
                                 " |a 1"
                                 ""
                                 " ;; b"
                                 " b 2"
                                 ""
                                 " ;; c"
                                 " c 3}"))
    ;; avoids commenting out closing bracket
    (assert-drag-forward (h/code "[b"
                                 " |a ;; a comment"
                                 " ]")
                         (h/code "[|a ;; a comment"
                                 " b]"))
    (assert-drag-forward (h/code "{:b 2"
                                 " |:a 1 ;; one comment"
                                 " }")
                         (h/code "{|:a 1 ;; one comment"
                                 " :b 2}"))
    ;; from leading comment
    (assert-drag-forward (h/code "{;; b comment"
                                 " :b 2 ;; two comment"
                                 " |;; a comment"
                                 " :a 1 ;; one comment"
                                 " :c 3}")
                         (h/code "{;; |a comment"
                                 " :a 1 ;; one comment"
                                 " ;; b comment"
                                 " :b 2 ;; two comment"
                                 " :c 3}"))
    ;; from trailing comment
    (assert-drag-forward (h/code "{:b 2 ;; two comment"
                                 " |:a 1 ;; one comment"
                                 " :c 3}")
                         (h/code "{:a 1 ;; |one comment"
                                 " :b 2 ;; two comment"
                                 " :c 3}")))
  (testing "multi-line elements"
    (assert-drag-forward (h/code "[:c"
                                 " |[:a"
                                 "  :b]]")
                         (h/code "[|[:a"
                                 "  :b]"
                                 " :c]"))
    (assert-drag-forward (h/code "[[:a"
                                 "  :b]"
                                 " |:c]")
                         (h/code "[|:c"
                                 " [:a"
                                 "  :b]]"))
    (assert-drag-forward (h/code "{:c 3"
                                 " |:a {:a/a 1"
                                 "     :a/b 2}}")
                         (h/code "{|:a {:a/a 1"
                                 "     :a/b 2}"
                                 " :c 3}"))
    (assert-drag-forward (h/code "{:a {:a/a 1"
                                 "     :a/b 2}"
                                 " |:c 3}")
                         (h/code "{|:c 3"
                                 " :a {:a/a 1"
                                 "     :a/b 2}}"))))

;; These are macros so test failures have the right line numbers
(defmacro assert-no-erroneous-backwards [code]
  `(let [ws-zloc# (h/load-code-and-zloc ~code)]
     (is (can-drag-zloc-backward? ws-zloc#))
     (is (nil? (as-string (drag-zloc-backward ws-zloc#))))))
(defmacro assert-no-erroneous-forwards [code]
  `(let [ws-zloc# (h/load-code-and-zloc ~code)]
     (is (can-drag-zloc-forward? ws-zloc#))
     (is (nil? (as-string (drag-zloc-forward ws-zloc#))))))

(deftest erroneous-swaps
  ;; NOTE: ideally can-drag-*? and drag-* would always agree, but when they
  ;; don't at least no erroneous swaps happen
  (testing "from blank line after first element"
    (assert-no-erroneous-backwards (h/code "[:a"
                                           "|"
                                           "]"))
    (assert-no-erroneous-backwards (h/code "{:a 1"
                                           "|"
                                           "}"))
    (assert-no-erroneous-backwards (h/code "(case a"
                                           "   1 :first"
                                           "|"
                                           ")")))

  (testing "from comment after first element"
    (assert-no-erroneous-backwards (h/code "[:a |;; a comment"
                                           "]"))
    (assert-no-erroneous-backwards (h/code "{:a 1 |;; one comment"
                                           "}"))
    (assert-no-erroneous-backwards (h/code "(case a"
                                           "   1 :first |;; one comment"
                                           ")")))

  (testing "from blank line before last element"
    (assert-no-erroneous-forwards (h/code "[|"
                                          " :a]"))
    (assert-no-erroneous-forwards (h/code "{|"
                                          " :a 1}"))
    (assert-no-erroneous-forwards (h/code "(case a"
                                          "|"
                                          "   1 :first :else)")))

  (testing "from comment before last element"
    (assert-no-erroneous-forwards (h/code "[|;; a comment"
                                          " :a]"))
    (assert-no-erroneous-forwards (h/code "{|;; a comment"
                                          " :a 1}"))
    (assert-no-erroneous-forwards (h/code "(case a"
                                          "   |;; one comment"
                                          "   1 :first :else)"))))
