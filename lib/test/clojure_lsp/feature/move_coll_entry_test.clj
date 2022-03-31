(ns clojure-lsp.feature.move-coll-entry-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.move-coll-entry :as f.move-coll-entry]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]
   [clojure-lsp.shared :as shared]))

(h/reset-db-after-test)

(defn can-move-zloc-up? [zloc]
  (f.move-coll-entry/can-move-entry-up? zloc h/default-uri @db/db))

(defn can-move-zloc-down? [zloc]
  (f.move-coll-entry/can-move-entry-down? zloc h/default-uri @db/db))

(defn can-move-code-up? [code]
  (can-move-zloc-up? (h/load-code-and-zloc code)))

(defn can-move-code-down? [code]
  (can-move-zloc-down? (h/load-code-and-zloc code)))

;; These are only the negative cases, proving when move-up is NOT offered in the
;; actions menu. The positive cases are all tested indirectly via
;; assert-move-up, since if a movement happens, it implicitly passed
;; can-move-entry-up?
(deftest can-move-entry-up?
  (testing "unbalanced pairs"
    (is (not (can-move-code-up? "{1 2 |3}")))
    (is (not (can-move-code-up? "(let [1 2 |3])"))))
  (testing "outside collection"
    (is (not (can-move-code-up? "|")))
    (is (not (can-move-code-up? "|[]")))
    (is (not (can-move-code-up? "|{:a :b :c :d}")))
    (is (not (can-move-code-up? "|#{:a :b :c :d}")))
    (is (not (can-move-code-up? "|[:a :b :c :d]")))
    (is (not (can-move-code-up? "|'(:a :b :c :d)"))))
  (testing "on first entry"
    (is (not (can-move-code-up? "[|1]")))
    (is (not (can-move-code-up? "#{|1 2 3}")))
    (is (not (can-move-code-up? "[|1 2 3]")))
    (is (not (can-move-code-up? "'(|1 2 3)")))
    (is (not (can-move-code-up? "{|1 2}")))
    (is (not (can-move-code-up? "{1 |2}")))
    (is (not (can-move-code-up? "{|:a :b :c :d}")))
    (is (not (can-move-code-up? "{:a |:b :c :d}")))
    (is (not (can-move-code-up? "(let [|a 1 c 2])")))
    (is (not (can-move-code-up? "(let [a |1 c 2])")))
    (is (not (can-move-code-up? "|(def a) (def b)")))
    (testing "of special functions"
      ;; from rind of cond
      (is (not (can-move-code-up? "(|cond a 1 b 2)")))
      ;; from first pair of cond
      (is (not (can-move-code-up? "(cond |a 1 b 2)")))
      (is (not (can-move-code-up? "(cond a |1 b 2)")))
      ;; from rind of assoc
      (is (not (can-move-code-up? "(|assoc x :a 1 :b 2)")))
      (is (not (can-move-code-up? "(assoc |x :a 1 :b 2)")))
      ;; from first pair of assoc
      (is (not (can-move-code-up? "(assoc x |:a 1 :b 2)")))
      (is (not (can-move-code-up? "(assoc x :a |1 :b 2)")))
      ;; from rind of assoc!
      (is (not (can-move-code-up? "(|assoc! x :a 1 :b 2)")))
      (is (not (can-move-code-up? "(assoc! |x :a 1 :b 2)")))
      ;; from first pair of assoc!
      (is (not (can-move-code-up? "(assoc! x |:a 1 :b 2)")))
      (is (not (can-move-code-up? "(assoc! x :a |1 :b 2)")))
      ;; from rind of cond->
      (is (not (can-move-code-up? "(|cond-> x a inc b dec)")))
      (is (not (can-move-code-up? "(cond-> |x a inc b dec)")))
      ;; from first pair of cond->
      (is (not (can-move-code-up? "(cond-> x |a inc b dec)")))
      (is (not (can-move-code-up? "(cond-> x a |inc b dec)")))
      ;; from rind of cond->>
      (is (not (can-move-code-up? "(|cond->> x a inc b dec)")))
      (is (not (can-move-code-up? "(cond->> |x a inc b dec)")))
      ;; from first pair of cond->>
      (is (not (can-move-code-up? "(cond->> x |a inc b dec)")))
      (is (not (can-move-code-up? "(cond->> x a |inc b dec)")))
      ;; from rind of case
      (is (not (can-move-code-up? "(|case a 1 :first 2 :second)")))
      (is (not (can-move-code-up? "(case |a 1 :first 2 :second)")))
      (is (not (can-move-code-up? "(case a 1 :first |:else)")))
      (is (not (can-move-code-up? "(case a |:else)")))
      ;; from first pair of case
      (is (not (can-move-code-up? "(case a |1 :first)")))
      (is (not (can-move-code-up? "(case a 1 |:first)")))
      (is (not (can-move-code-up? "(case a |1 :first 2 :second)")))
      (is (not (can-move-code-up? "(case a 1 |:first 2 :second)")))
      (is (not (can-move-code-up? "(case a |1 :first :else)")))
      (is (not (can-move-code-up? "(case a 1 |:first :else)")))
      ;; from rind of condp
      (is (not (can-move-code-up? "(|condp = x a 1 b 2)")))
      (is (not (can-move-code-up? "(condp |= x a 1 b 2)")))
      (is (not (can-move-code-up? "(condp = |x a 1 b 2)")))
      (is (not (can-move-code-up? "(condp = x a 1 |:else)")))
      (is (not (can-move-code-up? (h/code "(condp some [1 2 3 4]"
                                          "  #{0 6 7} :>> inc"
                                          "  #{4 5 9} :>> dec"
                                          "  |:else)"))))
      ;; from first pair of condp
      (is (not (can-move-code-up? "(condp = |x a 1 b 2)")))
      (is (not (can-move-code-up? "(condp = x |a 1 b 2)")))
      (is (not (can-move-code-up? (h/code "(condp some [1 2 3 4]"
                                          "  #{0 6 7} :>> |inc)"))))
      ;; in invalid ternary form of condp
      (is (not (can-move-code-up? (h/code "(condp some [1 2 3 4]"
                                          "  #{0 6 7} :>> inc"
                                          "  |#{4 5 9} :>> dec"
                                          "  :else :invalid)"))))
      ;; from rind of are
      (is (not (can-move-code-up? (h/code "(|are [x] (= x 1) 1)"))))
      (is (not (can-move-code-up? (h/code "(are |[x] (= x 1) 1)"))))
      (is (not (can-move-code-up? (h/code "(are [x] |(= x 1) 1)"))))
      ;; from first group of are
      (is (not (can-move-code-up? (h/code "(are [expected x] (= expected x) |1 2 3 4)"))))
      (is (not (can-move-code-up? (h/code "(are [expected x] (= expected x) 1 |2 3 4)"))))
      ;; from unbalanced are
      (is (not (can-move-code-up? (h/code "(are [expected x] (= expected x) 1 2 |3 4 5)"))))
      ;; from invalid are
      (is (not (can-move-code-up? (h/code "(are [] (= 1 1) 1 |2)")))))))

;; These are only the negative cases, proving when move-down is NOT offered in
;; the actions menu. The positive cases are all tested indirectly via
;; assert-move-down, since if a movement happens, it implicitly passed
;; can-move-entry-down?
(deftest can-move-entry-down?
  (testing "unbalanced pairs"
    (is (not (can-move-code-down? "{|1 2 3}")))
    (is (not (can-move-code-down? "(let [|1 2 3])"))))
  (testing "outside collection"
    (is (not (can-move-code-down? "|[]")))
    (is (not (can-move-code-down? "|{:a :b :c :d}")))
    (is (not (can-move-code-down? "|#{:a :b :c :d}")))
    (is (not (can-move-code-down? "|[:a :b :c :d]")))
    (is (not (can-move-code-down? "|'(:a :b :c :d)"))))
  (testing "on last entry"
    (is (not (can-move-code-down? "[|1]")))
    (is (not (can-move-code-down? "#{1 2 |3}")))
    (is (not (can-move-code-down? "[1 2 |3]")))
    (is (not (can-move-code-down? "'(1 2 |3)")))
    (is (not (can-move-code-down? "{|1 2}")))
    (is (not (can-move-code-down? "{1 |2}")))
    (is (not (can-move-code-down? "{:a :b |:c :d}")))
    (is (not (can-move-code-down? "{:a :b :c |:d}")))
    (is (not (can-move-code-down? "(let [a 1 |c 2])")))
    (is (not (can-move-code-down? "(let [a 1 c |2])")))
    (is (not (can-move-code-down? "(def a) |(def b)")))
    (testing "of special functions"
      ;; from rind of cond
      (is (not (can-move-code-down? "(|cond a 1 b 2)")))
      ;; from last pair of cond
      (is (not (can-move-code-down? "(cond a 1 |b 2)")))
      (is (not (can-move-code-down? "(cond a 1 b |2)")))
      ;; from rind of cond->
      (is (not (can-move-code-down? "(|cond-> x a inc b dec)")))
      (is (not (can-move-code-down? "(cond-> |x a inc b dec)")))
      ;; from last pair of cond->
      (is (not (can-move-code-down? "(cond-> x a inc |b dec)")))
      (is (not (can-move-code-down? "(cond-> x a inc b |dec)")))
      ;; from rind of assoc
      (is (not (can-move-code-down? "(|assoc x :a 1 :b 2)")))
      (is (not (can-move-code-down? "(assoc |x :a 1 :b 2)")))
      ;; from last pair of assoc
      (is (not (can-move-code-down? "(assoc x :a 1 |:b 2)")))
      (is (not (can-move-code-down? "(assoc x :a 1 :b |2)")))
      ;; from rind of assoc!
      (is (not (can-move-code-down? "(|assoc! x :a 1 :b 2)")))
      (is (not (can-move-code-down? "(assoc! |x :a 1 :b 2)")))
      ;; from last pair of assoc!
      (is (not (can-move-code-down? "(assoc! x :a 1 |:b 2)")))
      (is (not (can-move-code-down? "(assoc! x :a 1 :b |2)")))
      ;; from rind of cond->>
      (is (not (can-move-code-down? "(|cond->> x a inc b dec)")))
      (is (not (can-move-code-down? "(cond->> |x a inc b dec)")))
      ;; from last pair of cond->>
      (is (not (can-move-code-down? "(cond->> x a inc |b dec)")))
      (is (not (can-move-code-down? "(cond->> x a inc b |dec)")))
      ;; from rind of case
      (is (not (can-move-code-down? "(|case a 1 :first 2 :second)")))
      (is (not (can-move-code-down? "(case |a 1 :first 2 :second)")))
      (is (not (can-move-code-down? "(case a 1 :first |:else)")))
      (is (not (can-move-code-down? "(case a |:else)")))
      ;; from last pair of case
      (is (not (can-move-code-down? "(case a 1 :first |2 :second)")))
      (is (not (can-move-code-down? "(case a 1 :first 2 |:second)")))
      (is (not (can-move-code-down? "(case a 1 :first |2 :second :else)")))
      (is (not (can-move-code-down? "(case a 1 :first 2 |:second :else)")))
      ;; from rind of condp
      (is (not (can-move-code-down? "(|condp = x a 1 b 2)")))
      (is (not (can-move-code-down? "(condp |= x a 1 b 2)")))
      (is (not (can-move-code-down? "(condp = |x a 1 b 2)")))
      (is (not (can-move-code-down? "(condp = x a 1 |:else)")))
      ;; from last pair of condp
      (is (not (can-move-code-down? "(condp = x a 1 |b 2)")))
      (is (not (can-move-code-down? "(condp = x a 1 b |2)")))
      (is (not (can-move-code-down? (h/code "(condp some [1 2 3 4]"
                                            "  |#{4 5 9} :>> dec"
                                            "  :else)"))))
      ;; from rind of are
      (is (not (can-move-code-down? (h/code "(|are [x] (= x 1) 1)"))))
      (is (not (can-move-code-down? (h/code "(are |[x] (= x 1) 1)"))))
      (is (not (can-move-code-down? (h/code "(are [x] |(= x 1) 1)"))))
      ;; from last group of are
      (is (not (can-move-code-down? (h/code "(are [expected x] (= expected x) 1 2 |3 4)"))))
      (is (not (can-move-code-down? (h/code "(are [expected x] (= expected x) 1 2 3 |4)"))))
      ;; from unbalanced are
      (is (not (can-move-code-down? (h/code "(are [expected x] (= expected x) |1 2 3 4 5)"))))
      ;; from invalid are
      (is (not (can-move-code-down? (h/code "(are [] (= 1 1) |1 2)")))))))

(defn move-zloc-up [zloc]
  (f.move-coll-entry/move-up zloc h/default-uri @db/db))

(defn move-zloc-down [zloc]
  (f.move-coll-entry/move-down zloc h/default-uri @db/db))

(defn move-code-up [code]
  (move-zloc-up (h/load-code-and-zloc code)))

(defn move-code-down [code]
  (move-zloc-down (h/load-code-and-zloc code)))

(defn- as-string [change]
  (some-> change
          :changes-by-uri
          (get h/default-uri)
          first
          :loc
          z/root-string))

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
(defmacro assert-move-up [expected code]
  `(let [moved#         (move-code-up ~code)
         [text# [pos#]] (h/positions-from-text ~expected)]
     (is (= text# (as-string moved#)))
     ;; Range should only be single char
     (is (= [pos# pos#] (as-position moved#)))))
(defmacro assert-move-down [expected code]
  `(let [moved#         (move-code-down ~code)
         [text# [pos#]] (h/positions-from-text ~expected)]
     (is (= text# (as-string moved#)))
     (is (= [pos# pos#] (as-position moved#)))))

(deftest move-up
  (testing "common cases"
    (assert-move-up (h/code "{|3 4 1 2}")
                    (h/code "{1 2 |3 4}"))
    (assert-move-up (h/code "{|3 4, 1 2}")
                    (h/code "{1 2, |3 4}"))
    (assert-move-up (h/code "{|3 4"
                            " 1 2}")
                    (h/code "{1 2"
                            " |3 4}"))
    (assert-move-up (h/code "{|3 4"
                            " 1 2}")
                    (h/code "{1 2"
                            " 3 |4}"))
    (assert-move-up (h/code "{|3 4,"
                            " 1 2}")
                    (h/code "{1 2,"
                            " |3 4}"))
    (assert-move-up (h/code "{|:b (+ 1 1)"
                            " :a 1"
                            " :c 3}")
                    (h/code "{:a 1"
                            " |:b (+ 1 1)"
                            " :c 3}"))
    (assert-move-up (h/code "[|2 1]")
                    (h/code "[1 |2]"))
    (assert-move-up (h/code "[|:b"
                            " :a]")
                    (h/code "[:a"
                            " |:b]"))
    (assert-move-up (h/code "[1 |3 2]")
                    (h/code "[1 2 |3]"))
    (assert-move-up (h/code "(def a 1)"
                            "[|2 a]")
                    (h/code "(def a 1)"
                            "[a |2]"))
    (assert-move-up (h/code "(let [a [1 2]"
                            "      |c 3"
                            "      b 2])")
                    (h/code "(let [a [1 2]"
                            "      b 2"
                            "      |c 3])"))
    (assert-move-up (h/code "(binding [|b 2"
                            "          a 1]"
                            "  (+ a b))")
                    (h/code "(binding [a 1"
                            "          |b 2]"
                            "  (+ a b))"))
    (assert-move-up (h/code "(for [|b [3 4]"
                            "      a [1 2]"
                            "      :let [c (inc a)]]"
                            "  (+ a b c))")
                    (h/code "(for [a [1 2]"
                            "      |b [3 4]"
                            "      :let [c (inc a)]]"
                            "  (+ a b c))"))
    (assert-move-up (h/code "(for [a [1 2]"
                            "      :let [|c (inc b)"
                            "            b (inc a)]]"
                            "  (+ a b c))")
                    (h/code "(for [a [1 2]"
                            "      :let [b (inc a)"
                            "            |c (inc b)]]"
                            "  (+ a b c))"))
    (assert-move-up (h/code "(ns foo"
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
    (assert-move-up (h/code "|(def b) (def a)")
                    (h/code "(def a) |(def b)"))
    (assert-move-up (h/code "|(def b) (def a) (def c)")
                    (h/code "(def a) |(def b) (def c)"))
    (is (= ;; preferrably would be:
          #_{:row 1 :col 1
             :end-row 2 :end-col 8}
          shared/full-file-position
          (as-range
            (move-code-up (h/code "(def a)"
                                  "|(def b)"))))))
  (testing "within special functions"
    (assert-move-up (h/code "(cond |b 2 a 1)")
                    (h/code "(cond a 1 |b 2)"))
    (assert-move-up (h/code "(cond |b 2 a 1)")
                    (h/code "(cond a 1 b |2)"))
    (assert-move-up (h/code "(assoc x |:b 1 :a 2)")
                    (h/code "(assoc x :a 2 |:b 1)"))
    (assert-move-up (h/code "(assoc x |:b 1 :a 2)")
                    (h/code "(assoc x :a 2 :b |1)"))
    (assert-move-up (h/code "#(assoc % |:b 2 :a 1)")
                    (h/code "#(assoc % :a 1 |:b 2)"))
    (assert-move-up (h/code "(-> {}"
                            "    (assoc |:b 1 :a 2))")
                    (h/code "(-> {}"
                            "    (assoc :a 2 |:b 1))"))
    (assert-move-up (h/code "(some-> {}"
                            "        (assoc |:b 1 :a 2))")
                    (h/code "(some-> {}"
                            "        (assoc :a 2 |:b 1))"))
    (assert-move-up (h/code "(cond-> {}"
                            "  true (assoc |:b 1 :a 2))")
                    (h/code "(cond-> {}"
                            "  true (assoc :a 2 |:b 1))"))
    (assert-move-up (h/code "(assoc! x |:b 1 :a 2)")
                    (h/code "(assoc! x :a 2 |:b 1)"))
    (assert-move-up (h/code "(assoc! x |:b 1 :a 2)")
                    (h/code "(assoc! x :a 2 :b |1)"))
    (assert-move-up (h/code "(-> {}"
                            "    (assoc! |:b 1 :a 2))")
                    (h/code "(-> {}"
                            "    (assoc! :a 2 |:b 1))"))
    (assert-move-up (h/code "(some-> {}"
                            "        (assoc! |:b 1 :a 2))")
                    (h/code "(some-> {}"
                            "        (assoc! :a 2 |:b 1))"))
    (assert-move-up (h/code "(cond-> {}"
                            "  true (assoc! |:b 1 :a 2))")
                    (h/code "(cond-> {}"
                            "  true (assoc! :a 2 |:b 1))"))
    (assert-move-up (h/code "(cond-> x |b dec a inc)")
                    (h/code "(cond-> x a inc |b dec)"))
    (assert-move-up (h/code "(cond-> x |b dec a inc)")
                    (h/code "(cond-> x a inc b |dec)"))
    (assert-move-up (h/code "(-> 1"
                            "    (cond-> |b dec a inc))")
                    (h/code "(-> 1"
                            "    (cond-> a inc |b dec))"))
    (assert-move-up (h/code "(some-> 1"
                            "        (cond-> |b dec a inc))")
                    (h/code "(some-> 1"
                            "        (cond-> a inc |b dec))"))
    (assert-move-up (h/code "(cond-> 1"
                            "  true (cond-> |b dec a inc))")
                    (h/code "(cond-> 1"
                            "  true (cond-> a inc |b dec))"))
    (assert-move-up (h/code "(cond->> x |b dec a inc)")
                    (h/code "(cond->> x a inc |b dec)"))
    (assert-move-up (h/code "(cond->> x |b dec a inc)")
                    (h/code "(cond->> x a inc b |dec)"))
    (assert-move-up (h/code "(-> 1"
                            "    (cond->> |b dec a inc))")
                    (h/code "(-> 1"
                            "    (cond->> a inc |b dec))"))
    (assert-move-up (h/code "(some-> 1"
                            "        (cond->> |b dec a inc))")
                    (h/code "(some-> 1"
                            "        (cond->> a inc |b dec))"))
    (assert-move-up (h/code "(cond-> 1"
                            "  true (cond->> |b dec a inc))")
                    (h/code "(cond-> 1"
                            "  true (cond->> a inc |b dec))"))
    (assert-move-up (h/code "(case a |2 :second 1 :first)")
                    (h/code "(case a 1 :first |2 :second)"))
    (assert-move-up (h/code "(case a |2 :second 1 :first)")
                    (h/code "(case a 1 :first 2 |:second)"))
    (assert-move-up (h/code "(case a |2 :second 1 :first :else)")
                    (h/code "(case a 1 :first |2 :second :else)"))
    (assert-move-up (h/code "(case a |2 :second 1 :first :else)")
                    (h/code "(case a 1 :first 2 |:second :else)"))
    (assert-move-up (h/code "(-> 1"
                            "    (case |2 :second 1 :first))")
                    (h/code "(-> 1"
                            "    (case 1 :first |2 :second))"))
    (assert-move-up (h/code "(some-> 1"
                            "        (case |2 :second 1 :first))")
                    (h/code "(some-> 1"
                            "        (case 1 :first |2 :second))"))
    (assert-move-up (h/code "(cond-> 1"
                            "  true (case |2 :second 1 :first))")
                    (h/code "(cond-> 1"
                            "  true (case 1 :first |2 :second))"))
    (assert-move-up (h/code "(condp = x |b 2 a 1)")
                    (h/code "(condp = x a 1 |b 2)"))
    (assert-move-up (h/code "(condp = x |b 2 a 1)")
                    (h/code "(condp = x a 1 b |2)"))
    (assert-move-up (h/code "(condp = x |b 2 a 1 :else)")
                    (h/code "(condp = x a 1 |b 2 :else)"))
    (assert-move-up (h/code "(condp = x |b 2 a 1 :else)")
                    (h/code "(condp = x a 1 b |2 :else)"))
    (assert-move-up (h/code "(condp some [1 2 3 4]"
                            "  |#{4 5 9} :>> dec"
                            "  #{0 6 7} :>> inc)")
                    (h/code "(condp some [1 2 3 4]"
                            "  #{0 6 7} :>> inc"
                            "  |#{4 5 9} :>> dec)"))
    (assert-move-up (h/code "(condp some [1 2 3 4]"
                            "  |#{4 5 9} :>> dec"
                            "  #{0 6 7} :>> inc)")
                    (h/code "(condp some [1 2 3 4]"
                            "  #{0 6 7} :>> inc"
                            "  #{4 5 9} |:>> dec)"))
    (assert-move-up (h/code "(condp some [1 2 3 4]"
                            "  |#{4 5 9} :>> dec"
                            "  #{0 6 7} :>> inc)")
                    (h/code "(condp some [1 2 3 4]"
                            "  #{0 6 7} :>> inc"
                            "  #{4 5 9} :>> |dec)"))
    (assert-move-up (h/code "(condp some [1 2 3 4]"
                            "  |#{4 5 9} :>> dec"
                            "  #{0 6 7} :>> inc"
                            "  :else)")
                    (h/code "(condp some [1 2 3 4]"
                            "  #{0 6 7} :>> inc"
                            "  #{4 5 9} :>> |dec"
                            "  :else)"))
    (assert-move-up (h/code "(are [x] (= x 1) |2 1)")
                    (h/code "(are [x] (= x 1) 1 |2)"))
    (assert-move-up (h/code "(are [expected x] (= expected x) |3 4 1 2)")
                    (h/code "(are [expected x] (= expected x) 1 2 |3 4)"))
    (assert-move-up (h/code "(are [expected x] (= expected x) |3 4 1 2)")
                    (h/code "(are [expected x] (= expected x) 1 2 3 |4)")))
  (testing "with destructuring"
    (assert-move-up (h/code "(let [[a b] [1 2]"
                            "      |e 2"
                            "      {:keys [c d]} {:c 1 :d 2}])")
                    (h/code "(let [[a b] [1 2]"
                            "      {:keys [c d]} {:c 1 :d 2}"
                            "      |e 2])"))
    (assert-move-up (h/code "(let [|{:keys [c d]} {:c 1 :d 2}"
                            "      [a b] [1 2]"
                            "      e 2])")
                    (h/code "(let [[a b] [1 2]"
                            "      |{:keys [c d]} {:c 1 :d 2}"
                            "      e 2])"))
    (assert-move-up (h/code "(for [a [1 2]"
                            "      :let [|{:keys [c]} {:c 1}"
                            "            {:keys [b]} {:b 1}]])")
                    (h/code "(for [a [1 2]"
                            "      :let [{:keys [b]} {:b 1}"
                            "            |{:keys [c]} {:c 1}]])")))
  (testing "blank lines"
    (assert-move-up (h/code "{|:b 2"
                            ""
                            " :a 1}")
                    (h/code "{:a 1"
                            "|"
                            " :b 2}"))
    (assert-move-up (h/code "{:a 1"
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
    (assert-move-up (h/code "{|:b (+ 1 1) ;; two comment"
                            " :a 1 ;; one comment"
                            " :c 3} ;; three comment")
                    (h/code "{:a 1 ;; one comment"
                            " |:b (+ 1 1) ;; two comment"
                            " :c 3} ;; three comment"))
    (assert-move-up (h/code ";; main comment"
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
    (assert-move-up (h/code "{|:b 2"
                            " :a 1"
                            " ;; trailing comment"
                            " }")
                    (h/code "{:a 1"
                            " |:b 2"
                            " ;; trailing comment"
                            " }"))
    (assert-move-up (h/code "{;; a"
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
    (assert-move-up (h/code "{|;; b"
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
    (assert-move-up (h/code "[|b"
                            " a ;; a comment"
                            " ]")
                    (h/code "[a ;; a comment"
                            " |b]"))
    (assert-move-up (h/code "[|b"
                            "    a ;; a comment"
                            " ]")
                    (h/code "[a ;; a comment"
                            "    |b]"))
    (assert-move-up (h/code "{|:b 2"
                            " :a 1 ;; one comment"
                            " }")
                    (h/code "{:a 1 ;; one comment"
                            " |:b 2}"))
    (assert-move-up (h/code "{|:b 2"
                            "    :a 1 ;; one comment"
                            " }")
                    (h/code "{:a 1 ;; one comment"
                            "    |:b 2}"))
    ;; moves from leading comment
    (assert-move-up (h/code "{|;; b comment"
                            " :b 2 ;; two comment"
                            " :a 1 ;; one comment"
                            " :c 3}")
                    (h/code "{:a 1 ;; one comment"
                            " ;; |b comment"
                            " :b 2 ;; two comment"
                            " :c 3}"))
    ;; moves from trailing comment
    (assert-move-up (h/code "{|:b 2 ;; two comment"
                            " :a 1 ;; one comment"
                            " :c 3}")
                    (h/code "{:a 1 ;; one comment"
                            " :b 2 ;; |two comment"
                            " :c 3}"))
    ;; treats uneval as comment
    (assert-move-up (h/code "(case 1"
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
    (assert-move-up (h/code "[|:c"
                            " [:a"
                            "  :b]]")
                    (h/code "[[:a"
                            "  :b]"
                            " |:c]"))
    (assert-move-up (h/code "[|[:a"
                            "  :b]"
                            " :c]")
                    (h/code "[:c"
                            " |[:a"
                            "  :b]]"))
    (assert-move-up (h/code "{|:c 3"
                            " :a {:a/a 1"
                            "     :a/b 2}}")
                    (h/code "{:a {:a/a 1"
                            "     :a/b 2}"
                            " |:c 3}"))
    (assert-move-up (h/code "{|:a {:a/a 1"
                            "     :a/b 2}"
                            " :c 3}")
                    (h/code "{:c 3"
                            " |:a {:a/a 1"
                            "     :a/b 2}}"))))

(deftest move-down
  (testing "common cases"
    (assert-move-down (h/code "{3 4 |1 2}")
                      (h/code "{|1 2 3 4}"))
    (assert-move-down (h/code "{3 4, |1 2}")
                      (h/code "{|1 2, 3 4}"))
    (assert-move-down (h/code "{3 4"
                              " |1 2}")
                      (h/code "{|1 2"
                              " 3 4}"))
    (assert-move-down (h/code "{3 4"
                              " |1 2}")
                      (h/code "{1 |2"
                              " 3 4}"))
    (assert-move-down (h/code "{3 4,"
                              " |1 2}")
                      (h/code "{|1 2,"
                              " 3 4}"))
    (assert-move-down (h/code "{:b (+ 1 1)"
                              " |:a 1"
                              " :c 3}")
                      (h/code "{|:a 1"
                              " :b (+ 1 1)"
                              " :c 3}"))
    (assert-move-down (h/code "[2 |1]")
                      (h/code "[|1 2]"))
    (assert-move-down (h/code "[2 |1 3]")
                      (h/code "[|1 2 3]"))
    (assert-move-down (h/code "[:b"
                              " |:a]")
                      (h/code "[|:a"
                              " :b]"))
    (assert-move-down (h/code "(def a 1)"
                              "[2 |a]")
                      (h/code "(def a 1)"
                              "[|a 2]"))
    (assert-move-down (h/code "(let [a [1 2]"
                              "      c 3"
                              "      |b 2])")
                      (h/code "(let [a [1 2]"
                              "      |b 2"
                              "      c 3])"))
    (assert-move-down (h/code "(binding [b 2"
                              "          |a 1]"
                              "  (+ a b))")
                      (h/code "(binding [|a 1"
                              "          b 2]"
                              "  (+ a b))"))
    (assert-move-down (h/code "(for [b [3 4]"
                              "      |a [1 2]"
                              "      :let [c (inc a)]]"
                              "  (+ a b c))")
                      (h/code "(for [|a [1 2]"
                              "      b [3 4]"
                              "      :let [c (inc a)]]"
                              "  (+ a b c))"))
    (assert-move-down (h/code "(for [a [1 2]"
                              "      :let [c (inc b)"
                              "            |b (inc a)]]"
                              "  (+ a b c))")
                      (h/code "(for [a [1 2]"
                              "      :let [|b (inc a)"
                              "            c (inc b)]]"
                              "  (+ a b c))"))
    (assert-move-down (h/code "(ns foo"
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
    (assert-move-down (h/code "(def b) |(def a)")
                      (h/code "|(def a) (def b)"))
    (assert-move-down (h/code "(def a) (def c) |(def b)")
                      (h/code "(def a) |(def b) (def c)"))
    (is (= ;; preferrably would be:
          #_{:row 1 :col 1
             :end-row 2 :end-col 8}
          shared/full-file-position
          (as-range
            (move-code-down (h/code "|(def a)"
                                    "(def b)"))))))
  (testing "within special functions"
    (assert-move-down (h/code "(cond b 2 |a 1)")
                      (h/code "(cond |a 1 b 2)"))
    (assert-move-down (h/code "(cond b 2 |a 1)")
                      (h/code "(cond a |1 b 2)"))
    (assert-move-down (h/code "(assoc x :b 1 |:a 2)")
                      (h/code "(assoc x |:a 2 :b 1)"))
    (assert-move-down (h/code "(assoc x :b 1 |:a 2)")
                      (h/code "(assoc x :a |2 :b 1)"))
    (assert-move-down (h/code "(-> {}"
                              "    (assoc :b 1 |:a 2))")
                      (h/code "(-> {}"
                              "    (assoc |:a 2 :b 1))"))
    (assert-move-down (h/code "(some-> {}"
                              "        (assoc :b 1 |:a 2))")
                      (h/code "(some-> {}"
                              "        (assoc |:a 2 :b 1))"))
    (assert-move-down (h/code "(cond-> {}"
                              "  true (assoc :b 1 |:a 2))")
                      (h/code "(cond-> {}"
                              "  true (assoc |:a 2 :b 1))"))
    (assert-move-down (h/code "(assoc! x :b 1 |:a 2)")
                      (h/code "(assoc! x |:a 2 :b 1)"))
    (assert-move-down (h/code "(assoc! x :b 1 |:a 2)")
                      (h/code "(assoc! x :a |2 :b 1)"))
    (assert-move-down (h/code "(-> {}"
                              "    (assoc! :b 1 |:a 2))")
                      (h/code "(-> {}"
                              "    (assoc! |:a 2 :b 1))"))
    (assert-move-down (h/code "(some-> {}"
                              "        (assoc! :b 1 |:a 2))")
                      (h/code "(some-> {}"
                              "        (assoc! |:a 2 :b 1))"))
    (assert-move-down (h/code "(cond-> {}"
                              "  true (assoc! :b 1 |:a 2))")
                      (h/code "(cond-> {}"
                              "  true (assoc! |:a 2 :b 1))"))
    (assert-move-down (h/code "(cond-> x b dec |a inc)")
                      (h/code "(cond-> x |a inc b dec)"))
    (assert-move-down (h/code "(cond-> x b dec |a inc)")
                      (h/code "(cond-> x a |inc b dec)"))
    (assert-move-down (h/code "(-> 1"
                              "    (cond-> b dec |a inc))")
                      (h/code "(-> 1"
                              "    (cond-> |a inc b dec))"))
    (assert-move-down (h/code "(some-> 1"
                              "        (cond-> b dec |a inc))")
                      (h/code "(some-> 1"
                              "        (cond-> |a inc b dec))"))
    (assert-move-down (h/code "(cond-> 1"
                              "  true (cond-> b dec |a inc))")
                      (h/code "(cond-> 1"
                              "  true (cond-> |a inc b dec))"))
    (assert-move-down (h/code "(cond->> x b dec |a inc)")
                      (h/code "(cond->> x |a inc b dec)"))
    (assert-move-down (h/code "(cond->> x b dec |a inc)")
                      (h/code "(cond->> x a |inc b dec)"))
    (assert-move-down (h/code "(-> 1"
                              "    (cond->> b dec |a inc))")
                      (h/code "(-> 1"
                              "    (cond->> |a inc b dec))"))
    (assert-move-down (h/code "(some-> 1"
                              "        (cond->> b dec |a inc))")
                      (h/code "(some-> 1"
                              "        (cond->> |a inc b dec))"))
    (assert-move-down (h/code "(cond-> 1"
                              "  true (cond->> b dec |a inc))")
                      (h/code "(cond-> 1"
                              "  true (cond->> |a inc b dec))"))
    (assert-move-down (h/code "(case a 2 :second |1 :first)")
                      (h/code "(case a |1 :first 2 :second)"))
    (assert-move-down (h/code "(case a 2 :second |1 :first)")
                      (h/code "(case a 1 |:first 2 :second)"))
    (assert-move-down (h/code "(case a 2 :second |1 :first :else)")
                      (h/code "(case a |1 :first 2 :second :else)"))
    (assert-move-down (h/code "(case a 2 :second |1 :first :else)")
                      (h/code "(case a 1 |:first 2 :second :else)"))
    (assert-move-down (h/code "(-> 1"
                              "    (case 2 :second |1 :first))")
                      (h/code "(-> 1"
                              "    (case |1 :first 2 :second))"))
    (assert-move-down (h/code "(some-> 1"
                              "        (case 2 :second |1 :first))")
                      (h/code "(some-> 1"
                              "        (case |1 :first 2 :second))"))
    (assert-move-down (h/code "(cond-> 1"
                              "  true (case 2 :second |1 :first))")
                      (h/code "(cond-> 1"
                              "  true (case |1 :first 2 :second))"))
    (assert-move-down (h/code "(condp = x b 2 |a 1)")
                      (h/code "(condp = x |a 1 b 2)"))
    (assert-move-down (h/code "(condp = x b 2 |a 1)")
                      (h/code "(condp = x a |1 b 2)"))
    (assert-move-down (h/code "(condp = x b 2 |a 1 :else)")
                      (h/code "(condp = x |a 1 b 2 :else)"))
    (assert-move-down (h/code "(condp = x b 2 |a 1 :else)")
                      (h/code "(condp = x a |1 b 2 :else)"))
    (assert-move-down (h/code "(condp some [1 2 3 4]"
                              "  #{4 5 9} :>> dec"
                              "  |#{0 6 7} :>> inc)")
                      (h/code "(condp some [1 2 3 4]"
                              "  |#{0 6 7} :>> inc"
                              "  #{4 5 9} :>> dec)"))
    (assert-move-down (h/code "(condp some [1 2 3 4]"
                              "  #{4 5 9} :>> dec"
                              "  |#{0 6 7} :>> inc)")
                      (h/code "(condp some [1 2 3 4]"
                              "  #{0 6 7} |:>> inc"
                              "  #{4 5 9} :>> dec)"))
    (assert-move-down (h/code "(condp some [1 2 3 4]"
                              "  #{4 5 9} :>> dec"
                              "  |#{0 6 7} :>> inc)")
                      (h/code "(condp some [1 2 3 4]"
                              "  #{0 6 7} :>> |inc"
                              "  #{4 5 9} :>> dec)"))
    (assert-move-down (h/code "(condp some [1 2 3 4]"
                              "  #{4 5 9} :>> dec"
                              "  |#{0 6 7} :>> inc"
                              "  :else)")
                      (h/code "(condp some [1 2 3 4]"
                              "  #{0 6 7} :>> |inc"
                              "  #{4 5 9} :>> dec"
                              "  :else)"))
    (assert-move-down (h/code "(are [x] (= x 1) 2 |1)")
                      (h/code "(are [x] (= x 1) |1 2)"))
    (assert-move-down (h/code "(are [expected x] (= expected x) 3 4 |1 2)")
                      (h/code "(are [expected x] (= expected x) |1 2 3 4)"))
    (assert-move-down (h/code "(are [expected x] (= expected x) 3 4 |1 2)")
                      (h/code "(are [expected x] (= expected x) 1 |2 3 4)")))
  (testing "with destructuring"
    (assert-move-down (h/code "(let [[a b] [1 2]"
                              "      {:keys [c d]} {:c 1 :d 2}"
                              "      |e 2])")
                      (h/code "(let [[a b] [1 2]"
                              "      |e 2"
                              "      {:keys [c d]} {:c 1 :d 2}])"))
    (assert-move-down (h/code "(let [[a b] [1 2]"
                              "      e 2"
                              "      |{:keys [c d]} {:c 1 :d 2}])")
                      (h/code "(let [[a b] [1 2]"
                              "      |{:keys [c d]} {:c 1 :d 2}"
                              "      e 2])"))
    (assert-move-down (h/code "(for [a [1 2]"
                              "      :let [{:keys [c]} {:c 1}"
                              "            |{:keys [b]} {:b 1}]])")
                      (h/code "(for [a [1 2]"
                              "      :let [|{:keys [b]} {:b 1}"
                              "            {:keys [c]} {:c 1}]])")))
  (testing "blank lines"
    (assert-move-down (h/code "{"
                              " :b 2"
                              ""
                              " |:a 1}")
                      (h/code "{|"
                              " :a 1"
                              ""
                              " :b 2}"))
    (assert-move-down (h/code "{:a 1"
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
    (assert-move-down (h/code "{:b (+ 1 1) ;; two comment"
                              " |:a 1 ;; one comment"
                              " :c 3} ;; three comment")
                      (h/code "{|:a 1 ;; one comment"
                              " :b (+ 1 1) ;; two comment"
                              " :c 3} ;; three comment"))
    (assert-move-down (h/code ";; main comment"
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
    (assert-move-down (h/code "{:b 2"
                              " |:a 1"
                              " ;; trailing comment"
                              " }")
                      (h/code "{|:a 1"
                              " :b 2"
                              " ;; trailing comment"
                              " }"))
    (assert-move-down (h/code "{;; a"
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
    (assert-move-down (h/code "{;; b"
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
    (assert-move-down (h/code "[b"
                              " |a ;; a comment"
                              " ]")
                      (h/code "[|a ;; a comment"
                              " b]"))
    (assert-move-down (h/code "{:b 2"
                              " |:a 1 ;; one comment"
                              " }")
                      (h/code "{|:a 1 ;; one comment"
                              " :b 2}"))
    ;; moves from leading comment
    (assert-move-down (h/code "{;; b comment"
                              " :b 2 ;; two comment"
                              " |;; a comment"
                              " :a 1 ;; one comment"
                              " :c 3}")
                      (h/code "{;; |a comment"
                              " :a 1 ;; one comment"
                              " ;; b comment"
                              " :b 2 ;; two comment"
                              " :c 3}"))
    ;; moves from trailing comment
    (assert-move-down (h/code "{:b 2 ;; two comment"
                              " |:a 1 ;; one comment"
                              " :c 3}")
                      (h/code "{:a 1 ;; |one comment"
                              " :b 2 ;; two comment"
                              " :c 3}")))
  (testing "multi-line elements"
    (assert-move-down (h/code "[:c"
                              " |[:a"
                              "  :b]]")
                      (h/code "[|[:a"
                              "  :b]"
                              " :c]"))
    (assert-move-down (h/code "[[:a"
                              "  :b]"
                              " |:c]")
                      (h/code "[|:c"
                              " [:a"
                              "  :b]]"))
    (assert-move-down (h/code "{:c 3"
                              " |:a {:a/a 1"
                              "     :a/b 2}}")
                      (h/code "{|:a {:a/a 1"
                              "     :a/b 2}"
                              " :c 3}"))
    (assert-move-down (h/code "{:a {:a/a 1"
                              "     :a/b 2}"
                              " |:c 3}")
                      (h/code "{|:c 3"
                              " :a {:a/a 1"
                              "     :a/b 2}}"))))

;; These are macros so test failures have the right line numbers
(defmacro assert-no-erroneous-up [code]
  `(let [ws-zloc# (h/load-code-and-zloc ~code)]
     (is (can-move-zloc-up? ws-zloc#))
     (is (nil? (as-string (move-zloc-up ws-zloc#))))))
(defmacro assert-no-erroneous-down [code]
  `(let [ws-zloc# (h/load-code-and-zloc ~code)]
     (is (can-move-zloc-down? ws-zloc#))
     (is (nil? (as-string (move-zloc-down ws-zloc#))))))

(deftest erroneous-swaps
  ;; NOTE: ideally can-move-*? and move-* would always agree, but when they
  ;; don't at least no erroneous swaps happen
  (testing "from blank line after first element"
    (assert-no-erroneous-up (h/code "[:a"
                                    "|"
                                    "]"))
    (assert-no-erroneous-up (h/code "{:a 1"
                                    "|"
                                    "}"))
    (assert-no-erroneous-up (h/code "(case a"
                                    "   1 :first"
                                    "|"
                                    ")")))

  (testing "from comment after first element"
    (assert-no-erroneous-up (h/code "[:a |;; a comment"
                                    "]"))
    (assert-no-erroneous-up (h/code "{:a 1 |;; one comment"
                                    "}"))
    (assert-no-erroneous-up (h/code "(case a"
                                    "   1 :first |;; one comment"
                                    ")")))

  (testing "from blank line before last element"
    (assert-no-erroneous-down (h/code "[|"
                                      " :a]"))
    (assert-no-erroneous-down (h/code "{|"
                                      " :a 1}"))
    (assert-no-erroneous-down (h/code "(case a"
                                      "|"
                                      "   1 :first :else)")))

  (testing "from comment before last element"
    (assert-no-erroneous-down (h/code "[|;; a comment"
                                      " :a]"))
    (assert-no-erroneous-down (h/code "{|;; a comment"
                                      " :a 1}"))
    (assert-no-erroneous-down (h/code "(case a"
                                      "   |;; one comment"
                                      "   1 :first :else)"))))
