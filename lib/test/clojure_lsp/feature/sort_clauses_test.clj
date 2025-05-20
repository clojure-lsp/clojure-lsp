(ns clojure-lsp.feature.sort-clauses-test
  (:require
   [clojure-lsp.feature.sort-clauses :as f.sort-clauses]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is testing]]))

(defn ^:private can-sort-zloc? [zloc]
  (f.sort-clauses/can-sort? zloc h/default-uri (h/db)))

(defn ^:private sort-zloc [zloc]
  (f.sort-clauses/sort-clauses zloc h/default-uri (h/db)))

(defn ^:private as-string [changes]
  (h/changes->code changes (h/db)))

(defmacro ^:private assert-cannot-sort [code]
  `(let [zloc# (h/load-code-and-zloc ~code)]
     (is (not (can-sort-zloc? zloc#))
         (as-string (sort-zloc zloc#)))))

(deftest should-not-sort
  (testing "top-level forms"
    (assert-cannot-sort "|")
    (assert-cannot-sort "|[]")
    (assert-cannot-sort "|1")
    (assert-cannot-sort "|{}"))
  (testing "binding vectors"
    (assert-cannot-sort "(let [b 1 |a (inc b)] (+ b a))"))
  (testing "malformed code"
    (assert-cannot-sort "{|:a}")
    (assert-cannot-sort "{|:a 2 :b}")))

(defmacro ^:private assert-sorts [sorted-code original-code]
  `(let [original# ~original-code
         zloc# (h/load-code-and-zloc original#)
         expected# ~sorted-code]
     (is (can-sort-zloc? zloc#) original#)
     (is (= expected#
            (as-string (sort-zloc zloc#)))
         original#)))

(deftest should-sort-clauses
  (testing "sorts maps"
    (assert-sorts "{:a 1 :b 2}" "{|:b 2 :a 1}")
    (assert-sorts "{:a 1 :b 2}" "|{:b 2 :a 1}") ;; outside map
    (assert-sorts "{:g 1}" "{|:g 1}")
    (assert-sorts "{:d 3 :g 1}" "{|:d 3 :g 1}")
    (assert-sorts "{:d 3 :g 1}" "{|:g 1 :d 3}")
    (assert-sorts "{:G 1 :d 3}" "{|:G 1 :d 3}")
    (assert-sorts "{:d 3 :e 4 :g 1}" "{|:g 1 :d 3 :e 4}")
    (assert-sorts "{:a 1 :b {:c 2 :d 3}}" "{|:b {:c 2 :d 3} :a 1}")
    (assert-sorts "{:a [9 0] :b {:c 2 :d 3} :c #{2 5}}"
                  "{|:c #{2 5} :a [9 0] :b {:c 2 :d 3}}")
    (assert-sorts "{:a #_2 3 :b 4}" "{:b 4 |:a #_2 3}"))
  (testing "sorts other things with clauses"
    (assert-sorts "[1 2 3 4]" "[2 |1 3 4]")
    (assert-sorts "#{1 2 3 4}" "#{2 |1 3 4}")
    (assert-sorts "(1 2)" "(2 |1)")
    (assert-sorts "(assoc m :a 1 :b 2)" "(|assoc m :b 2 :a 1)"))
  (testing "commas"
    (assert-sorts "{:a,[9,0],:b,{:c,2,:d,3},:c,#{2,5}}"
                  "{|:c,#{2,5},:a,[9,0],:b,{:c,2,:d,3}}")
    (assert-sorts "{:a [9 0], :b {:c 2, :d 3}, :c #{2 5}}"
                  "{|:c #{2 5}, :a [9 0], :b {:c 2, :d 3}}")
    (assert-sorts "{:a [9,, 0] :b, {:c,, 2,,, :d, 3},, :c, #{2 5},,}"
                  "{|:c, #{2 5} :a [9,, 0],, :b, {:c,, 2,,, :d, 3},,}"))
  (testing "spaces"
    (assert-sorts "{:a    [9   0 ] :b   {   :c    2 :d 3 }   :c #{ 2 5 }  }"
                  "{|:c #{ 2 5 } :a    [9   0 ]   :b   {   :c    2 :d 3 }  }"))
  (testing "new-lines"
    (assert-sorts (h/code "{:a 1"
                          " :b 2"
                          " :c 3}")
                  (h/code "{|:c 3"
                          " :a 1"
                          " :b 2}"))
    (assert-sorts (h/code "{:a 1"
                          ""
                          " :b 2"
                          " :c 3}")
                  (h/code "{|:c 3"
                          ""
                          " :a 1"
                          " :b 2}"))
    (assert-sorts (h/code "{  :a 1"
                          "  :b 2"
                          "    :c 3}")
                  (h/code "{|  :c 3"
                          "  :a 1"
                          "    :b 2}"))
    (assert-sorts (h/code "{:a [9 0]"
                          " :b {:c 2"
                          "     :d 3}"
                          " :c #{2 5}}")
                  (h/code "{|:c #{2 5}"
                          " :a [9 0]"
                          " :b {:c 2"
                          "     :d 3}}")))
  (testing "comments"
    ;; adds extra line to fix trailing comment
    (assert-sorts (h/code "{:a 1"
                          " :b 2"
                          " :c 3 ;; something"
                          " }")
                  (h/code "{|:a 1"
                          " :c 3 ;; something"
                          " :b 2}"))
    ;; doesn't add extra line when rind provides extra line
    (assert-sorts (h/code "(case x"
                          "  :a 1"
                          "  :b 2"
                          "  :c 3 ;; something"
                          "  :default)")
                  (h/code "(case x"
                          "  :a 1"
                          "  |:c 3 ;; something"
                          "  :b 2"
                          "  :default)"))
    ;; even if rind ends in a comment
    (assert-sorts (h/code "(case x"
                          "  :a 1"
                          "  :b 2"
                          "  :c 3 ;; something"
                          "  :default ;; default comment"
                          "  )")
                  (h/code "(case x"
                          "  :a 1"
                          "  |:c 3 ;; something"
                          "  :b 2"
                          "  :default ;; default comment"
                          "  )"))
    ;; does not remove existing trailing comment fix
    (assert-sorts (h/code "{:a 1"
                          " :b 2 ;; something"
                          " :c 3"
                          "}")
                  (h/code "{|:a 1"
                          " :c 3"
                          " :b 2 ;; something"
                          "}"))))
