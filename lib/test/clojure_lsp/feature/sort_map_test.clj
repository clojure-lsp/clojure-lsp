(ns clojure-lsp.feature.sort-map-test
  (:require
   [clojure-lsp.feature.sort-map :as f.sort-map]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(h/reset-components-before-test)

(deftest sortable-map?
  (is (not (f.sort-map/sortable-map-zloc (z/of-string "[]"))))
  (is (not (f.sort-map/sortable-map-zloc (z/of-string "[1 2 3 4]"))))
  (is (not (f.sort-map/sortable-map-zloc (z/of-string "1"))))
  (is (not (f.sort-map/sortable-map-zloc (z/of-string "(1 2)"))))
  (is (not (f.sort-map/sortable-map-zloc (z/of-string "{}"))))
  (is (not (f.sort-map/sortable-map-zloc (z/of-string "{:a}"))))
  (is (not (f.sort-map/sortable-map-zloc (z/of-string "{:a #_2 3}"))))
  (is (not (f.sort-map/sortable-map-zloc (z/of-string "{:a 2 :b}"))))
  (is (f.sort-map/sortable-map-zloc (z/of-string "{:a 2}")))
  (is (f.sort-map/sortable-map-zloc (z/of-string "{:a 2 :b 3}")))
  (is (f.sort-map/sortable-map-zloc (z/of-string "{:a {:c 4} :b 3}")))
  (is (f.sort-map/sortable-map-zloc (-> (z/of-string "{:a {:c 4} :b 3}")
                                        (z/find-next-value z/next :b))))
  (is (f.sort-map/sortable-map-zloc (-> (z/of-string "{:a {:c 4} :b 3}")
                                        (z/find-next-value z/next :c))))
  (is (not (f.sort-map/sortable-map-zloc (-> (z/of-string "{:a {:c 4} :b (foo 2)}")
                                             (z/find-next-value z/next 'foo))))))

(defn ^:private assert-sort [expected subject]
  (is (= expected
         (-> subject
             z/of-string
             (#'f.sort-map/sort-map-by-key)
             z/root-string))))

(deftest sort-map-by-key
  (testing "common cases"
    (assert-sort "{:g 1}" "{:g 1}")
    (assert-sort "{:g 1}" "{:g 1}")
    (assert-sort "{:d 3 :g 1}" "{:d 3 :g 1}")
    (assert-sort "{:d 3 :g 1}" "{:g 1 :d 3}")
    (assert-sort "{:G 1 :d 3}" "{:G 1 :d 3}")
    (assert-sort "{:d 3 :e 4 :g 1}" "{:g 1 :d 3 :e 4}")
    (assert-sort "{:d 3 :e 4 :g 1}" "{:g 1 :d 3 :e 4}")
    (assert-sort "{:a 1 :b {:c 2 :d 3}}" "{:b {:c 2 :d 3} :a 1}")
    (assert-sort "{:a [9 0] :b {:c 2 :d 3} :c #{2 5}}"
                 "{:c #{2 5} :a [9 0] :b {:c 2 :d 3}}"))
  (testing "commas"
    (assert-sort "{:a,[9,0],:b,{:c,2,:d,3},:c,#{2,5}}"
                 "{:c,#{2,5},:a,[9,0],:b,{:c,2,:d,3}}")
    (assert-sort "{:a [9 0], :b {:c 2, :d 3}, :c #{2 5}}"
                 "{:c #{2 5}, :a [9 0], :b {:c 2, :d 3}}")
    (assert-sort "{:a [9,, 0] :b, {:c,, 2,,, :d, 3},, :c, #{2 5},,}"
                 "{:c, #{2 5} :a [9,, 0],, :b, {:c,, 2,,, :d, 3},,}"))
  (testing "spaces"
    (assert-sort "{:a    [9   0 ] :b   {   :c    2 :d 3 }   :c #{ 2 5 }  }"
                 "{:c #{ 2 5 } :a    [9   0 ]   :b   {   :c    2 :d 3 }  }"))
  (testing "new-lines"
    (assert-sort (h/code "{:a 1"
                         " :b 2"
                         " :c 3}")
                 (h/code "{:c 3"
                         " :a 1"
                         " :b 2}"))
    (assert-sort (h/code "{:a 1"
                         ""
                         " :b 2"
                         " :c 3}")
                 (h/code "{:c 3"
                         ""
                         " :a 1"
                         " :b 2}"))
    (assert-sort (h/code "{  :a 1"
                         "  :b 2"
                         "    :c 3}")
                 (h/code "{  :c 3"
                         "  :a 1"
                         "    :b 2}"))
    (assert-sort (h/code "{:a [9 0]"
                         " :b {:c 2"
                         "     :d 3}"
                         " :c #{2 5}}")
                 (h/code "{:c #{2 5}"
                         " :a [9 0]"
                         " :b {:c 2"
                         "     :d 3}}")))
  (testing "comments"
    (assert-sort (h/code "{:a 1 :b 2 :c 3 ;; something"
                         ""
                         "}")
                 (h/code "{:a 1 :b 2 :c 3 ;; something"
                         "}"))
    (assert-sort (h/code "{:a 1 ;; some comment"
                         ""
                         " :b 2 ;; other comment"
                         ""
                         ""
                         " :c 3 ;; another comment"
                         ""
                         "}")
                 (h/code "{:c 3 ;; another comment"
                         " :a 1 ;; some comment"
                         ""
                         " :b 2 ;; other comment"
                         "}")))
  (testing "mixed cases"
    (assert-sort (h/code "{:a, 1 ;; some comment"
                         ""
                         " :b ,   [1 2 3] ;; other comment"
                         ""
                         ","
                         " ,:cde {:d 4 ;; another comment"
                         "        :r {:a ,4 :b, 6}}"
                         "}")
                 (h/code "{:a, 1 ;; some comment"
                         " :b ,   [1 2 3] ;; other comment"
                         ","
                         " ,:cde {:d 4 ;; another comment"
                         "        :r {:a ,4 :b, 6}}"
                         "}"))))
