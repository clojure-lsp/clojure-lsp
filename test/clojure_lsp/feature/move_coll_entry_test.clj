(ns clojure-lsp.feature.move-coll-entry-test
  (:require
   [clojure-lsp.feature.move-coll-entry :as f.move-coll-entry]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(deftest can-move-entry-up?
  (testing "common cases"
    (is (not (f.move-coll-entry/can-move-entry-up? (z/of-string "[]"))))
    (is (not (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "[1]")
                                                       (z/find-next-value z/next 1)))))
    (is (not (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "#{1 2 3}")
                                                       (z/find-next-value z/next 2)))))
    (is (not (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "[1 2 3]")
                                                       (z/find-next-value z/next 2)))))
    (is (not (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "{1 2 3}")
                                                       (z/find-next-value z/next 2)))))
    (is (not (f.move-coll-entry/can-move-entry-up? (z/of-string "{:a :b :c :d}"))))
    (is (not (f.move-coll-entry/can-move-entry-up? (z/of-string "#{:a :b :c :d}"))))
    (is (not (f.move-coll-entry/can-move-entry-up? (z/of-string "[:a :b :c :d]"))))
    (is (not (f.move-coll-entry/can-move-entry-up? (z/of-string "'(:a :b :c :d)"))))
    (is (not (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "{1 2}")
                                                       (z/find-next-value z/next 2)))))
    (is (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "{1 2 3 4}")
                                                  (z/find-next-value z/next 3))))
    (is (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "{:a :b :c :d}")
                                                  (z/find-next-value z/next :c))))
    (is (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "[1 2 3 4]")
                                                  (z/find-next-value z/next 3))))
    (is (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "#{1 2 3 4}")
                                                  (z/find-next-value z/next 3))))
    (is (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "'[a 1 c 2]")
                                                  (z/find-next-value z/next 'c)))))
  (testing "comments"
    (is (f.move-coll-entry/can-move-entry-up? (-> (z/of-string (h/code "{1 2 ;; some comment"
                                                                       " 3 4}"))
                                                  (z/find-next-value z/next 3))))
    (is (f.move-coll-entry/can-move-entry-up? (-> (z/of-string (h/code "{1 2"
                                                                       " ;; some comment"
                                                                       " 3 4}"))
                                                  (z/find-next-value z/next 3)))))
  (testing "when on first entry"
    (is (not (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "#{1 2 3 4}")
                                                       (z/find-next-value z/next 1)))))
    (is (not (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "#{1 2 3 4}")
                                                       (z/find-next-value z/next 2)))))
    (is (not (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "{:a :b :c :d}")
                                                       (z/find-next-value z/next :a)))))
    (is (not (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "{:a :b :c :d}")
                                                       (z/find-next-value z/next :b)))))
    (is (not (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "'[a 1 c 2]")
                                                       (z/find-next-value z/next 'a)))))
    (is (not (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "'[a 1 c 2]")
                                                       (z/find-next-value z/next 1)))))))

(deftest can-move-entry-down?
  (testing "common cases"
    (is (not (f.move-coll-entry/can-move-entry-down? (z/of-string "[]"))))
    (is (not (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "[1]")
                                                         (z/find-next-value z/next 1)))))
    (is (not (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "#{1 2 3}")
                                                         (z/find-next-value z/next 2)))))
    (is (not (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "[1 2 3]")
                                                         (z/find-next-value z/next 2)))))
    (is (not (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "{1 2 3}")
                                                         (z/find-next-value z/next 2)))))
    (is (not (f.move-coll-entry/can-move-entry-down? (z/of-string "{:a :b :c :d}"))))
    (is (not (f.move-coll-entry/can-move-entry-down? (z/of-string "#{:a :b :c :d}"))))
    (is (not (f.move-coll-entry/can-move-entry-down? (z/of-string "[:a :b :c :d]"))))
    (is (not (f.move-coll-entry/can-move-entry-down? (z/of-string "'(:a :b :c :d)"))))
    (is (not (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "{1 2}")
                                                         (z/find-next-value z/next 1)))))
    (is (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "{1 2 3 4}")
                                                    (z/find-next-value z/next 1))))
    (is (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "{:a :b :c :d}")
                                                    (z/find-next-value z/next :a))))
    (is (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "[1 2 3 4]")
                                                    (z/find-next-value z/next 1))))
    (is (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "#{1 2 3 4}")
                                                    (z/find-next-value z/next 1))))
    (is (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "'[a 1 c 2]")
                                                    (z/find-next-value z/next 'a)))))
  (testing "comments"
    (is (f.move-coll-entry/can-move-entry-down? (-> (z/of-string (h/code "{1 2 ;; some comment"
                                                                         " 3 4 ;; other coment"
                                                                         " 5 6}"))
                                                    (z/find-next-value z/next 3))))
    (is (f.move-coll-entry/can-move-entry-down? (-> (z/of-string (h/code "{1 2"
                                                                         " ;; some comment"
                                                                         " 3 4"
                                                                         " 5 6}"))
                                                    (z/find-next-value z/next 3)))))
  (testing "when on last entry"
    (is (not (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "#{1 2 3 4}")
                                                         (z/find-next-value z/next 3)))))
    (is (not (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "#{1 2 3 4}")
                                                         (z/find-next-value z/next 4)))))
    (is (not (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "{:a :b :c :d}")
                                                         (z/find-next-value z/next :c)))))
    (is (not (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "{:a :b :c :d}")
                                                         (z/find-next-value z/next :d)))))
    (is (not (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "'[a 1 c 2]")
                                                         (z/find-next-value z/next 'c)))))
    (is (not (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "'[a 1 c 2]")
                                                         (z/find-next-value z/next 2)))))))

(defn- string-move-up [subject cursor]
  (some-> (z/of-string subject {:track-position? true})
          (z/find-next-value z/next cursor)
          (f.move-coll-entry/move-up "file:///a.clj")
          :changes-by-uri
          (get "file:///a.clj")
          first
          :loc
          z/root-string))

(defn- string-move-down [subject cursor]
  (some-> (z/of-string subject {:track-position? true})
          (z/find-next-value z/next cursor)
          (f.move-coll-entry/move-down "file:///a.clj")
          :changes-by-uri
          (get "file:///a.clj")
          first
          :loc
          z/root-string))

(defmacro assert-move-up [expected subject cursor]
  ;; This is a macro so test failures have the right line numbers
  `(is (= ~expected (string-move-up ~subject ~cursor))))

(defmacro assert-move-down [expected subject cursor]
  ;; This is a macro so test failures have the right line numbers
  `(is (= ~expected (string-move-down ~subject ~cursor))))

(deftest move-up
  (testing "common cases"
    ;; TODO fix this case
    ;; (assert-move-up "{3 4 1 2}" "{1 2 3 4}" 3)
    (assert-move-up (h/code "{3 4"
                            " 1 2}")
                    (h/code "{1 2"
                            " 3 4}") 3)
    (assert-move-up (h/code "{:b (+ 1 1)"
                            " :a 1"
                            " :c 3}")
                    (h/code "{:a 1"
                            " :b (+ 1 1)"
                            " :c 3}") :b)
    (assert-move-up (h/code "[a [1 2]"
                            " c 3"
                            " b 2]")
                    (h/code "[a [1 2]"
                            " b 2"
                            " c 3]") 'c)
    (assert-move-up (h/code "#{a 1"
                            "  c 3"
                            "  b 2}")
                    (h/code "#{a 1"
                            "  b 2"
                            "  c 3}") 'c)
    (testing "with destructuring"
      (assert-move-up (h/code "(let [[a b] [1 2]"
                              "      e 2"
                              "      {:keys [c d]} {:c 1 :d 2}])")
                      (h/code "(let [[a b] [1 2]"
                              "      {:keys [c d]} {:c 1 :d 2}"
                              "      e 2])") 'e))
    (testing "comments"
      (assert-move-up (h/code "{:b (+ 1 1) ;; one comment"
                              " :a 1 ;; two comment"
                              " :c 3} ;; three comment")
                      (h/code "{:a 1 ;; two comment"
                              " :b (+ 1 1) ;; one comment"
                              " :c 3} ;; three comment") :b)
      (assert-move-up (h/code ";; main comment"
                              "{;; b comment"
                              " :b (+ 1 1) ;; one comment"
                              " :a 1 ;; two comment"
                              " ;; c comment"
                              " :c 3} ;; three comment")
                      (h/code ";; main comment"
                              "{:a 1 ;; two comment"
                              " ;; b comment"
                              " :b (+ 1 1) ;; one comment"
                              " ;; c comment"
                              " :c 3} ;; three comment") :b))))

(deftest move-down
  (testing "common cases"
    ;; TODO fix this case
    ;; (assert-move-down "{3 4 1 2}" "{1 2 3 4}" 3)
    (assert-move-down (h/code "{3 4"
                              " 1 2}")
                      (h/code "{1 2"
                              " 3 4}") 1)
    (assert-move-down (h/code "{:b (+ 1 1)"
                              " :a 1"
                              " :c 3}")
                      (h/code "{:a 1"
                              " :b (+ 1 1)"
                              " :c 3}") :a)
    (assert-move-down (h/code "[a [1 2]"
                              " c 3"
                              " b 2]")
                      (h/code "[a [1 2]"
                              " b 2"
                              " c 3]") 'b)
    (assert-move-down (h/code "#{a 1"
                              "  c 3"
                              "  b 2}")
                      (h/code "#{a 1"
                              "  b 2"
                              "  c 3}") 'b)
    (testing "with destructuring"
      (assert-move-down (h/code "(let [[a b] [1 2]"
                                "      {:keys [c d]} {:c 1 :d 2}"
                                "      e 2])")
                        (h/code "(let [[a b] [1 2]"
                                "      e 2"
                                "      {:keys [c d]} {:c 1 :d 2}])")
                        'e))
    (testing "comments"
      (assert-move-down (h/code "{:b (+ 1 1) ;; one comment"
                                " :a 1 ;; two comment"
                                " :c 3} ;; three comment")
                        (h/code "{:a 1 ;; two comment"
                                " :b (+ 1 1) ;; one comment"
                                " :c 3} ;; three comment") :a)
      (assert-move-down (h/code ";; main comment"
                                "{;; b comment"
                                " :b (+ 1 1) ;; one comment"
                                " :a 1 ;; two comment"
                                " ;; c comment"
                                " :c 3} ;; three comment")
                        (h/code ";; main comment"
                                "{:a 1 ;; two comment"
                                " ;; b comment"
                                " :b (+ 1 1) ;; one comment"
                                " ;; c comment"
                                " :c 3} ;; three comment") :a))))
