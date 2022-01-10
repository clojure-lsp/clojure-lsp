(ns clojure-lsp.feature.move-coll-entry-test
  (:require
   [clojure-lsp.feature.move-coll-entry :as f.move-coll-entry]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.node :as n]
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
    (is (not (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "#{1 2 3 4}")
                                                       (z/find-next-value z/next 3)))))
    (is (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "{1 2 3 4}")
                                                  (z/find-next-value z/next 3))))
    (is (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "{:a :b :c :d}")
                                                  (z/find-next-value z/next :c))))
    (is (f.move-coll-entry/can-move-entry-up? (-> (z/of-string "[1 2 3 4]")
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
  (testing "with destructuring"
    (is (f.move-coll-entry/can-move-entry-up? (-> (z/of-string (h/code "[[a b] [1 2]"
                                                                       " {:keys [c d]} {:c 1 :d 2}"
                                                                       " e 2]"))
                                                  (z/find-next-depth-first #(= (z/sexpr %)
                                                                               {:keys '[c d]}))))))
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
    (is (not (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "#{1 2 3 4}")
                                                         (z/find-next-value z/next 1)))))
    (is (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "{1 2 3 4}")
                                                    (z/find-next-value z/next 1))))
    (is (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "{:a :b :c :d}")
                                                    (z/find-next-value z/next :a))))
    (is (f.move-coll-entry/can-move-entry-down? (-> (z/of-string "[1 2 3 4]")
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
  (testing "with destructuring"
    (is (f.move-coll-entry/can-move-entry-down? (-> (z/of-string (h/code "[[a b] [1 2]"
                                                                         " {:keys [c d]} {:c 1 :d 2}"
                                                                         " e 2]"))
                                                    (z/find-next-depth-first #(= (z/sexpr %)
                                                                                 {:keys '[c d]}))))))
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

(defn- move-up* [subject cursor]
  (some-> (z/of-string subject)
          (z/find-next-depth-first #(= (z/sexpr %) cursor))
          (f.move-coll-entry/move-up "file:///a.clj")))

(defn- move-down* [subject cursor]
  (some-> (z/of-string subject)
          (z/find-next-depth-first #(= (z/sexpr %) cursor))
          (f.move-coll-entry/move-down "file:///a.clj")))

(defn- as-string [change]
  (some-> change
          :changes-by-uri
          (get "file:///a.clj")
          first
          :loc
          z/root-string))

(defn- as-position [change]
  (when-let [{:keys [row col]} (some-> change
                                       :show-document-after-edit
                                       :range)]
    [row col]))

;; These are macros so test failures have the right line numbers
(defmacro assert-move-up [expected subject cursor]
  `(is (= ~expected (as-string (move-up* ~subject ~cursor)))))
(defmacro assert-move-down [expected subject cursor]
  `(is (= ~expected (as-string (move-down* ~subject ~cursor)))))
(defmacro assert-move-up-position [expected subject cursor]
  `(is (= ~expected (as-position (move-up* ~subject ~cursor)))))
(defmacro assert-move-down-position [expected subject cursor]
  `(is (= ~expected (as-position (move-down* ~subject ~cursor)))))

(deftest move-up
  (testing "common cases"
    (assert-move-up (h/code "{3 4 1 2}")
                    (h/code "{1 2 3 4}") 3)
    (assert-move-up (h/code "{3 4, 1 2}")
                    (h/code "{1 2, 3 4}") 3)
    (assert-move-up (h/code "{3 4"
                            " 1 2}")
                    (h/code "{1 2"
                            " 3 4}") 3)
    (assert-move-up (h/code "{3 4"
                            " 1 2}")
                    (h/code "{1 2"
                            " 3 4}") 4)
    (assert-move-up (h/code "{3 4,"
                            " 1 2}")
                    (h/code "{1 2,"
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
    (testing "with destructuring"
      (assert-move-up (h/code "(let [[a b] [1 2]"
                              "      e 2"
                              "      {:keys [c d]} {:c 1 :d 2}])")
                      (h/code "(let [[a b] [1 2]"
                              "      {:keys [c d]} {:c 1 :d 2}"
                              "      e 2])") 'e)
      (assert-move-up (h/code "(let [{:keys [c d]} {:c 1 :d 2}"
                              "      [a b] [1 2]"
                              "      e 2])")
                      (h/code "(let [[a b] [1 2]"
                              "      {:keys [c d]} {:c 1 :d 2}"
                              "      e 2])") {:keys ['c 'd]}))
    (testing "blank lines"
      (is (= (h/code "{:b 2"
                     ""
                     " :a 1}")
             (some-> (z/of-string (h/code "{:a 1"
                                          ""
                                          " :b 2}"))
                     ;; move cursor to blank line between a and b
                     (z/down)
                     (z/find-next-value z/right 1)
                     (z/right*)
                     (f.move-coll-entry/move-up "file:///a.clj")
                     as-string)))
      (is (nil?
           (some-> (z/of-string (h/code "{:a 1"
                                        ""
                                        " :b 2"
                                        ""
                                        "}"))
                   ;; move cursor to blank line after b
                   (z/down)
                   (z/find-next-value z/right 2)
                   (z/right*)
                   (f.move-coll-entry/move-up "file:///a.clj")))))
    (testing "comments"
      (assert-move-up (h/code "{:b (+ 1 1) ;; two comment"
                              " :a 1 ;; one comment"
                              " :c 3} ;; three comment")
                      (h/code "{:a 1 ;; one comment"
                              " :b (+ 1 1) ;; two comment"
                              " :c 3} ;; three comment") :b)
      (assert-move-up (h/code ";; main comment"
                              "{;; b comment"
                              " :b (+ 1 1) ;; two comment"
                              " :a 1 ;; one comment"
                              " ;; c comment"
                              " :c 3} ;; three comment")
                      (h/code ";; main comment"
                              "{:a 1 ;; one comment"
                              " ;; b comment"
                              " :b (+ 1 1) ;; two comment"
                              " ;; c comment"
                              " :c 3} ;; three comment") :b)
      (assert-move-up (h/code "{:b 2"
                              " :a 1"
                              " ;; trailing comment"
                              " }")
                      (h/code "{:a 1"
                              " :b 2"
                              " ;; trailing comment"
                              " }") :b)
      (assert-move-up (h/code "{;; a"
                              " a 1"
                              ""
                              " ;; c"
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
                              " c 3}") 'c)
      (assert-move-up (h/code "{;; b"
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
                              " b 2"
                              ""
                              " ;; c"
                              " c 3}") 'b)
      ;; avoids commenting out closing bracket
      (assert-move-up (h/code "{:b 2"
                              " :a 1 ;; one comment"
                              " }")
                      (h/code "{:a 1 ;; one comment"
                              " :b 2}") :b)
      ;; moves from leading comment / whitespace
      (is (= (h/code "{;; b comment"
                     " :b 2 ;; two comment"
                     " :a 1 ;; one comment"
                     " :c 3}")
             (some-> (z/of-string (h/code "{:a 1 ;; one comment"
                                          " ;; b comment"
                                          " :b 2 ;; two comment"
                                          " :c 3}"))
                     ;; move cursor to <comment '; b comment'>
                     (z/down)
                     (z/find-next-value z/right :b)
                     (z/find z/left* (comp n/comment? z/node))
                     (f.move-coll-entry/move-up "file:///a.clj")
                     as-string)))
      ;; moves from trailing comment / whitespace
      (is (= (h/code "{:b 2 ;; two comment"
                     " :a 1 ;; one comment"
                     " :c 3}")
             (some-> (z/of-string (h/code "{:a 1 ;; one comment"
                                          " :b 2 ;; two comment"
                                          " :c 3}"))
                     ;; move cursor to <comment '; two comment'>
                     (z/down)
                     (z/find-next-value z/right :b)
                     (z/find z/right* (comp n/comment? z/node))
                     (f.move-coll-entry/move-up "file:///a.clj")
                     as-string))))
    (testing "relocation"
      (assert-move-up-position [1 2]
                               (h/code "{:a 1 ;; one comment"
                                       " :b 2}") :b)
      ;; moves cursor to start of entry pair
      (assert-move-up-position [1 2]
                               (h/code "{:a :x ;; one comment"
                                       " :b    :y}") :y))))

(deftest move-down
  (testing "common cases"
    (assert-move-down (h/code "{3 4 1 2}")
                      (h/code "{1 2 3 4}") 1)
    (assert-move-down (h/code "{3 4, 1 2}")
                      (h/code "{1 2, 3 4}") 1)
    (assert-move-down (h/code "{3 4"
                              " 1 2}")
                      (h/code "{1 2"
                              " 3 4}") 1)
    (assert-move-down (h/code "{3 4"
                              " 1 2}")
                      (h/code "{1 2"
                              " 3 4}") 2)
    (assert-move-down (h/code "{3 4,"
                              " 1 2}")
                      (h/code "{1 2,"
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
    (testing "with destructuring"
      (assert-move-down (h/code "(let [[a b] [1 2]"
                                "      {:keys [c d]} {:c 1 :d 2}"
                                "      e 2])")
                        (h/code "(let [[a b] [1 2]"
                                "      e 2"
                                "      {:keys [c d]} {:c 1 :d 2}])")
                        'e)
      (assert-move-down (h/code "(let [[a b] [1 2]"
                                "      e 2"
                                "      {:keys [c d]} {:c 1 :d 2}])")
                        (h/code "(let [[a b] [1 2]"
                                "      {:keys [c d]} {:c 1 :d 2}"
                                "      e 2])") {:keys ['c 'd]}))
    (testing "blank lines"
      (is (= (h/code "{"
                     " :b 2"
                     ""
                     " :a 1}")
             (some-> (z/of-string (h/code "{"
                                          " :a 1"
                                          ""
                                          " :b 2}"))
                     ;; move cursor to blank line above a
                     (z/down*)
                     (f.move-coll-entry/move-down "file:///a.clj")
                     as-string)))
      (is (nil?
           (some-> (z/of-string (h/code "{:a 1"
                                        ""
                                        " :b 2}"))
                   ;; move cursor to blank line between a and b
                   (z/down)
                   (z/find-next-value z/right 1)
                   (z/right*)
                   (f.move-coll-entry/move-down "file:///a.clj")
                   as-string))))
    (testing "comments"
      (assert-move-down (h/code "{:b (+ 1 1) ;; two comment"
                                " :a 1 ;; one comment"
                                " :c 3} ;; three comment")
                        (h/code "{:a 1 ;; one comment"
                                " :b (+ 1 1) ;; two comment"
                                " :c 3} ;; three comment") :a)
      (assert-move-down (h/code ";; main comment"
                                "{;; b comment"
                                " :b (+ 1 1) ;; two comment"
                                " :a 1 ;; one comment"
                                " ;; c comment"
                                " :c 3} ;; three comment")
                        (h/code ";; main comment"
                                "{:a 1 ;; one comment"
                                " ;; b comment"
                                " :b (+ 1 1) ;; two comment"
                                " ;; c comment"
                                " :c 3} ;; three comment") :a)
      (assert-move-down (h/code "{:b 2"
                                " :a 1"
                                " ;; trailing comment"
                                " }")
                        (h/code "{:a 1"
                                " :b 2"
                                " ;; trailing comment"
                                " }") :a)
      (assert-move-down (h/code "{;; a"
                                " a 1"
                                ""
                                " ;; c"
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
                                " c 3}") 'b)
      (assert-move-down (h/code "{;; b"
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
                                " b 2"
                                ""
                                " ;; c"
                                " c 3}") 'a)
      ;; avoids commenting out closing bracket
      (assert-move-down (h/code "{:b 2"
                                " :a 1 ;; one comment"
                                " }")
                        (h/code "{:a 1 ;; one comment"
                                " :b 2}") :a)
      ;; moves from leading comment / whitespace
      (is (= (h/code "{;; b comment"
                     " :b 2 ;; two comment"
                     " ;; a comment"
                     " :a 1 ;; one comment"
                     " :c 3}")
             (some-> (z/of-string (h/code "{;; a comment"
                                          " :a 1 ;; one comment"
                                          " ;; b comment"
                                          " :b 2 ;; two comment"
                                          " :c 3}"))
                     ;; move cursor to <comment '; a comment'>
                     (z/down*)
                     (f.move-coll-entry/move-down "file:///a.clj")
                     as-string)))
      (is (= (h/code "{:b 2 ;; two comment"
                     " :a 1 ;; one comment"
                     " :c 3}")
             (some-> (z/of-string (h/code "{:a 1 ;; one comment"
                                          " :b 2 ;; two comment"
                                          " :c 3}"))
                     ;; move cursor to <comment '; one comment'>
                     (z/down)
                     (z/find z/right* (comp n/comment? z/node))
                     (f.move-coll-entry/move-down "file:///a.clj")
                     as-string))))
    (testing "relocation"
      (assert-move-down-position [2 2]
                                 (h/code "{:a 1 ;; one comment"
                                         " :b 2}") :a)
      ;; moves cursor to start of entry pair
      (assert-move-down-position [2 2]
                                 (h/code "{:a :x ;; one comment"
                                         " :b    :y}") :x))))
