(ns clojure-lsp.feature.move-coll-entry-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.move-coll-entry :as f.move-coll-entry]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(h/reset-db-after-test)

(def ^:private uri (h/file-uri "file:///a.cljc"))

(defn zloc-at [row col]
  (-> @db/db
      (get-in [:documents uri])
      :text
      (parser/loc-at-pos row col)))

(defn load-code-and-zloc [code]
  (let [[[row col]] (h/load-code-and-locs code uri)]
    (zloc-at row col)))

(defn can-move-code-up? [code]
  (let [zloc (load-code-and-zloc code)]
    (f.move-coll-entry/can-move-entry-up? zloc uri @db/db)))

(defn can-move-code-down? [code]
  (let [zloc (load-code-and-zloc code)]
    (f.move-coll-entry/can-move-entry-down? zloc uri @db/db)))

(deftest can-move-entry-up?
  (testing "common cases"
    (is (not (can-move-code-up? "|[]")))
    (is (not (can-move-code-up? "[|1]")))
    (is (not (can-move-code-up? "{1 |2 3}")))
    (is (not (can-move-code-up? "|{:a :b :c :d}")))
    (is (not (can-move-code-up? "|#{:a :b :c :d}")))
    (is (not (can-move-code-up? "|[:a :b :c :d]")))
    (is (not (can-move-code-up? "|'(:a :b :c :d)")))
    (is (not (can-move-code-up? "{|1 2}")))
    (is (not (can-move-code-up? "{1 |2}")))
    (is (can-move-code-up? "#{1 |2 3}"))
    (is (can-move-code-up? "[1 |2 3]"))
    (is (can-move-code-up? "'(1 |2 3)"))
    (is (can-move-code-up? "{1 2 |3 4}"))
    (is (can-move-code-up? "(let [a 1 |c 2])"))
    (is (can-move-code-up? "(binding [a 1 |c 2])")))
  (testing "comments"
    (is (can-move-code-up? (h/code "{1 2 ;; some comment"
                                   " |3 4}")))
    (is (can-move-code-up? (h/code "{1 2"
                                   " ;; some comment"
                                   " |3 4}"))))
  (testing "with destructuring"
    (is (can-move-code-up? (h/code "(let [[a b] [1 2]"
                                   "      |{:keys [c d]} {:c 1 :d 2}"
                                   "      e 2])"))))
  (testing "when on first entry"
    (is (not (can-move-code-up? "#{|1 2 3}")))
    (is (not (can-move-code-up? "[|1 2 3]")))
    (is (not (can-move-code-up? "'(|1 2 3)")))
    (is (not (can-move-code-up? "{|:a :b :c :d}")))
    (is (not (can-move-code-up? "{:a |:b :c :d}")))
    (is (not (can-move-code-up? "(let [|a 1 c 2])")))
    (is (not (can-move-code-up? "(let [a |1 c 2])")))))

(deftest can-move-entry-down?
  (testing "common cases"
    (is (not (can-move-code-down? "|[]")))
    (is (not (can-move-code-down? "[|1]")))
    (is (not (can-move-code-down? "{1 |2 3}")))
    (is (not (can-move-code-down? "|{:a :b :c :d}")))
    (is (not (can-move-code-down? "|#{:a :b :c :d}")))
    (is (not (can-move-code-down? "|[:a :b :c :d]")))
    (is (not (can-move-code-down? "|'(:a :b :c :d)")))
    (is (not (can-move-code-down? "{|1 2}")))
    (is (can-move-code-down? "#{1 |2 3}"))
    (is (can-move-code-down? "[1 |2 3]"))
    (is (can-move-code-down? "'(1 |2 3)"))
    (is (can-move-code-down? "{|1 2 3 4}"))
    (is (can-move-code-down? "(let [|a 1 c 2])"))
    (is (can-move-code-down? "(binding [|a 1 c 2])")))
  (testing "comments"
    (is (can-move-code-down? (h/code "{1 2 ;; some comment"
                                     " |3 4 ;; other coment"
                                     " 5 6}")))
    (is (can-move-code-down? (h/code "{1 2"
                                     " ;; some comment"
                                     " |3 4"
                                     " 5 6}"))))
  (testing "with destructuring"
    (is (can-move-code-down? (h/code "(let [[a b] [1 2]"
                                     "      |{:keys [c d]} {:c 1 :d 2}"
                                     "      e 2])"))))
  (testing "when on last entry"
    (is (not (can-move-code-down? "#{1 2 |3}")))
    (is (not (can-move-code-down? "[1 2 |3]")))
    (is (not (can-move-code-down? "'(1 2 |3)")))
    (is (not (can-move-code-down? "{:a :b |:c :d}")))
    (is (not (can-move-code-down? "{:a :b :c |:d}")))
    (is (not (can-move-code-down? "(let [a 1 |c 2])")))
    (is (not (can-move-code-down? "(let [a 1 c |2])")))))

(defn move-zloc-up [zloc]
  (f.move-coll-entry/move-up zloc uri @db/db))

(defn move-zloc-down [zloc]
  (f.move-coll-entry/move-down zloc uri @db/db))

(defn move-code-up [code]
  (move-zloc-up (load-code-and-zloc code)))

(defn move-code-down [code]
  (move-zloc-down (load-code-and-zloc code)))

(defn- as-string [change]
  (some-> change
          :changes-by-uri
          (get uri)
          first
          :loc
          z/root-string))

(defn- as-position [change]
  (when-let [{:keys [row col]} (some-> change
                                       :show-document-after-edit
                                       :range)]
    [row col]))

;; These are macros so test failures have the right line numbers
(defmacro assert-move-up [expected code]
  `(is (= ~expected (as-string (move-code-up ~code)))))
(defmacro assert-move-down [expected code]
  `(is (= ~expected (as-string (move-code-down ~code)))))
(defmacro assert-move-up-position [expected code]
  `(is (= ~expected (as-position (move-code-up ~code)))))
(defmacro assert-move-down-position [expected code]
  `(is (= ~expected (as-position (move-code-down ~code)))))

(deftest move-up
  (testing "common cases"
    (assert-move-up (h/code "{3 4 1 2}")
                    (h/code "{1 2 |3 4}"))
    (assert-move-up (h/code "{3 4, 1 2}")
                    (h/code "{1 2, |3 4}"))
    (assert-move-up (h/code "{3 4"
                            " 1 2}")
                    (h/code "{1 2"
                            " |3 4}"))
    (assert-move-up (h/code "{3 4"
                            " 1 2}")
                    (h/code "{1 2"
                            " 3 |4}"))
    (assert-move-up (h/code "{3 4,"
                            " 1 2}")
                    (h/code "{1 2,"
                            " |3 4}"))
    (assert-move-up (h/code "{:b (+ 1 1)"
                            " :a 1"
                            " :c 3}")
                    (h/code "{:a 1"
                            " |:b (+ 1 1)"
                            " :c 3}"))
    (assert-move-up (h/code "[2 1]")
                    (h/code "[1 |2]"))
    (assert-move-up (h/code "[1 3 2]")
                    (h/code "[1 2 |3]"))
    (assert-move-up (h/code "(def a 1)"
                            "[2 a]")
                    (h/code "(def a 1)"
                            "[a |2]"))
    (assert-move-up (h/code "(let [a [1 2]"
                            "      c 3"
                            "      b 2])")
                    (h/code "(let [a [1 2]"
                            "      b 2"
                            "      |c 3])"))
    (assert-move-up (h/code "(binding [b 2"
                            "          a 1]"
                            "  (+ a b))")
                    (h/code "(binding [a 1"
                            "          |b 2]"
                            "  (+ a b))"))
    (assert-move-up (h/code "(for [b [3 4]"
                            "      a [1 2]"
                            "      :let [c (inc a)]]"
                            "  (+ a b c))")
                    (h/code "(for [a [1 2]"
                            "      |b [3 4]"
                            "      :let [c (inc a)]]"
                            "  (+ a b c))"))
    (assert-move-up (h/code "(for [a [1 2]"
                            "      :let [c (inc b)"
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
                            "(foo [b 2"
                            "      a 1]"
                            "  (inc a))")
                    (h/code "(ns foo"
                            "  {:clj-kondo/config '{:lint-as {foo/foo clojure.core/let}}})"
                            "(defmacro foo [bs & form]"
                            "  `(let [~@bs]"
                            "     ~@form))"
                            "(foo [a 1"
                            "      |b 2]"
                            "  (inc a))")))
  (testing "with destructuring"
    (assert-move-up (h/code "(let [[a b] [1 2]"
                            "      e 2"
                            "      {:keys [c d]} {:c 1 :d 2}])")
                    (h/code "(let [[a b] [1 2]"
                            "      {:keys [c d]} {:c 1 :d 2}"
                            "      |e 2])"))
    (assert-move-up (h/code "(let [{:keys [c d]} {:c 1 :d 2}"
                            "      [a b] [1 2]"
                            "      e 2])")
                    (h/code "(let [[a b] [1 2]"
                            "      |{:keys [c d]} {:c 1 :d 2}"
                            "      e 2])"))
    (assert-move-up (h/code "(for [a [1 2]"
                            "      :let [{:keys [c]} {:c 1}"
                            "            {:keys [b]} {:b 1}]])")
                    (h/code "(for [a [1 2]"
                            "      :let [{:keys [b]} {:b 1}"
                            "            |{:keys [c]} {:c 1}]])")))
  (testing "blank lines"
    (is (= (h/code "{:b 2"
                   ""
                   " :a 1}")
           (-> (h/code "{:a 1"
                       "|"
                       " :b 2}")
               load-code-and-zloc
               ;; load-code moves cursor in whitespace to outer form;
               ;; move cursor to blank line between a and b
               (z/down)
               (z/find-next-value z/right 1)
               (z/right*)
               move-zloc-up
               as-string)))
    (is (nil? (-> (h/code "{:a 1"
                          ""
                          " :b 2"
                          "|"
                          "}")
                  load-code-and-zloc
                  ;; load-code moves cursor in whitespace to outer form;
                  ;; move cursor to blank line after b
                  (z/down)
                  (z/find-next-value z/right 2)
                  z/right*
                  move-zloc-up))))
  (testing "comments"
    (assert-move-up (h/code "{:b (+ 1 1) ;; two comment"
                            " :a 1 ;; one comment"
                            " :c 3} ;; three comment")
                    (h/code "{:a 1 ;; one comment"
                            " |:b (+ 1 1) ;; two comment"
                            " :c 3} ;; three comment"))
    (assert-move-up (h/code ";; main comment"
                            "{;; b comment"
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
    (assert-move-up (h/code "{:b 2"
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
                            " |c 3}"))
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
                            " |b 2"
                            ""
                            " ;; c"
                            " c 3}"))
    ;; avoids commenting out closing bracket
    (assert-move-up (h/code "[b"
                            " a ;; a comment"
                            " ]")
                    (h/code "[a ;; a comment"
                            " |b]"))
    (assert-move-up (h/code "{:b 2"
                            " :a 1 ;; one comment"
                            " }")
                    (h/code "{:a 1 ;; one comment"
                            " |:b 2}"))
    ;; moves from leading comment / whitespace
    (is (= (h/code "{;; b comment"
                   " :b 2 ;; two comment"
                   " :a 1 ;; one comment"
                   " :c 3}")
           (-> (h/code "{:a 1 ;; one comment"
                       " ;; |b comment"
                       " :b 2 ;; two comment"
                       " :c 3}")
               load-code-and-zloc
               ;; load-code moves cursor in whitespace to outer form;
               ;; move cursor to <comment '; b comment'>
               (z/down)
               (z/find-next-value z/right :b)
               (z/find z/left* (comp n/comment? z/node))
               move-zloc-up
               as-string)))
    ;; moves from trailing comment / whitespace
    (is (= (h/code "{:b 2 ;; two comment"
                   " :a 1 ;; one comment"
                   " :c 3}")
           (-> (h/code "{:a 1 ;; one comment"
                       " :b 2 ;; |two comment"
                       " :c 3}")
               load-code-and-zloc
               ;; load-code moves cursor in whitespace to outer form;
               ;; move cursor to <comment '; two comment'>
               (z/down)
               (z/find-next-value z/right :b)
               (z/find z/right* (comp n/comment? z/node))
               move-zloc-up
               as-string))))
  (testing "relocation"
    (assert-move-up-position [1 2]
                             (h/code "{:a 1 ;; one comment"
                                     " |:b 2}"))
    ;; moves cursor to start of entry pair
    (assert-move-up-position [1 2]
                             (h/code "{:a :x ;; one comment"
                                     " :b    |:y}"))))

(deftest move-down
  (testing "common cases"
    (assert-move-down (h/code "{3 4 1 2}")
                      (h/code "{|1 2 3 4}"))
    (assert-move-down (h/code "{3 4, 1 2}")
                      (h/code "{|1 2, 3 4}"))
    (assert-move-down (h/code "{3 4"
                              " 1 2}")
                      (h/code "{|1 2"
                              " 3 4}"))
    (assert-move-down (h/code "{3 4"
                              " 1 2}")
                      (h/code "{1 |2"
                              " 3 4}"))
    (assert-move-down (h/code "{3 4,"
                              " 1 2}")
                      (h/code "{|1 2,"
                              " 3 4}"))
    (assert-move-down (h/code "{:b (+ 1 1)"
                              " :a 1"
                              " :c 3}")
                      (h/code "{|:a 1"
                              " :b (+ 1 1)"
                              " :c 3}"))
    (assert-move-down (h/code "[2 1]")
                      (h/code "[|1 2]"))
    (assert-move-down (h/code "[2 1 3]")
                      (h/code "[|1 2 3]"))
    (assert-move-down (h/code "(def a 1)"
                              "[2 a]")
                      (h/code "(def a 1)"
                              "[|a 2]"))
    (assert-move-down (h/code "(let [a [1 2]"
                              "      c 3"
                              "      b 2])")
                      (h/code "(let [a [1 2]"
                              "      |b 2"
                              "      c 3])"))
    (assert-move-down (h/code "(binding [b 2"
                              "          a 1]"
                              "  (+ a b))")
                      (h/code "(binding [|a 1"
                              "          b 2]"
                              "  (+ a b))"))
    (assert-move-down (h/code "(for [b [3 4]"
                              "      a [1 2]"
                              "      :let [c (inc a)]]"
                              "  (+ a b c))")
                      (h/code "(for [|a [1 2]"
                              "      b [3 4]"
                              "      :let [c (inc a)]]"
                              "  (+ a b c))"))
    (assert-move-down (h/code "(for [a [1 2]"
                              "      :let [c (inc b)"
                              "            b (inc a)]]"
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
                              "      a 1]"
                              "  (inc a))")
                      (h/code "(ns foo"
                              "  {:clj-kondo/config '{:lint-as {foo/foo clojure.core/let}}})"
                              "(defmacro foo [bs & form]"
                              "  `(let [~@bs]"
                              "     ~@form))"
                              "(foo [|a 1"
                              "      b 2]"
                              "  (inc a))")))
  (testing "with destructuring"
    (assert-move-down (h/code "(let [[a b] [1 2]"
                              "      {:keys [c d]} {:c 1 :d 2}"
                              "      e 2])")
                      (h/code "(let [[a b] [1 2]"
                              "      |e 2"
                              "      {:keys [c d]} {:c 1 :d 2}])"))
    (assert-move-down (h/code "(let [[a b] [1 2]"
                              "      e 2"
                              "      {:keys [c d]} {:c 1 :d 2}])")
                      (h/code "(let [[a b] [1 2]"
                              "      |{:keys [c d]} {:c 1 :d 2}"
                              "      e 2])"))
    (assert-move-down (h/code "(for [a [1 2]"
                              "      :let [{:keys [c]} {:c 1}"
                              "            {:keys [b]} {:b 1}]])")
                      (h/code "(for [a [1 2]"
                              "      :let [|{:keys [b]} {:b 1}"
                              "            {:keys [c]} {:c 1}]])")))
  (testing "blank lines"
    (is (= (h/code "{"
                   " :b 2"
                   ""
                   " :a 1}")
           (-> (h/code "{|"
                       " :a 1"
                       ""
                       " :b 2}")
               load-code-and-zloc
               ;; load-code moves cursor in whitespace to outer form;
               ;; move cursor to blank line above a
               (z/down*)
               move-zloc-down
               as-string)))
    (is (nil? (-> (h/code "{:a 1"
                          "|"
                          " :b 2}")
                  load-code-and-zloc
                  ;; load-code moves cursor in whitespace to outer form;
                  ;; move cursor to blank line between a and b
                  z/down
                  z/right
                  z/right*
                  z/right*
                  move-zloc-down))))
  (testing "comments"
    (assert-move-down (h/code "{:b (+ 1 1) ;; two comment"
                              " :a 1 ;; one comment"
                              " :c 3} ;; three comment")
                      (h/code "{|:a 1 ;; one comment"
                              " :b (+ 1 1) ;; two comment"
                              " :c 3} ;; three comment"))
    (assert-move-down (h/code ";; main comment"
                              "{;; b comment"
                              " :b (+ 1 1) ;; two comment"
                              " :a 1 ;; one comment"
                              " ;; c comment"
                              " :c 3} ;; three comment")
                      (h/code ";; main comment"
                              "{|:a 1 ;; one comment"
                              " ;; b comment"
                              " :b (+ 1 1) ;; two comment"
                              " ;; c comment"
                              " :c 3} ;; three comment"))
    (assert-move-down (h/code "{:b 2"
                              " :a 1"
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
                              " ;; b"
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
                              " ;; a"
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
                              " a ;; a comment"
                              " ]")
                      (h/code "[|a ;; a comment"
                              " b]"))
    (assert-move-down (h/code "{:b 2"
                              " :a 1 ;; one comment"
                              " }")
                      (h/code "{|:a 1 ;; one comment"
                              " :b 2}"))
    ;; moves from leading comment / whitespace
    (is (= (h/code "{;; b comment"
                   " :b 2 ;; two comment"
                   " ;; a comment"
                   " :a 1 ;; one comment"
                   " :c 3}")
           (-> (h/code "{;; |a comment"
                       " :a 1 ;; one comment"
                       " ;; b comment"
                       " :b 2 ;; two comment"
                       " :c 3}")
               load-code-and-zloc
               ;; load-code moves cursor in whitespace to outer form;
               ;; move cursor to <comment '; a comment'>
               (z/down*)
               move-zloc-down
               as-string)))
    ;; moves from trailing comment / whitespace
    (is (= (h/code "{:b 2 ;; two comment"
                   " :a 1 ;; one comment"
                   " :c 3}")
           (-> (h/code "{:a 1 ;; |one comment"
                       " :b 2 ;; two comment"
                       " :c 3}")
               load-code-and-zloc
               ;; load-code moves cursor in whitespace to outer form;
               ;; move cursor to <comment '; one comment'>
               (z/down)
               (z/find z/right* (comp n/comment? z/node))
               move-zloc-down
               as-string))))
  (testing "relocation"
    (assert-move-down-position [2 2]
                               (h/code "{|:a 1 ;; one comment"
                                       " :b 2}"))
    ;; moves cursor to start of entry pair
    (assert-move-down-position [2 2]
                               (h/code "{:a    |:x ;; one comment"
                                       " :b :y}"))))
