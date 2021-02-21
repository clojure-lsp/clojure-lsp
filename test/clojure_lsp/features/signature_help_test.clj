(ns clojure-lsp.features.signature-help-test
  (:require
   [clojure-lsp.feature.signature-help :as f.signature-help]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest testing is]]))

(h/reset-db-after-test)

(deftest signature-help-unavailable
  (testing "insider defn"
    (let [[[row col]] (h/load-code-and-locs "(defn foo [a b]| (bar 1 2))")]
      (is (= nil (f.signature-help/signature-help "file:///a.clj" row col)))))
  (testing "inside let binding"
    (let [[[row col]] (h/load-code-and-locs "(defn foo [a b] (let [a 1]| (bar 1 2)))")]
      (is (= nil (f.signature-help/signature-help "file:///a.clj" row col)))))
  (testing "inside unknown function"
    (let [[[row col]] (h/load-code-and-locs "(defn foo [a b] (let [a 1] (bar |1 2)))")]
      (is (= nil (f.signature-help/signature-help "file:///a.clj" row col))))))

(deftest signature-help-cursor-position
  (let [[[before-r before-c]
         [after-r after-c]
         [first-arg-r first-arg-c]
         [second-arg-r second-arg-c]
         [outside-r outside-c]] (h/load-code-and-locs
                                 (h/code "(defn bar [a b] 1)"
                                         "(defn foo [a b] (|bar| |1 |2) |)"))]
    (testing "before function name"
      (is (= {:signatures [{:label "(bar [a b])"}]
              :active-signature 0}
             (f.signature-help/signature-help "file:///a.clj" before-r before-c))))
    (testing "after function name"
      (is (= {:signatures [{:label "(bar [a b])"}]
              :active-signature 0}
             (f.signature-help/signature-help "file:///a.clj" after-r after-c))))
    (testing "on first arg"
      (is (= {:signatures [{:label "(bar [a b])"}]
              :active-signature 0}
             (f.signature-help/signature-help "file:///a.clj" first-arg-r first-arg-c))))
    (testing "on second arg"
      (is (= {:signatures [{:label "(bar [a b])"}]
              :active-signature 0}
             (f.signature-help/signature-help "file:///a.clj" second-arg-r second-arg-c))))
    (testing "outside function"
      (is (= nil
             (f.signature-help/signature-help "file:///a.clj" outside-r outside-c))))))

(deftest signature-help-multiple-signatures
  (testing "With fixed arities"
    (let [[[zero-r zero-c]
           [one-r one-c]
           [two-r two-c]
           [three-r three-c]
           [four-r four-c]] (h/load-code-and-locs
                             (h/code "(defn bar ([a b] 1) ([a b c] 2))"
                                     "(bar|)"
                                     "(bar| 1)"
                                     "(bar| 1 2)"
                                     "(bar| 1 2 3)"
                                     "(bar| 1 2 3 4)"))]
      (testing "zero arity"
        (is (= {:signatures [{:label "(bar [a b])"}
                             {:label "(bar [a b c])"}]
                :active-signature 0}
               (f.signature-help/signature-help "file:///a.clj" zero-r zero-c))))
      (testing "one arity"
        (is (= {:signatures [{:label "(bar [a b])"}
                             {:label "(bar [a b c])"}]
                :active-signature 0}
               (f.signature-help/signature-help "file:///a.clj" one-r one-c))))
      (testing "two arity"
        (is (= {:signatures [{:label "(bar [a b])"}
                             {:label "(bar [a b c])"}]
                :active-signature 0}
               (f.signature-help/signature-help "file:///a.clj" two-r two-c))))
      (testing "three arity"
        (is (= {:signatures [{:label "(bar [a b])"}
                             {:label "(bar [a b c])"}]
                :active-signature 1}
               (f.signature-help/signature-help "file:///a.clj" three-r three-c))))
      (testing "four arity"
        (is (= {:signatures [{:label "(bar [a b])"}
                             {:label "(bar [a b c])"}]
                :active-signature 1}
               (f.signature-help/signature-help "file:///a.clj" four-r four-c))))))
  (testing "With & rest arity only"
    (let [[[zero-r zero-c]
           [one-r one-c]
           [two-r two-c]] (h/load-code-and-locs
                             (h/code "(defn bar [& rest] 2)"
                                     "(bar|)"
                                     "(bar| 1)"
                                     "(bar| 1 2)"))]
      (testing "zero arity"
        (is (= {:signatures [{:label "(bar [& rest])"}]
                :active-signature 0}
               (f.signature-help/signature-help "file:///a.clj" zero-r zero-c))))
      (testing "one arity"
        (is (= {:signatures [{:label "(bar [& rest])"}]
                :active-signature 0}
               (f.signature-help/signature-help "file:///a.clj" one-r one-c))))
      (testing "two arity"
        (is (= {:signatures [{:label "(bar [& rest])"}]
                :active-signature 0}
               (f.signature-help/signature-help "file:///a.clj" two-r two-c))))))
  (testing "With fixed arities and & rest arity"
    (let [[[zero-r zero-c]
           [one-r one-c]
           [two-r two-c]
           [three-r three-c]
           [four-r four-c]] (h/load-code-and-locs
                             (h/code "(defn bar ([a] 1) ([a & rest] 2))"
                                     "(bar|)"
                                     "(bar| 1)"
                                     "(bar| 1 2)"
                                     "(bar| 1 2 3)"
                                     "(bar| 1 2 3 4)"))]
      (testing "zero arity"
        (is (= {:signatures [{:label "(bar [a])"}
                             {:label "(bar [a & rest])"}]
                :active-signature 0}
               (f.signature-help/signature-help "file:///a.clj" zero-r zero-c))))
      (testing "one arity"
        (is (= {:signatures [{:label "(bar [a])"}
                             {:label "(bar [a & rest])"}]
                :active-signature 0}
               (f.signature-help/signature-help "file:///a.clj" one-r one-c))))
      (testing "two arity"
        (is (= {:signatures [{:label "(bar [a])"}
                             {:label "(bar [a & rest])"}]
                :active-signature 1}
               (f.signature-help/signature-help "file:///a.clj" two-r two-c))))
      (testing "three arity"
        (is (= {:signatures [{:label "(bar [a])"}
                             {:label "(bar [a & rest])"}]
                :active-signature 1}
               (f.signature-help/signature-help "file:///a.clj" three-r three-c))))
      (testing "four arity"
        (is (= {:signatures [{:label "(bar [a])"}
                             {:label "(bar [a & rest])"}]
                :active-signature 1}
               (f.signature-help/signature-help "file:///a.clj" four-r four-c)))))))
