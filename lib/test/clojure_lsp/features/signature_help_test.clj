(ns clojure-lsp.features.signature-help-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.signature-help :as f.signature-help]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(deftest signature-help-unavailable
  (testing "insider defn"
    (let [[[row col]] (h/load-code-and-locs "(defn foo [a b]| (bar 1 2))")]
      (is (= nil (f.signature-help/signature-help (h/file-uri "file:///a.clj") row col db/db*)))))
  (testing "inside let binding"
    (let [[[row col]] (h/load-code-and-locs "(defn foo [a b] (let [a 1]| (bar 1 2)))")]
      (is (= nil (f.signature-help/signature-help (h/file-uri "file:///a.clj") row col db/db*)))))
  (testing "inside unknown function"
    (let [[[row col]] (h/load-code-and-locs "(defn foo [a b] (let [a 1] (bar |1 2)))")]
      (is (= nil (f.signature-help/signature-help (h/file-uri "file:///a.clj") row col db/db*))))))

(deftest signature-help-cursor-position
  (let [[[before-r before-c]
         [after-r after-c]
         [first-arg-r first-arg-c]
         [second-arg-r second-arg-c]
         [outside-r outside-c]] (h/load-code-and-locs
                                  (h/code "(defn bar [a b] 1)"
                                          "(defn foo [a b] (|bar| |1 |2) |)"))]
    (testing "before function name"
      (is (= {:signatures [{:label "(bar [a b])"
                            :parameters [{:label "a"}
                                         {:label "b"}]}]
              :active-parameter 0
              :active-signature 0}
             (f.signature-help/signature-help (h/file-uri "file:///a.clj") before-r before-c db/db*))))
    (testing "after function name"
      (is (= {:signatures [{:label "(bar [a b])"
                            :parameters [{:label "a"}
                                         {:label "b"}]}]
              :active-parameter 0
              :active-signature 0}
             (f.signature-help/signature-help (h/file-uri "file:///a.clj") after-r after-c db/db*))))
    (testing "on first arg"
      (is (= {:signatures [{:label "(bar [a b])"
                            :parameters [{:label "a"}
                                         {:label "b"}]}]
              :active-parameter 0
              :active-signature 0}
             (f.signature-help/signature-help (h/file-uri "file:///a.clj") first-arg-r first-arg-c db/db*))))
    (testing "on second arg"
      (is (= {:signatures [{:label "(bar [a b])"
                            :parameters [{:label "a"}
                                         {:label "b"}]}]
              :active-parameter 1
              :active-signature 0}
             (f.signature-help/signature-help (h/file-uri "file:///a.clj") second-arg-r second-arg-c db/db*))))
    (testing "outside function"
      (is (= nil
             (f.signature-help/signature-help (h/file-uri "file:///a.clj") outside-r outside-c db/db*))))))

(deftest signature-help-multiple-definitions
  (let [[[after-r after-c]] (h/load-code-and-locs
                              (h/code "(def bar)"
                                      "(defn bar [a b] 1)"
                                      "(defn foo [a b] (bar| 1 2))"))]
    (testing "after function name"
      (is (= {:signatures [{:label "(bar [a b])"
                            :parameters [{:label "a"}
                                         {:label "b"}]}]
              :active-parameter 0
              :active-signature 0}
             (f.signature-help/signature-help (h/file-uri "file:///a.clj") after-r after-c db/db*))))))

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
        (is (= {:signatures [{:label "(bar [a b])"
                              :parameters [{:label "a"}
                                           {:label "b"}]}
                             {:label "(bar [a b c])"
                              :parameters [{:label "a"}
                                           {:label "b"}
                                           {:label "c"}]}]
                :active-parameter 0
                :active-signature 0}
               (f.signature-help/signature-help (h/file-uri "file:///a.clj") zero-r zero-c db/db*))))
      (testing "one arity"
        (is (= {:signatures [{:label "(bar [a b])"
                              :parameters [{:label "a"}
                                           {:label "b"}]}
                             {:label "(bar [a b c])"
                              :parameters [{:label "a"}
                                           {:label "b"}
                                           {:label "c"}]}]
                :active-parameter 0
                :active-signature 0}
               (f.signature-help/signature-help (h/file-uri "file:///a.clj") one-r one-c db/db*))))
      (testing "two arity"
        (is (= {:signatures [{:label "(bar [a b])"
                              :parameters [{:label "a"}
                                           {:label "b"}]}
                             {:label "(bar [a b c])"
                              :parameters [{:label "a"}
                                           {:label "b"}
                                           {:label "c"}]}]
                :active-parameter 0
                :active-signature 0}
               (f.signature-help/signature-help (h/file-uri "file:///a.clj") two-r two-c db/db*))))
      (testing "three arity"
        (is (= {:signatures [{:label "(bar [a b])"
                              :parameters [{:label "a"}
                                           {:label "b"}]}
                             {:label "(bar [a b c])"
                              :parameters [{:label "a"}
                                           {:label "b"}
                                           {:label "c"}]}]
                :active-parameter 0
                :active-signature 1}
               (f.signature-help/signature-help (h/file-uri "file:///a.clj") three-r three-c db/db*))))
      (testing "four arity"
        (is (= {:signatures [{:label "(bar [a b])"
                              :parameters [{:label "a"}
                                           {:label "b"}]}
                             {:label "(bar [a b c])"
                              :parameters [{:label "a"}
                                           {:label "b"}
                                           {:label "c"}]}]
                :active-parameter 0
                :active-signature 1}
               (f.signature-help/signature-help (h/file-uri "file:///a.clj") four-r four-c db/db*))))))
  (testing "With & rest arity only"
    (let [[[zero-r zero-c]
           [one-r one-c]
           [two-r two-c]] (h/load-code-and-locs
                            (h/code "(defn bar [& rest] 2)"
                                    "(bar|)"
                                    "(bar| 1)"
                                    "(bar| 1 2)"))]
      (testing "zero arity"
        (is (= {:signatures [{:label "(bar [& rest])"
                              :parameters [{:label "& rest"}]}]
                :active-parameter 0
                :active-signature 0}
               (f.signature-help/signature-help (h/file-uri "file:///a.clj") zero-r zero-c db/db*))))
      (testing "one arity"
        (is (= {:signatures [{:label "(bar [& rest])"
                              :parameters [{:label "& rest"}]}]
                :active-parameter 0
                :active-signature 0}
               (f.signature-help/signature-help (h/file-uri "file:///a.clj") one-r one-c db/db*))))
      (testing "two arity"
        (is (= {:signatures [{:label "(bar [& rest])"
                              :parameters [{:label "& rest"}]}]
                :active-parameter 0
                :active-signature 0}
               (f.signature-help/signature-help (h/file-uri "file:///a.clj") two-r two-c db/db*))))))
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
        (is (= {:signatures [{:label "(bar [a])"
                              :parameters [{:label "a"}]}
                             {:label "(bar [a & rest])"
                              :parameters [{:label "a"}
                                           {:label "& rest"}]}]
                :active-parameter 0
                :active-signature 0}
               (f.signature-help/signature-help (h/file-uri "file:///a.clj") zero-r zero-c db/db*))))
      (testing "one arity"
        (is (= {:signatures [{:label "(bar [a])"
                              :parameters [{:label "a"}]}
                             {:label "(bar [a & rest])"
                              :parameters [{:label "a"}
                                           {:label "& rest"}]}]
                :active-parameter 0
                :active-signature 0}
               (f.signature-help/signature-help (h/file-uri "file:///a.clj") one-r one-c db/db*))))
      (testing "two arity"
        (is (= {:signatures [{:label "(bar [a])"
                              :parameters [{:label "a"}]}
                             {:label "(bar [a & rest])"
                              :parameters [{:label "a"}
                                           {:label "& rest"}]}]
                :active-parameter 0
                :active-signature 1}
               (f.signature-help/signature-help (h/file-uri "file:///a.clj") two-r two-c db/db*))))
      (testing "three arity"
        (is (= {:signatures [{:label "(bar [a])"
                              :parameters [{:label "a"}]}
                             {:label "(bar [a & rest])"
                              :parameters [{:label "a"}
                                           {:label "& rest"}]}]
                :active-parameter 0
                :active-signature 1}
               (f.signature-help/signature-help (h/file-uri "file:///a.clj") three-r three-c db/db*))))
      (testing "four arity"
        (is (= {:signatures [{:label "(bar [a])"
                              :parameters [{:label "a"}]}
                             {:label "(bar [a & rest])"
                              :parameters [{:label "a"}
                                           {:label "& rest"}]}]
                :active-parameter 0
                :active-signature 1}
               (f.signature-help/signature-help (h/file-uri "file:///a.clj") four-r four-c db/db*)))))))

(deftest signature-help-active-parameter
  (let [[[first-arg-r first-arg-c]
         [second-arg-r second-arg-c]
         [third-arg-r third-arg-c]
         [end-function-r end-function-c]] (h/load-code-and-locs
                                            (h/code "(defn bar [a & more] 1)"
                                                    "(defn foo [a b] (bar |1 |2 {:a| 2} |))"))]
    (testing "on first arg"
      (is (= {:signatures [{:label "(bar [a & more])"
                            :parameters [{:label "a"}
                                         {:label "& more"}]}]
              :active-parameter 0
              :active-signature 0}
             (f.signature-help/signature-help (h/file-uri "file:///a.clj") first-arg-r first-arg-c db/db*))))
    (testing "on second arg"
      (is (= {:signatures [{:label "(bar [a & more])"
                            :parameters [{:label "a"}
                                         {:label "& more"}]}]
              :active-parameter 1
              :active-signature 0}
             (f.signature-help/signature-help (h/file-uri "file:///a.clj") second-arg-r second-arg-c db/db*))))
    (testing "on third arg"
      (is (= {:signatures [{:label "(bar [a & more])"
                            :parameters [{:label "a"}
                                         {:label "& more"}]}]
              :active-parameter 1
              :active-signature 0}
             (f.signature-help/signature-help (h/file-uri "file:///a.clj") third-arg-r third-arg-c db/db*))))
    (testing "on end of the function"
      (is (= {:signatures [{:label "(bar [a & more])"
                            :parameters [{:label "a"}
                                         {:label "& more"}]}]
              :active-parameter 1
              :active-signature 0}
             (f.signature-help/signature-help (h/file-uri "file:///a.clj") end-function-r end-function-c db/db*))))))
