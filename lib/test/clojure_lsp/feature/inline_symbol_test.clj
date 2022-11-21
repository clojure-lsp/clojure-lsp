(ns clojure-lsp.feature.inline-symbol-test
  (:require
   [clojure-lsp.feature.inline-symbol :as f.inline-symbol]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(defn inline-symbol [source-file source-text & others]
  (let [[[source-r source-c]] (h/load-code-and-locs source-text source-file)]
    (doseq [[other-file other-text] (partition 2 others)]
      (h/load-code other-text other-file))
    (let [db (h/db)
          results (f.inline-symbol/inline-symbol source-file source-r source-c db)]
      (reduce-kv (fn [result uri changes]
                   (assoc result uri
                          (h/changes->code changes uri db)))
                 {}
                 (:changes-by-uri results)))))

(deftest inline-first-def-in-file
  ;; TODO remove the extra line?
  (is (= {(h/file-uri "file:///a.clj") (h/code ""
                                               "(def another 5)"
                                               "(* 2 60)")}
         (inline-symbol (h/file-uri "file:///a.clj")
                        (h/code "(def |something (* 2 60))"
                                "(def another 5)"
                                "something")))))

(deftest inline-last-def-in-file
  (is (= {(h/file-uri "file:///a.clj") (h/code "(def another 5)"
                                               "(* 2 60)")}
         (inline-symbol (h/file-uri "file:///a.clj")
                        (h/code "(def another 5)"
                                "(def |something (* 2 60))"
                                "something")))))

(deftest inline-def-with-usage-in-another-file
  (is (= {(h/file-uri "file:///a.clj") "(ns a)"
          (h/file-uri "file:///b.clj") "(ns b (:require a)) (inc (* 2 60))"}
         (inline-symbol (h/file-uri "file:///a.clj")
                        "(ns a) (def |something (* 2 60))"
                        (h/file-uri "file:///b.clj")
                        "(ns b (:require a)) (inc a/something)"))))

(deftest inline-def-with-docstring
  (is (= {(h/file-uri "file:///a.clj") (h/code ""
                                               "1")}
         (inline-symbol (h/file-uri "file:///a.clj")
                        (h/code "(def |something \"a thing\" 1)"
                                "something")))))

(deftest inline-in-invalid-location
  (is (= {}
         (inline-symbol (h/file-uri "file:///a.clj") "|;; comment"))))

(deftest inline-one-binding-let
  (testing "no wrapper"
    ;; one child
    (is (= {(h/file-uri "file:///a.clj") "(inc 1)"}
           (inline-symbol (h/file-uri "file:///a.clj") "(let [|something 1] (inc something))")))
    ;; two children
    ;; TODO: improve spacing?
    (is (= {(h/file-uri "file:///a.clj") (h/code "(do (dec 1)"
                                                 "  (inc 1))")}
           (inline-symbol (h/file-uri "file:///a.clj") (h/code "(let [|something 1]"
                                                               "  (dec something)"
                                                               "  (inc something))")))))
  (testing "wrapper without implicit do"
    ;; one child
    (is (= {(h/file-uri "file:///a.clj") "(boolean (inc 1))"}
           (inline-symbol (h/file-uri "file:///a.clj") "(boolean (let [|something 1] (inc something)))")))
    ;; two children
    (is (= {(h/file-uri "file:///a.clj") "(boolean (do (dec 1) (inc 1)))"}
           (inline-symbol (h/file-uri "file:///a.clj") "(boolean (let [|something 1] (dec something) (inc something)))"))))
  (testing "wrapper with implicit do"
    ;; one child
    (is (= {(h/file-uri "file:///a.clj") "(try (inc 1))"}
           (inline-symbol (h/file-uri "file:///a.clj") "(try (let [|something 1] (inc something)))")))
    ;; two children
    (is (= {(h/file-uri "file:///a.clj") "(try (dec 1) (inc 1))"}
           (inline-symbol (h/file-uri "file:///a.clj") "(try (let [|something 1] (dec something) (inc something)))")))))

(deftest inline-two-binding-let
  ;; TODO: remove the extra space?
  (is (= {(h/file-uri "file:///a.clj") "(let [ other 2] 1 other 1)"}
         (inline-symbol (h/file-uri "file:///a.clj") "(let [|something 1 other 2] something other something)")))
  (is (= {(h/file-uri "file:///a.clj") "(let [something 1] something 2 something)"}
         (inline-symbol (h/file-uri "file:///a.clj") "(let [something 1 |other 2] something other something)"))))
