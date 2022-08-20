(ns clojure-lsp.feature.move-form-test
  (:require
   [clojure-lsp.feature.move-form :as move-form]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(deftest move-form-test
  (testing "simple"
    (let [components (h/make-components)
          a-uri (h/file-uri "file:///a.clj")
          b-uri (h/file-uri "file:///b.clj")
          zloc (h/load-code-and-zloc (h/code "(ns apple)" "|(def bar inc)" "(bar 1)") a-uri components)
          _ (h/load-code "(ns bread)" b-uri components)
          results (:changes-by-uri (move-form/move-form zloc a-uri components "/b.clj"))
          db (h/db components)
          a-results (h/changes-by-uri->code results a-uri db)
          b-results (h/changes-by-uri->code results b-uri db)]
      (is (= (h/code "(ns apple "
                     "  (:require"
                     "   bread))"
                     "(bread/bar 1)") a-results))
      (is (= (h/code "(ns bread)"
                     ""
                     "(def bar inc)") b-results))))
  (testing "complex"
    (let [components (h/make-components)
          a-uri (h/file-uri "file:///a.clj")
          b-uri (h/file-uri "file:///b.clj")
          c-uri (h/file-uri "file:///c.clj")
          d-uri (h/file-uri "file:///d.clj")
          e-uri (h/file-uri "file:///e.clj")
          f-uri (h/file-uri "file:///f.clj")
          zloc (h/load-code-and-zloc (h/code "(ns apple (:require [bread :as b]))"
                                             ""
                                             "(def qux 1)"
                                             ""
                                             "|(def bar b/foo)"
                                             ""
                                             "(bar 1)") a-uri components)
          _ (h/load-code (h/code "(ns bread)"
                                          "(def foo inc)") b-uri components)
          _ (h/load-code (h/code "(ns crumb (:require [apple :as a :refer [bar]]))"
                                          "(bar 1)"
                                          "(apple/bar 2)"
                                          "(a/bar 3)")
                                  c-uri components)
          _ (h/load-code (h/code "(ns diner (:require [apple :as a :refer [bar]]))"
                                          "(a/qux 1)"
                                          "(apple/foo 1)"
                                          "(bar 2)") d-uri components)
          _ (h/load-code (h/code "(ns eater (:require [apple :as a] [crumb :as c]))"
                                          "(a/bar 2)"
                                          "(c/c 3)") e-uri components)
          _ (h/load-code (h/code "(ns fruit (:require [apple :as a] [bread :as b]))"
                                          "(a/bar 2)"
                                          "(b/foo 3)") f-uri components)
          results (:changes-by-uri (move-form/move-form zloc a-uri components "/b.clj"))
          db (h/db components)
          a-results (h/changes-by-uri->code results a-uri db)
          b-results (h/changes-by-uri->code results b-uri db)
          c-results (h/changes-by-uri->code results c-uri db)
          d-results (h/changes-by-uri->code results d-uri db)
          e-results (h/changes-by-uri->code results e-uri db)
          f-results (h/changes-by-uri->code results f-uri db)]
      (is (= (h/code "(ns apple (:require [bread :as b]))"
                     ""
                     "(def qux 1)"
                     ""
                     "(b/bar 1)")
             a-results))
      (is (= (h/code "(ns bread)"
                     "(def foo inc)"
                     ""
                     "(def bar foo)")
             b-results))
      (is (= (h/code "(ns crumb (:require"
                     "           [bread :as b :refer [bar]]))"
                     "(bar 1)"
                     "(b/bar 2)"
                     "(b/bar 3)")
             c-results))
      (is (= (h/code "(ns diner (:require"
                     "           [apple :as a]"
                     "           [bread :as b :refer [bar]]))"
                     "(a/qux 1)"
                     "(apple/foo 1)"
                     "(bar 2)")
             d-results))
      (is (= (h/code "(ns eater (:require"
                     "           [bread :as b]"
                     "           [crumb :as c]))"
                     "(b/bar 2)"
                     "(c/c 3)")
             e-results))
      (is (= (h/code "(ns fruit (:require"
                     "           [bread :as b]))"
                     "(b/bar 2)"
                     "(b/foo 3)")
             f-results)))))
