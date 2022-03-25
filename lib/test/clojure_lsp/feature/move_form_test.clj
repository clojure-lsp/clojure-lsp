(ns clojure-lsp.feature.move-form-test
  (:require
    [clojure.test :refer [deftest testing is]]
    [clojure-lsp.test-helper :as h]
    [clojure-lsp.feature.move-form :as move-form]
    [clojure-lsp.db :as db]))

(deftest move-form-test
  (testing "simple"
    (h/clean-db!)
    (let [a-uri (h/file-uri "file:///a.clj")
          b-uri (h/file-uri "file:///b.clj")
          zloc (h/load-code-and-zloc "(ns apple) |(def bar inc) (bar 1)")
          _ (h/load-code-and-locs "(ns bread)" b-uri)
          results (:changes-by-uri (move-form/move-form zloc a-uri db/db "/b.clj"))
          a-results (h/changes->code results a-uri db/db)
          b-results (h/changes->code results b-uri db/db)]
      (is (= "(ns apple \n  (:require\n    bread))  (bread/bar 1)" a-results))
      (is (= "(ns bread)\n(def bar inc)" b-results))))
  (testing "complex"
    (h/clean-db!)
    (let [a-uri (h/file-uri "file:///a.clj")
          b-uri (h/file-uri "file:///b.clj")
          c-uri (h/file-uri "file:///c.clj")
          d-uri (h/file-uri "file:///d.clj")
          zloc (h/load-code-and-zloc "(ns apple (:require [bread :as b])) |(def bar b/foo) (bar 1)")
          _ (h/load-code-and-locs "(ns bread) (def foo inc)" b-uri)
          _ (h/load-code-and-locs "(ns crumb (:require [apple :as a :refer [bar]])) (bar 1) (apple/bar 2) (a/bar 3)" c-uri)
          _ (h/load-code-and-locs "(ns diner (:require [apple :refer [bar]])) (apple/foo 1) (bar 2)" d-uri)
          results (:changes-by-uri (move-form/move-form zloc a-uri db/db "/b.clj"))
          a-results (h/changes->code results a-uri db/db)
          b-results (h/changes->code results b-uri db/db)
          c-results (h/changes->code results c-uri db/db)
          d-results (h/changes->code results d-uri db/db)]
      (is (= "(ns apple (:require [bread :as b]))  (b/bar 1)" a-results))
      (is (= "(ns bread) (def foo inc)\n(def bar foo)" b-results))
      (is (= "(ns crumb (:require [bread :as b :refer [bar]])) (bar 1) (b/bar 2) (b/bar 3)" c-results))
      (is (= "(ns diner (:require [apple]\n                    [bread :as b :refer [bar]])) (apple/foo 1) (bar 2)" d-results)))))
