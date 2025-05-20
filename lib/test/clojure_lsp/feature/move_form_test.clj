(ns clojure-lsp.feature.move-form-test
  (:require
   [clojure-lsp.feature.move-form :as move-form]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(deftest move-form-test
  (testing "simple"
    (h/reset-components!)
    (let [a-uri (h/file-uri "file:///a.clj")
          b-uri (h/file-uri "file:///b.clj")
          zloc (h/load-code-and-zloc (h/code "(ns apple)" "|(def bar inc)" "(bar 1)"))
          _ (h/load-code-and-locs "(ns bread)" b-uri)
          results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.clj")))
          a-results (h/changes-by-uri->code results a-uri (h/db))
          b-results (h/changes-by-uri->code results b-uri (h/db))]
      (is (= (h/code "(ns apple "
                     "  (:require"
                     "   [bread]))"
                     "(bread/bar 1)") a-results))
      (is (= (h/code "(ns bread)"
                     ""
                     "(def bar inc)") b-results))))

  (testing "simple cljc"
    (h/reset-components!)
    (let [a-uri (h/file-uri "file:///a.cljc")
          b-uri (h/file-uri "file:///b.cljc")
          zloc (h/load-code-and-zloc (h/code "(ns apple)" "|(def bar inc)" "(bar 1)")
                                     a-uri)
          _ (h/load-code-and-locs "(ns bread)" b-uri)
          results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.cljc")))
          a-results (h/changes-by-uri->code results a-uri (h/db))
          b-results (h/changes-by-uri->code results b-uri (h/db))]

      (is (= (h/code "(ns apple "
                     "  (:require"
                     "   [bread]))"
                     "(bread/bar 1)") a-results))
      (is (= (h/code "(ns bread)"
                     ""
                     "(def bar inc)") b-results))))

  (let [a-code {:initial (h/code "(ns apple (:require [bread :as b]))"
                                 ""
                                 "(def qux 1)"
                                 ""
                                 "|(def bar b/foo)"
                                 ""
                                 "(bar 1)")
                :result (h/code "(ns apple (:require [bread :as b]))"
                                ""
                                "(def qux 1)"
                                ""
                                "(b/bar 1)")}
        b-code {:initial (h/code "(ns bread)"
                                 "(def foo inc)")
                :result (h/code "(ns bread)"
                                "(def foo inc)"
                                ""
                                "(def bar foo)")}
        c-code {:initial (h/code "(ns crumb (:require [apple :as a :refer [bar]]))"
                                 "(bar 1)"
                                 "(apple/bar 2)"
                                 "(a/bar 3)")
                :result (h/code "(ns crumb (:require"
                                "           [bread :as b :refer [bar]]))"
                                "(bar 1)"
                                "(b/bar 2)"
                                "(b/bar 3)")}

        d-code {:initial (h/code "(ns diner (:require [apple :as a :refer [bar]]))"
                                 "(a/qux 1)"
                                 "(apple/foo 1)"
                                 "(bar 2)")
                :result (h/code "(ns diner (:require"
                                "           [apple :as a]"
                                "           [bread :as b :refer [bar]]))"
                                "(a/qux 1)"
                                "(apple/foo 1)"
                                "(bar 2)")}

        e-code {:initial (h/code "(ns eater (:require [apple :as a] [crumb :as c]))"
                                 "(a/bar 2)"
                                 "(c/c 3)")
                :result (h/code "(ns eater (:require"
                                "           [bread :as b]"
                                "           [crumb :as c]))"
                                "(b/bar 2)"
                                "(c/c 3)")}

        f-code {:initial (h/code "(ns fruit (:require [apple :as a] [bread :as b]))"
                                 "(a/bar 2)"
                                 "(b/foo 3)")
                :result (h/code "(ns fruit (:require"
                                "           [bread :as b]))"
                                "(b/bar 2)"
                                "(b/foo 3)")}]
    (testing "complex"
      (h/reset-components!)
      (let [a-uri (h/file-uri "file:///a.clj")
            b-uri (h/file-uri "file:///b.clj")
            c-uri (h/file-uri "file:///c.clj")
            d-uri (h/file-uri "file:///d.clj")
            e-uri (h/file-uri "file:///e.clj")
            f-uri (h/file-uri "file:///f.clj")
            zloc (h/load-code-and-zloc (:initial a-code))
            _ (h/load-code-and-locs (:initial b-code) b-uri)
            _ (h/load-code-and-locs (:initial c-code) c-uri)
            _ (h/load-code-and-locs (:initial d-code) d-uri)
            _ (h/load-code-and-locs (:initial e-code) e-uri)
            _ (h/load-code-and-locs (:initial f-code) f-uri)
            results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.clj")))
            a-results (h/changes-by-uri->code results a-uri (h/db))
            b-results (h/changes-by-uri->code results b-uri (h/db))
            c-results (h/changes-by-uri->code results c-uri (h/db))
            d-results (h/changes-by-uri->code results d-uri (h/db))
            e-results (h/changes-by-uri->code results e-uri (h/db))
            f-results (h/changes-by-uri->code results f-uri (h/db))]
        (is (= (:result a-code)
               a-results))
        (is (= (:result b-code)
               b-results))
        (is (= (:result c-code)
               c-results))
        (is (= (:result d-code)
               d-results))
        (is (= (:result e-code)
               e-results))
        (is (= (:result f-code)
               f-results))))

    (testing "complex cljc"
      (h/reset-components!)
      (let [a-uri (h/file-uri "file:///a.cljc")
            b-uri (h/file-uri "file:///b.cljc")
            c-uri (h/file-uri "file:///c.cljc")
            d-uri (h/file-uri "file:///d.cljc")
            e-uri (h/file-uri "file:///e.cljc")
            f-uri (h/file-uri "file:///f.cljc")
            zloc (h/load-code-and-zloc (:initial a-code) a-uri)
            _ (h/load-code-and-locs (:initial b-code) b-uri)
            _ (h/load-code-and-locs (:initial c-code) c-uri)
            _ (h/load-code-and-locs (:initial d-code) d-uri)
            _ (h/load-code-and-locs (:initial e-code) e-uri)
            _ (h/load-code-and-locs (:initial f-code) f-uri)
            results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.cljc")))
            a-results (h/changes-by-uri->code results a-uri (h/db))
            b-results (h/changes-by-uri->code results b-uri (h/db))
            c-results (h/changes-by-uri->code results c-uri (h/db))
            d-results (h/changes-by-uri->code results d-uri (h/db))
            e-results (h/changes-by-uri->code results e-uri (h/db))
            f-results (h/changes-by-uri->code results f-uri (h/db))]
        (is (= (:result a-code)
               a-results))
        (is (= (:result b-code)
               b-results))
        (is (= (:result c-code)
               c-results))
        (is (= (:result d-code)
               d-results))
        (is (= (:result e-code)
               e-results))
        (is (= (:result f-code)
               f-results))))))
