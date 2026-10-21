(ns clojure-lsp.feature.move-form-test
  (:require
   [clojure-lsp.feature.move-form :as move-form]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(deftest move-form-test
  (testing "simple"
    (h/reset-components!)
    (swap! (h/db*) shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :next-line}}})
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
    (swap! (h/db*) shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :next-line}}})
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
                :result (h/code "(ns crumb (:require [bread :as b :refer [bar]]))"
                                "(bar 1)"
                                "(b/bar 2)"
                                "(b/bar 3)")}

        d-code {:initial (h/code "(ns diner (:require [apple :as a :refer [bar]]))"
                                 "(a/qux 1)"
                                 "(apple/foo 1)"
                                 "(bar 2)")
                :result (h/code "(ns diner (:require [apple :as a]"
                                "                    [bread :as b :refer [bar]]))"
                                "(a/qux 1)"
                                "(apple/foo 1)"
                                "(bar 2)")}

        e-code {:initial (h/code "(ns eater (:require [apple :as a] [crumb :as c]))"
                                 "(a/bar 2)"
                                 "(c/c 3)")
                :result (h/code "(ns eater (:require [bread :as b]"
                                "                    [crumb :as c]))"
                                "(b/bar 2)"
                                "(c/c 3)")}

        f-code {:initial (h/code "(ns fruit (:require [apple :as a] [bread :as b]))"
                                 "(a/bar 2)"
                                 "(b/foo 3)")
                :result (h/code "(ns fruit (:require [bread :as b]))"
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
               f-results)))))

  ;; https://github.com/clojure-lsp/clojure-lsp/issues/1444
  (let [a-code {:initial (h/code "(ns apple)"
                                 "|(def bar 1)"
                                 "(def qux 2)")
                :result (h/code "(ns apple)"
                                "(def qux 2)")}
        b-code {:initial (h/code "(ns bread)")
                :result (h/code "(ns bread)"
                                ""
                                "(def bar 1)")}
        c-code {:initial (h/code "(ns crumb (:require [apple :as a]))"
                                 "(a/bar 1)"
                                 "(a/qux 2)")
                :result (h/code "(ns crumb (:require [apple :as a]"
                                "                    [bread]))"
                                "(bread/bar 1)"
                                "(a/qux 2)")}
        d-code {:initial (h/code "(ns diner (:require [apple :as a :refer [bar]]))"
                                 "(bar 1)"
                                 "(a/qux 2)")
                :result (h/code "(ns diner (:require [apple :as a]"
                                "                    [bread :refer [bar]]))"
                                "(bar 1)"
                                "(a/qux 2)")}]
    (testing "keeps source require when other source vars are used via alias"
      (h/reset-components!)
      (let [a-uri (h/file-uri "file:///a.clj")
            b-uri (h/file-uri "file:///b.clj")
            c-uri (h/file-uri "file:///c.clj")
            d-uri (h/file-uri "file:///d.clj")
            zloc (h/load-code-and-zloc (:initial a-code) a-uri)
            _ (h/load-code-and-locs (:initial b-code) b-uri)
            _ (h/load-code-and-locs (:initial c-code) c-uri)
            _ (h/load-code-and-locs (:initial d-code) d-uri)
            results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.clj")))]
        (is (= (:result a-code)
               (h/changes-by-uri->code results a-uri (h/db))))
        (is (= (:result b-code)
               (h/changes-by-uri->code results b-uri (h/db))))
        (is (= (:result c-code)
               (h/changes-by-uri->code results c-uri (h/db))))
        (is (= (:result d-code)
               (h/changes-by-uri->code results d-uri (h/db))))))

    (testing "keeps source require when other source vars are used via alias cljc"
      (h/reset-components!)
      (let [a-uri (h/file-uri "file:///a.cljc")
            b-uri (h/file-uri "file:///b.cljc")
            c-uri (h/file-uri "file:///c.cljc")
            d-uri (h/file-uri "file:///d.cljc")
            zloc (h/load-code-and-zloc (:initial a-code) a-uri)
            _ (h/load-code-and-locs (:initial b-code) b-uri)
            _ (h/load-code-and-locs (:initial c-code) c-uri)
            _ (h/load-code-and-locs (:initial d-code) d-uri)
            results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.cljc")))]
        (is (= (:result a-code)
               (h/changes-by-uri->code results a-uri (h/db))))
        (is (= (:result b-code)
               (h/changes-by-uri->code results b-uri (h/db))))
        (is (= (:result c-code)
               (h/changes-by-uri->code results c-uri (h/db))))
        (is (= (:result d-code)
               (h/changes-by-uri->code results d-uri (h/db)))))))

  ;; https://github.com/clojure-lsp/clojure-lsp/issues/2494
  (let [a-code {:initial (h/code "(ns apple)"
                                 ""
                                 "|(defn area [w h] (* w h))"
                                 ""
                                 "(defn other [] 1)")
                :result (h/code "(ns apple)"
                                ""
                                "(defn other [] 1)")}
        b-code {:initial (h/code "(ns bread"
                                 "  (:require"
                                 "   [apple :as a]))"
                                 ""
                                 "(defn box [] (a/area 2 3))")
                :result (h/code "(ns bread)"
                                ""
                                "(defn area [w h] (* w h))"
                                ""
                                "(defn box [] (area 2 3))")}]
    (testing "destination already using the moved var through the source alias"
      (h/reset-components!)
      (let [a-uri (h/file-uri "file:///a.clj")
            b-uri (h/file-uri "file:///b.clj")
            zloc (h/load-code-and-zloc (:initial a-code) a-uri)
            _ (h/load-code-and-locs (:initial b-code) b-uri)
            results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.clj")))]
        (is (= (:result a-code)
               (h/changes-by-uri->code results a-uri (h/db))))
        (is (= (:result b-code)
               (h/changes-by-uri->code results b-uri (h/db))))))

    (testing "destination already using the moved var through the source alias cljc"
      (h/reset-components!)
      (let [a-uri (h/file-uri "file:///a.cljc")
            b-uri (h/file-uri "file:///b.cljc")
            zloc (h/load-code-and-zloc (:initial a-code) a-uri)
            _ (h/load-code-and-locs (:initial b-code) b-uri)
            results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.cljc")))]
        (is (= (:result a-code)
               (h/changes-by-uri->code results a-uri (h/db))))
        (is (= (:result b-code)
               (h/changes-by-uri->code results b-uri (h/db)))))))

  (testing "destination drops the source require when the source still uses the moved var"
    (h/reset-components!)
    (swap! (h/db*) shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :next-line}}})
    (let [a-uri (h/file-uri "file:///a.clj")
          b-uri (h/file-uri "file:///b.clj")
          zloc (h/load-code-and-zloc (h/code "(ns apple)"
                                             ""
                                             "|(defn area [w h] (* w h))"
                                             ""
                                             "(defn square [x] (area x x))")
                                     a-uri)
          _ (h/load-code-and-locs (h/code "(ns bread"
                                          "  (:require"
                                          "   [apple :as a]))"
                                          ""
                                          "(defn box [] (a/area 2 3))")
                                  b-uri)
          results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.clj")))]
      (is (= (h/code "(ns apple "
                     "  (:require"
                     "   [bread]))"
                     ""
                     "(defn square [x] (bread/area x x))")
             (h/changes-by-uri->code results a-uri (h/db))))
      (is (= (h/code "(ns bread)"
                     ""
                     "(defn area [w h] (* w h))"
                     ""
                     "(defn box [] (area 2 3))")
             (h/changes-by-uri->code results b-uri (h/db))))))

  (testing "destination keeps the source require when still using other source vars"
    (h/reset-components!)
    (let [a-uri (h/file-uri "file:///a.clj")
          b-uri (h/file-uri "file:///b.clj")
          zloc (h/load-code-and-zloc (h/code "(ns apple)"
                                             "|(defn area [w h] (* w h))"
                                             "(defn other [] 1)")
                                     a-uri)
          _ (h/load-code-and-locs (h/code "(ns bread (:require [apple :as a]))"
                                          "(defn box [] (a/area 2 (a/other)))")
                                  b-uri)
          results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.clj")))]
      (is (= (h/code "(ns apple)"
                     "(defn other [] 1)")
             (h/changes-by-uri->code results a-uri (h/db))))
      (is (= (h/code "(ns bread (:require [apple :as a]))"
                     ""
                     "(defn area [w h] (* w h))"
                     "(defn box [] (area 2 (a/other)))")
             (h/changes-by-uri->code results b-uri (h/db))))))

  (testing "destination referring the moved var drops the source require"
    (h/reset-components!)
    (let [a-uri (h/file-uri "file:///a.clj")
          b-uri (h/file-uri "file:///b.clj")
          zloc (h/load-code-and-zloc (h/code "(ns apple)"
                                             "|(defn area [w h] (* w h))"
                                             "(defn other [] 1)")
                                     a-uri)
          _ (h/load-code-and-locs (h/code "(ns bread (:require [apple :refer [area]]))"
                                          "(defn box [] (area 2 3))")
                                  b-uri)
          results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.clj")))]
      (is (= (h/code "(ns bread)"
                     ""
                     "(defn area [w h] (* w h))"
                     "(defn box [] (area 2 3))")
             (h/changes-by-uri->code results b-uri (h/db))))))

  (testing "destination referring the moved var drops the refer when still using other source vars"
    (h/reset-components!)
    (let [a-uri (h/file-uri "file:///a.clj")
          b-uri (h/file-uri "file:///b.clj")
          zloc (h/load-code-and-zloc (h/code "(ns apple)"
                                             "|(defn area [w h] (* w h))"
                                             "(defn other [] 1)")
                                     a-uri)
          _ (h/load-code-and-locs (h/code "(ns bread (:require [apple :as a :refer [area]]))"
                                          "(defn box [] (area 2 (a/other)))")
                                  b-uri)
          results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.clj")))]
      (is (= (h/code "(ns bread (:require [apple :as a]))"
                     ""
                     "(defn area [w h] (* w h))"
                     "(defn box [] (area 2 (a/other)))")
             (h/changes-by-uri->code results b-uri (h/db))))))

  (testing "destination gets the moved form before its first usage, keeping comments"
    (h/reset-components!)
    (let [a-uri (h/file-uri "file:///a.clj")
          b-uri (h/file-uri "file:///b.clj")
          zloc (h/load-code-and-zloc (h/code "(ns apple)"
                                             "|(defn area [w h] (* w h))")
                                     a-uri)
          _ (h/load-code-and-locs (h/code "(ns bread (:require [apple :as a]))"
                                          ""
                                          "(def x 1)"
                                          ""
                                          ";; Box helper"
                                          "(defn box [] (a/area 2 3))"
                                          ""
                                          "(defn crate [] (a/area 4 5))")
                                  b-uri)
          results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.clj")))]
      (is (= (h/code "(ns bread)"
                     ""
                     "(def x 1)"
                     ""
                     "(defn area [w h] (* w h))"
                     ""
                     ";; Box helper"
                     "(defn box [] (area 2 3))"
                     ""
                     "(defn crate [] (area 4 5))")
             (h/changes-by-uri->code results b-uri (h/db))))))

  (testing "destination keeps the source require when still using its alias in keywords"
    (doseq [keyword-code ["(def m {::a/id 1})"
                          "(def m #::a{:id 1})"]]
      (h/reset-components!)
      (let [a-uri (h/file-uri "file:///a.clj")
            b-uri (h/file-uri "file:///b.clj")
            zloc (h/load-code-and-zloc (h/code "(ns apple)"
                                               "|(defn area [w h] (* w h))")
                                       a-uri)
            _ (h/load-code-and-locs (h/code "(ns bread (:require [apple :as a]))"
                                            "(defn box [] (a/area 2 3))"
                                            keyword-code)
                                    b-uri)
            results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.clj")))]
        (is (= (h/code "(ns bread (:require [apple :as a]))"
                       ""
                       "(defn area [w h] (* w h))"
                       "(defn box [] (area 2 3))"
                       keyword-code)
               (h/changes-by-uri->code results b-uri (h/db)))
            keyword-code))))

  (testing "refuses to move when source and destination would require each other"
    (doseq [dest-code [;; still uses other source vars
                       (h/code "(ns bread (:require [apple :as a]))"
                               "(defn box [] (a/area 2 (a/other)))")
                       ;; still uses the source alias in keywords
                       (h/code "(ns bread (:require [apple :as a]))"
                               "(defn box [] {::a/id (a/area 2 3)})")
                       ;; doesn't use the moved var, so its ns isn't touched
                       (h/code "(ns bread (:require [apple :as a]))"
                               "(defn box [] (a/other))")]]
      (h/reset-components!)
      (let [a-uri (h/file-uri "file:///a.clj")
            b-uri (h/file-uri "file:///b.clj")
            zloc (h/load-code-and-zloc (h/code "(ns apple)"
                                               "|(defn area [w h] (* w h))"
                                               "(defn other [] 1)"
                                               "(defn square [x] (area x x))")
                                       a-uri)
            _ (h/load-code-and-locs dest-code b-uri)]
        (is (= {:error {:message "Can't move area to bread: apple still uses it and bread requires apple, which would create a circular dependency."
                        :code :invalid-request}}
               (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.clj")))
            dest-code))))

  (testing "other files keep their requires unused in the source"
    (h/reset-components!)
    (let [a-uri (h/file-uri "file:///a.clj")
          b-uri (h/file-uri "file:///b.clj")
          c-uri (h/file-uri "file:///c.clj")
          zloc (h/load-code-and-zloc (h/code "(ns apple (:require [clojure.string :as str]))"
                                             "|(def bar 1)")
                                     a-uri)
          _ (h/load-code-and-locs (h/code "(ns bread)") b-uri)
          _ (h/load-code-and-locs (h/code "(ns crumb (:require [apple :as a] [clojure.string :as str]))"
                                          "(a/bar 1)"
                                          "(str/join [])")
                                  c-uri)
          results (:changes-by-uri (move-form/move-form zloc a-uri (h/components) (h/file-path "/b.clj")))]
      (is (= (h/code "(ns crumb (:require [bread]"
                     "                    [clojure.string :as str]))"
                     "(bread/bar 1)"
                     "(str/join [])")
             (h/changes-by-uri->code results c-uri (h/db)))))))
