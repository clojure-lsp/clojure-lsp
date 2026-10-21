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

(defn ^:private move-to-dest
  "Moves the form at the `|` of `source-code` to `dest-code`, returning the
  resulting code of both files, or the error. Other `files` can be given as
  a map of filename to code."
  [source-code dest-code & [{:keys [ext settings files] :or {ext "clj"}}]]
  (h/reset-components!)
  (when settings
    (swap! (h/db*) shared/deep-merge {:settings settings}))
  (doseq [[filename code] files]
    (h/load-code-and-locs code (h/file-uri (str "file:///" filename))))
  (let [source-uri (h/file-uri (str "file:///a." ext))
        dest-uri (h/file-uri (str "file:///b." ext))
        zloc (h/load-code-and-zloc source-code source-uri)
        _ (h/load-code-and-locs dest-code dest-uri)
        {:keys [changes-by-uri error]} (move-form/move-form zloc source-uri (h/components) (h/file-path (str "/b." ext)))]
    (if error
      {:error error}
      {:source (h/changes-by-uri->code changes-by-uri source-uri (h/db))
       :dest (h/changes-by-uri->code changes-by-uri dest-uri (h/db))})))

;; https://github.com/clojure-lsp/clojure-lsp/issues/2495
(deftest move-form-dependencies-test
  (testing "adds the require of the libs the form uses through an alias"
    (is (= (h/code "(ns bread "
                   "  (:require"
                   "   [clojure.string :as str]))"
                   ""
                   "(defn x [] 1)"
                   ""
                   "(defn shout [s] (str/upper-case s))")
           (:dest (move-to-dest (h/code "(ns apple"
                                        "  (:require"
                                        "   [clojure.string :as str]))"
                                        ""
                                        "|(defn shout [s] (str/upper-case s))")
                                (h/code "(ns bread)"
                                        ""
                                        "(defn x [] 1)")
                                {:settings {:clean {:ns-inner-blocks-indentation :next-line}}})))))

  (testing "uses the alias the destination already has for the lib"
    (is (= (h/code "(ns bread (:require [clojure.string :as string]))"
                   "(defn x [] (string/join []))"
                   ""
                   "(defn shout [s] (string/upper-case s))")
           (:dest (move-to-dest (h/code "(ns apple (:require [clojure.string :as str]))"
                                        "|(defn shout [s] (str/upper-case s))")
                                (h/code "(ns bread (:require [clojure.string :as string]))"
                                        "(defn x [] (string/join []))"))))))

  (testing "fully qualifies when the destination requires the lib without alias"
    (is (= (h/code "(ns bread (:require [clojure.string]))"
                   "(defn x [] (clojure.string/join []))"
                   ""
                   "(defn shout [s] (clojure.string/upper-case s))")
           (:dest (move-to-dest (h/code "(ns apple (:require [clojure.string :as str]))"
                                        "|(defn shout [s] (str/upper-case s))")
                                (h/code "(ns bread (:require [clojure.string]))"
                                        "(defn x [] (clojure.string/join []))"))))))

  (testing "fully qualifies when the alias is taken in the destination"
    (is (= (h/code "(ns bread (:require [clojure.string]"
                   "                    [foo.str :as str]))"
                   "(defn x [] (str/foo))"
                   ""
                   "(defn shout [s] (clojure.string/upper-case s))")
           (:dest (move-to-dest (h/code "(ns apple (:require [clojure.string :as str]))"
                                        "|(defn shout [s] (str/upper-case s))")
                                (h/code "(ns bread (:require [foo.str :as str]))"
                                        "(defn x [] (str/foo))"))))))

  (testing "adds the refers the form uses"
    (is (= (h/code "(ns bread "
                   "  (:require"
                   "   [clojure.string :refer [lower-case upper-case]]))"
                   ""
                   "(defn x [] 1)"
                   ""
                   "(defn shout [s] (upper-case (lower-case s)))")
           (:dest (move-to-dest (h/code "(ns apple (:require [clojure.string :refer [lower-case upper-case]]))"
                                        "|(defn shout [s] (upper-case (lower-case s)))")
                                (h/code "(ns bread)"
                                        ""
                                        "(defn x [] 1)")
                                {:settings {:clean {:ns-inner-blocks-indentation :next-line}}})))))

  (testing "adds the refers to the lib the destination already requires"
    (is (= (h/code "(ns bread (:require [clojure.string :as string :refer [upper-case]]))"
                   "(defn x [] (string/join []))"
                   ""
                   "(defn shout [s] (upper-case s))")
           (:dest (move-to-dest (h/code "(ns apple (:require [clojure.string :refer [upper-case]]))"
                                        "|(defn shout [s] (upper-case s))")
                                (h/code "(ns bread (:require [clojure.string :as string]))"
                                        "(defn x [] (string/join []))"))))))

  (testing "qualifies the referred vars whose name is taken in the destination"
    (is (= (h/code "(ns bread (:require [clojure.string :as string]))"
                   "(defn upper-case [s] (string/join [s]))"
                   ""
                   "(defn shout [s] (string/upper-case s))")
           (:dest (move-to-dest (h/code "(ns apple (:require [clojure.string :refer [upper-case]]))"
                                        "|(defn shout [s] (upper-case s))")
                                (h/code "(ns bread (:require [clojure.string :as string]))"
                                        "(defn upper-case [s] (string/join [s]))"))))))

  (testing "adds the require of the libs the form uses fully qualified"
    (is (= (h/code "(ns bread "
                   "  (:require"
                   "   [clojure.set]))"
                   ""
                   "(defn x [] 1)"
                   ""
                   "(defn both [a b] (clojure.set/union a b))")
           (:dest (move-to-dest (h/code "(ns apple (:require [clojure.set]))"
                                        "|(defn both [a b] (clojure.set/union a b))")
                                (h/code "(ns bread)"
                                        ""
                                        "(defn x [] 1)")
                                {:settings {:clean {:ns-inner-blocks-indentation :next-line}}})))))

  (testing "adds the imports of the classes the form uses"
    (is (= (h/code "(ns bread "
                   "  (:import"
                   "   [java.util Date UUID]))"
                   ""
                   "(defn x [] 1)"
                   ""
                   "(defn stamp ^Date [] [(Date.) (UUID/randomUUID) (java.io.File. \"f\")])")
           (:dest (move-to-dest (h/code "(ns apple (:import [java.io File] [java.util Date UUID]))"
                                        "|(defn stamp ^Date [] [(Date.) (UUID/randomUUID) (java.io.File. \"f\")])")
                                (h/code "(ns bread)"
                                        ""
                                        "(defn x [] 1)")
                                {:settings {:clean {:ns-inner-blocks-indentation :next-line}}})))))

  (testing "keeps the imports the destination already has"
    (is (= (h/code "(ns bread (:import [java.util Date]))"
                   "(defn x [] (Date.))"
                   ""
                   "(defn now [] (Date.))")
           (:dest (move-to-dest (h/code "(ns apple (:import [java.util Date]))"
                                        "|(defn now [] (Date.))")
                                (h/code "(ns bread (:import [java.util Date]))"
                                        "(defn x [] (Date.))"))))))

  (testing "fully qualifies the classes whose name is taken in the destination"
    (is (= (h/code "(ns bread (:import [java.sql Date]))"
                   "(defn x [] (Date. 0))"
                   ""
                   "(defn now ^java.util.Date [] (java.util.Date.))")
           (:dest (move-to-dest (h/code "(ns apple (:import [java.util Date]))"
                                        "|(defn now ^Date [] (Date.))")
                                (h/code "(ns bread (:import [java.sql Date]))"
                                        "(defn x [] (Date. 0))"))))))

  (testing "keeps the meaning of the auto-resolved keywords"
    (is (= (h/code "(ns bread (:require [clojure.string :as string]))"
                   "(defn x [] (string/join []))"
                   ""
                   "(defn ks [] [:apple/kw ::string/k :crumb/id ::own"
                   "             #:apple{:a 1} #::string{:b 2} #:crumb{:c 3} #::{:d 4}"
                   "             (string/join [])])")
           (:dest (move-to-dest (h/code "(ns apple (:require [bread :as b] [clojure.string :as str] [crumb :as-alias c]))"
                                        "|(defn ks [] [::kw ::str/k ::c/id ::b/own"
                                        "             #::{:a 1} #::str{:b 2} #::c{:c 3} #::b{:d 4}"
                                        "             (str/join [])])")
                                (h/code "(ns bread (:require [clojure.string :as string]))"
                                        "(defn x [] (string/join []))"))))))

  (testing "keeps auto-resolved keywords using the alias added to the destination"
    (is (= (h/code "(ns bread "
                   "  (:require"
                   "   [clojure.string :as str]))"
                   ""
                   "(defn x [] 1)"
                   ""
                   "(defn f [] [(str/join []) ::str/k])")
           (:dest (move-to-dest (h/code "(ns apple (:require [clojure.string :as str]))"
                                        "|(defn f [] [(str/join []) ::str/k])")
                                (h/code "(ns bread)"
                                        ""
                                        "(defn x [] 1)")
                                {:settings {:clean {:ns-inner-blocks-indentation :next-line}}})))))

  (testing "adds the requires and imports the form uses in a single lang within reader conditionals"
    (is (= (h/code "(ns bread "
                   "  (:require"
                   "   #?(:cljs [goog.string :as gstr])"
                   "   [clojure.string :as str]) "
                   "  (:import"
                   "   #?(:clj [java.util Date])))"
                   ""
                   "(defn x [] 1)"
                   ""
                   "(defn shout [s]"
                   "  #?(:clj (str (Date.) (str/upper-case s))"
                   "     :cljs (gstr/format \"%s\" (str/upper-case s))))")
           (:dest (move-to-dest (h/code "(ns apple"
                                        "  (:require"
                                        "   [clojure.string :as str]"
                                        "   #?(:cljs [goog.string :as gstr]))"
                                        "  #?(:clj (:import [java.util Date])))"
                                        ""
                                        "|(defn shout [s]"
                                        "  #?(:clj (str (Date.) (str/upper-case s))"
                                        "     :cljs (gstr/format \"%s\" (str/upper-case s))))")
                                (h/code "(ns bread)"
                                        ""
                                        "(defn x [] 1)")
                                {:ext "cljc"
                                 :settings {:clean {:ns-inner-blocks-indentation :next-line}}})))))

  (testing "adds the form requires while dropping the source one from the destination"
    (is (= (h/code "(ns bread (:require [clojure.string :as str]))"
                   ""
                   "(defn area [w h] (str/join [w h]))"
                   "(defn box [] (area 2 3))")
           (:dest (move-to-dest (h/code "(ns apple (:require [clojure.string :as str]))"
                                        "|(defn area [w h] (str/join [w h]))")
                                (h/code "(ns bread (:require [apple :as a]))"
                                        "(defn box [] (a/area 2 3))"))))))

  (testing "refuses to move when a lib the form uses requires the destination"
    (is (= {:error {:message "Can't move shout to bread: it uses crumb, which requires bread, so it would create a circular dependency."
                    :code :invalid-request}}
           (move-to-dest (h/code "(ns apple (:require [crumb :as c]))"
                                 "|(defn shout [s] (c/loud s))")
                         (h/code "(ns bread)"
                                 "(defn x [] 1)")
                         {:files {"c.clj" (h/code "(ns crumb (:require [bread :as b]))"
                                                  "(defn loud [s] (b/x) s)")}})))))
