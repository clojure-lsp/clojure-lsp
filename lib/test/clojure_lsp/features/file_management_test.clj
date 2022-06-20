(ns clojure-lsp.features.file-management-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.core.async :as async]
   [clojure.test :refer [are deftest is testing]]
   [medley.core :as medley]))

(h/reset-db-after-test)

(deftest update-text
  (is (= "(comment\n   )" (#'f.file-management/replace-text "(comment)" "\n   " 0 8 0 8)))
  (is (= "some \nboring\n text" (#'f.file-management/replace-text "some \ncool\n text" "boring" 1 0 1 4)))
  (is (= "(+ 1 2)" (#'f.file-management/replace-text "(+ 1 1)" "2" 0 5 0 6)))
  (is (= "(+ 1)" (#'f.file-management/replace-text "(+ 1 1)" "" 0 4 0 6)))
  (is (= "\n\n (+ 1 2)\n" (#'f.file-management/replace-text "\n\n (let [a 1\n   b 2]\n   (+ 1 2))\n" "(+ 1 2)" 2 1 4 11)))
  (is (= "\r\n\r\n (+ 1 2)\r\n" (#'f.file-management/replace-text "\r\n\r\n (let [a 1\r\n   b 2]\r\n   (+ 1 2))\r\n" "(+ 1 2)" 2 1 4 11)))
  (is (= "\n\n (let [a 1\n   b 2]\n   (+ 1 2))\n" (#'f.file-management/replace-text "\n\n (+ 1 2)\n" "(let [a 1\n   b 2]\n   (+ 1 2))" 2 1 2 8)))
  (is (= "(+ 1 1)\n\n" (#'f.file-management/replace-text "(+ 1 1)\n" "\n" 1 0 1 0))))

(deftest did-close
  (swap! db/db* medley/deep-merge {:settings {:source-paths #{(h/file-path "/user/project/src/clj")}}
                                  :project-root-uri (h/file-uri "file:///user/project")})
  (h/load-code-and-locs "(ns foo) a b c" (h/file-uri "file:///user/project/src/clj/foo.clj"))
  (h/load-code-and-locs "(ns bar) d e f" (h/file-uri "file:///user/project/src/clj/bar.clj"))
  (h/load-code-and-locs "(ns some-jar)" (h/file-uri "file:///some/path/to/jar.jar:/some/file.clj"))
  (testing "when file exists on disk"
    (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
    (with-redefs [shared/file-exists? (constantly true)]
      (f.file-management/did-close "file:///user/project/src/clj/foo.clj" db/db*))
    (is (get-in @db/db* [:analysis "/user/project/src/clj/foo.clj"]))
    (is (get-in @db/db* [:findings "/user/project/src/clj/foo.clj"]))
    (is (get-in @db/db* [:documents "file:///user/project/src/clj/foo.clj"])))
  (testing "when local file not exists on disk"
    (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
    (with-redefs [shared/file-exists? (constantly false)]
      (f.file-management/did-close "file:///user/project/src/clj/bar.clj" db/db*))
    (is (nil? (get-in @db/db* [:analysis "/user/project/src/clj/bar.clj"])))
    (is (nil? (get-in @db/db* [:findings "/user/project/src/clj/bar.clj"])))
    (is (nil? (get-in @db/db* [:documents "file:///user/project/src/clj/bar.clj"]))))
  (testing "when file is external we do not remove analysis"
    (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
    (with-redefs [shared/file-exists? (constantly false)]
      (f.file-management/did-close "file:///some/path/to/jar.jar:/some/file.clj" db/db*))
    (is (get-in @db/db* [:analysis "/some/path/to/jar.jar:/some/file.clj"]))
    (is (get-in @db/db* [:findings "/some/path/to/jar.jar:/some/file.clj"]))
    (is (get-in @db/db* [:documents "file:///some/path/to/jar.jar:/some/file.clj"]))))

(deftest outgoing-reference-filenames
  (swap! db/db* medley/deep-merge {:settings {:source-paths #{(h/file-path "/src")}}
                                   :project-root-uri (h/file-uri "file:///")})
  (h/load-code-and-locs (h/code "(ns a)"
                                "(def a)"
                                "(def b)") (h/file-uri "file:///src/a.clj"))
  (h/load-code-and-locs (h/code "(ns b (:require [a]))"
                                "(def x)"
                                "a/a"
                                "a/a") (h/file-uri "file:///src/b.clj"))
  (let [db-before @db/db*]
    (are [expected new-code]
         (do
           (h/load-code-and-locs new-code (h/file-uri "file:///src/b.clj"))
           (let [db-after @db/db*]
             (is (= expected
                    (f.file-management/reference-filenames "/src/b.clj" db-before db-after)))))
      ;; increasing
      #{"/src/a.clj"} (h/code "(ns b (:require [a]))"
                              "(def x)"
                              "a/a"
                              "a/a"
                              "a/a")
      ;; decreasing
      #{"/src/a.clj"} (h/code "(ns b (:require [a]))"
                              "(def x)"
                              "a/a")
      ;; removing
      #{"/src/a.clj"} (h/code "(ns b (:require [a]))"
                              "(def x)")
      ;; adding
      #{"/src/a.clj"} (h/code "(ns b (:require [a]))"
                              "(def x)"
                              "a/a"
                              "a/a"
                              "a/b")
      ;; same
      #{} (h/code "(ns b (:require [a]))"
                  "(def x)"
                  "a/a"
                  "a/a")
      ;; external ns
      #{} (h/code "(ns b (:require [a]))"
                  "(def x)"
                  "a/a"
                  "a/a"
                  "inc")
      ;; same ns
      #{} (h/code "(ns b (:require [a]))"
                  "(def x)"
                  "a/a"
                  "a/a"
                  "x"))))

(deftest incoming-reference-filenames
  (swap! db/db* medley/deep-merge {:settings {:source-paths #{(h/file-path "/src")}}
                                   :project-root-uri (h/file-uri "file:///")})
  (h/load-code-and-locs (h/code "(ns a)"
                                "(def a)"
                                "(def b)") (h/file-uri "file:///src/a.clj"))
  (h/load-code-and-locs (h/code "(ns b (:require [a]))"
                                "a/a"
                                "a/c") (h/file-uri "file:///src/b.clj"))
  (let [db-before @db/db*]
    (are [expected new-code]
         (do
           (h/load-code-and-locs new-code (h/file-uri "file:///src/a.clj"))
           (let [db-after @db/db*]
             (is (= expected
                    (f.file-management/reference-filenames "/src/a.clj" db-before db-after)))))
      ;; remove existing
      #{"/src/b.clj"} (h/code "(ns a)"
                              "(def b)")
      ;; create missing
      #{"/src/b.clj"} (h/code "(ns a)"
                              "(def a)"
                              "(def b)"
                              "(def c)")
      ;; remove unused
      #{} (h/code "(ns a)"
                  "(def a)")
      ;; add unused
      #{} (h/code "(ns a)"
                  "(def a)"
                  "(def b)"
                  "(def d)"))))
