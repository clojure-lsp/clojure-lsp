(ns clojure-lsp.features.file-management-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.core.async :as async]
   [clojure.test :refer [deftest is testing]]))

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

(deftest uri->namespace
  (testing "when don't have a project root"
    (reset! db/db {})
    (is (nil? (#'f.file-management/uri->namespace (h/file-uri "file:///user/project/src/foo/bar.clj") db/db))))
  (testing "when it has a project root and not a source-path"
    (swap! db/db merge {:settings {:auto-add-ns-to-new-files? true
                                   :source-paths #{(h/file-uri "file:///user/project/bla")}}
                        :project-root-uri (h/file-uri "file:///user/project")})
    (is (nil? (#'f.file-management/uri->namespace (h/file-uri "file:///user/project/src/foo/bar.clj") db/db))))
  (testing "when it has a project root and a source-path"
    (swap! db/db merge {:settings {:auto-add-ns-to-new-files? true
                                   :source-paths #{(h/file-path "/user/project/src")}}
                        :project-root-uri (h/file-uri "file:///user/project")})
    (is (= "foo.bar"
           (#'f.file-management/uri->namespace (h/file-uri "file:///user/project/src/foo/bar.clj") db/db))))
  (testing "when it has a project root a source-path on mono repos"
    (swap! db/db merge {:settings {:auto-add-ns-to-new-files? true
                                   :source-paths #{(h/file-path "/user/project/src/clj")
                                                   (h/file-path "/user/project/src/cljs")}}
                        :project-root-uri (h/file-uri "file:///user/project")})
    (is (= "foo.bar"
           (#'f.file-management/uri->namespace (h/file-uri "file:///user/project/src/clj/foo/bar.clj") db/db)))))

(deftest did-close
  (h/load-code-and-locs "(ns foo) a b c")
  (h/load-code-and-locs "(ns bar) d e f" (h/file-uri "file:///b.clj"))
  (testing "when file exists on disk"
    (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
    (with-redefs [shared/file-exists? (constantly true)]
      (f.file-management/did-close "file:///a.clj" db/db))
    (is (get-in @db/db [:analysis "/a.clj"]))
    (is (get-in @db/db [:findings "/a.clj"]))
    (is (get-in @db/db [:documents "file:///a.clj"])))
  (testing "when file not exists on disk"
    (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
    (with-redefs [shared/file-exists? (constantly false)]
      (f.file-management/did-close "file:///b.clj" db/db))
    (is (nil? (get-in @db/db [:analysis "/b.clj"])))
    (is (nil? (get-in @db/db [:findings "/b.clj"])))
    (is (nil? (get-in @db/db [:documents "file:///b.clj"])))))
