(ns clojure-lsp.features.file-management-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.file-management :as f.file-management]
    [clojure-lsp.test-helper :as h]
    [clojure.test :refer [deftest testing is]]))

(h/reset-db-after-test)

(deftest uri->namespace
  (testing "when don't have a project root"
    (reset! db/db {})
    (is (nil? (#'f.file-management/uri->namespace "file:///user/project/src/foo/bar.clj"))))
  (testing "when it has a project root and not a source-path"
    (swap! db/db merge {:settings {:auto-add-ns-to-new-files? true
                                   :source-paths #{"file:///user/project/bla"}}
                        :project-root "file:///user/project"})
    (is (nil? (#'f.file-management/uri->namespace "file:///user/project/src/foo/bar.clj"))))
  (testing "when it has a project root and a source-path"
    (swap! db/db merge {:settings {:auto-add-ns-to-new-files? true
                                   :source-paths #{"/user/project/src"}}
                        :project-root "file:///user/project"})
    (is (= "foo.bar"
           (#'f.file-management/uri->namespace "file:///user/project/src/foo/bar.clj"))))
  (testing "when it has a project root a source-path on mono repos"
    (swap! db/db merge {:settings {:auto-add-ns-to-new-files? true
                                   :source-paths #{"/user/project/src/clj"
                                                   "/user/project/src/cljs"}}
                        :project-root "file:///user/project"})
    (is (= "foo.bar"
           (#'f.file-management/uri->namespace "file:///user/project/src/clj/foo/bar.clj")))))
