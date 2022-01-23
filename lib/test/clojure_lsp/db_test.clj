(ns clojure-lsp.db-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(def project-path (h/file-path "/user/project"))
(def default-db-path (str (io/file project-path ".lsp" ".cache")))

(deftest sqlite-db-file-setting
  (testing "when not set"
    (h/clean-db!)
    (is (= default-db-path (#'db/db-file->abs-path project-path db/db))))
  (testing "cache-path"
    (testing "when set to relative path"
      (let [settings-path "subdir/.cache"
            expected (.getAbsolutePath (io/file project-path settings-path))]
        (swap! db/db shared/deep-merge {:settings {:cache-path settings-path}})
        (is (= expected
               (#'db/db-file->abs-path project-path db/db)))))
    (testing "when set to absolute path"
      (swap! db/db shared/deep-merge {:settings {:cache-path (h/file-path "/db-dir/.cache")}})
      (is (= (h/file-path "/db-dir/.cache")
             (#'db/db-file->abs-path project-path db/db))))))
