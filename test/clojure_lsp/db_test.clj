(ns clojure-lsp.db-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.test-helper :as h]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(def project-path (h/file-path "/user/project"))
(def default-db-path (str (io/file project-path ".lsp" ".cache" "sqlite.db")))

(deftest sqlite-db-file-setting
  (testing "when not set"
    (reset! db/db {})
    (is (= default-db-path (#'db/get-db-file-path project-path db/db))))
  (testing "sqlite-db-path"
    (testing "when set to relative path"
      (let [settings-path "subdir/sqlite.db"
            expected (.getAbsolutePath (io/file project-path settings-path))]
        (reset! db/db {:settings {:sqlite-db-path settings-path}})
        (is (= expected
               (#'db/get-db-file-path project-path db/db)))))
    (testing "when set to absolute path"
      (let [settings-path (h/file-path "/db-dir/sqlite.db")]
        (reset! db/db {:settings {:sqlite-db-path settings-path}})
        (is (= settings-path
               (#'db/get-db-file-path project-path db/db))))))
  (testing "cache-path"
    (testing "when set to relative path"
      (let [settings-path "subdir/.cache"
            expected (.getAbsolutePath (io/file project-path settings-path "sqlite.db"))]
        (reset! db/db {:settings {:cache-path settings-path}})
        (is (= expected
               (#'db/get-db-file-path project-path db/db)))))
    (testing "when set to absolute path"
      (reset! db/db {:settings {:cache-path (h/file-path "/db-dir/.cache")}})
      (is (= (h/file-path "/db-dir/.cache/sqlite.db")
             (#'db/get-db-file-path project-path db/db))))))
