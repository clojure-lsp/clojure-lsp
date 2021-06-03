(ns clojure-lsp.db-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.test-helper :as h]
   [clojure.string :as s]
   [clojure.test :refer [deftest testing is]]
   [clojure.java.io :as io]))

(h/reset-db-after-test)

(def project-path (h/file-path "/user/project"))
(def default-db-path (str (io/file project-path ".lsp" "sqlite.db")))

(deftest sqlite-db-file-setting
  (testing "when not set"
    (reset! db/db {})
    (is (= default-db-path (#'db/get-sqlite-db-file-path project-path))))
  (testing "when set to relative path"
    (let [settings-path "subdir/sqlite.db"
          expected (.getAbsolutePath (io/file project-path settings-path))]
      (reset! db/db {:settings {:sqlite-db-path settings-path}})
      (is (= expected
             (#'db/get-sqlite-db-file-path project-path)))))
  (testing "when set to absolute path"
    (let [settings-path (h/file-path "/db-dir/sqlite.db")]
      (reset! db/db {:settings {:sqlite-db-path settings-path}})
      (is (= settings-path
             (#'db/get-sqlite-db-file-path project-path))))))
