(ns clojure-lsp.db-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.test-helper :as h]
   [clojure.string :as s]
   [clojure.test :refer [deftest testing is]]))

(h/reset-db-after-test)

(def project-path "/user/project")
(def default-db-path (s/join "/" [project-path ".lsp" "sqlite.db"]))

(deftest sqlite-db-file-setting
  (testing "when not set"
    (reset! db/db {})
    (is (= default-db-path (#'db/get-sqlite-db-file-path project-path))))
  (testing "when set to relative path"
    (let [settings-path "subdir/sqlite.db"
          expected (s/join "/" [project-path settings-path])]
      (reset! db/db {:settings {:sqlite-db-path settings-path}})
      (is (= expected
             (#'db/get-sqlite-db-file-path project-path)))))
  (testing "when set to absolute path"
    (let [settings-path "/db-dir/sqlite.db"]
      (reset! db/db {:settings {:sqlite-db-path settings-path}})
      (is (= settings-path
             (#'db/get-sqlite-db-file-path project-path))))))
