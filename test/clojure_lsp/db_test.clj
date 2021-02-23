(ns clojure-lsp.db-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.test-helper :as h]
   [clojure.string :as s]
   [clojure.test :refer [deftest testing is]]
   [clojure.java.io :as io]
   [clojure-lsp.shared :as shared]))

(h/reset-db-after-test)

(def project-path "/user/project")
(def default-db-path (s/join "/" [project-path ".lsp" "sqlite.db"]))

(deftest sqlite-db-file-setting
  (testing "when not set"
    (reset! db/db {})
    (is (= (-> default-db-path io/file .getAbsolutePath) (#'db/get-sqlite-db-file-path project-path))))
  (testing "when set to relative path"
    (let [settings-path "subdir/sqlite.db"
          expected (s/join "/" [project-path settings-path])]
      (reset! db/db {:settings {:sqlite-db-path settings-path}})
      (is (= (-> expected io/file .getAbsolutePath)
             (#'db/get-sqlite-db-file-path project-path)))))
  (testing "when set to absolute path"
    (let [settings-path (if shared/windows-os?
                          "D:/db-dir/sqlite.db"
                          "/db-dir/sqlite.db")]
      (reset! db/db {:settings {:sqlite-db-path settings-path}})
      (is (= (-> settings-path io/file .getAbsolutePath)
             (#'db/get-sqlite-db-file-path project-path))))))
