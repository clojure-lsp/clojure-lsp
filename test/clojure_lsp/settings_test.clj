(ns clojure-lsp.settings-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.settings :as settings]
   [clojure.test :refer [deftest is]]))

(deftest all-test
  (reset! db/db {:settings {:a {:b {:c 2}}}})
  (is (= {:a {:b {:c 2}}} (settings/all db/db))))

(deftest get-test
  (reset! db/db {:settings {:a {:b {:c 2}}}})
  (is (= 2 (settings/get db/db [:a :b :c])))
  (is (= {:c 2} (settings/get db/db [:a :b])))
  (is (= {:b {:c 2}} (settings/get db/db [:a])))
  (is (= 10 (settings/get db/db [:d] 10))))
