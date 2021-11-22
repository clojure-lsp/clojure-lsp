(ns clojure-lsp.settings-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is]]))

(h/reset-db-after-test)

(deftest all-test
  (swap! db/db shared/deep-merge {:settings {:a {:b {:c 2}}}})
  (is (= {:a {:b {:c 2}}} (settings/all db/db))))

(deftest get-test
  (swap! db/db shared/deep-merge {:settings {:a {:b {:c 2}}}})
  (is (= 2 (settings/get db/db [:a :b :c])))
  (is (= {:c 2} (settings/get db/db [:a :b])))
  (is (= {:b {:c 2}} (settings/get db/db [:a])))
  (is (= 10 (settings/get db/db [:d] 10))))
