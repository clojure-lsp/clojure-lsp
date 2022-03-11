(ns clojure-lsp.features.format-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.format :as f.format]
   [lsp4clj.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(deftest test-formatting
  (swap! db/db shared/deep-merge {:project-root-uri (h/file-uri "file:///project")})
  (h/load-code-and-locs "(a  )\n(b c d)")
  (testing "when custom config file doesn't exists"
    (with-redefs [shared/file-exists? (constantly false)]
      (is (= "(a)\n(b c d)"
             (:new-text (first (f.format/formatting (h/file-uri "file:///a.clj") db/db)))))))
  (testing "when custom config file exists"
    (with-redefs [shared/file-exists? (constantly true)
                  slurp (constantly "{}")]
      (is (= "(a)\n(b c d)"
             (:new-text (first (f.format/formatting (h/file-uri "file:///a.clj") db/db))))))))

(deftest test-formatting-noop
  (swap! db/db shared/deep-merge {:project-root-uri (h/file-uri "file:///project")})
  (h/load-code-and-locs "(a)\n(b c d)")
  (with-redefs [shared/file-exists? (constantly false)]
    (let [r (f.format/formatting (h/file-uri "file:///a.clj") db/db)]
      (is (empty? r))
      (is (vector? r)))))

(deftest test-range-formatting
  (swap! db/db shared/deep-merge {:project-root-uri (h/file-uri "file:///project")})
  (h/load-code-and-locs "(a  )\n(b c d)")
  (testing "when custom config file doesn't exists"
    (with-redefs [shared/file-exists? (constantly false)]
      (is (= [{:range {:start {:line 0 :character 0}
                       :end {:line 0 :character 5}}
               :new-text "(a)"}]
             (f.format/range-formatting (h/file-uri "file:///a.clj") {:row 1 :col 1 :end-row 1 :end-col 4} db/db)))))
  (testing "when custom config file exists"
    (with-redefs [shared/file-exists? (constantly true)
                  slurp (constantly "{}")]
      (is (= [{:range {:start {:line 0 :character 0}
                       :end {:line 0 :character 5}}
               :new-text "(a)"}]
             (f.format/range-formatting (h/file-uri "file:///a.clj") {:row 1 :col 1 :end-row 1 :end-col 4} db/db))))))
