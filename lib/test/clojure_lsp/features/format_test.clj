(ns clojure-lsp.features.format-test
  (:require
   [clojure-lsp.feature.format :as f.format]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(deftest test-formatting
  (swap! (h/db*) shared/deep-merge {:project-root-uri (h/file-uri "file:///project")})
  (h/load-code-and-locs "(a  )\n(b c d)")
  (testing "when custom config file doesn't exists"
    (with-redefs [shared/file-exists? (constantly false)]
      (is (= "(a)\n(b c d)"
             (:new-text (first (f.format/formatting (h/file-uri "file:///a.clj") (h/db*))))))))
  (testing "when custom config file exists"
    (with-redefs [shared/file-exists? (constantly true)
                  slurp (constantly "{}")]
      (is (= "(a)\n(b c d)"
             (:new-text (first (f.format/formatting (h/file-uri "file:///a.clj") (h/db*)))))))))

(deftest test-formatting-noop
  (swap! (h/db*) shared/deep-merge {:project-root-uri (h/file-uri "file:///project")})
  (h/load-code-and-locs "(a)\n(b c d)")
  (with-redefs [shared/file-exists? (constantly false)]
    (let [r (f.format/formatting (h/file-uri "file:///a.clj") (h/db*))]
      (is (empty? r))
      (is (vector? r)))))

(defn range-formatting [code]
  (let [[[row col] [end-row end-col] :as positions] (h/load-code-and-locs code)]
    (let [position-count (count positions)]
       (assert (= 2 position-count) (format "Expected two cursors, got %s" position-count)))
    (f.format/range-formatting (h/file-uri "file:///a.clj") {:row row :col col :end-row end-row :end-col end-col} (h/db))))

(deftest test-range-formatting
  (swap! (h/db*) shared/deep-merge {:project-root-uri (h/file-uri "file:///project")})
  (testing "when custom config file doesn't exists"
    (with-redefs [shared/file-exists? (constantly false)]
      (is (= [{:range {:start {:line 0 :character 0}
                       :end {:line 0 :character 5}}
               :new-text "(a)"}]
             (range-formatting "|(a | )\n(b c d)")))))
  (testing "when custom config file exists"
    (with-redefs [shared/file-exists? (constantly true)
                  slurp (constantly "{}")]
      (is (= [{:range {:start {:line 0 :character 0}
                       :end {:line 0 :character 5}}
               :new-text "(a)"}]
             (range-formatting "|(a | )\n(b c d)")))))
  (testing "of several forms"
    (is (= [{:range {:start {:line 0 :character 0}
                     :end {:line 1 :character 5}}
             :new-text "(a)\n(b)"}]
           (range-formatting "(a | )\n|(b  )\n(c d)")))
    (is (= [{:range {:start {:line 0 :character 0}
                     :end {:line 1 :character 5}}
             :new-text "(a)\n(b)"}]
           (range-formatting "(a | )\n(|b  )\n(c d)")))
    (is (= [{:range {:start {:line 0 :character 0}
                     :end {:line 1 :character 0}}
             :new-text "(a)\n"}]
           (range-formatting "(a | )|\n(b  )\n(c d)")))
    (is (= [{:range {:start {:line 0 :character 0}
                     :end {:line 0 :character 5}}
             :new-text "(a)"}]
           (range-formatting "(a | |)\n(b  )\n(c d)")))))
