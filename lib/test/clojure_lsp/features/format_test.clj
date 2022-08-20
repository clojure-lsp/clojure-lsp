(ns clojure-lsp.features.format-test
  (:require
   [clojure-lsp.feature.format :as f.format]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(deftest test-formatting
  (let [components (h/make-components {:project-root-uri (h/file-uri "file:///project")})]
    (h/load-code "(a  )\n(b c d)" h/default-uri components)
    (testing "when custom config file doesn't exists"
      (with-redefs [shared/file-exists? (constantly false)]
        (is (= "(a)\n(b c d)"
               (:new-text (first (f.format/formatting h/default-uri components)))))))
    (testing "when custom config file exists"
      (with-redefs [shared/file-exists? (constantly true)
                    slurp (constantly "{}")]
        (is (= "(a)\n(b c d)"
               (:new-text (first (f.format/formatting h/default-uri components)))))))))

(deftest test-formatting-noop
  (let [components (h/make-components {:project-root-uri (h/file-uri "file:///project")})]
    (with-redefs [shared/file-exists? (constantly false)]
      (let [r (f.format/formatting (h/file-uri "file:///a.clj") components)]
        (is (empty? r))
        (is (vector? r))))))

(defn range-formatting [components code]
  (let [[[row col] [end-row end-col] :as positions] (h/load-code code h/default-uri components)
        db (h/db components)]
    (let [position-count (count positions)]
      (assert (= 2 position-count) (format "Expected two cursors, got %s" position-count)))
    (f.format/range-formatting h/default-uri {:row row :col col :end-row end-row :end-col end-col} db)))

(deftest test-range-formatting
  (let [components (h/make-components {:project-root-uri (h/file-uri "file:///project")})]
    (testing "when custom config file doesn't exists"
      (with-redefs [shared/file-exists? (constantly false)]
        (is (= [{:range {:start {:line 0 :character 0}
                         :end {:line 0 :character 5}}
                 :new-text "(a)"}]
               (range-formatting components "|(a | )\n(b c d)")))))
    (testing "when custom config file exists"
      (with-redefs [shared/file-exists? (constantly true)
                    slurp (constantly "{}")]
        (is (= [{:range {:start {:line 0 :character 0}
                         :end {:line 0 :character 5}}
                 :new-text "(a)"}]
               (range-formatting components "|(a | )\n(b c d)")))))
    (testing "of several forms"
      (is (= [{:range {:start {:line 0 :character 0}
                       :end {:line 1 :character 5}}
               :new-text "(a)\n(b)"}]
             (range-formatting components "(a | )\n|(b  )\n(c d)")))
      (is (= [{:range {:start {:line 0 :character 0}
                       :end {:line 1 :character 5}}
               :new-text "(a)\n(b)"}]
             (range-formatting components "(a | )\n(|b  )\n(c d)")))
      (is (= [{:range {:start {:line 0 :character 0}
                       :end {:line 1 :character 0}}
               :new-text "(a)\n"}]
             (range-formatting components "(a | )|\n(b  )\n(c d)")))
      (is (= [{:range {:start {:line 0 :character 0}
                       :end {:line 0 :character 5}}
               :new-text "(a)"}]
             (range-formatting components "(a | |)\n(b  )\n(c d)"))))))
