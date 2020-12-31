(ns clojure-lsp.shared-test
  (:require [clojure-lsp.shared :as shared]
            [clojure.test :refer [deftest testing is]]))

(deftest uri->project-related-path
  (is (= "/src/my-project/some/ns.clj"
        (shared/uri->project-related-path "file://home/foo/bar/my-project/src/my-project/some/ns.clj" "file://home/foo/bar/my-project"))))

(deftest ->range-test
  (testing "should subtract 1 from row and col values"
    (is (= {:start {:line      1
                    :character 1}
            :end   {:line      1
                    :character 1}}
           (shared/->range {:row 2 :end-row 2 :col 2 :end-col 2}))))
  (testing "should not return negative line and character values"
    (is (= {:start {:line      0
                    :character 0}
            :end   {:line      0
                    :character 0}}
           (shared/->range {:row 0 :end-row 0 :col 0 :end-col 0})))))
