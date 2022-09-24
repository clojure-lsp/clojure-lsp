(ns clojure-lsp.clj-depend-test
  (:require
   [clojure-lsp.clj-depend :as clj-depend]
   [clojure.test :refer [deftest is testing]]))

(deftest config-with-source-paths-filled-test
  (testing "Should add source-paths to config when it is nil"
    (is (= {:source-paths #{"src" "test"}}
           (clj-depend/config-with-source-paths {} #{"src" "test"}))))

  (testing "Should add source-paths to config when it is empty"
    (is (= {:source-paths #{"src" "test"}}
           (clj-depend/config-with-source-paths {:source-paths #{}} #{"src" "test"}))))

  (testing "Should not override source-paths when it is not nil or empty"
    (is (= {:source-paths #{"src"}}
           (clj-depend/config-with-source-paths {:source-paths #{"src"}} #{"src" "test"})))))
