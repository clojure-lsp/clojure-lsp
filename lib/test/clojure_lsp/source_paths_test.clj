(ns clojure-lsp.source-paths-test
  (:require
   [clojure-lsp.source-paths :as source-paths]
   [clojure-lsp.test-helper :as h]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(deftest classpath->source-paths
  (testing "when classpath is empty"
    (is (= nil
           (#'source-paths/classpath->source-paths (.toPath (io/file "/project/root")) []))))
  (testing "when classpath has only external files"
    (is (= nil
           (#'source-paths/classpath->source-paths (.toPath (io/file "/project/root"))
                                                   ["/project/root/some/file.jar"
                                                    "/path/to/some/external/file"]))))
  (testing "when classpath has only project paths"
    (h/assert-submap
      {:origins #{:classpath}
       :source-paths ["/project/root/src" "/project/root/other-src"]
       :classpath-paths ["/project/root/src" "/project/root/other-src"]}
      (#'source-paths/classpath->source-paths (.toPath (io/file "/project/root"))
                                              ["/project/root/src"
                                               "/project/root/other-src"])))
  (testing "when classpath has project paths and external paths"
    (h/assert-submap
      {:origins #{:classpath}
       :source-paths ["/project/root/src" "/project/root/other-src"]
       :classpath-paths ["/project/root/src" "/project/root/other-src"]}
      (#'source-paths/classpath->source-paths (.toPath (io/file "/project/root"))
                                              ["/project/root/src"
                                               "/project/root/other-src"
                                               "/path/to/some/external/file"])))
  (testing "when classpath has project paths, external paths and project jar files"
    (h/assert-submap
      {:origins #{:classpath}
       :source-paths ["/project/root/src" "/project/root/other-src"]
       :classpath-paths ["/project/root/src" "/project/root/other-src"]}
      (#'source-paths/classpath->source-paths (.toPath (io/file "/project/root"))
                                              ["/project/root/src"
                                               "/project/root/other-src"
                                               "/project/root/some/file.jar"
                                               "/path/to/some/external/file"]))))
