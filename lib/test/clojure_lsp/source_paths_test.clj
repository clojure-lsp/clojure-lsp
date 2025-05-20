(ns clojure-lsp.source-paths-test
  (:require
   [clojure-lsp.source-paths :as source-paths]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(deftest classpath->source-paths
  (testing "when classpath is empty"
    (is (= nil
           (#'source-paths/classpath->source-paths (.toPath (io/file (h/file-path "/project/root"))) []))))
  (testing "when classpath has only external files"
    (is (= nil
           (#'source-paths/classpath->source-paths (.toPath (io/file (h/file-path "/project/root")))
                                                   (mapv h/file-path ["/project/root/some/file.jar"
                                                                      "/path/to/some/external/file"])))))
  (testing "when classpath has only project paths"
    (h/assert-submap
      {:origins #{:classpath}
       :source-paths (mapv h/file-path ["/project/root/src" "/project/root/other-src"])
       :classpath-paths (mapv h/file-path ["/project/root/src" "/project/root/other-src"])}
      (#'source-paths/classpath->source-paths (.toPath (io/file (h/file-path "/project/root")))
                                              (mapv h/file-path ["/project/root/src"
                                                                 "/project/root/other-src"]))))
  (testing "when classpath has project paths and external paths"
    (h/assert-submap
      {:origins #{:classpath}
       :source-paths (mapv h/file-path ["/project/root/src" "/project/root/other-src"])
       :classpath-paths (mapv h/file-path ["/project/root/src" "/project/root/other-src"])}
      (#'source-paths/classpath->source-paths (.toPath (io/file (h/file-path "/project/root")))
                                              (mapv h/file-path ["/project/root/src"
                                                                 "/project/root/other-src"
                                                                 "/path/to/some/external/file"]))))
  (testing "when classpath has project paths, external paths and project jar files"
    (h/assert-submap
      {:origins #{:classpath}
       :source-paths (mapv h/file-path ["/project/root/src" "/project/root/other-src"])
       :classpath-paths (mapv h/file-path ["/project/root/src" "/project/root/other-src"])}
      (#'source-paths/classpath->source-paths (.toPath (io/file (h/file-path "/project/root")))
                                              (mapv h/file-path ["/project/root/src"
                                                                 "/project/root/other-src"
                                                                 "/project/root/some/file.jar"
                                                                 "/path/to/some/external/file"]))))

  (testing "when root-path is in a different current directory"
    (h/assert-submap
      {:origins #{:classpath}
       :source-paths (mapv h/file-path ["/project/root/src" "/project/root/other-src"])
       :classpath-paths (mapv h/file-path ["/project/root/src" "/project/root/other-src"])}
      (#'source-paths/classpath->source-paths (.toPath (io/file (h/file-path "/project/root")))
                                              (mapv h/file-path ["src"
                                                                 "other-src"
                                                                 "/foo/bar"])))))

(deftest absolutize-source-paths-test
  (is (= [(h/file-path "/project/root/src")
          (h/file-path "/project/root/test")]
         (#'source-paths/absolutize-source-paths ["src" "test"]
                                                 (.toPath (io/file (h/file-path "/project/root")))
                                                 ["resources.*" "target.*"]))))
