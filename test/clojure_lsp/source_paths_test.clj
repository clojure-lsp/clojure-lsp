(ns clojure-lsp.source-paths-test
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.source-paths :as source-paths]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(deftest resolve-deps-source-paths
  (testing "when on not a deps.edn project"
    (is (= #{} (#'source-paths/resolve-deps-source-paths nil {}))))
  (testing "when paths and extra-paths does not exist"
    (is (= #{}
           (#'source-paths/resolve-deps-source-paths {} {}))))
  (testing "when only paths exists"
    (is (= #{"a"}
           (#'source-paths/resolve-deps-source-paths {:paths ["a"]} {}))))
  (testing "when only extra-paths exists"
    (is (= #{"b"}
           (#'source-paths/resolve-deps-source-paths {:extra-paths ["b"]} {}))))
  (testing "when both exists"
    (is (= #{"a" "b"}
           (#'source-paths/resolve-deps-source-paths {:paths ["a"] :extra-paths ["b"]} {}))))
  (testing "when paths contains a non existent alias"
    (is (= #{"a" "b"}
           (#'source-paths/resolve-deps-source-paths {:paths ["a" :foo] :extra-paths ["b"]} {}))))
  (testing "when paths contains an existent alias"
    (is (= #{"a" "b" "c" "d"}
           (#'source-paths/resolve-deps-source-paths {:paths ["a" :foo]
                                                      :extra-paths ["b"]
                                                      :aliases {:foo ["c" "d"]}} {}))))
  (testing "when paths contains multiple aliases"
    (is (= #{"a" "b" "c" "d" "e"}
           (#'source-paths/resolve-deps-source-paths {:paths ["a" :foo :bar :baz]
                                                      :extra-paths ["b"]
                                                      :aliases {:foo ["c" "d"]
                                                                :bar {:other :things}
                                                                :baz ["e"]}} {}))))
  (testing "checking default source aliases"
    (testing "with default settings"
      (testing "when some default source alias is not present"
        (is (= #{"a" "b" "c"}
               (#'source-paths/resolve-deps-source-paths {:paths ["a"]
                                                          :extra-paths ["b"]
                                                          :aliases {:dev {:paths ["c"]}
                                                                    :bar {:other :things}
                                                                    :baz ["x"]}} {}))))
      (testing "when all source-aliases have paths"
        (is (= #{"a" "b" "c" "d" "e" "f" "g" "h"}
               (#'source-paths/resolve-deps-source-paths {:paths ["a"]
                                                          :extra-paths ["b"]
                                                          :aliases {:dev {:paths ["c"]
                                                                          :extra-paths ["d" "e"]}
                                                                    :bar {:other :things}
                                                                    :test {:paths ["f" "g"]
                                                                           :extra-paths ["h"]}
                                                                    :baz ["x"]}} {})))))
    (testing "with custom source-aliases"
      (testing "when one of the specified alias does not exists"
        (is (= #{"a"}
               (#'source-paths/resolve-deps-source-paths {:aliases {:dev {:paths ["y"]}
                                                                    :bar {:other :things
                                                                          :paths ["a"]}
                                                                    :baz ["x"]}}
                                                         {:source-aliases #{:foo :bar}}))))
      (testing "when all source aliases exists"
        (is (= #{"a" "b"}
               (#'source-paths/resolve-deps-source-paths {:aliases {:dev {:paths ["y"]}
                                                                    :foo {:extra-paths ["b"]}
                                                                    :bar {:other :things
                                                                          :paths ["a"]}
                                                                    :baz ["x"]}}
                                                         {:source-aliases #{:foo :bar}}))))
      (testing "when settings exists but is nil"
        (is (= #{"a"}
               (#'source-paths/resolve-deps-source-paths {:aliases {:dev {:paths ["a"]}
                                                                    :foo {:extra-paths ["x"]}
                                                                    :bar {:other :things
                                                                          :paths ["y"]}
                                                                    :baz ["z"]}}
                                                         {:source-aliases nil})))))))

(deftest resolve-lein-source-paths
  (testing "when on not a lein project"
    (is (= nil (#'source-paths/resolve-lein-source-paths nil {}))))
  (testing "when source-paths and test-paths does not exist"
    (is (= #{"src" "src/main/clojure" "test" "src/test/clojure"}
           (#'source-paths/resolve-lein-source-paths {} {}))))
  (testing "when only source-paths exists"
    (is (= #{"a" "test" "src/test/clojure"}
           (#'source-paths/resolve-lein-source-paths {:source-paths ["a"]} {}))))
  (testing "when only test-paths exists"
    (is (= #{"b" "src" "src/main/clojure"}
           (#'source-paths/resolve-lein-source-paths {:test-paths ["b"]} {}))))
  (testing "when both exists"
    (is (= #{"a" "b"}
           (#'source-paths/resolve-lein-source-paths {:source-paths ["a"] :test-paths ["b"]} {}))))
  (testing "when paths contains a non existent alias"
    (is (= #{"a" "b"}
           (#'source-paths/resolve-lein-source-paths {:source-paths ["a" :foo] :test-paths ["b"]} {}))))
  (testing "when paths contains an existent alias"
    (is (= #{"a" "b" "c" "d"}
           (#'source-paths/resolve-lein-source-paths {:source-paths ["a" :foo]
                                                      :test-paths ["b"]
                                                      :profiles {:foo ["c" "d"]}} {}))))
  (testing "when paths contains multiple aliases"
    (is (= #{"a" "b" "c" "d" "e"}
           (#'source-paths/resolve-lein-source-paths {:source-paths ["a" :foo :bar :baz]
                                                      :test-paths ["b"]
                                                      :profiles {:foo ["c" "d"]
                                                                 :bar {:other :things}
                                                                 :baz ["e"]}} {}))))
  (testing "when source-paths are not static"
    (is (= #{"src" "src/main/clojure" "b"}
           (#'source-paths/resolve-lein-source-paths '{:test-paths ["b"]
                                                       :source-paths ~(get-path "lib" ["clojure"])} {})))
    (is (= #{"test" "src/test/clojure" "b"}
           (#'source-paths/resolve-lein-source-paths '{:source-paths ["b"]
                                                       :test-paths (get-path "lib" ["clojure"])} {}))))
  (testing "checking default source aliases"
    (testing "with default settings"
      (testing "when some default source alias is not present"
        (is (= #{"a" "b" "c"}
               (#'source-paths/resolve-lein-source-paths {:source-paths ["a"]
                                                          :test-paths ["b"]
                                                          :profiles {:dev {:source-paths ["c"]}
                                                                     :bar {:other :things}
                                                                     :baz ["x"]}} {}))))
      (testing "when all source-aliases have paths"
        (is (= #{"a" "b" "c" "d" "e" "f" "g" "h"}
               (#'source-paths/resolve-lein-source-paths {:source-paths ["a"]
                                                          :test-paths ["b"]
                                                          :profiles {:dev {:source-paths ["c"]
                                                                           :test-paths ["d" "e"]}
                                                                     :bar {:other :things}
                                                                     :test {:source-paths ["f" "g"]
                                                                            :test-paths ["h"]}
                                                                     :baz ["x"]}} {})))))
    (testing "with custom source-aliases"
      (testing "when one of the specified alias does not exists"
        (is (= #{"a" "src" "src/main/clojure" "test" "src/test/clojure"}
               (#'source-paths/resolve-lein-source-paths {:profiles {:dev {:source-paths ["y"]}
                                                                     :bar {:other :things
                                                                           :source-paths ["a"]}
                                                                     :baz ["x"]}}
                                                         {:source-aliases #{:foo :bar}}))))
      (testing "when all source aliases exists"
        (is (= #{"a" "b" "src" "src/main/clojure" "test" "src/test/clojure"}
               (#'source-paths/resolve-lein-source-paths {:profiles {:dev {:source-paths ["y"]}
                                                                     :foo {:test-paths ["b"]}
                                                                     :bar {:other :things
                                                                           :source-paths ["a"]}
                                                                     :baz ["x"]}}
                                                         {:source-aliases #{:foo :bar}}))))
      (testing "when settings exists but is nil"
        (is (= #{"a" "src" "src/main/clojure" "test" "src/test/clojure"}
               (#'source-paths/resolve-lein-source-paths {:profiles {:dev {:source-paths ["a"]}
                                                                     :foo {:test-paths ["x"]}
                                                                     :bar {:other :things
                                                                           :source-paths ["y"]}
                                                                     :baz ["z"]}}
                                                         {:source-aliases nil})))))))

(def root-path "/")

(deftest resolve-source-paths
  (testing "when source-paths are provided via settings"
    (is (= {:origins #{:settings}
            :source-paths #{"some" "paths"}}
           (#'source-paths/resolve-source-paths root-path {} #{"some" "paths"}))))
  (testing "when there is a deps.edn with valid :paths"
    (with-redefs [shared/to-file #(when (= "deps.edn" %2) (io/file ""))
                  source-paths/exists? (complement nil?)
                  config/read-edn-file (constantly {:paths ["some" "paths"]})]
      (is (= {:origins #{:deps-edn}
              :source-paths #{"some" "paths"}
              :deps-source-paths #{"some" "paths"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is a deps.edn without :paths"
    (with-redefs [shared/to-file #(when (= "deps.edn" %2) (io/file ""))
                  source-paths/exists? (complement nil?)
                  config/read-edn-file (constantly {})]
      (is (= {:origins #{:empty-deps-edn}
              :source-paths #{"src" "test"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is a project.clj with valid :source-paths"
    (with-redefs [shared/to-file #(when (= "project.clj" %2) (io/file ""))
                  source-paths/exists? (complement nil?)
                  z/of-file (constantly (z/of-string "(defproject project \"0.1\" :source-paths [\"some\" \"paths\"])"))]
      (is (= {:origins #{:leiningen}
              :source-paths #{"paths" "some" "test" "src/test/clojure"}
              :lein-source-paths #{"paths" "some" "test" "src/test/clojure"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is a project.clj without :source-paths"
    (with-redefs [shared/to-file #(when (= "project.clj" %2) (io/file ""))
                  source-paths/exists? (complement nil?)
                  z/of-file (constantly nil)
                  parser/lein-zloc->edn (constantly nil)]
      (is (= {:origins #{:empty-leiningen}
              :source-paths #{"src" "test"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is a bb.edn with valid :paths"
    (with-redefs [shared/to-file #(when (= "bb.edn" %2) (io/file ""))
                  source-paths/exists? (complement nil?)
                  config/read-edn-file (constantly {:paths ["some" "paths"]})]
      (is (= {:origins #{:bb}
              :source-paths #{"some" "paths"}
              :bb-source-paths #{"some" "paths"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is a bb.edn without :paths"
    (with-redefs [shared/to-file #(when (= "bb.edn" %2) (io/file ""))
                  source-paths/exists? (complement nil?)
                  config/read-edn-file (constantly {})]
      (is (= {:origins #{:empty-bb}
              :source-paths #{"src" "test" "script" "scripts"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is a deps.edn with :paths and bb.edn with :paths"
    (with-redefs [shared/to-file #(case %2
                                    "deps.edn" (io/file "deps")
                                    "bb.edn" (io/file "bb")
                                    nil)
                  source-paths/exists? (complement nil?)
                  config/read-edn-file #(if (= "deps" (.getName %1))
                                          {:paths ["some" "paths"]}
                                          {:paths ["scripts"]})]
      (is (= {:origins #{:deps-edn :bb}
              :bb-source-paths #{"scripts"}
              :deps-source-paths #{"some" "paths"}
              :source-paths #{"some" "paths" "scripts"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is no file, fallback to default source paths"
    (with-redefs [shared/to-file (constantly nil)
                  source-paths/exists? (complement nil?)]
      (is (= {:origins #{:default}
              :source-paths #{"src" "test"}}
             (#'source-paths/resolve-source-paths root-path {} nil))))))
