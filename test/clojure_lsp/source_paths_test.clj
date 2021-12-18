(ns clojure-lsp.source-paths-test
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.source-paths :as source-paths]
   [clojure-lsp.test-helper :as h]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(h/reset-db-after-test)

(deftest resolve-deps-source-paths
  (testing "when paths and extra-paths don't exist"
    (with-redefs [config/read-edn-file (constantly {})]
      (is (= #{}
             (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root")))))
  (testing "when only paths exists"
    (with-redefs [config/read-edn-file (constantly {:paths ["a"]})]
      (is (= #{"a"}
             (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root")))))
  (testing "when only extra-paths exists"
    (with-redefs [config/read-edn-file (constantly {:extra-paths ["b"]})]
      (is (= #{"b"}
             (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root")))))
  (testing "when both exists"
    (with-redefs [config/read-edn-file (constantly {:paths ["a"] :extra-paths ["b"]})]
      (is (= #{"a" "b"}
             (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root")))))
  (testing "when paths contains a non existent alias"
    (with-redefs [config/read-edn-file (constantly {:paths ["a" :foo] :extra-paths ["b"]})]
      (is (= #{"a" "b"}
             (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root")))))
  (testing "when paths contains an existent alias"
    (with-redefs [config/read-edn-file (constantly {:paths ["a" :foo]
                                                    :extra-paths ["b"]
                                                    :aliases {:foo ["c" "d"]}})]
      (is (= #{"a" "b" "c" "d"}
             (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root")))))
  (testing "when paths contains multiple aliases"
    (with-redefs [config/read-edn-file (constantly {:paths ["a" :foo :bar :baz]
                                                    :extra-paths ["b"]
                                                    :aliases {:foo ["c" "d"]
                                                              :bar {:other :things}
                                                              :baz ["e"]}})]
      (is (= #{"a" "b" "c" "d" "e"}
             (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root")))))
  (testing "checking default source aliases"
    (testing "with default settings"
      (testing "when some default source alias is not present"
        (with-redefs [config/read-edn-file (constantly {:paths ["a"]
                                                        :extra-paths ["b"]
                                                        :aliases {:dev {:paths ["c"]}
                                                                  :bar {:other :things}
                                                                  :baz ["x"]}})]
          (is (= #{"a" "b" "c"}
                 (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root")))))
      (testing "when all source-aliases have paths"
        (with-redefs [config/read-edn-file (constantly {:paths ["a"]
                                                        :extra-paths ["b"]
                                                        :aliases {:dev {:paths ["c"]
                                                                        :extra-paths ["d" "e"]}
                                                                  :bar {:other :things}
                                                                  :test {:paths ["f" "g"]
                                                                         :extra-paths ["h"]}
                                                                  :baz ["x"]}})]
          (is (= #{"a" "b" "c" "d" "e" "f" "g" "h"}
                 (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root"))))))
    (testing "with custom source-aliases"
      (testing "when one of the specified alias does not exists"
        (with-redefs [config/read-edn-file (constantly {:aliases {:dev {:paths ["y"]}
                                                                  :bar {:other :things
                                                                        :paths ["a"]}
                                                                  :baz ["x"]}})]
          (is (= #{"a"}
                 (#'source-paths/resolve-deps-source-paths (io/file "deps-root")
                                                           {:source-aliases #{:foo :bar}} "/project/root")))))
      (testing "when all source aliases exists"
        (with-redefs [config/read-edn-file (constantly {:aliases {:dev {:paths ["y"]}
                                                                  :foo {:extra-paths ["b"]}
                                                                  :bar {:other :things
                                                                        :paths ["a"]}
                                                                  :baz ["x"]}})]
          (is (= #{"a" "b"}
                 (#'source-paths/resolve-deps-source-paths (io/file "deps-root")
                                                           {:source-aliases #{:foo :bar}} "/project/root")))))
      (testing "when settings exists but is nil"
        (with-redefs [config/read-edn-file (constantly {:aliases {:dev {:paths ["a"]}
                                                                  :foo {:extra-paths ["x"]}
                                                                  :bar {:other :things
                                                                        :paths ["y"]}
                                                                  :baz ["z"]}})]
          (is (= #{"a"}
                 (#'source-paths/resolve-deps-source-paths (io/file "deps-root")
                                                           {:source-aliases nil} "/project/root")))))))
  (testing "local-root"
    (testing "absolute path"
      (with-redefs [config/read-edn-file (fn [file]
                                           (if (= "deps-root" (str file))
                                             {:paths ["src"]
                                              :deps {'some.lib {:local/root "/some/lib"}}}
                                             {:paths ["foo" "bar"]}))
                    shared/file-exists? #(= "/project/root/../../some/lib/deps.edn" (str %))]
        (is (= #{"../../some/lib/foo" "../../some/lib/bar" "src"}
               (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root")))))
    (testing "single root from :deps"
      (with-redefs [config/read-edn-file (fn [file]
                                           (if (= "deps-root" (str file))
                                             {:paths ["src"]
                                              :deps {'some.lib {:local/root "./some/lib"}}}
                                             {:paths ["foo" "bar"]}))
                    shared/file-exists? #(= "/project/root/some/lib/deps.edn" (str %))]
        (is (= #{"some/lib/foo" "some/lib/bar" "src"}
               (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root")))))
    (testing "multiple root from :deps"
      (with-redefs [config/read-edn-file (fn [file]
                                           (case (str file)
                                             "deps-root"
                                             {:paths ["src"]
                                              :deps '{some.lib {:local/root "./some/lib"}
                                                      other-lib {:mvn/version "1.2.3"}
                                                      another.lib/foo {:local/root "../another/lib"}}}

                                             "/project/root/some/lib/deps.edn"
                                             {:paths ["foo" "bar"]}

                                             "/project/root/../another/lib/deps.edn"
                                             {:extra-paths ["baz"]}))
                    shared/file-exists? #(or (= "/project/root/some/lib/deps.edn" (str %))
                                             (= "/project/root/../another/lib/deps.edn" (str %)))]
        (is (= #{"some/lib/foo" "some/lib/bar" "src" "../another/lib/baz"}
               (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root")))))
    (testing "root on :dev alias"
      (with-redefs [config/read-edn-file (fn [file]
                                           (case (str file)
                                             "deps-root"
                                             {:paths ["src"]
                                              :deps '{some.lib {:local/root "./some/lib"}
                                                      other-lib {:mvn/version "1.2.3"}}
                                              :aliases {:dev {:extra-deps '{another.lib/foo {:local/root "../another/lib"}}}}}

                                             "/project/root/some/lib/deps.edn"
                                             {:paths ["foo" "bar"]}

                                             "/project/root/../another/lib/deps.edn"
                                             {:extra-paths ["baz"]}))
                    shared/file-exists? #(or (= "/project/root/some/lib/deps.edn" (str %))
                                             (= "/project/root/../another/lib/deps.edn" (str %)))]
        (is (= #{"some/lib/foo" "some/lib/bar" "src" "../another/lib/baz"}
               (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root")))))
    (testing "nested local/root"
      (with-redefs [config/read-edn-file (fn [file]
                                           (case (str file)
                                             "deps-root"
                                             {:paths ["src"]
                                              :deps '{some.lib {:local/root "./some/lib"}}}

                                             "/project/root/some/lib/deps.edn"
                                             {:paths ["foo"]
                                              :deps '{other.lib {:local/root "../other"}}}

                                             "/project/root/some/other/deps.edn"
                                             {:extra-paths ["bar"]}))
                    shared/file-exists? #(or (= "/project/root/some/lib/deps.edn" (str %))
                                             (= "/project/root/./some/lib/../other/deps.edn" (str %))
                                             (= "/project/root/some/other/deps.edn" (str %)))]
        (is (= #{"some/lib/foo" "src" "some/other/bar"}
               (#'source-paths/resolve-deps-source-paths (io/file "deps-root") {} "/project/root")))))))

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
                  shared/file-exists? (complement nil?)
                  config/read-edn-file (constantly {:paths ["some" "paths"]})]
      (is (= {:origins #{:deps-edn}
              :source-paths #{"some" "paths"}
              :deps-source-paths #{"some" "paths"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is a deps.edn without :paths"
    (with-redefs [shared/to-file #(when (= "deps.edn" %2) (io/file ""))
                  shared/file-exists? (complement nil?)
                  config/read-edn-file (constantly {})]
      (is (= {:origins #{:empty-deps-edn}
              :source-paths #{"src" "test"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is a project.clj with valid :source-paths"
    (with-redefs [shared/to-file #(when (= "project.clj" %2) (io/file ""))
                  shared/file-exists? (complement nil?)
                  z/of-file (constantly (z/of-string "(defproject project \"0.1\" :source-paths [\"some\" \"paths\"])"))]
      (is (= {:origins #{:leiningen}
              :source-paths #{"paths" "some" "test" "src/test/clojure"}
              :lein-source-paths #{"paths" "some" "test" "src/test/clojure"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is a project.clj without :source-paths"
    (with-redefs [shared/to-file #(when (= "project.clj" %2) (io/file ""))
                  shared/file-exists? (complement nil?)
                  z/of-file (constantly nil)
                  parser/lein-zloc->edn (constantly nil)]
      (is (= {:origins #{:empty-leiningen}
              :source-paths #{"src" "test"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is a bb.edn with valid :paths"
    (with-redefs [shared/to-file #(when (= "bb.edn" %2) (io/file ""))
                  shared/file-exists? (complement nil?)
                  config/read-edn-file (constantly {:paths ["some" 'paths]})]
      (is (= {:origins #{:bb}
              :source-paths #{"some" "paths"}
              :bb-source-paths #{"some" "paths"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is a bb.edn without :paths"
    (with-redefs [shared/to-file #(when (= "bb.edn" %2) (io/file ""))
                  shared/file-exists? (complement nil?)
                  config/read-edn-file (constantly {})]
      (is (= {:origins #{:empty-bb}
              :source-paths #{"src" "test" "script" "scripts"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is a deps.edn with :paths and bb.edn with :paths"
    (with-redefs [shared/to-file #(case %2
                                    "deps.edn" (io/file "deps")
                                    "bb.edn" (io/file "bb")
                                    nil)
                  shared/file-exists? (complement nil?)
                  config/read-edn-file #(if (= "deps" (.getName %1))
                                          {:paths ["some" "paths"]}
                                          {:paths ["scripts"]})]
      (is (= {:origins #{:deps-edn :bb}
              :bb-source-paths #{"scripts"}
              :deps-source-paths #{"some" "paths"}
              :source-paths #{"some" "paths" "scripts"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is a deps.edn with :paths and empty project.clj"
    (with-redefs [shared/to-file #(case %2
                                    "deps.edn" (io/file "deps")
                                    "project.clj" (io/file "lein")
                                    nil)
                  shared/file-exists? (complement nil?)
                  z/of-file (constantly nil)
                  config/read-edn-file #(if (= "deps" (.getName %1))
                                          {:paths ["some" "paths"]}
                                          nil)]
      (is (= {:origins #{:deps-edn :empty-leiningen}
              :deps-source-paths #{"some" "paths"}
              :source-paths #{"some" "paths" "src" "test"}}
             (#'source-paths/resolve-source-paths root-path {} nil)))))
  (testing "when there is no file, fallback to default source paths"
    (with-redefs [shared/to-file (constantly nil)
                  shared/file-exists? (complement nil?)]
      (is (= {:origins #{:default}
              :source-paths #{"src" "test"}}
             (#'source-paths/resolve-source-paths root-path {} nil))))))
