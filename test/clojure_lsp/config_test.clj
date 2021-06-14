(ns clojure-lsp.config-test
  (:require
    [clojure-lsp.config :as config]
    [clojure-lsp.test-helper :as h]
    [clojure.test :refer [deftest is testing]]))

(deftest resolve-config
  (testing "when user doesn't have a home config or a project config"
    (with-redefs [config/get-property (constantly nil)
                  config/file-exists? (constantly nil)
                  config/get-env (constantly nil)]
      (is (= {} (config/resolve-config (h/file-uri "file:///home/user/some/project/"))))))
  (testing "when user doesn't have a home config but has a project config"
    (with-redefs [config/get-property (constantly nil)
                  config/get-env (constantly nil)
                  config/file-exists? #(= (h/file-path "/home/user/some/project/.lsp/config.edn") (str %))
                  slurp (constantly "{:foo {:bar 1}}")]
      (is (= {:foo {:bar 1}} (config/resolve-config (h/file-uri "file:///home/user/some/project/"))))))
  (testing "when user has a home config but doesn't have a project config"
    (with-redefs [config/get-property (constantly (h/file-path "/home/user"))
                  config/get-env (constantly nil)
                  config/file-exists? #(= (h/file-path "/home/user/.lsp/config.edn") (str %))
                  slurp (constantly "{:foo {:bar 1}}")]
      (is (= {:foo {:bar 1}} (config/resolve-config (h/file-uri "file:///home/user/some/project/"))))))
  (testing "when user has a home config and a project config we merge with project as priority"
    (with-redefs [config/get-property (constantly (h/file-path "/home/user"))
                  config/get-env (constantly nil)
                  config/file-exists? #(or (= (h/file-path "/home/user/.lsp/config.edn") (str %))
                                           (= (h/file-path "/home/user/some/project/.lsp/config.edn") (str %)))
                  slurp (fn [f]
                          (if (= (h/file-path "/home/user/.lsp/config.edn") (str f))
                            "{:foo {:bar 1 :baz 1} :home :bla}"
                            "{:foo {:baz 2}}"))]
      (is (= {:foo {:bar 1
                    :baz 2}
              :home :bla} (config/resolve-config (h/file-uri "file:///home/user/some/project/")))))))

(deftest resolve-deps-source-paths
  (testing "when on not a deps.edn project"
    (is (= #{} (config/resolve-deps-source-paths nil {}))))
  (testing "when paths and extra-paths does not exist"
    (is (= #{}
           (config/resolve-deps-source-paths {} {}))))
  (testing "when only paths exists"
    (is (= #{"a"}
           (config/resolve-deps-source-paths {:paths ["a"]} {}))))
  (testing "when only extra-paths exists"
    (is (= #{"b"}
           (config/resolve-deps-source-paths {:extra-paths ["b"]} {}))))
  (testing "when both exists"
    (is (= #{"a" "b"}
           (config/resolve-deps-source-paths {:paths ["a"] :extra-paths ["b"]} {}))))
  (testing "when paths contains a non existent alias"
    (is (= #{"a" "b"}
           (config/resolve-deps-source-paths {:paths ["a" :foo] :extra-paths ["b"]} {}))))
  (testing "when paths contains an existent alias"
    (is (= #{"a" "b" "c" "d"}
           (config/resolve-deps-source-paths {:paths ["a" :foo]
                                              :extra-paths ["b"]
                                              :aliases {:foo ["c" "d"]}} {}))))
  (testing "when paths contains multiple aliases"
    (is (= #{"a" "b" "c" "d" "e"}
           (config/resolve-deps-source-paths {:paths ["a" :foo :bar :baz]
                                              :extra-paths ["b"]
                                              :aliases {:foo ["c" "d"]
                                                        :bar {:other :things}
                                                        :baz ["e"]}} {}))))
  (testing "checking default source aliases"
    (testing "with default settings"
      (testing "when some default source alias is not present"
        (is (= #{"a" "b" "c"}
               (config/resolve-deps-source-paths {:paths ["a"]
                                                  :extra-paths ["b"]
                                                  :aliases {:dev {:paths ["c"]}
                                                            :bar {:other :things}
                                                            :baz ["x"]}} {}))))
      (testing "when all source-aliases have paths"
        (is (= #{"a" "b" "c" "d" "e" "f" "g" "h"}
               (config/resolve-deps-source-paths {:paths ["a"]
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
               (config/resolve-deps-source-paths {:aliases {:dev {:paths ["y"]}
                                                            :bar {:other :things
                                                                  :paths ["a"]}
                                                            :baz ["x"]}}
                                                 {:source-aliases #{:foo :bar}}))))
      (testing "when all source aliases exists"
        (is (= #{"a" "b"}
               (config/resolve-deps-source-paths {:aliases {:dev {:paths ["y"]}
                                                            :foo {:extra-paths ["b"]}
                                                            :bar {:other :things
                                                                  :paths ["a"]}
                                                            :baz ["x"]}}
                                                 {:source-aliases #{:foo :bar}}))))
      (testing "when settings exists but is nil"
        (is (= #{"a"}
               (config/resolve-deps-source-paths {:aliases {:dev {:paths ["a"]}
                                                            :foo {:extra-paths ["x"]}
                                                            :bar {:other :things
                                                                  :paths ["y"]}
                                                            :baz ["z"]}}
                                                 {:source-aliases nil})))))))
