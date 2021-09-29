(ns clojure-lsp.config-test
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(deftest resolve-config
  (testing "when user doesn't have a home config or a project config"
    (with-redefs [config/get-property (constantly nil)
                  shared/file-exists? (constantly nil)
                  config/get-env (constantly nil)]
      (is (= {} (config/resolve-for-root (h/file-uri "file:///home/user/some/project/"))))))
  (testing "when user doesn't have a home config but has a project config"
    (with-redefs [config/get-property (constantly nil)
                  config/get-env (constantly nil)
                  shared/file-exists? #(= (h/file-path "/home/user/some/project/.lsp/config.edn") (str %))
                  slurp (constantly "{:foo {:bar 1}}")]
      (is (= {:foo {:bar 1}} (config/resolve-for-root (h/file-uri "file:///home/user/some/project/"))))))
  (testing "when user has a home config but doesn't have a project config"
    (with-redefs [config/get-property (constantly (h/file-path "/home/user"))
                  config/get-env (constantly nil)
                  shared/file-exists? #(= (h/file-path "/home/user/.lsp/config.edn") (str %))
                  slurp (constantly "{:foo {:bar 1}}")]
      (is (= {:foo {:bar 1}} (config/resolve-for-root (h/file-uri "file:///home/user/some/project/"))))))
  (testing "when user has a home config and a project config we merge with project as priority"
    (with-redefs [config/get-property (constantly (h/file-path "/home/user"))
                  config/get-env (constantly nil)
                  shared/file-exists? #(or (= (h/file-path "/home/user/.lsp/config.edn") (str %))
                                           (= (h/file-path "/home/user/some/project/.lsp/config.edn") (str %)))
                  slurp (fn [f]
                          (if (= (h/file-path "/home/user/.lsp/config.edn") (str f))
                            "{:foo {:bar 1 :baz 1} :home :bla}"
                            "{:foo {:baz 2}}"))]
      (is (= {:foo {:bar 1
                    :baz 2}
              :home :bla} (config/resolve-for-root (h/file-uri "file:///home/user/some/project/"))))))
  (testing "when XDG_CONFIG_HOME is unset but user has XDG config and a project config we merge with project as priority"
    (with-redefs [config/get-property (constantly "/home/user")
                  shared/file-exists? #(or (= (h/file-path "/home/user/.config/.lsp/config.edn") (str %))
                                           (= (h/file-path "/home/user/some/project/.lsp/config.edn") (str %)))
                  config/get-env (constantly nil)
                  slurp (fn [f]
                          (if (= (h/file-path "/home/user/.config/.lsp/config.edn") (str f))
                            "{:foo {:bar 1 :baz 1} :xdg :bla}"
                            "{:foo {:baz 2}}"))]
      (is (= {:foo {:bar 1
                    :baz 2}
              :xdg :bla} (config/resolve-for-root (h/file-uri "file:///home/user/some/project/"))))))
  (testing "when XDG_CONFIG_HOME is set and both global and a project config exist we merge with project as priority"
    (with-redefs [config/get-property (constantly "/home/user")
                  shared/file-exists? #(or (= (h/file-path "/tmp/config/.lsp/config.edn") (str %))
                                           (= (h/file-path "/home/user/.config/.lsp/config.edn") (str %))
                                           (= (h/file-path "/home/user/some/project/.lsp/config.edn") (str %)))
                  config/get-env (constantly "/tmp/config")
                  slurp (fn [f]
                          (cond (= (h/file-path "/tmp/config/.lsp/config.edn") (str f))
                                "{:foo {:bar 1 :baz 1} :xdg :tmp}"
                                (= (h/file-path "/home/user/.config/.lsp/config.edn") (str f))
                                "{:foo {:bar 1 :baz 2} :xdg :home.config}"
                                :else
                                "{:foo {:baz 3}}"))]
      (is (= {:foo {:bar 1
                    :baz 3}
              :xdg :tmp} (config/resolve-for-root (h/file-uri "file:///home/user/some/project/")))))))

(deftest resolve-from-classpath-config-paths
  (testing "when empty classpath and no classpath-config-paths is provided"
    (is (= nil
           (config/resolve-from-classpath-config-paths []
                                                       {}))))
  (testing "when no classpath-config-paths is provided and classpath has no libs with clojure-lsp config"
    (is (= nil
           (config/resolve-from-classpath-config-paths ["/my/lib.jar"]
                                                       {}))))
  (testing "when classpath-config-paths is provided and classpath has no libs with clojure-lsp config"
    (with-redefs [shared/file-exists? (constantly true)
                  config/jar-file->config (constantly nil)]
      (is (= nil
             (config/resolve-from-classpath-config-paths ["/my/lib.jar"]
                                                         {:classpath-config-paths ["my/other-lib"]})))))
  (testing "when classpath-config-paths is provided and classpath has no libs with clojure-lsp config"
    (with-redefs [shared/file-exists? (constantly true)
                  config/jar-file->config (constantly {:a 1 :b {:c 2 :d 3}})]
      (is (= {:a 1 :b {:c 2 :d 3}}
             (config/resolve-from-classpath-config-paths ["/my/lib.jar"]
                                                         {:classpath-config-paths ["my/other-lib"]}))))))

(deftest deep-merge-fixing-cljfmt
  (is (= {} (#'config/deep-merge-fixing-cljfmt {} {})))
  (is (= {:a 1 :b 2} (#'config/deep-merge-fixing-cljfmt {:a 1} {:b 2})))
  (is (= {:a {:b 1 :c 2}} (#'config/deep-merge-fixing-cljfmt {:a {:b 1}} {:a {:c 2}})))
  (is (= {:a 1 :b 2 :cljfmt {:indents {'a [[:block 0]]}}}
         (#'config/deep-merge-fixing-cljfmt {:a 1 :cljfmt {:indents {'a [[:block 0]]}}}
                                            {:b 2})))
  (is (= {:a 1 :b 2 :cljfmt {:indents {'a [[:block 0]]}}}
         (#'config/deep-merge-fixing-cljfmt {:a 1}
                                            {:b 2 :cljfmt {:indents {'a [[:block 0]]}}})))
  (is (= {:a 1 :b 2 :cljfmt {:indents {'a [[:block 1]]}}}
         (#'config/deep-merge-fixing-cljfmt {:a 1 :cljfmt {:indents {'a [[:block 0]]}}}
                                            {:b 2 :cljfmt {:indents {'a [[:block 1]]}}})))
  (is (= {:a 1 :b 2 :cljfmt {:indents {'a [[:block 1]]
                                       'b [[:inner 0]]}}}
         (#'config/deep-merge-fixing-cljfmt {:a 1 :cljfmt {:indents {'a [[:block 0]]}}}
                                            {:b 2 :cljfmt {:indents {'a [[:block 1]]
                                                                     'b [[:inner 0]]}}})))
  (is (= {:a 1 :b 2 :cljfmt {:indents {'a [[:block 1]]
                                       'b [[:inner 0]]}}}
         (#'config/deep-merge-fixing-cljfmt {:a 1 :cljfmt {:indents {'a [[:block 0]]
                                                                     'b [[:inner 0]]}}}
                                            {:b 2 :cljfmt {:indents {'a [[:block 1]]}}}))))
