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
              :home :bla} (config/resolve-config (h/file-uri "file:///home/user/some/project/"))))))
  (testing "when XDG_CONFIG_HOME is unset but user has XDG config and a project config we merge with project as priority"
    (with-redefs [config/get-property (constantly "/home/user")
                  config/file-exists? #(or (= (h/file-path "/home/user/.config/.lsp/config.edn") (str %))
                                           (= (h/file-path "/home/user/some/project/.lsp/config.edn") (str %)))
                  config/get-env (constantly nil)
                  slurp (fn [f]
                          (if (= (h/file-path "/home/user/.config/.lsp/config.edn") (str f))
                            "{:foo {:bar 1 :baz 1} :xdg :bla}"
                            "{:foo {:baz 2}}"))]
      (is (= {:foo {:bar 1
                    :baz 2}
              :xdg :bla} (config/resolve-config (h/file-uri "file:///home/user/some/project/"))))))
  (testing "when XDG_CONFIG_HOME is set and both global and a project config exist we merge with project as priority"
    (with-redefs [config/get-property (constantly "/home/user")
                  config/file-exists? #(or (= (h/file-path "/tmp/config/.lsp/config.edn") (str %))
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
              :xdg :tmp} (config/resolve-config (h/file-uri "file:///home/user/some/project/")))))))
