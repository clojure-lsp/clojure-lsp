(ns clojure-lsp.config-test
  (:require
   [clojure-lsp.config :as config]
   [clojure.test :refer [deftest testing is]]))

(deftest resolve-config
  (testing "when user doesn't have a home config or a project config"
    (with-redefs [config/get-property (constantly nil)
                  config/file-exists? (constantly nil)
                  config/get-env (constantly nil)]
      (is (= {} (config/resolve-config "/home/user/some/project/")))))
  (testing "when user doesn't have a home config but has a project config"
    (with-redefs [config/get-property (constantly nil)
                  config/get-env (constantly nil)
                  config/file-exists? #(= "/home/user/some/project/.lsp/config.edn" (str %))
                  slurp (constantly "{:foo {:bar 1}}")]
      (is (= {:foo {:bar 1}} (config/resolve-config "/home/user/some/project/")))))
  (testing "when user has a home config but doesn't have a project config"
    (with-redefs [config/get-property (constantly "/home/user")
                  config/get-env (constantly nil)
                  config/file-exists? #(= "/home/user/.lsp/config.edn" (str %))
                  slurp (constantly "{:foo {:bar 1}}")]
      (is (= {:foo {:bar 1}} (config/resolve-config "/home/user/some/project/")))))
  (testing "when user has a home config and a project config we merge with project as priority"
    (with-redefs [config/get-property (constantly "/home/user")
                  config/get-env (constantly nil)
                  config/file-exists? #(or (= "/home/user/.lsp/config.edn" (str %))
                                           (= "/home/user/some/project/.lsp/config.edn" (str %)))
                  slurp (fn [f]
                          (if (= "/home/user/.lsp/config.edn" (str f))
                            "{:foo {:bar 1 :baz 1} :home :bla}"
                            "{:foo {:baz 2}}"))]
      (is (= {:foo {:bar 1
                    :baz 2}
              :home :bla} (config/resolve-config "/home/user/some/project/"))))))
