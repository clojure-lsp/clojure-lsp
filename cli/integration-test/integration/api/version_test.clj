(ns integration.api.version-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest version
  (testing "Checking --version"
    (with-open [rdr (lsp/cli! "--version")]
      (let [current-version (string/trim (slurp "../lib/resources/CLOJURE_LSP_VERSION"))
            output (slurp rdr)]
        (is (string/includes? output (format "clojure-lsp %s" current-version)))
        (is (string/includes? output "clj-kondo"))))))
