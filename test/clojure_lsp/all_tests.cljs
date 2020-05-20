(ns clojure-lsp.all-tests
  (:require [clojure.test :as test]
            [clojure-lsp.handlers-test]
            [clojure-lsp.parser-test]))

(defn ^:dev/after-load main []
  (test/run-all-tests))
