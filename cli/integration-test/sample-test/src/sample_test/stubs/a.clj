(ns sample-test.stubs.a
  (:require [datomic.api :as d]))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn datomic-test []
  (d/create-database))
