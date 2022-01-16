(ns sample-test.src.sample-test.stubs.a
  (:require [datomic.api :as d]))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn datomic-test []
  (d/create-database))
