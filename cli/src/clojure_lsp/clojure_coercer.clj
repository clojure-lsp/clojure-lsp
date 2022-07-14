(ns clojure-lsp.clojure-coercer
  (:require
   [clojure.spec.alpha :as s]
   [lsp4clj.coercer :as coercer]))

(set! *warn-on-reflection* true)

(def test-tree-kind-enum
  {:namespace 1 :deftest 2 :testing 3})

(s/def :test-tree/kind (s/and keyword?
                              test-tree-kind-enum
                              (s/conformer test-tree-kind-enum)))

(s/def :test-tree/name-range ::coercer/range)
(s/def :test-tree/children (s/coll-of :test-tree/test-node))

(s/def :test-tree/test-node (s/keys :req-un [::coercer/name
                                             ::coercer/range
                                             :test-tree/name-range
                                             :test-tree/kind]
                                    :opt-un [:test-tree/children]))

(s/def :test-tree/tree :test-tree/test-node)

(s/def ::publish-test-tree-params (s/keys :req-un [::coercer/uri
                                                   :test-tree/tree]))
