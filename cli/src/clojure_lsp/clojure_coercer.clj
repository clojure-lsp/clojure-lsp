(ns clojure-lsp.clojure-coercer
  (:require
   [clojure-lsp.coercer-v1 :as coercer-v1]
   [clojure.spec.alpha :as s]))

(set! *warn-on-reflection* true)

(def test-tree-kind-enum
  {:namespace 1 :deftest 2 :testing 3})

(s/def :test-tree/kind (s/and keyword?
                              test-tree-kind-enum
                              (s/conformer test-tree-kind-enum)))

(s/def :test-tree/name-range ::coercer-v1/range)
(s/def :test-tree/children (s/coll-of :test-tree/test-node))

(s/def :test-tree/test-node (s/keys :req-un [::coercer-v1/name
                                             ::coercer-v1/range
                                             :test-tree/name-range
                                             :test-tree/kind]
                                    :opt-un [:test-tree/children]))

(s/def :test-tree/tree :test-tree/test-node)

(s/def ::publish-test-tree-params (s/keys :req-un [::coercer-v1/uri
                                                   :test-tree/tree]))
