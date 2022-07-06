(ns clojure-lsp.clojure-coercer
  (:require
   [clojure.spec.alpha :as s]
   [lsp4clj.coercer :as coercer])
  (:import
   (clojure_lsp.feature.test_tree TestTreeKind TestTreeNode TestTreeParams)))

(set! *warn-on-reflection* true)

(def test-tree-kind-enum
  {:namespace 1 :deftest 2 :testing 3})

(s/def :test-tree/kind (s/and keyword?
                              test-tree-kind-enum
                              (s/conformer (fn [v] (TestTreeKind/forValue (get test-tree-kind-enum v))))))

(s/def :test-tree/name-range ::coercer/range)
(s/def :test-tree/children (s/coll-of :test-tree/test-node))

(s/def :test-tree/test-node (s/and (s/keys :req-un [::coercer/name ::coercer/range :test-tree/name-range :test-tree/kind]
                                           :opt-un [:test-tree/children])
                                   (s/conformer #(doto (TestTreeNode.)
                                                   (.setName (:name %1))
                                                   (.setRange (:range %1))
                                                   (.setNameRange (:name-range %1))
                                                   (.setKind (:kind %1))
                                                   (.setChildren (:children %1))))))

(s/def :test-tree/tree :test-tree/test-node)

(s/def ::publish-test-tree-params (s/and (s/keys :req-un [::coercer/uri :test-tree/tree])
                                         (s/conformer #(doto (TestTreeParams.)
                                                         (.setUri (:uri %1))
                                                         (.setTree (:tree %1))))))

(s/def ::server-info-raw ::coercer/bean)
(s/def ::cursor-info-raw ::coercer/bean)
(s/def ::clojuredocs-raw ::coercer/bean)
