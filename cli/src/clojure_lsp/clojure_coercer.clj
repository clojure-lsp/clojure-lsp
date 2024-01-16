(ns clojure-lsp.clojure-coercer
  (:require
   [clojure.set :as set]
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

(def project-tree-type-enum
  {:project 1 :source-path 2 :library 3 :jar 4 :ns 5
   :class 6 :function 7 :variable 8 :interface 9})

(s/def :project-tree/type
  (s/and (s/or :keyword (s/and keyword?
                               (s/conformer project-tree-type-enum))
               :number (s/and number?
                              (s/conformer #(get (set/map-invert project-tree-type-enum) %))))
         (s/conformer second)))

(s/def :project-tree/leaf (s/keys :req-un [::coercer/name
                                           :project-tree/type
                                           :project-tree/final]
                                  :opt-un [:project-tree/id
                                           :project-tree/uri
                                           :project-tree/detail]))

(s/def :project-tree/node (s/and (s/or :final :project-tree/leaf
                                       :nodes :project-tree/branch)
                                 (s/conformer second)))

(s/def :project-tree/nodes (s/coll-of :project-tree/node))

(s/def :project-tree/branch (s/keys :req-un [::coercer/name
                                             :project-tree/type
                                             :project-tree/nodes]
                                    :opt-un [:project-tree/id
                                             :project-tree/uri
                                             :project-tree/detail]))

(s/def ::project-tree-params (s/nilable :project-tree/leaf))
(s/def ::project-tree-response :project-tree/branch)
