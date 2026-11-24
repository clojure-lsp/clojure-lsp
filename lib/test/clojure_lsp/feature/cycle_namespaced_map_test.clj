(ns clojure-lsp.feature.cycle-namespaced-map-test
  (:require
   [clojure-lsp.feature.cycle-namespaced-map :as f.cycle-namespaced-map]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is testing]]))

(defn ^:private cycle-namespaced-map [code]
  (h/edits-as-strings
    (f.cycle-namespaced-map/cycle-namespaced-map
      (h/zloc-from-code code))))

(defn ^:private cycle-namespaced-map-status [code]
  (:status (f.cycle-namespaced-map/cycle-namespaced-map-status
             (h/zloc-from-code code))))

(deftest cycle-namespaced-map-status-test
  (testing "on a map"
    (is (= :from-map-to-namespaced (cycle-namespaced-map-status "|{:foo/bar 1}")))
    (is (= :from-map-to-namespaced (cycle-namespaced-map-status "{|:foo/bar 1}")))
    (is (= :from-map-to-namespaced (cycle-namespaced-map-status "{:foo/bar |1}")))
    (is (= :from-map-to-namespaced (cycle-namespaced-map-status "|{::bar 1}")))
    (is (= :from-map-to-namespaced (cycle-namespaced-map-status "|{::b/bar 1}")))
    (testing "without qualified keys"
      (is (= nil (cycle-namespaced-map-status "|{:bar 1}")))
      (is (= nil (cycle-namespaced-map-status "|{\"bar\" 1}")))
      (is (= nil (cycle-namespaced-map-status "|{}"))))
    (testing "with keys with the `_` namespace"
      (is (= nil (cycle-namespaced-map-status "|{:_/bar 1 :foo/baz 2}")))))
  (testing "on a namespaced map"
    (is (= :from-namespaced-to-map (cycle-namespaced-map-status "|#:foo{:bar 1}")))
    (is (= :from-namespaced-to-map (cycle-namespaced-map-status "#:foo|{:bar 1}")))
    (is (= :from-namespaced-to-map (cycle-namespaced-map-status "#:foo{|:bar 1}")))
    (is (= :from-namespaced-to-map (cycle-namespaced-map-status "|#::{:bar 1}")))
    (is (= :from-namespaced-to-map (cycle-namespaced-map-status "|#::b{:bar 1}")))
    (testing "with unqualified symbol keys and an auto-resolved namespace"
      (is (= nil (cycle-namespaced-map-status "|#::b{bar 1}")))))
  (testing "on other elements"
    (is (= nil (cycle-namespaced-map-status "|[:foo/bar 1]")))
    (is (= nil (cycle-namespaced-map-status "|:foo/bar")))
    (is (= nil (cycle-namespaced-map-status "(assoc m |:foo/bar 1)")))))

(deftest cycle-namespaced-map-test
  (testing "changing map to namespaced map"
    (is (= ["#:foo{:bar 1}"] (cycle-namespaced-map "|{:foo/bar 1}")))
    (is (= ["#:foo{:bar 1 :baz 2}"] (cycle-namespaced-map "|{:foo/bar 1 :foo/baz 2}")))
    (testing "respecting auto-resolved keywords"
      (is (= ["#::{:bar 1}"] (cycle-namespaced-map "|{::bar 1}")))
      (is (= ["#::b{:bar 1}"] (cycle-namespaced-map "|{::b/bar 1}"))))
    (testing "keeping unqualified keys unqualified"
      (is (= ["#:foo{:bar 1 :_/baz 2}"] (cycle-namespaced-map "|{:foo/bar 1 :baz 2}"))))
    (testing "keeping keys with other namespaces"
      (is (= ["#:foo{:bar 1 :baz/qux 2}"] (cycle-namespaced-map "|{:foo/bar 1 :baz/qux 2}"))))
    (testing "choosing the most frequent namespace"
      (is (= ["#:baz{:foo/bar 1 :a 2 :b 3}"] (cycle-namespaced-map "|{:foo/bar 1 :baz/a 2 :baz/b 3}"))))
    (testing "not changing values or nested maps"
      (is (= ["#:foo{:bar {:baz 1}}"] (cycle-namespaced-map "|{:foo/bar {:baz 1}}"))))
    (testing "not changing non keyword/symbol keys"
      (is (= ["#:foo{:bar 1 \"s\" 2}"] (cycle-namespaced-map "|{:foo/bar 1 \"s\" 2}"))))
    (testing "changing symbol keys"
      (is (= ["#:foo{bar 1 :baz 2}"] (cycle-namespaced-map "|{foo/bar 1 :foo/baz 2}")))
      (is (= ["#:foo{:bar 1 _/sym 2}"] (cycle-namespaced-map "|{:foo/bar 1 sym 2}"))))
    (testing "preserving whitespace and comments"
      (is (= [(h/code "#:foo{:bar 1"
                      " :baz 2}")]
             (cycle-namespaced-map (h/code "|{:foo/bar 1"
                                           " :foo/baz 2}"))))
      (is (= [(h/code "#:foo{:bar 1 ;; comment"
                      " :baz 2}")]
             (cycle-namespaced-map (h/code "|{:foo/bar 1 ;; comment"
                                           " :foo/baz 2}"))))))
  (testing "changing namespaced map to map"
    (is (= ["{:foo/bar 1}"] (cycle-namespaced-map "|#:foo{:bar 1}")))
    (is (= ["{:foo/bar 1}"] (cycle-namespaced-map "#:foo|{:bar 1}")))
    (is (= ["{:foo/bar 1}"] (cycle-namespaced-map "#:foo{|:bar 1}")))
    (is (= ["{:foo/bar 1 :foo/baz 2}"] (cycle-namespaced-map "|#:foo{:bar 1 :baz 2}")))
    (testing "respecting auto-resolved namespaces"
      (is (= ["{::bar 1}"] (cycle-namespaced-map "|#::{:bar 1}")))
      (is (= ["{::b/bar 1}"] (cycle-namespaced-map "|#::b{:bar 1}"))))
    (testing "unqualifying keys with the `_` namespace"
      (is (= ["{:foo/bar 1 :baz 2}"] (cycle-namespaced-map "|#:foo{:bar 1 :_/baz 2}"))))
    (testing "keeping keys with other namespaces"
      (is (= ["{:foo/bar 1 :baz/qux 2}"] (cycle-namespaced-map "|#:foo{:bar 1 :baz/qux 2}")))
      (is (= ["{:foo/bar 1 ::x 2}"] (cycle-namespaced-map "|#:foo{:bar 1 ::x 2}"))))
    (testing "not changing values or nested maps"
      (is (= ["{:foo/bar {:baz 1}}"] (cycle-namespaced-map "|#:foo{:bar {:baz 1}}"))))
    (testing "changing symbol keys"
      (is (= ["{foo/bar 1}"] (cycle-namespaced-map "|#:foo{bar 1}"))))
    (testing "with whitespace between namespace and map"
      (is (= ["{:foo/bar 1}"] (cycle-namespaced-map "|#:foo {:bar 1}"))))
    (testing "with unqualified symbol keys and an auto-resolved namespace"
      (is (= [] (cycle-namespaced-map "|#::b{bar 1}"))))))
