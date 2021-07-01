(ns clojure-lsp.main-test
  (:require
   [clojure-lsp.main :as main]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(def default-root (.getAbsolutePath (io/file "src")))

(deftest parse
  (testing "parsing options"
    (testing "settings"
      (is (= nil (:settings (:options (#'main/parse [])))))
      (is (= 1 (:settings (:options (#'main/parse ["--settings" "1"])))))
      (is (= {} (:settings (:options (#'main/parse ["-s" "{}"])))))
      (is (= nil (:settings (:options (#'main/parse ["-s" "}"])))))
      (is (= {:a {:b 1} :c 2} (:settings (:options (#'main/parse ["-s" "{:a {:b 1} :c 2}"]))))))
    (testing "project-root"
      (is (= default-root (.getAbsolutePath (:project-root (:options (#'main/parse ["--project-root" "src"]))))))
      (is (= default-root (.getAbsolutePath (:project-root (:options (#'main/parse ["-p" "src"]))))))
      (is (= nil (:project-root (:options (#'main/parse ["-p" "1"])))))
      (is (= nil (:project-root (:options (#'main/parse ["p" "/this/is/not/a/valid/path"]))))))
    (testing "namespace"
      (is (= [] (:namespace (:options (#'main/parse [])))))
      (is (= '[abc] (:namespace (:options (#'main/parse ["--namespace" "abc"])))))
      (is (= '[abc] (:namespace (:options (#'main/parse ["-n" "abc"])))))
      (is (= '[abc bcd] (:namespace (:options (#'main/parse ["-n" "abc" "-n" "bcd"]))))))
    (testing "from"
      (is (= nil (:from (:options (#'main/parse [])))))
      (is (= nil (:from (:options (#'main/parse ["--from" "abc"])))))
      (is (= 'bla/abc (:from (:options (#'main/parse ["--from" "bla/abc"]))))))
    (testing "to"
      (is (= nil (:to (:options (#'main/parse [])))))
      (is (= nil (:to (:options (#'main/parse ["--to" "abc"])))))
      (is (= 'bla/abc (:to (:options (#'main/parse ["--to" "bla/abc"])))))))
  (testing "commands"
    (is (= "listen" (:action (#'main/parse []))))
    (is (= "listen" (:action (#'main/parse ["listen"]))))
    (is (= "clean-ns" (:action (#'main/parse ["clean-ns"]))))
    (is (= "rename" (:action (#'main/parse ["rename"]))))
    (is (= nil (:action (#'main/parse ["clean-ns" "listen"])))))
  (testing "final options"
    (is (string? (:exit-message (#'main/parse ["--help"]))))
    (is (string? (:exit-message (#'main/parse ["-h"]))))
    (is (string? (:exit-message (#'main/parse ["--version"]))))))
