(ns clojure-lsp.main-test
  (:require
   [clojure-lsp.main :as main]
   [clojure.test :refer [deftest is testing]]
   [clojure.java.io :as io]))

(def default-root (io/file (System/getProperty "user.dir")))

(deftest parse
  (testing "parsing options"
    (testing "settings"
      (is (= nil (:settings (:options (#'main/parse [])))))
      (is (= 1 (:settings (:options (#'main/parse ["--settings" "1"])))))
      (is (= {} (:settings (:options (#'main/parse ["-s" "{}"])))))
      (is (= nil (:settings (:options (#'main/parse ["-s" "}"])))))
      (is (= {:a {:b 1} :c 2} (:settings (:options (#'main/parse ["-s" "{:a {:b 1} :c 2}"]))))))
    (testing "project-root"
      (is (= default-root (:project-root (:options (#'main/parse ["--project-root" (System/getProperty "user.dir")])))))
      (is (= default-root (:project-root (:options (#'main/parse ["-p" (System/getProperty "user.dir")])))))
      (is (= nil (:project-root (:options (#'main/parse ["-p" "1"])))))
      (is (= nil (:project-root (:options (#'main/parse ["p" "/this/is/not/a/valid/path"]))))))
    (testing "namespace"
      (is (= [] (:namespace (:options (#'main/parse [])))))
      (is (= '[abc] (:namespace (:options (#'main/parse ["--namespace" "abc"])))))
      (is (= '[abc] (:namespace (:options (#'main/parse ["-n" "abc"])))))
      (is (= '[abc bcd] (:namespace (:options (#'main/parse ["-n" "abc" "-n" "bcd"])))))))
  (testing "commands"
    (is (= "listen" (:action (#'main/parse []))))
    (is (= "listen" (:action (#'main/parse ["listen"]))))
    (is (= "clean-ns" (:action (#'main/parse ["clean-ns"]))))
    (is (= nil (:action (#'main/parse ["clean-ns" "listen"])))))
  (testing "final options"
    (is (string? (:exit-message (#'main/parse ["--help"]))))
    (is (string? (:exit-message (#'main/parse ["-h"]))))
    (is (string? (:exit-message (#'main/parse ["--version"]))))))
