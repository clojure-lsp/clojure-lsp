(ns clojure-lsp.main-test
  (:require
   [clojure-lsp.main :as main]
   [clojure-lsp.test-helper :as h]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(def default-root (.getAbsolutePath (io/file "src")))

(deftest parse
  (testing "parsing options"
    (testing "settings"
      (is (= nil (:settings (:options (#'main/parse [])))))
      (is (= 1 (:settings (:options (#'main/parse ["--settings" "1"])))))
      (is (= {} (:settings (:options (#'main/parse ["-s" "{}"])))))
      (is (= nil (:settings (:options (#'main/parse ["-s" "}"])))))
      (is (= {:a {:b 1} :c 2} (:settings (:options (#'main/parse ["-s" "{:a {:b 1} :c 2}"]))))))
    (testing "log-path"
      (is (= "/custom/path" (:log-path (:options (#'main/parse ["--log-path" "/custom/path"])))))
      (is (= nil (:log-path (:options (#'main/parse []))))))
    (testing "dry"
      (is (not (:dry? (:options (#'main/parse [])))))
      (is (:dry? (:options (#'main/parse ["--dry"])))))
    (testing "raw"
      (is (not (:raw? (:options (#'main/parse [])))))
      (is (:raw? (:options (#'main/parse ["--raw"])))))
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
    (testing "filenames"
      (is (= nil (:filenames (:options (#'main/parse [])))))
      (is (= nil (:filenames (:options (#'main/parse ["--filenames"])))))
      (is (= nil (:filenames (:options (#'main/parse ["--filenames" "some-file" "other-file"])))))
      (is (= nil (:filenames (:options (#'main/parse ["--filenames" "some-file other-file"])))))
      (is (= '["deps.edn" "src"] (map str (:filenames (:options (#'main/parse ["--filenames" "deps.edn:src"])))))) ;; these file and directory exists at cli dir
      (is (= '["deps.edn" "src"] (map str (:filenames (:options (#'main/parse ["--filenames" "deps.edn,src"])))))))
    (testing "ns-exclude-regex"
      (is (= "foo" (str (:ns-exclude-regex (:options (#'main/parse ["--ns-exclude-regex" "foo"]))))))
      (is (= nil (:ns-exclude-regex (:options (#'main/parse [])))))
      (is (= nil (:ns-exclude-regex (:options (#'main/parse ["--ns-exclude-regex" "*invalid-regex*"]))))))
    (testing "output"
      (is (= nil (:output (:options (#'main/parse [])))))
      (is (= 1 (:output (:options (#'main/parse ["--output" "1"])))))
      (is (= {} (:output (:options (#'main/parse ["-o" "{}"])))))
      (is (= nil (:output (:options (#'main/parse ["-o" "}"])))))
      (is (= {:a {:b 1} :c 2} (:output (:options (#'main/parse ["-o" "{:a {:b 1} :c 2}"]))))))
    (testing "from"
      (is (= nil (:from (:options (#'main/parse [])))))
      (is (= 'abc (:from (:options (#'main/parse ["--from" "abc"])))))
      (is (= 'bla/abc (:from (:options (#'main/parse ["--from" "bla/abc"]))))))
    (testing "to"
      (is (= nil (:to (:options (#'main/parse [])))))
      (is (= 'abc (:to (:options (#'main/parse ["--to" "abc"])))))
      (is (= 'bla/abc (:to (:options (#'main/parse ["--to" "bla/abc"]))))))
    (testing "analysis"
      (is (= nil (:analysis (:options (#'main/parse [])))))
      (is (= 1 (:analysis (:options (#'main/parse ["--analysis" "1"])))))
      (is (= {} (:analysis (:options (#'main/parse ["--analysis" "{}"])))))
      (is (= nil (:analysis (:options (#'main/parse ["--analysis" "}"])))))
      (is (= {:a {:b 1} :c 2} (:analysis (:options (#'main/parse ["--analysis" "{:a {:b 1} :c 2}"])))))))
  (testing "commands"
    (is (= "listen" (:action (#'main/parse []))))
    (is (= "listen" (:action (#'main/parse ["listen"]))))
    (is (= "clean-ns" (:action (#'main/parse ["clean-ns"]))))
    (is (= "rename" (:action (#'main/parse ["rename"]))))
    (is (= "dump" (:action (#'main/parse ["dump"]))))
    (is (= nil (:action (#'main/parse ["clean-ns" "listen"])))))
  (testing "final options"
    (is (string? (:exit-message (#'main/parse ["--help"]))))
    (is (string? (:exit-message (#'main/parse ["-h"]))))
    (is (string? (:exit-message (#'main/parse ["--version"]))))))
