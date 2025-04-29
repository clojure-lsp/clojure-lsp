(ns clojure-lsp.main-test
  (:require
   [clojure-lsp.main :as main]
   [clojure-lsp.test-helper :as h]
   [clojure.java.io :as io]
   [clojure.test :refer [are deftest is testing use-fixtures]]
   [matcher-combinators.config]
   [matcher-combinators.matchers :as m]
   [matcher-combinators.test :refer [match?]]))

(h/reset-components-before-test)

(def default-root (.getAbsolutePath (io/file "src")))

(use-fixtures :once #(binding [matcher-combinators.config/*use-abbreviation* true] (%)))

(deftest parse-opts-test
  (are [args expected] (match? expected (#'main/parse-opts args))
    ;; help
    [] {:options {:help m/absent}}
    ["--help"] {:options {:help true}}
    ["-h"] {:options {:help true}}
    ;; version
    [] {:options {:version m/absent}}
    ["--version"] {:options {:version true}}
    ["-v"] {:options {:version m/absent}}
    ;; verbose
    [] {:options {:verbose m/absent}}
    ["--verbose"] {:options {:verbose true}}
    ["-v"] {:options {:verbose m/absent}}
    ;; trace-level
    ["--trace-level" "off"] {:options {:trace-level "off"}
                             :errors nil}
    ["--trace-level" "messages"] {:options {:trace-level "messages"}
                                  :errors nil}
    ["--trace-level" "verbose"] {:options {:trace-level "verbose"}
                                 :errors nil}
    ["--trace-level" "unknown"] {:errors ["Failed to validate \"--trace-level unknown\": Must be in #{\"off\" \"messages\" \"verbose\"}"]}
    ;; settings
    [] {:options {:settings m/absent}}
    ["--settings" "1"] {:options {:settings 1}}
    ["-s" "{}"] {:options {:settings {}}}
    ["-s" "}"] {:errors ["Failed to validate \"-s }\": Invalid --settings EDN"]}
    ["-s" "{:a {:b 1} :c 2}"] {:options {:settings {:a {:b 1} :c 2}}}
    ;; log-path
    [] {:options {:log-path m/absent}}
    ["--log-path" "/custom/path"] {:options {:log-path "/custom/path"}}
    ;; dry?
    [] {:options {:dry? false}}
    ["--dry"] {:options {:dry? true}}
    ;; raw?
    [] {:options {:raw? false}}
    ["--raw"] {:options {:raw? true}}
    ;; project-root
    [] {:options {:project-root m/absent}}
    ["--project-root" "src"] {:options {:project-root (io/file "src")}}
    ["-p" "src"] {:options {:project-root (io/file "src")}}
    ["-p" "1"] {:errors ["Failed to validate \"-p 1\": Specify a valid path after --project-root"]}
    ["-p" "/this/is/not/a/valid/path"] {:errors ["Failed to validate \"-p /this/is/not/a/valid/path\": Specify a valid path after --project-root"]}
    ;; namespace
    [] {:options {:namespace []}}
    ["--namespace" "abc"] {:options {:namespace '[abc]}}
    ["-n" "abc"] {:options {:namespace '[abc]}}
    ["-n" "abc" "-n" "bcd"] {:options {:namespace '[abc bcd]}}
    ;; filenames
    [] {:options {:filenames m/absent}}
    ["--filenames"] {:options {:filenames m/absent}}
    ["--filenames" "some-file other-file"] {:options {:filenames m/absent}
                                            :errors ["Failed to validate \"--filenames some-file other-file\": Filenames should be separated by comma or double colon."]}
    ["--filenames" "deps.edn:src"] {:options {:filenames [(io/file "deps.edn") (io/file "src")]}
                                    :errors nil}
    ["--filenames" "deps.edn,src"] {:options {:filenames [(io/file "deps.edn") (io/file "src")]}
                                    :errors nil}
    ;; ns-exclude-regex
    [] {:options {:ns-exclude-regex m/absent}}
    ["--ns-exclude-regex" "foo"] {:options {:ns-exclude-regex #(= (str %) (str #"foo"))}}
    ["--ns-exclude-regex" "*invalid-regex*"] {:errors ["Error while parsing option \"--ns-exclude-regex *invalid-regex*\": java.util.regex.PatternSyntaxException: Dangling meta character '*' near index 0\n*invalid-regex*\n^"]}
    ;; output
    [] {:options {:output m/absent}}
    ["--output" "1"] {:options {:output 1}}
    ["-o" "{}"] {:options {:output {}}}
    ["-o" "}"] {:errors ["Failed to validate \"-o }\": Invalid --output EDN"]}
    ["-o" "{:a {:b 1} :c 2}"] {:options {:output {:a {:b 1} :c 2}}}
    ;; from
    [] {:options {:from m/absent}}
    ["--from" "abc"] {:options {:from 'abc}}
    ["--from" "bla/abc"] {:options {:from 'bla/abc}}
    ;; to
    [] {:options {:to m/absent}}
    ["--to" "1"] {:options {:to (symbol "1")}}
    ["--to" "abc"] {:options {:to 'abc}}
    ["--to" "bla/abc"] {:options {:to 'bla/abc}}
    ;; analysis
    [] {:options {:analysis m/absent}}
    ["--analysis" "1"] {:options {:analysis 1}}
    ["--analysis" "{}"] {:options {:analysis {}}}
    ["--analysis" "}"] {:errors ["Failed to validate \"--analysis }\": Invalid --analysis EDN"]}
    ["--analysis" "{:a {:b 1} :c 2}"] {:options {:analysis {:a {:b 1} :c 2}}}
    #_()))

(deftest parse
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
