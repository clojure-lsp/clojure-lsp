(ns clojure-lsp.features.rename-test
  (:require
    [clojure-lsp.feature.rename :as f.rename]
    [clojure-lsp.test-helper :as h]
    [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(deftest rename-simple-keywords
  (let [[a-start _a-stop
         a-binding-start a-binding-stop
         a-local-usage-start a-local-usage-stop] (h/load-code-and-locs
                                                   "|:a| (let [{:keys [:|a|]} {}] |a|)"
                                                   (h/file-uri "file:///a.cljc"))]
    (testing "should not rename plain keywords"
      (let [[row col] a-start
            changes (:changes (f.rename/rename (h/file-uri "file:///a.cljc") ":b" row col))]
        (is (= nil changes))))

    (testing "should rename local in destructure not keywords"
      (let [[row col] a-binding-start
            changes (:changes (f.rename/rename (h/file-uri "file:///a.cljc") ":b" row col))]
        (is (= {(h/file-uri "file:///a.cljc") [{:new-text "b" :range (h/->range a-binding-start a-binding-stop)}
                                  {:new-text "b" :range (h/->range a-local-usage-start a-local-usage-stop)}]}
               changes))))))

(deftest rename-keywords-corner-cases
  (let [[a-b-start a-b-stop
         b-ns-start b-ns-stop]
        (h/load-code-and-locs
          (h/code ":a"
                  "::a"
                  "|:a/b|"
                  "#:a{|:b| 1"
                  "    :c/d 2"
                  "    :_/e 3"
                  "    ::f 4}")
          (h/file-uri "file:///a.cljc"))]
    (testing "renaming keywords renames correctly namespaced maps as well"
      (let [[row col] a-b-start
            changes (:changes (f.rename/rename (h/file-uri "file:///a.cljc") ":a/g" row col))]
        (is (= {(h/file-uri "file:///a.cljc") [{:new-text ":a/g" :range (h/->range a-b-start a-b-stop)}
                                  {:new-text ":g" :range (h/->range b-ns-start b-ns-stop)}]}
               changes))))))
