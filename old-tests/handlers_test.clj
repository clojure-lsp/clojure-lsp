(ns clojure-lsp.handlers-test
  (:require
    [clojure.core.async :as async]
    [clojure-lsp.db :as db]
    [clojure.tools.logging :as log]
    [clojure-lsp.test-helper :as h]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.handlers :as handlers]
    [clojure.string :as string]
    [clojure.test :refer [deftest is testing use-fixtures]])
  (:import
    (org.eclipse.lsp4j
      Diagnostic
      DiagnosticSeverity
      Range
      Position)))

(use-fixtures
  :each
  (fn [f]
    (reset! db/db {})
    (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1))
      (f))))

(deftest test-rename
  (let [[abar-start abar-stop
         akwbar-start akwbar-stop
         abaz-start abaz-stop] (h/load-code-and-locs "(ns a) (def |bar| ::|bar|) (def ^:m |baz| 1)" "file:///a.clj")
        [balias-start balias-stop
         ba1-start ba1-stop
         bbar-start bbar-stop
         ba2-start ba2-stop
         bkwbar-start bkwbar-stop] (h/load-code-and-locs "(ns b (:require [a :as |aa|])) (def x |aa|/|bar|) ::|aa|/|bar| :aa/bar" "file:///b.clj")
        [cbar-start cbar-stop
         cbaz-start cbaz-stop] (h/load-code-and-locs "(ns c (:require [a :as aa])) (def x aa/|bar|) ^:xab aa/|baz|" "file:///c.clj")]
    ;; TODO kondo (keyword analysis)
    (testing "on ::keyword"
      (let [changes (:changes (handlers/rename {:textDocument "file:///a.clj"
                                                :position (h/->position akwbar-start)
                                                :newName "foo"}))]
        (is (= {"file:///a.clj" [{:new-text "foo" :range (h/->range akwbar-start akwbar-stop)}]
                "file:///b.clj" [{:new-text "foo" :range (h/->range bkwbar-start bkwbar-stop)}]}
               changes))))
    ;; TODO kondo alias change on rename
    (testing "on alias changes namespaces inside file"
      (let [changes (:changes (handlers/rename {:textDocument "file:///b.clj"
                                                :position (h/->position balias-start)
                                                :newName "xx"}))]
        (is (= {"file:///b.clj" [{:new-text "xx" :range (h/->range balias-start balias-stop)}
                                 {:new-text "xx" :range (h/->range ba1-start ba1-stop)}
                                 {:new-text "xx" :range (h/->range ba2-start ba2-stop)}]}
               changes))))))

;; TODO kondo (keyword analysis)
(deftest test-rename-simple-keywords
  (reset! db/db {:file-envs
                 {"file://a.cljc" (parser/find-usages ":a (let [{:keys [a]} {}] a)" :cljc {})}})
  (testing "should not rename plain keywords"
    (let [changes (:changes (handlers/rename {:textDocument "file://a.cljc"
                                              :position {:line 0 :character 0}
                                              :newName "b"}))]
      (is (= nil changes))))

  (testing "should rename local in destructure not keywords"
    (let [changes (:changes (handlers/rename {:textDocument "file://a.cljc"
                                              :position {:line 0 :character 17}
                                              :newName "b"}))]
      (is (= [18 26] (mapv (comp inc :character :start :range) (get-in changes ["file://a.cljc"])))))))
