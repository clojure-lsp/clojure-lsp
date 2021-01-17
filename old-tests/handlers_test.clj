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

(defn diagnostics-or-timeout []
  (first (async/alts!!
           [(async/timeout 500)
            db/diagnostics-chan])))

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

#_
(deftest test-completion
  (let [db-state {:file-envs
                  {"file://a.cljc" (parser/find-usages
                                     (str "(ns alpaca.ns (:require [user :as alpaca]))\n"
                                          "(def barr)\n"
                                          "(def bazz)")
                                     :clj
                                     {})
                   "file://b.clj" (parser/find-usages
                                    (str "(ns user)\n"
                                         "(def alpha)\n"
                                         "alp\n"
                                         "ba")
                                    :clj
                                    {})
                   "file://c.cljs" (parser/find-usages
                                     (str "(ns alpaca.ns)\n"
                                          "(def baff)\n")
                                     :cljs
                                     {})
                   "file://d.clj" (parser/find-usages
                                    (str "(ns d (:require [alpaca.ns :as alpaca]))")
                                    :clj
                                    {})
                   "file://e.clj" (parser/find-usages
                                    (str "(ns e (:require [alpaca.ns :refer [ba]]))")
                                    :clj
                                    {})}}]
    (testing "complete-a"
      (reset! db/db db-state)
      (is (= [{:label "alpha" :data "user/alpha"}
              {:label "alpaca" :detail "user"}
              {:label "alpaca" :detail "alpaca.ns"}
              {:label "alpaca.ns" :detail "alpaca.ns"}]
             (handlers/completion {:textDocument "file://b.clj"
                                   :position {:line 2 :character 17}}))))
    (testing "complete-ba"
      (reset! db/db db-state)
      (is (= [{:label "bases" :data "clojure.core/bases"}]
             (handlers/completion {:textDocument "file://b.clj"
                                   :position {:line 3 :character 2}}))))
    (testing "complete-alph"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'user/alph :str "alph"}))
      (is (= [{:label "alpha" :data "user/alpha"}]
             (handlers/completion {:textDocument "file://b.clj"
                                   :position {:line 2 :character 2}}))))
    (testing "complete-alpaca"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'alpaca :str "alpaca"}))
      (is (= [{:label "alpaca" :detail "user"}
              {:label "alpaca" :detail "alpaca.ns"}
              {:label "alpaca.ns" :detail "alpaca.ns"}
              {:label "alpaca/barr" :detail "alpaca.ns" :data "alpaca.ns/barr"}
              {:label "alpaca/bazz" :detail "alpaca.ns" :data "alpaca.ns/bazz"}]
             (handlers/completion {:textDocument "file://b.clj"
                                   :position {:line 2 :character 17}}))))
    (testing "complete-within-refering"
      (reset! db/db db-state)
      (is (= [{:label "barr" :detail "alpaca.ns/barr" :data "alpaca.ns/barr"}
              {:label "bazz" :detail "alpaca.ns/bazz" :data "alpaca.ns/bazz"}]
             (handlers/completion {:textDocument "file://e.clj"
                                   :position {:line 0 :character 37}}))))
    (testing "complete-core-stuff"
      (get-in @db/db [:file-envs "file://b.clj"])
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'freq :str "freq"}))
      (is (= [{:label "frequencies" :data "clojure.core/frequencies"}]
             (handlers/completion {:textDocument "file://b.clj"
                                   :position {:line 2 :character 17}})))
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'Sys :str "Sys"}))
      (is (= [{:label "System" :data "java.lang.System"}]
             (handlers/completion {:textDocument "file://b.clj"
                                   :position {:line 2 :character 17}}))))
    (testing "resolving completion item"
      (reset! db/db db-state)
      (let [{:keys [label data documentation]} (handlers/resolve-completion-item {:label "alpha"
                                                                                  :data  "user/alpha"})]
        (and (is (= label "alpha"))
             (is (= data "user/alpha"))
             (is (string/includes? documentation data)))))))

(deftest test-code-actions-handle
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        "file://a.clj")
  (h/load-code-and-locs (str "(ns other-ns (:require [some-ns :as sns]))\n"
                             "(def bar 1)\n"
                             "(defn baz []\n"
                             "  bar)")
                        "file://b.clj")
  (h/load-code-and-locs (str "(ns another-ns)\n"
                             "(def bar ons/bar)\n"
                             "(def foo sns/foo)\n"
                             "(deftest some-test)\n"
                             "MyClass.\n"
                             "Date.")
                        "file://c.clj")
  (testing "when it has unresolved-namespace and can find namespace"
    (is (some #(= (:title %) "Add missing 'some-ns' require")
              (handlers/code-actions
                {:textDocument "file://c.clj"
                 :context {:diagnostics [{:code "unresolved-namespace"
                                          :range {:start {:line 2 :character 10}}}]}
                 :range {:start {:line 2 :character 10}}})))))
