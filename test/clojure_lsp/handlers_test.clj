(ns clojure-lsp.handlers-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [taoensso.timbre :as log]))

(h/reset-db-after-test)

(deftest initialize
  (testing "detects URI format with lower-case drive letter and encoded colons"
    (h/clean-db!)
    (with-redefs [lsp.kondo/config-hash (constantly "123")]
      (handlers/initialize "file:///c%3A/project/root" {} {} nil))
    (is (= {:encode-colons-in-path?   true
            :upper-case-drive-letter? false}
           (get-in @db/db [:settings :uri-format]))))
  (testing "detects URI format with upper-case drive letter and non-encoded colons"
    (h/clean-db!)
    (with-redefs [lsp.kondo/config-hash (constantly "123")]
      (handlers/initialize "file:///C:/project/root" {} {} nil))
    (is (= {:encode-colons-in-path?   false
            :upper-case-drive-letter? true}
           (get-in @db/db [:settings :uri-format])))))

(deftest did-open
  (testing "opening a existing file"
    (h/clean-db!)
    (h/with-mock-diagnostics
      (let [_ (h/load-code-and-locs "(ns a) (when)")]
        (is (some? (get-in @db/db [:analysis (h/file-path "/a.clj")])))
        (h/assert-submaps
          [{:code "missing-body-in-when"}
           {:code "invalid-arity"}]
          (get @h/mock-diagnostics "file:///a.clj")))))
  (testing "opening a new clojure file adding the ns"
    (h/clean-db!)
    (swap! db/db merge {:settings {:auto-add-ns-to-new-files? true
                                   :source-paths #{(h/file-path "/project/src")}}
                        :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                        :project-root-uri (h/file-uri "file:///project")})
    (h/load-code-and-locs "" (h/file-uri "file:///project/src/foo/bar.clj"))
    (h/edits
      #(h/assert-submaps
         [{:edits [{:range {:start {:line 0, :character 0}
                            :end {:line 999998, :character 999998}}
                    :new-text "(ns foo.bar)"}]}]
         (:document-changes %)))
    (is (some? (get-in @db/db [:analysis (h/file-path "/project/src/foo/bar.clj")]))))
  (testing "opening a new edn file not adding the ns"
    (h/clean-db!)
    (swap! db/db merge {:settings {:auto-add-ns-to-new-files? true
                                   :source-paths #{(h/file-path "/project/src")}}
                        :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                        :project-root-uri (h/file-uri "file:///project")})
    (h/load-code-and-locs "" (h/file-uri "file:///project/src/foo/baz.edn"))
    (h/edits
      #(is (= [] (:document-changes %))))
    (is (some? (get-in @db/db [:analysis (h/file-path "/project/src/foo/baz.edn")])))))

(deftest document-symbol
  (let [code "(ns a) (def bar ::bar) (def ^:m baz 1)"
        result [{:name "a"
                 :kind :namespace
                 :range {:start {:line 0 :character 0} :end {:line 999999 :character 999999}}
                 :selection-range {:start {:line 0 :character 0} :end {:line 0 :character 5}}
                 :children [{:name "bar"
                             :kind :variable
                             :range {:start {:line 0 :character 7} :end {:line 0 :character 22}}
                             :selection-range {:start {:line 0 :character 12} :end {:line 0 :character 15}}
                             :tags []}
                            {:name "baz"
                             :kind :variable
                             :range {:start {:line 0 :character 23} :end {:line 0 :character 38}}
                             :selection-range {:start {:line 0 :character 32} :end {:line 0 :character 35}}
                             :tags []}]}]]
    (testing "clj files"
      (h/load-code-and-locs code)
      (h/assert-submaps result
                        (handlers/document-symbol {:textDocument (h/file-uri "file:///a.clj")})))
    (testing "cljs files"
      (h/load-code-and-locs code (h/file-uri "file:///b.cljs"))
      (h/assert-submaps result
                        (handlers/document-symbol {:textDocument (h/file-uri "file:///b.cljs")})))
    (testing "cljc files"
      (h/load-code-and-locs code (h/file-uri "file:///c.cljc"))
      (h/assert-submaps result
                        (handlers/document-symbol {:textDocument (h/file-uri "file:///c.cljc")})))))

(deftest document-highlight
  (let [[bar-start] (h/load-code-and-locs "(ns a) (def |bar ::bar) (def ^:m baz 1)")]
    (h/assert-submaps
      [{:range {:start {:line 0 :character 12} :end {:line 0 :character 15}}}]
      (handlers/document-highlight {:textDocument (h/file-uri "file:///a.clj")
                                    :position (h/->position bar-start)}))))

(deftest references
  (testing "simple single reference"
    (let [[bar-def-pos] (h/load-code-and-locs "(ns a) (def |bar 1)")
          _ (h/load-code-and-locs "(ns b (:require [a :as foo])) (foo/bar)" (h/file-uri "file:///b.clj"))]
      (h/assert-submaps
        [{:uri (h/file-uri "file:///b.clj")
          :range {:start {:line 0 :character 31} :end {:line 0 :character 38}}}]
        (handlers/references {:textDocument (h/file-uri "file:///a.clj")
                              :position (h/->position bar-def-pos)}))))
  (testing "when including declaration"
    (let [[bar-def-pos] (h/load-code-and-locs "(ns a) (def |bar 1)")
          _ (h/load-code-and-locs "(ns b (:require [a :as foo])) (foo/bar)" (h/file-uri "file:///b.clj"))]
      (h/assert-submaps
        [{:uri (h/file-uri "file:///a.clj")
          :range {:start {:line 0 :character 12} :end {:line 0 :character 15}}}
         {:uri (h/file-uri "file:///b.clj")
          :range {:start {:line 0 :character 31} :end {:line 0 :character 38}}}]
        (handlers/references {:textDocument (h/file-uri "file:///a.clj")
                              :position (h/->position bar-def-pos)
                              :context {:includeDeclaration true}})))))

(deftest test-rename
  (let [[abar-start abar-stop
         akw-start akwbar-start akwbar-stop
         abaz-start abaz-stop] (h/load-code-and-locs (h/code "(ns a.aa)"
                                                             "(def |bar| |::|bar|)"
                                                             "(def ^:m |baz| 1)") (h/file-uri "file:///a.clj"))
        [balias-start balias-stop
         ba1-start _ba1-stop
         bbar-start bbar-stop
         ba2-kw-start ba2-kw-stop] (h/load-code-and-locs (h/code "(ns b.bb (:require [a.aa :as |aa|]))"
                                                                 "(def x |aa|/|bar|)"
                                                                 "|::aa/bar|"
                                                                 ":aa/bar") (h/file-uri "file:///b.clj"))
        [cbar-start cbar-stop
         cbaz-start cbaz-stop] (h/load-code-and-locs (h/code "(ns c.cc (:require [a.aa :as aa]))"
                                                             "(def x aa/|bar|)"
                                                             "^:xab aa/|baz|") (h/file-uri "file:///c.clj"))
        [d-name-kw-start d-name-kw-stop] (h/load-code-and-locs (h/code "(ns d.dd)"
                                                                       "(def name |::name|)") (h/file-uri "file:///d.clj"))
        [kw-aliased-start kw-aliased-stop
         kw-unaliased-start kw-unaliased-stop] (h/load-code-and-locs (h/code "(ns e.ee (:require [d.dd :as dd]))"
                                                                             "(def name |::dd/name|)"
                                                                             "(def other-name |:d.dd/name|)") (h/file-uri "file:///e.clj"))
        [main-uname-kw-start main-uname-kw-end] (h/load-code-and-locs (h/code "(ns main (:require [user :as u]))"
                                                                              "(def name |::u/name|)") (h/file-uri "file:///main.cljc"))
        [uname-kw-start uname-kw-end] (h/load-code-and-locs (h/code "(ns user)"
                                                                    "(def name |::name|)") (h/file-uri "file:///user.cljc"))]
    (testing "on symbol without namespace"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///a.clj")
                                                :position (h/->position abar-start)
                                                :newName "foo"}))]
        (is (= {(h/file-uri "file:///a.clj") [{:new-text "foo" :range (h/->range abar-start abar-stop)}]
                (h/file-uri "file:///b.clj") [{:new-text "foo" :range (h/->range bbar-start bbar-stop)}]
                (h/file-uri "file:///c.clj") [{:new-text "foo" :range (h/->range cbar-start cbar-stop)}]}
               changes))))
    (testing "on symbol with metadata namespace"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///a.clj")
                                                :position (h/->position abaz-start)
                                                :newName "qux"}))]
        (is (= {(h/file-uri "file:///a.clj") [{:new-text "qux" :range (h/->range abaz-start abaz-stop)}]
                (h/file-uri "file:///c.clj") [{:new-text "qux" :range (h/->range cbaz-start cbaz-stop)}]}
               changes))))
    (testing "on symbol with namespace adds existing namespace"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///b.clj")
                                                :position (h/->position [(first bbar-start) (dec (second bbar-start))])
                                                :newName "foo"}))]
        (is (= {(h/file-uri "file:///a.clj") [{:new-text "foo" :range (h/->range abar-start abar-stop)}]
                (h/file-uri "file:///b.clj") [{:new-text "foo" :range (h/->range bbar-start bbar-stop)}]
                (h/file-uri "file:///c.clj") [{:new-text "foo" :range (h/->range cbar-start cbar-stop)}]}
               changes))))
    (testing "on symbol with namespace removes passed-in namespace"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///b.clj")
                                                :position (h/->position bbar-start)
                                                :newName "aa/foo"}))]
        (is (= {(h/file-uri "file:///a.clj") [{:new-text "foo" :range (h/->range abar-start abar-stop)}]
                (h/file-uri "file:///b.clj") [{:new-text "foo" :range (h/->range bbar-start bbar-stop)}]
                (h/file-uri "file:///c.clj") [{:new-text "foo" :range (h/->range cbar-start cbar-stop)}]}
               changes))))
    (testing "on ::keyword"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///a.clj")
                                                :position (h/->position akwbar-start)
                                                :newName "::foo"}))]
        (is (= {(h/file-uri "file:///a.clj") [{:new-text "::foo" :range (h/->range akw-start akwbar-stop)}]
                (h/file-uri "file:///b.clj") [{:new-text "::aa/foo" :range (h/->range ba2-kw-start ba2-kw-stop)}]}
               changes))))
    (testing "on single-name-namespace'd keyword"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///main.cljc")
                                                :position (h/->position main-uname-kw-start)
                                                :newName "::full-name"}))]
        (is (= {(h/file-uri "file:///main.cljc") [{:new-text "::u/full-name" :range (h/->range main-uname-kw-start main-uname-kw-end)}]
                (h/file-uri "file:///user.cljc") [{:new-text "::full-name" :range (h/->range uname-kw-start uname-kw-end)}]}
               changes))))
    (testing "on qualified keyword without alias"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///d.clj")
                                                :position (h/->position d-name-kw-start)
                                                :newName "::other-name"}))]
        (is (= {(h/file-uri "file:///d.clj") [{:new-text "::other-name" :range (h/->range d-name-kw-start d-name-kw-stop)}]
                (h/file-uri "file:///e.clj") [{:new-text "::dd/other-name" :range (h/->range kw-aliased-start kw-aliased-stop)}
                                              {:new-text ":d.dd/other-name" :range (h/->range kw-unaliased-start kw-unaliased-stop)}]}
               changes))))
    (testing "on alias changes namespaces inside file"
      (let [changes (:changes (handlers/rename {:textDocument (h/file-uri "file:///b.clj")
                                                :position (h/->position balias-start)
                                                :newName "xx"}))]
        (is (= {(h/file-uri "file:///b.clj") [{:new-text "xx" :range (h/->range balias-start balias-stop)}
                                              {:new-text "xx/bar" :range (h/->range ba1-start bbar-stop)}
                                              {:new-text "::xx/bar" :range (h/->range ba2-kw-start ba2-kw-stop)}]}
               changes))))))

(deftest test-code-actions-handle
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        (h/file-uri "file:///a.clj"))
  (h/load-code-and-locs (str "(ns other-ns (:require [some-ns :as sns]))\n"
                             "(def bar 1)\n"
                             "(defn baz []\n"
                             "  bar)")
                        (h/file-uri "file:///b.clj"))
  (h/load-code-and-locs (str "(ns another-ns)\n"
                             "(def bar ons/bar)\n"
                             "(def foo sns/foo)\n"
                             "(deftest some-test)\n"
                             "MyClass.\n"
                             "Date.")
                        (h/file-uri "file:///c.clj"))
  (testing "when it has unresolved-namespace and can find namespace"
    (is (some #(= (:title %) "Add require '[some-ns :as sns]'")
              (handlers/code-actions
                {:textDocument (h/file-uri "file:///c.clj")
                 :context {:diagnostics [{:code "unresolved-namespace"
                                          :range {:start {:line 2 :character 10}}}]}
                 :range {:start {:line 2 :character 10}}}))))
  (testing "without workspace edit client capability"
    (swap! db/db merge {:client-capabilities {:workspace {:workspace-edit false}}})
    (is (not-any? #(= (:title %) "Clean namespace")
                  (handlers/code-actions
                    {:textDocument (h/file-uri "file:///b.clj")
                     :context {:diagnostics []}
                     :range {:start {:line 1 :character 1}}}))))
  (testing "with workspace edit client capability"
    (swap! db/db merge {:client-capabilities {:workspace {:workspace-edit true}}})
    (is (some #(= (:title %) "Clean namespace")
              (handlers/code-actions
                {:textDocument (h/file-uri "file:///b.clj")
                 :context {:diagnostics []}
                 :range {:start {:line 1 :character 1}}})))))
