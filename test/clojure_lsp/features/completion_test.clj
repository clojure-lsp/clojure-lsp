(ns clojure-lsp.features.completion-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.completion :as f.completion]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(deftest test-completion
  (h/load-code-and-locs (h/code "(ns alpaca.ns (:require [user :as alpaca]))"
                                "(alpaca/)"
                                "(def barr)"
                                "(def bazz)") (h/file-uri "file:///a.cljc"))
  (h/load-code-and-locs (h/code "(ns user)"
                                "(def alpha)"
                                "alp"
                                "ba") (h/file-uri "file:///b.clj"))
  (h/load-code-and-locs (h/code "(ns alpaca.ns)"
                                "(def baff)") (h/file-uri "file:///c.cljs"))
  (h/load-code-and-locs (h/code "(ns d (:require [alpaca.ns :as alpaca])) frequen"
                                "(def bar \"some good docs\"123)"
                                "(defn barbaz [a b] 123)"
                                "(def some 123)")
                        (h/file-uri "file:///d.clj"))
  (h/load-code-and-locs (h/code "(ns e (:require [alpaca.ns :refer [ba]]"
                                "                [d :as d-alias]))"
                                "Syste"
                                "d-alias/b"
                                "123")
                        (h/file-uri "file:///e.clj"))
  (h/load-code-and-locs (h/code "(ns f (:require [alpaca.ns :refer [ba]]"
                                "                 alp))")
                        (h/file-uri "file:///f.clj"))
  (h/load-code-and-locs (h/code "(ns g (:require [alpaca.ns :as ba :refer [baq]]))"
                                "(defn bar [baz] ba)"
                                "(defn qux [bass] ba)"
                                "")
                        (h/file-uri "file:///g.clj"))

  (swap! db/db merge {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
  (testing "complete-alp"
    (h/assert-submaps
      [{:label "alpha" :kind :variable}
       {:label "alpaca" :kind :property :detail "alpaca.ns"}
       {:label "alpaca" :kind :property :detail "user"}
       {:label "ba" :detail "alpaca.ns"}]
      (f.completion/completion (h/file-uri "file:///b.clj") 3 3 db/db)))
  (testing "complete-ba"
    (h/assert-submaps
      [{:label "ba" :kind :property}
       {:label "ba/baff"}
       {:label "ba/barr"}
       {:label "ba/bazz"}
       {:label "bases" :detail "clojure.core/bases"}]
      (f.completion/completion (h/file-uri "file:///b.clj") 4 3 db/db)))
  (testing "complete-core-stuff"
    (h/assert-submaps
      [{:label "frequencies", :detail "clojure.core/frequencies"}]
      (f.completion/completion (h/file-uri "file:///d.clj") 1 49 db/db))
    (testing "complete symbols from alias"
      (h/assert-submaps
        [{:label "d-alias/bar"
          :kind :variable}
         {:label "d-alias/barbaz", :kind :function}]
        (f.completion/completion (h/file-uri "file:///e.clj") 4 10 db/db))))
  (testing "complete cljc files"
    (h/assert-submaps
      [{:label "alpaca/alpha" :kind :variable}
       {:label "alpaca/baff" :kind :variable}
       {:label "alpaca/barr" :kind :variable}
       {:label "alpaca/bazz" :kind :variable}]
      (f.completion/completion (h/file-uri "file:///a.cljc") 2 8 db/db)))
  (testing "complete-core-stuff"
    (h/assert-submaps
      [{:label "frequencies", :detail "clojure.core/frequencies"}]
      (f.completion/completion (h/file-uri "file:///d.clj") 1 49 db/db))
    (h/assert-submaps
      [{:label "System", :detail "java.lang.System"}]
      (f.completion/completion (h/file-uri "file:///e.clj") 3 6 db/db)))
  (testing "complete non symbols doesn't blow up"
    (is (= nil (f.completion/completion (h/file-uri "file:///e.clj") 5 3 db/db))))
  (testing "complete all available namespace definitions when inside require"
    (h/assert-submaps
      [{:label "alpaca.ns" :kind :module}]
      (f.completion/completion (h/file-uri "file:///f.clj") 2 21 db/db)))
  (testing "complete locals"
    (h/assert-submaps
      [{:label "bar" :kind :function}
       {:label "baz" :kind :variable}
       {:label "ba" :kind :property}
       {:label "ba/baff" :kind :variable}
       {:label "ba/barr" :kind :variable}
       {:label "ba/bazz" :kind :variable}
       {:label "bases" :detail "clojure.core/bases"}]
      (f.completion/completion (h/file-uri "file:///g.clj") 2 18 db/db)))
  (testing "complete without prefix return all available completions"
    (is (< 100 (count (f.completion/completion (h/file-uri "file:///g.clj") 3 1 db/db))))))

(deftest completing-full-ns
  (h/load-code-and-locs
    (h/code "(ns alpaca.ns)"
            "(def foo 1)"))
  (let [[[full-ns-r full-ns-c]] (h/load-code-and-locs
                                  (h/code "(ns foo)"
                                          "(alpaca.ns/f|)") (h/file-uri "file:///b.clj"))]
    (testing "completing a project full ns"
      (h/assert-submaps
        [{:label "alpaca.ns/foo" :kind :variable}]
        (f.completion/completion (h/file-uri "file:///b.clj") full-ns-r full-ns-c db/db)))))

(deftest completing-all-ns-vars
  (h/load-code-and-locs
    (h/code "(ns alpaca.ns)"
            "(def foo 1)"
            "(defn foobar [] 2)"
            "(def bar 3)"))
  (let [[[all-vars-r all-vars-c]] (h/load-code-and-locs
                                    (h/code "(ns other.ns"
                                            " (:require [alpaca.ns :as alpaca]))"
                                            "alpaca/|"
                                            "")
                                    (h/file-uri "file:///b.clj"))]
    (testing "completing all vars of a ns alias"
      (h/assert-submaps
        [{:label "alpaca/bar" :kind :variable}
         {:label "alpaca/foo" :kind :variable}
         {:label "alpaca/foobar" :kind :function}]
        (f.completion/completion (h/file-uri "file:///b.clj") all-vars-r all-vars-c db/db)))))

(deftest completing-with-reader-macros
  (let [[[before-reader-r before-reader-c]
         [after-reader-r after-reader-c]] (h/load-code-and-locs
                                            (h/code "(ns foo)"
                                                    "(def some-function 1)"
                                                    "some-fun|"
                                                    "1"
                                                    "#?(:clj \"clojure\" :cljs \"clojurescript\")"
                                                    "1"
                                                    "some-fun|") (h/file-uri "file:///a.cljc"))]
    (testing "before reader macro"
      (h/assert-submaps
        [{:label         "some-function"
          :kind          :variable}]
        (f.completion/completion (h/file-uri "file:///a.cljc") before-reader-r before-reader-c db/db)))
    (testing "after reader macro"
      (h/assert-submaps
        [{:label         "some-function"
          :kind          :variable}]
        (f.completion/completion (h/file-uri "file:///a.cljc") after-reader-r after-reader-c db/db)))))

(deftest resolve-item-test
  (h/load-code-and-locs "(ns a) (def foo \"Some docs\" 1)")
  (testing "When element does not contains data"
    (is (= {:label "Some" :kind :module}
           (f.completion/resolve-item {:label "Some" :kind :module} db/db))))
  (testing "When element contains data of a element/knows the element"
    (is (= {:label "foo"
            :documentation [{:language "clojure" :value "a/foo"}
                            "Some docs"
                            (h/file-path "/a.clj")]
            :kind :variable}
           (f.completion/resolve-item {:label "foo"
                                       :kind :variable
                                       :data {:name "foo"
                                              :filename (h/file-path "/a.clj")
                                              :name-row 1
                                              :name-col 13}}
                                      db/db))))
  (testing "When element contains data of a element/knows the element"
    (is (= {:label "foo"
            :documentation [{:language "clojure" :value "a/foo"}
                            "Some docs"
                            (h/file-path "/a.clj")]
            :kind :function}
           (f.completion/resolve-item {:label "foo"
                                       :kind :function
                                       :data {:name "foo"
                                              :filename (h/file-path "/a.clj")
                                              :name-row 1
                                              :name-col 13
                                              :ns "a"}}
                                      db/db)))))

(deftest completing-refers
  (h/load-code-and-locs
    (h/code "(ns some-ns)"
            "(def foob 1)"
            "(defn barb [] foo)") (h/file-uri "file:///a.clj"))
  (h/load-code-and-locs
    (h/code "(ns alpaca.ns)"
            "(def foo 1)"
            "(defn bar [] foo)") (h/file-uri "file:///b.clj"))
  (let [[[refer-vec-r refer-vec-c]] (h/load-code-and-locs
                                      (h/code "(ns foo"
                                              "  (:require [alpaca.ns :refer [|]]))") (h/file-uri "file:///c.clj"))
        [[ba-refer-r ba-refer-c]] (h/load-code-and-locs
                                    (h/code "(ns foo"
                                            "  (:require [alpaca.ns :refer [ba|]]))") (h/file-uri "file:///d.clj"))]
    (testing "completing all available refers"
      (h/assert-submaps
        [{:label "bar" :kind :function}
         {:label "foo" :kind :variable}]
        (f.completion/completion (h/file-uri "file:///c.clj") refer-vec-r refer-vec-c db/db)))
    (testing "completing specific refer"
      (h/assert-submaps
        [{:label "bar" :kind :function}]
        (f.completion/completion (h/file-uri "file:///d.clj") ba-refer-r ba-refer-c db/db)))))

(deftest completing-known-snippets
  (h/load-code-and-locs
    (h/code "(ns foo)"
            "comment$"))

  (testing "completing comment snippet when client does not support snippets"
    (swap! db/db merge {:client-capabilities {:text-document {:completion {:completion-item {:snippet-support false}}}}})
    (h/assert-submaps
      []
      (f.completion/completion (h/file-uri "file:///a.clj") 2 9 db/db)))

  (testing "completing with snippets enable return all available snippets"
    (swap! db/db merge {:client-capabilities {:text-document {:completion {:completion-item {:snippet-support true}}}}})
    (h/assert-submaps
      [{:label "comment$"
        :detail "Create comment block"
        :insert-text "(comment\n  $0\n  )"
        :kind :snippet
        :insert-text-format :snippet}]
      (filter (comp #(= "comment$" %) :label) (f.completion/completion (h/file-uri "file:///a.clj") 2 9 db/db)))))

(deftest completing-user-snippets
  (h/load-code-and-locs
    (h/code "(ns foo)"
            "(defn foo []"
            "  wrap-in(+ 1 2))"))

  (testing "completing replacing $current-form"
    (swap! db/db merge {:settings {:additional-snippets
                                   [{:name "wrap-in-let-sexpr$"
                                     :detail "Wrap in let"
                                     :snippet "(let [$1] $0$current-form)"}]}
                        :client-capabilities {:text-document {:completion {:completion-item {:snippet-support true}}}}})
    (h/assert-submaps
      [{:label "wrap-in-let-sexpr$"
        :detail "Wrap in let"
        :text-edit {:range {:start {:line 2 :character 2}
                            :end {:line 2 :character 16}}
                    :new-text "(let [$1] $0(+ 1 2))"}
        :kind :snippet
        :insert-text-format :snippet}]
      (filter (comp #(= "wrap-in-let-sexpr$" %) :label) (f.completion/completion (h/file-uri "file:///a.clj") 3 10 db/db)))))

(deftest completing-locals
  (h/load-code-and-locs
    (h/code "(ns foo)"
            "(defn some [bar]"
            "  ba"
            "  (let [baz 1]"
            "    ba)"
            "  ba)"
            "(defn other [bass] 1)"))
  (testing "outside before let"
    (h/assert-submaps
      [{:label "bar" :kind :variable}
       {:label "bases" :kind :reference :detail "clojure.core/bases"}]
      (f.completion/completion (h/file-uri "file:///a.clj") 3 5 db/db)))
  (testing "inside let"
    (h/assert-submaps
      [{:label "bar" :kind :variable}
       {:label "baz" :kind :variable}
       {:label "bases" :kind :reference :detail "clojure.core/bases"}]
      (f.completion/completion (h/file-uri "file:///a.clj") 5 7 db/db)))
  (testing "outside before let"
    (h/assert-submaps
      [{:label "bar" :kind :variable}
       {:label "bases" :kind :reference :detail "clojure.core/bases"}]
      (f.completion/completion (h/file-uri "file:///a.clj") 6 5 db/db))))

(deftest completing-sorting
  (h/load-code-and-locs
    (h/code "(ns foo)"
            ":foo"
            "(def foo 1)"
            "fo"))
  (testing "completing replacing $current-form"
    (h/assert-submaps
      [{:label "foo", :kind :module}
       {:label "foo", :kind :variable}
       {:label ":foo", :kind :keyword, :detail ""}
       {:label "for", :kind :reference, :detail "clojure.core/for"}
       {:label "force", :kind :reference, :detail "clojure.core/force"}
       {:label "format", :kind :reference, :detail "clojure.core/format"}]
      (f.completion/completion (h/file-uri "file:///a.clj") 4 2 db/db))))
