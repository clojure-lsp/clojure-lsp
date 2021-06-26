(ns clojure-lsp.features.completion-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.completion :as f.completion]
   [clojure-lsp.test-helper :as h]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(defn code [& strings] (string/join "\n" strings))

(deftest test-completion
  (h/load-code-and-locs (code "(ns alpaca.ns (:require [user :as alpaca]))"
                              "(alpaca/)"
                              "(def barr)"
                              "(def bazz)") (h/file-uri "file:///a.cljc"))
  (h/load-code-and-locs (code "(ns user)"
                              "(def alpha)"
                              "alp"
                              "ba") (h/file-uri "file:///b.clj"))
  (h/load-code-and-locs (code "(ns alpaca.ns)"
                              "(def baff)") (h/file-uri "file:///c.cljs"))
  (h/load-code-and-locs (code "(ns d (:require [alpaca.ns :as alpaca])) frequen"
                              "(def bar \"some good docs\"123)"
                              "(defn barbaz [a b] 123)"
                              "(def some 123)")
                        (h/file-uri "file:///d.clj"))
  (h/load-code-and-locs (code "(ns e (:require [alpaca.ns :refer [ba]]"
                              "                [d :as d-alias]))"
                              "Syste"
                              "d-alias/b"
                              "123")
                        (h/file-uri "file:///e.clj"))
  (h/load-code-and-locs (code "(ns f (:require [alpaca.ns :refer [ba]]"
                              "                 alp))")
                        (h/file-uri "file:///f.clj"))
  (h/load-code-and-locs (code "(ns g (:require [alpaca.ns :as ba :refer [baq]]))"
                              "(defn bar [baz] ba)"
                              "")
                        (h/file-uri "file:///g.clj"))

  (swap! db/db merge {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
  (testing "complete-alp"
    (h/assert-submaps
     [{:label "alpaca" :kind :property :detail "alpaca.ns"}
      {:label "alpaca" :kind :property :detail "user"}
      {:label "alpha" :kind :variable}
      {:label "ba" :detail "alpaca.ns"}]
     (f.completion/completion (h/file-uri "file:///b.clj") 3 3)))
  (testing "complete-ba"
    (h/assert-submaps
     [{:label "ba" :kind :property}
      {:label "ba/baff"}
      {:label "ba/barr"}
      {:label "ba/bazz"}
      {:label "bases" :detail "clojure.core/bases"}]
     (f.completion/completion (h/file-uri "file:///b.clj") 4 3)))
  (testing "complete-core-stuff"
    (h/assert-submaps
     [{:label "frequencies", :detail "clojure.core/frequencies"}]
     (f.completion/completion (h/file-uri "file:///d.clj") 1 49))
    (testing "complete symbols from alias"
      (h/assert-submaps
       [{:label "d-alias/bar"
         :kind :variable}
        {:label "d-alias/barbaz", :kind :function}]
       (f.completion/completion (h/file-uri "file:///e.clj") 4 10))))
  (testing "complete cljc files"
    (h/assert-submaps
     [{:label "alpaca/alpha" :kind :variable}
      {:label "alpaca/baff" :kind :variable}
      {:label "alpaca/barr" :kind :variable}
      {:label "alpaca/bazz" :kind :variable}]
     (f.completion/completion (h/file-uri "file:///a.cljc") 2 8)))
  (testing "complete-core-stuff"
    (h/assert-submaps
     [{:label "frequencies", :detail "clojure.core/frequencies"}]
     (f.completion/completion (h/file-uri "file:///d.clj") 1 49))
    (h/assert-submaps
     [{:label "System", :detail "java.lang.System"}]
     (f.completion/completion (h/file-uri "file:///e.clj") 3 6)))
  (testing "complete non symbols doesn't blow up"
    (is (= nil (f.completion/completion (h/file-uri "file:///e.clj") 5 3))))
  (testing "complete all available namespace definitions when inside require"
    (h/assert-submaps
     [{:label "alpaca.ns" :kind :module}
      {:label "alpaca.ns" :kind :module}]
     (f.completion/completion (h/file-uri "file:///f.clj") 2 21)))
  (testing "complete locals"
    (h/assert-submaps
     [{:label "ba" :kind :property}
      {:label "ba/baff"}
      {:label "ba/barr"}
      {:label "ba/bazz"}
      {:label "bar"}
      {:label "bases" :detail "clojure.core/bases"}
      {:label "baz"}
       ;; TODO should complete local refer
      #_{:label "baq"}]
     (f.completion/completion (h/file-uri "file:///g.clj") 2 18)))
  (testing "complete without prefix return all available completions"
    (is (< 100 (count (f.completion/completion (h/file-uri "file:///g.clj") 3 1))))))

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
       (f.completion/completion (h/file-uri "file:///b.clj") full-ns-r full-ns-c)))))

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
       (f.completion/completion (h/file-uri "file:///a.cljc") before-reader-r before-reader-c)))
    (testing "after reader macro"
      (h/assert-submaps
       [{:label         "some-function"
         :kind          :variable}]
       (f.completion/completion (h/file-uri "file:///a.cljc") after-reader-r after-reader-c)))))

(deftest resolve-item-test
  (h/load-code-and-locs "(ns a) (def foo \"Some docs\" 1)")
  (testing "When element does not contains data"
    (is (= {:label "Some" :kind :module}
           (f.completion/resolve-item {:label "Some" :kind :module}))))
  (testing "When element contains data of a element/knows the element"
    (is (= {:label "foo" :documentation [(str "a/foo\n\n----\nSome docs\n----\n" (h/file-path "/a.clj"))] :kind :variable}
           (f.completion/resolve-item {:label "foo"
                                       :kind :variable
                                       :data {:name "foo"
                                              :filename (h/file-path "/a.clj")
                                              :name-row 1
                                              :name-col 13}}))))
  (testing "When element contains data of a element/knows the element"
    (is (= {:label "foo" :documentation [(str "a/foo\n\n----\nSome docs\n----\n" (h/file-path "/a.clj"))] :kind :function}
           (f.completion/resolve-item {:label "foo"
                                       :kind :function
                                       :data {:name "foo"
                                              :filename (h/file-path "/a.clj")
                                              :name-row 1
                                              :name-col 13
                                              :ns "a"}})))))

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
       (f.completion/completion (h/file-uri "file:///c.clj") refer-vec-r refer-vec-c)))
    (testing "completing specific refer"
      (h/assert-submaps
        [{:label "bar" :kind :function}]
       (f.completion/completion (h/file-uri "file:///d.clj") ba-refer-r ba-refer-c)))))

(deftest completing-known-snippets
  (h/load-code-and-locs
    (h/code "(ns foo)"
            "comment$"))

  (testing "completing comment snippet when client does not support snippets"
    (swap! db/db merge {:client-capabilities {:text-document {:completion {:completion-item {:snippet-support false}}}}})
    (h/assert-submaps
      []
      (f.completion/completion (h/file-uri "file:///a.clj") 2 9)))

  (testing "completing with snippets enable return all available snippets"
    (swap! db/db merge {:client-capabilities {:text-document {:completion {:completion-item {:snippet-support true}}}}})
    (h/assert-submaps
      [{:label "comment$"
        :detail "Create comment block"
        :insert-text "(comment\n  $0\n  )"
        :kind :snippet
        :insert-text-format :snippet}]
      (filter (comp #(= "comment$" %) :label) (f.completion/completion (h/file-uri "file:///a.clj") 2 9)))))

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
      (filter (comp #(= "wrap-in-let-sexpr$" %) :label) (f.completion/completion (h/file-uri "file:///a.clj") 3 10)))))
