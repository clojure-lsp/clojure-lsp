(ns clojure-lsp.features.completion-test
  (:require
   [clojure-lsp.feature.completion :as f.completion]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(defn completion [components uri [row col]]
  (f.completion/completion (h/file-uri uri) row col (h/db components)))

(deftest test-completion
  (let [components (h/make-components {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}
                                                                             :completion {:completion-item {:resolve-support {:properties ["documentation"]}}}}}})
        [a-alpaca]
        (h/load-code (h/code "(ns alpaca.ns (:require [user :as alpaca]))"
                             "(alpaca|/)"
                             "(def barr)"
                             "(def bazz)") (h/file-uri "file:///a.cljc") components)
        [b-alp b-ba]
        (h/load-code (h/code "(ns user)"
                             "(def alpha)"
                             "al|p"
                             "ba|") (h/file-uri "file:///b.clj") components)
        [c-clj]
        (h/load-code (h/code "(ns alpaca.ns)"
                             "(def baff)"
                             "clj|-") (h/file-uri "file:///c.cljs") components)
        [d-freq]
        (h/load-code (h/code "(ns d (:require [alpaca.ns :as alpaca])) frequen|"
                             "(def bar \"some good docs\"123)"
                             "(defn barbaz [a b] 123)"
                             "(def some 123)")
                     (h/file-uri "file:///d.clj") components)
        [e-Syste e-d-alias-b e-12]
        (h/load-code (h/code "(ns e (:require [alpaca.ns :refer [ba]]"
                             "                [d :as d-alias]))"
                             "Syste|"
                             "d-alias/b|"
                             "12|3")
                     (h/file-uri "file:///e.clj") components)
        [f-alp]
        (h/load-code (h/code "(ns f (:require [alpaca.ns :refer [ba]]"
                             "                 alp|))")
                     (h/file-uri "file:///f.clj") components)
        [g-b g-outside]
        (h/load-code (h/code "(ns g (:require [alpaca.ns :as ba :refer [baq]]))"
                             "(defn bar [baz] b|a)"
                             "|(defn qux [bass] ba)"
                             "")
                     (h/file-uri "file:///g.clj") components)
        [h-commen]
        (h/load-code (h/code ";; commen|t")
                     (h/file-uri "file:///h.clj") components)]
    (testing "complete-alp"
      (h/assert-submaps
        [{:label "alpha" :kind :variable}
         {:label "alpaca" :kind :property :detail "alias to: alpaca.ns"}
         {:label "alpaca" :kind :property :detail "alias to: user"}
         {:label "alpaca.ns" :detail ":as ba"}]
        (completion components (h/file-uri "file:///b.clj") b-alp)))
    (testing "complete-ba"
      (h/assert-submaps
        [{:label "ba" :kind :property}
         {:label "ba/baff"}
         {:label "ba/barr"}
         {:label "ba/bazz"}
         {:label "bases" :detail "clojure.core/bases"}]
        (completion components (h/file-uri "file:///b.clj") b-ba)))
    (testing "complete symbols from alias"
      (h/assert-submaps
        [{:label "d-alias/bar"
          :kind :variable}
         {:label "d-alias/barbaz", :kind :function}]
        (completion components (h/file-uri "file:///e.clj") e-d-alias-b)))
    (testing "complete cljc files"
      (h/assert-submaps
        [{:label "alpaca/alpha" :kind :variable}]
        (completion components (h/file-uri "file:///a.cljc") a-alpaca)))
    (testing "complete-core-stuff"
      (h/assert-submaps
        [{:label "clj->js", :detail "cljs.core/clj->js"}]
        (completion components (h/file-uri "file:///c.cljs") c-clj))
      (h/assert-submaps
        [{:label "frequencies", :detail "clojure.core/frequencies"}]
        (completion components (h/file-uri "file:///d.clj") d-freq))
      (h/assert-submaps
        [{:label "System", :detail "java.lang.System"}]
        (completion components (h/file-uri "file:///e.clj") e-Syste)))
    (testing "complete non symbols doesn't blow up"
      (is (= nil (completion components (h/file-uri "file:///e.clj") e-12))))
    (testing "complete all available namespace definitions when inside require"
      (h/assert-submaps
        [{:label "alpaca.ns" :kind :module}]
        (completion components (h/file-uri "file:///f.clj") f-alp)))
    (testing "complete locals"
      (h/assert-submaps
        [{:label "baz" :kind :variable}
         {:label "bar" :kind :function}
         {:label "ba" :kind :property}
         {:label "ba/baff" :kind :variable}
         {:label "ba/barr" :kind :variable}
         {:label "ba/bazz" :kind :variable}
         {:label "bases" :detail "clojure.core/bases"}]
        (completion components (h/file-uri "file:///g.clj") g-b)))
    (testing "complete without prefix return all available completions"
      (is (< 100 (count (completion components (h/file-uri "file:///g.clj") g-outside)))))
    (testing "complete comment returns nothing"
      (is (empty? (completion components (h/file-uri "file:///h.clj") h-commen))))))

(deftest completing-aliases
  (let [components (h/make-components)]
    (h/load-code (h/code "(ns bbb)"
                         "(def bar)")
                 (h/file-uri "file:///bbb.clj") components)
    (h/load-code (h/code "(ns ccc"
                         "  (:require [bbb :as bb]))")
                 (h/file-uri "file:///ccc.clj") components)
    (let [[bb]
          (h/load-code (h/code "(ns aaa)"
                               "bb|")
                       (h/file-uri "file:///aaa.clj") components)]
      (testing "without resolve support"
        (swap! (:db* components) merge {:client-capabilities {:text-document {:completion {:completion-item {:resolve-support {:properties ["documentation"]}}}}}})
        (h/assert-submaps
          [;; to ns
           {:label "bb",
            :kind :property,
            :detail "alias to: bbb",
            :additional-text-edits
            [{:range {:start {:line 0, :character 0}, :end {:line 0, :character 8}},
              :new-text (h/code "(ns aaa "
                                "  (:require"
                                "    [bbb :as bb]))")}]}
           ;; to var in ns
           {:label "bb/bar",
            :kind :variable
            :data {"unresolved" [["documentation" {"name" "bar"
                                                   "filename" "/bbb.clj"
                                                   "name-row" 2
                                                   "name-col" 6}]]},
            :additional-text-edits
            [{:range {:start {:line 0, :character 0}, :end {:line 0, :character 8}},
              :new-text (h/code "(ns aaa "
                                "  (:require"
                                "    [bbb :as bb]))")}]}]
          (completion components (h/file-uri "file:///aaa.clj") bb)))
      (testing "with resolve support"
        (swap! (:db* components) merge {:client-capabilities {:text-document {:completion {:completion-item {:resolve-support {:properties ["documentation" "additionalTextEdits"]}}}}}})
        (h/assert-submaps
          [;; to ns
           {:label "bb",
            :kind :property,
            :detail "alias to: bbb",
            :data {"unresolved" [["alias" {"ns-to-add" "bbb"
                                           "alias-to-add" "bb"
                                           "uri" "file:///aaa.clj"}]]}}
           ;; to var in ns
           {:label "bb/bar",
            :kind :variable
            :data {"unresolved" [["documentation" {"name" "bar"
                                                   "filename" "/bbb.clj"
                                                   "name-row" 2
                                                   "name-col" 6}]
                                 ["alias" {"ns-to-add" "bbb"
                                           "alias-to-add" "bb"
                                           "uri" "file:///aaa.clj"}]]}}]
          (completion components (h/file-uri "file:///aaa.clj") bb))))))

(deftest completing-full-ns
  (let [components (h/make-components)]
    (h/load-code
      (h/code "(ns alpaca.ns)"
              "(def foo 1)") h/default-uri components)
    (let [[full-ns] (h/load-code
                      (h/code "(ns foo)"
                              "(alpaca.ns/f|)") (h/file-uri "file:///b.clj") components)]
      (testing "completing a project full ns"
        (h/assert-submaps
          [{:label "alpaca.ns/foo" :kind :variable}]
          (completion components (h/file-uri "file:///b.clj") full-ns))))))

(deftest completing-all-ns-vars
  (let [components (h/make-components)]
    (h/load-code
      (h/code "(ns alpaca.ns)"
              "(def foo 1)"
              "(defn foobar [] 2)"
              "(def bar 3)")
      h/default-uri components)
    (let [[all-vars] (h/load-code
                       (h/code "(ns other.ns"
                               " (:require [alpaca.ns :as alpaca]))"
                               "alpaca/|"
                               "")
                       (h/file-uri "file:///b.clj") components)]
      (testing "completing all vars of a ns alias"
        (h/assert-submaps
          [{:label "alpaca/bar" :kind :variable}
           {:label "alpaca/foo" :kind :variable}
           {:label "alpaca/foobar" :kind :function}]
          (completion components (h/file-uri "file:///b.clj") all-vars))))))

(deftest completing-with-reader-macros
  (let [components (h/make-components)
        [before-reader
         after-reader] (h/load-code
                         (h/code "(ns foo)"
                                 "(def some-function 1)"
                                 "some-fun|"
                                 "1"
                                 "#?(:clj \"clojure\" :cljs \"clojurescript\")"
                                 "1"
                                 "some-fun|") (h/file-uri "file:///a.cljc") components)]
    (testing "before reader macro"
      (h/assert-submaps
        [{:label         "some-function"
          :kind          :variable}]
        (completion components (h/file-uri "file:///a.cljc") before-reader)))
    (testing "after reader macro"
      (h/assert-submaps
        [{:label         "some-function"
          :kind          :variable}]
        (completion components (h/file-uri "file:///a.cljc") after-reader)))))

(deftest resolve-item-test
  (let [components (h/make-components {:settings {:completion {:additional-edits-warning-text "* includes additional edits"}}})]
    (h/load-code "(ns a) (def foo \"Some docs\" 1)" h/default-uri components)
    (testing "When element does not contains data"
      (is (= {:label "Some" :kind :module}
             (f.completion/resolve-item {:label "Some" :kind :module} (:db* components)))))
    (testing "When element needs documentation and has a position"
      (h/assert-submap {:label "foo"
                        :documentation [{:language "clojure" :value "a/foo"}
                                        "Some docs"
                                        (h/file-path "/a.clj")]
                        :kind :variable}
                       (f.completion/resolve-item {:label "foo"
                                                   :kind :variable
                                                   :data {:unresolved [["documentation"
                                                                        {:name "foo"
                                                                         :filename (h/file-path "/a.clj")
                                                                         :name-row 1
                                                                         :name-col 13}]]}}
                                                  (:db* components))))
    (testing "When element needs documentation and has a namespace"
      (h/assert-submap {:label "foo"
                        :documentation [{:language "clojure" :value "a/foo"}
                                        "Some docs"
                                        (h/file-path "/a.clj")]
                        :kind :function}
                       (f.completion/resolve-item {:label "foo"
                                                   :kind :function
                                                   :data {:unresolved [["documentation"
                                                                        {:name "foo"
                                                                         :filename (h/file-path "/a.clj")
                                                                         :ns "a"}]]}}
                                                  (:db* components))))
    (testing "When element needs an alias"
      (h/load-code "(ns aaa)" (h/file-uri "file:///aaa.clj") components)
      (h/assert-submap {:label "foo"
                        :kind :function
                        :additional-text-edits [{:range {:start {:line 0, :character 0}, :end {:line 0, :character 8}},
                                                 :new-text (h/code "(ns aaa "
                                                                   "  (:require"
                                                                   "    [bbb :as b]))")}]}
                       (f.completion/resolve-item {:label "foo"
                                                   :kind :function
                                                   :data {:unresolved [["alias"
                                                                        {:ns-to-add "bbb"
                                                                         :alias-to-add "b"
                                                                         :uri (h/file-uri "file:///aaa.clj")}]]}}
                                                  (:db* components))))
    (testing "When element needs an alias and documentation"
      (h/load-code "(ns aaa)" (h/file-uri "file:///aaa.clj") components)
      (h/assert-submap {:label "foo"
                        :documentation [{:language "clojure" :value "a/foo"}
                                        "* includes additional edits"
                                        "Some docs"
                                        (h/file-path "/a.clj")]
                        :kind :function
                        :additional-text-edits [{:range {:start {:line 0, :character 0}, :end {:line 0, :character 8}},
                                                 :new-text (h/code "(ns aaa "
                                                                   "  (:require"
                                                                   "    [bbb :as b]))")}]}
                       (f.completion/resolve-item {:label "foo"
                                                   :kind :function
                                                   :data {:unresolved [["documentation"
                                                                        {:name "foo"
                                                                         :filename (h/file-path "/a.clj")
                                                                         :name-row 1
                                                                         :name-col 13}]
                                                                       ["alias"
                                                                        {:ns-to-add "bbb"
                                                                         :alias-to-add "b"
                                                                         :uri (h/file-uri "file:///aaa.clj")}]]}}
                                                  (:db* components))))))

(deftest completing-refers
  (let [components (h/make-components)]
    (h/load-code
      (h/code "(ns some-ns)"
              "(def foob 1)"
              "(defn barb [] foo)") (h/file-uri "file:///a.clj") components)
    (h/load-code
      (h/code "(ns alpaca.ns)"
              "(def foo 1)"
              "(defn bar [] foo)") (h/file-uri "file:///b.clj") components)
    (let [[refer-vec] (h/load-code
                        (h/code "(ns foo"
                                "  (:require [alpaca.ns :refer [|]]))") (h/file-uri "file:///c.clj") components)
          [ba-refer] (h/load-code
                       (h/code "(ns foo"
                               "  (:require [alpaca.ns :refer [ba|]]))") (h/file-uri "file:///d.clj") components)]
      (testing "completing all available refers"
        (h/assert-submaps
          [{:label "bar" :kind :function}
           {:label "foo" :kind :variable}]
          (completion components (h/file-uri "file:///c.clj") refer-vec)))
      (testing "completing specific refer"
        (h/assert-submaps
          [{:label "bar" :kind :function}]
          (completion components (h/file-uri "file:///d.clj") ba-refer))))))

(deftest completing-known-snippets
  (let [components (h/make-components)
        [comment-rc
         comm-rc]
        (h/load-code
          (h/code "(ns foo)"
                  "comment|"
                  "(comm|)") h/default-uri components)]
    (testing "completing comment snippet when client does not support snippets"
      (swap! (:db* components) merge {:client-capabilities {:text-document {:completion {:completion-item {:snippet-support false
                                                                                                           :resolve-support {:properties ["documentation"]}}}}}})
      (h/assert-submaps
        [{:label "comment"
          :kind :function
          :data {"unresolved" [["documentation" {"filename" "/clojure.core.clj"
                                                 "name" "comment"
                                                 "ns" "clojure.core"}]]}
          :detail "clojure.core/comment"}]
        (completion components (h/file-uri "file:///a.clj") comment-rc)))
    (testing "completing with snippets enable return all available snippets"
      (swap! (:db* components) merge {:client-capabilities {:text-document {:completion {:completion-item {:snippet-support true}}}}})
      (h/assert-submaps
        [{:label "comment"
          :detail "Insert comment block"
          :insert-text "(comment\n  $0\n  )"
          :kind :snippet
          :insert-text-format :snippet}]
        (filter (comp #(= "comment" %) :label)
                (completion components (h/file-uri "file:///a.clj") comment-rc))))
    (testing "completing from a function call should not create duplicate parens"
      (swap! (:db* components) merge {:client-capabilities {:text-document {:completion {:completion-item {:snippet-support true}}}}})
      (h/assert-submaps
        [{:label "comment"
          :detail "Insert comment block"
          :insert-text "comment\n  $0\n  "
          :kind :snippet
          :insert-text-format :snippet}]
        (filter (comp #(= "comment" %) :label)
                (completion components (h/file-uri "file:///a.clj") comm-rc))))))

(deftest completing-user-snippets
  (let [components (h/make-components {:settings {:additional-snippets
                                                  [{:name "wrap-in-let-sexpr$"
                                                    :detail "Wrap in let"
                                                    :snippet "(let [$1] $0$current-form)"}]}
                                       :client-capabilities {:text-document {:completion {:completion-item {:snippet-support true}}}}})
        [range-start in-snippet range-end]
        (h/load-code
          (h/code "(ns foo)"
                  "(defn foo []"
                  "  |wrap-in|(+ 1 2)|)") h/default-uri components)]
    (testing "completing replacing $current-form"
      (h/assert-submaps
        [{:label "wrap-in-let-sexpr$"
          :detail "Wrap in let"
          :text-edit {:range (h/->range range-start range-end)
                      :new-text "(let [$1] $0(+ 1 2))"}
          :kind :snippet
          :insert-text-format :snippet}]
        (filter (comp #(= "wrap-in-let-sexpr$" %) :label)
                (completion components (h/file-uri "file:///a.clj") in-snippet))))))

(deftest completing-locals
  (let [components (h/make-components)
        [before-let in-let after-let]
        (h/load-code
          (h/code "(ns foo)"
                  "(defn some [bar]"
                  "  ba|"
                  "  (let [baz 1]"
                  "    ba|)"
                  "  ba|)"
                  "(defn other [bass] 1)") h/default-uri components)]
    (testing "outside before let"
      (h/assert-submaps
        [{:label "bar" :kind :variable}
         {:label "bases" :kind :function :detail "clojure.core/bases"}]
        (completion components h/default-uri before-let)))
    (testing "inside let"
      (h/assert-submaps
        [{:label "bar" :kind :variable}
         {:label "baz" :kind :variable}
         {:label "bases" :kind :function :detail "clojure.core/bases"}]
        (completion components h/default-uri in-let)))
    (testing "outside after let"
      (h/assert-submaps
        [{:label "bar" :kind :variable}
         {:label "bases" :kind :function :detail "clojure.core/bases"}]
        (completion components h/default-uri after-let)))))

(deftest completing-normal-keywords
  (let [components (h/make-components)
        ;; TODO: this fails if cursor is ":foo|". Is that a bug?
        [kw-rc]
        (h/load-code (h/code ":fo|o") h/default-uri components)]
    (h/assert-submaps
      []
      (completion components h/default-uri kw-rc))))

(deftest completing-aliased-keywords
  (let [components (h/make-components)]
    (h/load-code
      (h/code "(ns some.alpaca (:require [clojure.spec.alpha :as s]))"
              "(s/def ::foo 1)"
              "(s/def ::foob 1)"
              "(s/def ::bar 1)"
              "::fooba"
              ":some.alpaca/foobar"
              "(defn fooba [bass] 1)") (h/file-uri "file:///some/alpaca.clj") components)
    (let [[kw-name]
          (h/load-code
            (h/code "(ns other.ns (:require [some.alpaca :as alp]))"
                    "::alp/f|"
                    "") (h/file-uri "file:///other/ns.clj") components)
          [kw-slash]
          (h/load-code
            (h/code "(ns other.ns (:require [some.alpaca :as alp]))"
                    "::alp/|"
                    "") (h/file-uri "file:///someother/ns.clj") components)]
      (testing "return all matching reg keywords for that aliased keyword"
        (h/assert-submaps
          [{:label "::alp/foo" :kind :keyword}
           {:label "::alp/foob" :kind :keyword}]
          (completion components (h/file-uri "file:///other/ns.clj") kw-name)))
      (testing "return all reg keywords for plain alias"
        (h/assert-submaps
          [{:label "::alp/bar" :kind :keyword}
           {:label "::alp/foo" :kind :keyword}
           {:label "::alp/foob" :kind :keyword}]
          (completion components (h/file-uri "file:///someother/ns.clj") kw-slash))))))

(deftest completing-arg-keywords-from-function-definition
  (let [components (h/make-components)]
    (h/load-code (h/code "(ns some.a) (defn my-api [{:keys [foo bar baz] :as bla}] 1)")
                 (h/file-uri "file:///some/a.clj") components)
    (let [[colon-rc]
          (h/load-code (h/code "(ns some.b (:require [some.a :as a]))"
                               "(a/my-api {:|})")
                       (h/file-uri "file:///some/b.clj") components)
          [map-key-rc]
          (h/load-code (h/code "(ns some.c (:require [some.a :as a]))"
                               "(a/my-api {:foo 1"
                               "           :b|})")
                       (h/file-uri "file:///some/c.clj") components)]
      (h/assert-submaps
        [{:label ":bar" :kind :keyword}
         {:label ":baz" :kind :keyword}
         {:label ":foo" :kind :keyword}
         {:label ":as" :kind :keyword}
         {:label ":require" :kind :keyword}]
        (completion components (h/file-uri "file:///some/b.clj") colon-rc))
      (h/assert-submaps
        [{:label ":bar" :kind :keyword}
         {:label ":baz" :kind :keyword}]
        (completion components (h/file-uri "file:///some/c.clj") map-key-rc)))))

(deftest completing-java-class-usages
  (let [components (h/make-components)
        [Loca So]
        (h/load-code
          (h/code "(ns foo (:import (java.time LocalDateTime) (foo SomeClass)))"
                  "Loc|alD"
                  "S|omeC") h/default-uri components)]
    (testing "known class import"
      (h/assert-submaps
        [{:label "LocalDateTime" :kind :class :detail "java.time.LocalDateTime"}]
        (completion components h/default-uri Loca)))
    (testing "unknown class import"
      (h/assert-submaps
        [{:label "SomeClass" :kind :class :detail "foo.SomeClass"}]
        (completion components h/default-uri So)))))

(deftest completing-sorting
  (let [components (h/make-components)
        [rc] (h/load-code
               (h/code "(ns foo)"
                       ":foo"
                       "(def foo 1)"
                       "(let [foob 1]"
                       " fo|)") h/default-uri components)]
    (testing "items are sorted properly"
      (h/assert-submaps
        [{:label "foob", :kind :variable}
         {:label "foo", :kind :module}
         {:label "foo", :kind :variable}
         {:label ":foo", :kind :keyword, :detail ""}
         {:label "for", :kind :function, :detail "clojure.core/for"}
         {:label "force", :kind :function, :detail "clojure.core/force"}
         {:label "format", :kind :function, :detail "clojure.core/format"}]
        (completion components h/default-uri rc)))))
