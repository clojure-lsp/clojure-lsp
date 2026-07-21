(ns clojure-lsp.feature.clean-ns-test
  (:require
   [clojure-lsp.feature.clean-ns :as f.clean-ns]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(h/reset-components-before-test)

(defn- test-clean-ns
  ([db input-code expected-code]
   (test-clean-ns db input-code expected-code true))
  ([db input-code expected-code in-form]
   (test-clean-ns db input-code expected-code in-form "file:///a.clj"))
  ([db input-code expected-code in-form uri]
   (h/reset-components!)
   (swap! (h/db*) shared/deep-merge db)
   (h/load-code-and-locs input-code (h/file-uri uri))
   (let [zloc (when in-form
                (-> (z/of-string input-code) z/down z/right z/right))
         [{:keys [loc range]}] (f.clean-ns/clean-ns-edits zloc (h/file-uri uri) (h/db))]
     (is (some? range))
     (is (= expected-code
            (z/root-string loc))))))

(deftest clean-ns-test
  (testing "with :ns-inner-blocks-indentation on next line"
    (testing "With require comments"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "   [foo  :as f]"
                             "   ;; [bar :as b]"
                             "   baz))"
                             "f/foo")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  ;; [bar :as b]"
                             "  baz"
                             "  [foo  :as f]))"
                             "f/foo")))
    (testing "without requires at start"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "   [foo  :as f] [bar :refer [b]] baz [z] ))"
                             "(s/defn func []"
                             "  (f/some))")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  baz"
                             "  [foo  :as f]"
                             "  [z]))"
                             "(s/defn func []"
                             "  (f/some))")))
    (testing "with requires at start"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require [foo  :as f] [bar :refer [b]] baz [z] ))"
                             "(s/defn func []"
                             "  (f/some))")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  baz"
                             "  [foo  :as f]"
                             "  [z]))"
                             "(s/defn func []"
                             "  (f/some))"))))
  (testing "with :ns-inner-blocks-indentation on same line"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :same-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [foo  :as f] [bar :as b] baz [z] ))"
                           "(s/defn func []"
                           "  (f/some))")
                   (h/code "(ns foo.bar"
                           " (:require baz"
                           "           [foo  :as f]"
                           "           [z]))"
                           "(s/defn func []"
                           "  (f/some))")))
  (testing "with :ns-inner-blocks-indentation :keep"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :keep}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "     [foo  :as f] [bar :as b] baz [z] ))"
                           "(s/defn func []"
                           "  (f/some))")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "     baz"
                           "     [foo  :as f]"
                           "     [z]))"
                           "(s/defn func []"
                           "  (f/some))"))
    (testing "should no op if clean"
      (let [test-no-op #(test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :keep}}}
                                       %1
                                       %1)]
        (test-no-op (h/code "(ns foo.bar"
                            " (:require  b"
                            "            f))"))
        (test-no-op (h/code "(ns foo.bar"
                            " (:require"
                            "    b"
                            "    f))"))
        (test-no-op (h/code "(ns foo.bar"
                            " (:import  java.io.File"
                            "           java.util.Date))"
                            "File Date"))
        (test-no-op (h/code "(ns foo.bar"
                            " (:import"
                            "     java.io.File"
                            "     java.util.Date))"
                            "File Date")))))
  (testing "with first require as unused"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [foo  :as f] [bar :as b] baz [z] ))"
                           "(defn func []"
                           "  (b/some))")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [bar :as b]"
                           "  baz"
                           "  [z]))"
                           "(defn func []"
                           "  (b/some))")))
  (testing "with single unused require on ns"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [foo  :as f] ))"
                           "(defn func []"
                           "  (b/some))")
                   (h/code "(ns foo.bar)"
                           "(defn func []"
                           "  (b/some))")))
  (testing "with single used require on ns"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [foo  :as f] ))"
                           "(defn func []"
                           "  (f/some))")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [foo  :as f]))"
                           "(defn func []"
                           "  (f/some))")))
  (testing "with multiple unused requires on ns"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [foo  :as f]"
                           "   [bar :as b]))")
                   (h/code "(ns foo.bar)")))
  (testing "with duplicate require with different and unused alias"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [bar :as b]"
                           "  [foo :as f]"
                           "  [foo :as fa]))"
                           "f/bar b/bar")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [bar :as b]"
                           "  [foo :as f]))"
                           "f/bar b/bar")))
  (testing "with duplicate require with both used alias"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [bar :as b]"
                           "  [foo :as f]"
                           "  [foo :as fa]))"
                           "f/bar fa/bar b/bar")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [bar :as b]"
                           "  [foo :as f]"
                           "  [foo :as fa]))"
                           "f/bar fa/bar b/bar")))
  (testing "with refer at require"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [foo  :as f] [bar :refer [some]] baz [z] ))"
                           "(defn func []"
                           "  (f/some))")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  baz"
                           "  [foo  :as f]"
                           "  [z]))"
                           "(defn func []"
                           "  (f/some))")))
  (testing "with refer as single require"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [bar :refer [some]]))")
                   (h/code "(ns foo.bar)"))
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [bar :refer :all]))")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [bar :refer :all]))")))
  (testing "in any form"
    (let [to-clean (h/code "(ns foo.bar"
                           " (:require"
                           "   [foo  :as f] [bar :refer [some]] baz [z] ))"
                           ""
                           "(defn func []"
                           "  (f/some))")]
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}
                      :documents {(h/file-uri "file:///a.clj") {:text to-clean}}}
                     to-clean
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  baz"
                             "  [foo  :as f]"
                             "  [z]))"
                             ""
                             "(defn func []"
                             "  (f/some))")
                     false)))
  (testing "with first require as a refer"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [bar :refer [some] ] [foo :as f]))"
                           ""
                           "(defn func []"
                           "  (some))")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [bar :refer [some] ]))"
                           ""
                           "(defn func []"
                           "  (some))")))
  (testing "with first require as a refer with alias"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [bar :as b :refer [some] ] [foo :as f]))"
                           ""
                           "(defn func []"
                           "  b/some"
                           "  (some))")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [bar :as b :refer [some] ]))"
                           ""
                           "(defn func []"
                           "  b/some"
                           "  (some))")))
  (testing "unused refer from multiple refers"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [bar :refer [some other] ]))"
                           "(some)")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [bar :refer [some] ]))"
                           "(some)")))
  (testing "unused refer and alias"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [baz]"
                           "  [bar :refer [some other] :as b]))")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [baz]))")))
  (testing "unused refer from single refer but used alias before"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [aba :as a]"
                           "   [bar :as b :refer [some]]))"
                           "(a/bla)"
                           "(b/another)")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [aba :as a]"
                           "  [bar :as b]))"
                           "(a/bla)"
                           "(b/another)")))
  (testing "used refer from single refer and used alias after refer"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [aba :as a]"
                           "   [bar :refer [some] :as b]))"
                           "(a/bla)"
                           "(b/another)"
                           "some")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [aba :as a]"
                           "  [bar :refer [some] :as b]))"
                           "(a/bla)"
                           "(b/another)"
                           "some")))
  (testing "unused refer from single refer but used alias after"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [aba :as a]"
                           "   [bar :refer [some] :as b]))"
                           "(a/bla)"
                           "(b/another)")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [aba :as a]"
                           "  [bar :as b]))"
                           "(a/bla)"
                           "(b/another)")))
  (testing "unused refer from multiple refers but used alias"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [bar :as b :refer [some other]]))"
                           "(other)"
                           "(b/another)")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [bar :as b :refer [other]]))"
                           "(other)"
                           "(b/another)")))
  (testing "unused middle refer from multiple refers"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [bar :refer [some other baz another] ]))"
                           "(some)"
                           "(another)"
                           "(baz)")
                   (h/code "(ns foo.bar"
                           " (:require"
                           "  [bar :refer [another baz some] ]))"
                           "(some)"
                           "(another)"
                           "(baz)")))
  (testing "unused refer and alias"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:require"
                           "   [bar :refer [some] ]"
                           "   [baz :as b]))")
                   (h/code "(ns foo.bar)")))
  (testing "sorting"
    (testing "sorts according to symbols not brackets"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  []"
                             "  [clojure.string :as str]"
                             "  [\"foo.bar\" :default Foo])"
                             " (:import"
                             "  [zebra import1]"
                             "  apple))"
                             "import1."
                             "str/split"
                             "Foo"
                             "apple.")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  []"
                             "  [\"foo.bar\" :default Foo]"
                             "  [clojure.string :as str])"
                             " (:import"
                             "  apple"
                             "  [zebra import1]))"
                             "import1."
                             "str/split"
                             "Foo"
                             "apple.")))
    (testing "don't sort imports"
      (test-clean-ns {:settings {:clean {:sort {:import false}}}}
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  [zebra import1 abc]"
                             "  (foo.bar qux baz)"
                             "  ball"
                             "  apple))"
                             "import1."
                             "apple."
                             "qux. baz."
                             "ball.")
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  [zebra import1]"
                             "  (foo.bar qux baz)"
                             "  ball"
                             "  apple))"
                             "import1."
                             "apple."
                             "qux. baz."
                             "ball.")))
    (testing "don't sort imports classes"
      (test-clean-ns {:settings {:clean {:sort {:import-classes false}}}}
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  [zebra import1 abc]"
                             "  (foo.bar qux baz)"
                             "  ball"
                             "  apple))"
                             "import1."
                             "apple."
                             "qux. baz."
                             "ball.")
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  apple"
                             "  ball"
                             "  (foo.bar qux baz)"
                             "  [zebra import1]))"
                             "import1."
                             "apple."
                             "qux. baz."
                             "ball.")))
    (testing "don't sort requires"
      (test-clean-ns {:settings {:clean {:sort {:require false}}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [zebra import1]"
                             "  ball"
                             "  apple))"
                             "import1."
                             "apple."
                             "ball.")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [zebra import1]"
                             "  ball"
                             "  apple))"
                             "import1."
                             "apple."
                             "ball.")))
    (testing "sort requires lexicographically"
      (test-clean-ns {:settings {:clean {:sort {:require :lexicographically}}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  apple"
                             "  [\"foo.bar\" :default Foo]"
                             "  [ball import1]"
                             "  ball"
                             "  [zebra]"
                             "  ))"
                             "import1."
                             "apple."
                             "Foo"
                             "ball.")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [\"foo.bar\" :default Foo]"
                             "  [ball import1]"
                             "  [zebra]"
                             "  apple"
                             "  ball))"
                             "import1."
                             "apple."
                             "Foo"
                             "ball.")))
    (testing "unsorted used imports"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  a.c.d.e.A"
                             "  (a.b "
                             "    D C F)"
                             "  (a.b.b Foo)"
                             "  a.b.c.Z.C"
                             "  a.b.c.d.Eu"
                             "  a.b.c.D.Ei))"
                             "  A C Eu Ei D C F Foo")
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  (a.b C D F)"
                             "  (a.b.b Foo)"
                             "  a.b.c.D.Ei"
                             "  a.b.c.d.Eu"
                             "  a.b.c.Z.C"
                             "  a.c.d.e.A))"
                             "  A C Eu Ei D C F Foo")))
    (testing "unsorted used imports classes on same line"
      (test-clean-ns {:settings {:clean {:ns-import-classes-indentation :same-line}}}
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  a.c.d.e.A"
                             "  (a.a "
                             "    D C E)"
                             "  (a.b "
                             "    F H I G)"
                             "  a.b.c.Z.C))"
                             "  A C E Eu Ei D C F G H I")
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  (a.a C D E)"
                             "  (a.b F"
                             "       G"
                             "       H"
                             "       I)"
                             "  a.b.c.Z.C"
                             "  a.c.d.e.A))"
                             "  A C E Eu Ei D C F G H I")))
    (testing "unsorted used imports classes with custom classes-per-line"
      (test-clean-ns {:settings {:clean {:sort {:import-classes {:classes-per-line 4}}}}}
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  (a.b "
                             "    A B C D)"
                             "  (a.c "
                             "    E F G H I)))"
                             "  A B C D E F G H I")
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  (a.b A B C D)"
                             "  (a.c"
                             "   E"
                             "   F"
                             "   G"
                             "   H"
                             "   I)))"
                             "  A B C D E F G H I"))
      (test-clean-ns {:settings {:clean {:sort {:import-classes {:classes-per-line -1}}}}}
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  (a.b "
                             "    A B C D)"
                             "  (a.c "
                             "    E F G H I)))"
                             "  A B C D E F G H I")
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  (a.b A B C D)"
                             "  (a.c E F G H I)))"
                             "  A B C D E F G H I")))
    (testing "unsorted used refer"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "   [some :refer [Dee foo bar baz]]))"
                             "   foo bar baz Dee")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [some :refer [bar baz Dee foo]]))"
                             "   foo bar baz Dee")))
    (testing "unsorted used refer with less max-line-length"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line
                                         :sort {:refer {:max-line-length 30}}}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "   [some :refer [Dee foo bar baz bla blowning]]))"
                             "   foo bar baz Dee bla blowning")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [some :refer [bar baz bla"
                             "                blowning Dee"
                             "                foo]]))"
                             "   foo bar baz Dee bla blowning"))
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line
                                         :sort {:refer {:max-line-length 30}}}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [some :refer [bar baz bla"
                             "                blowning Dee"
                             "                foo]]))"
                             "   foo bar baz Dee bla blowning")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [some :refer [bar baz bla"
                             "                blowning Dee"
                             "                foo]]))"
                             "   foo bar baz Dee bla blowning"))
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line
                                         :sort {:refer {:max-line-length 30}}}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "   [some :refer [Dee foo bar baz bla blowning cat car clown cobble doo did done danger fear fight]]))"
                             "   foo bar baz Dee bla blowning cat car clown cobble doo did done danger fear fight")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [some :refer [bar baz bla"
                             "                blowning car"
                             "                cat clown"
                             "                cobble"
                             "                danger Dee"
                             "                did done doo"
                             "                fear fight"
                             "                foo]]))"
                             "   foo bar baz Dee bla blowning cat car clown cobble doo did done danger fear fight"))
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line
                                         :sort {:refer {:max-line-length 40}}}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "   [some :refer [Dee foo bar baz bla blowning] :as s]))"
                             "   foo bar baz Dee bla blowning"
                             "   s/foo")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [some :refer [bar baz bla blowning"
                             "                Dee foo] :as s]))"
                             "   foo bar baz Dee bla blowning"
                             "   s/foo"))
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line
                                         :sort {:refer {:max-line-length 40}}}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "   [some :as s :refer [Dee foo bar baz bla blowning]]))"
                             "   foo bar baz Dee bla blowning"
                             "   s/foo")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [some :as s :refer [bar baz bla"
                             "                      blowning Dee foo]]))"
                             "   foo bar baz Dee bla blowning"
                             "   s/foo")))
    (testing "unsorted used refer with infinite max-line-length"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line
                                         :sort {:refer {:max-line-length 0}}}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "   [some :refer [Dee foo bar baz bla blowning]]))"
                             "   foo bar baz Dee bla blowning")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [some :refer [bar baz bla blowning Dee foo]]))"
                             "   foo bar baz Dee bla blowning")))
    (testing "unsorted used refer with sort disabled"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line
                                         :sort {:refer false}}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "   [some :refer [foo bar baz]]))"
                             "   foo bar baz")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [some :refer [foo bar baz]]))"
                             "   foo bar baz")))
    (testing "ns children sorting"
      (testing "keep comments"
        (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                       (h/code "(ns foo"
                               " (:require"
                               "  [clojure.set :as set] ;; important comment"
                               "  [clojure.java.io :as io]"
                               "  [clojure.edn :as edn]))"
                               "io/a"
                               "set/a"
                               "edn/b")
                       (h/code "(ns foo"
                               " (:require"
                               "  [clojure.edn :as edn]"
                               "  [clojure.java.io :as io]"
                               "  [clojure.set :as set] ;; important comment"
                               "))"
                               "io/a"
                               "set/a"
                               "edn/b")))
      (testing "import before require"
        (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                       (h/code "(ns foo.bar"
                               " (:import"
                               "  [foo Bar])"
                               " (:require"
                               "  [clojure.string :as str]))"
                               "str/join"
                               "Bar")
                       (h/code "(ns foo.bar"
                               " (:require"
                               "  [clojure.string :as str])"
                               " (:import"
                               "  [foo Bar]))"
                               "str/join"
                               "Bar")))
      (testing "import before require with other list between"
        (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                       (h/code "(ns foo.bar"
                               " (:import"
                               "  [foo Bar"
                               "       Baz"
                               "       Qux])"
                               " (:refer-clojure :exclude [next])"
                               " (:require"
                               "  [clojure.string :as str]))"
                               "str/join"
                               "Bar Qux")
                       (h/code "(ns foo.bar"
                               " (:require"
                               "  [clojure.string :as str])"
                               " (:refer-clojure :exclude [next])"
                               " (:import"
                               "  [foo Bar Qux]))"
                               "str/join"
                               "Bar Qux")))
      (testing "don't sort when :ns sort config is disabled"
        (test-clean-ns {:settings {:clean {:sort {:ns false}}}}
                       (h/code "(ns foo.bar"
                               " (:import"
                               "  [foo Bar])"
                               " (:require"
                               "  [clojure.string :as str]))"
                               "str/join"
                               "Bar")
                       (h/code "(ns foo.bar"
                               " (:import"
                               "  [foo Bar])"
                               " (:require"
                               "  [clojure.string :as str]))"
                               "str/join"
                               "Bar")))))
  (testing "single unused full package import"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:import"
                           "  java.util.Date))")
                   (h/code "(ns foo.bar)")))
  (testing "single unused package import"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:import"
                           "  [java.util Date]))")
                   (h/code "(ns foo.bar)")))
  (testing "unused full package imports"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:import "
                           "  java.util.Date java.util.Calendar java.util.List))"
                           "Calendar.")
                   (h/code "(ns foo.bar"
                           " (:import"
                           "  java.util.Calendar))"
                           "Calendar.")))
  (testing "unused package imports"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:import "
                           "  [java.util Date Calendar List Map]))"
                           "Calendar."
                           "Map.")
                   (h/code "(ns foo.bar"
                           " (:import"
                           "  [java.util Calendar Map]))"
                           "Calendar."
                           "Map.")))
  (testing "unused package imports with ns-inner-blocks-indentation :same-line"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :same-line}}}
                   (h/code "(ns foo.bar"
                           " (:import [java.util Date Calendar List Map Foo Bar]))"
                           "Calendar."
                           "Map."
                           "Foo. Bar.")
                   (h/code "(ns foo.bar"
                           " (:import [java.util"
                           "           Bar"
                           "           Calendar"
                           "           Foo"
                           "           Map]))"
                           "Calendar."
                           "Map."
                           "Foo. Bar.")))
  (testing "unused package imports with single import"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:import"
                           "  [java.util Date List]"
                           "  java.util.Calendar))"
                           "Calendar."
                           "List.")
                   (h/code "(ns foo.bar"
                           " (:import"
                           "  [java.util List]"
                           "  java.util.Calendar))"
                           "Calendar."
                           "List.")))
  (testing "unused package imports spacing"
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:import"
                           "  [java.util Date"
                           "             Calendar"
                           "             List]))"
                           "Date."
                           "List.")
                   (h/code "(ns foo.bar"
                           " (:import"
                           "  [java.util Date List]))"
                           "Date."
                           "List."))
    (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                   (h/code "(ns foo.bar"
                           " (:import"
                           "  [java.util Date"
                           "             List]))"
                           "Date."
                           "List.")
                   (h/code "(ns foo.bar"
                           " (:import"
                           "  [java.util Date List]))"
                           "Date."
                           "List.")))
  (testing "cljc conditional readers"
    (testing "remove reader conditional after removing unused single require alias"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  #?(:clj [other.zeta :as z])))")
                     (h/code "(ns foo.bar)")
                     true
                     "file:///a.cljc"))
    (testing "remove reader conditional after removing unused require alias"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  #?(:cljs [other.foo :as o]
                                       [other.foof :as f])"
                             "  #?(:clj [other.zeta :as z])"
                             "  [some.bar :as b]))"
                             "f/foo"
                             "b/bar")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  #?(:cljs [other.foof :as f])"
                             "  [some.bar :as b]))"
                             "f/foo"
                             "b/bar")
                     true
                     "file:///a.cljc"))
    (testing "remove reader conditional after removing unused require refer"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  #?(:cljs [other.foo :refer [o]]
                                       [other.foof :refer [f]])"
                             "  #?(:clj [other.zeta :refer [z]])"
                             "  [some.bar :refer [b]]))"
                             "f"
                             "b")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  #?(:cljs [other.foof :refer [f]])"
                             "  [some.bar :refer [b]]))"
                             "f"
                             "b")
                     true
                     "file:///a.cljc"))
    (testing "remove reader conditional after removing unused import"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  #?(:cljs [other.foo O]
                                       [other.foof F])"
                             "  #?(:clj [other.zeta Z])"
                             "  [some.bar B]"
                             "  [some.baz C D]))"
                             "F B C D")
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  #?(:cljs [other.foof F])"
                             "  [some.bar B]"
                             "  [some.baz C D]))"
                             "F B C D")
                     true
                     "file:///a.cljc"))
    (testing "remove single unused import inside splicing reader conditional"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  [java.util Calendar]"
                             "  #?@(:clj [(java.time DateTime)
                                        (java.io File)])"
                             "  #?@(:clj [(java.util Date)])))"
                             "#?(:clj (do Calendar))")
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  [java.util Calendar]))"
                             "#?(:clj (do Calendar))")
                     true
                     "file:///a.cljc"))
    (testing "only used import in specific lang for splicing reader conditional"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  [java.util Calendar]"
                             "  #?@(:clj [(java.time DateTime)
                                        (java.io File)])"
                             "  #?@(:clj [(java.util Date)])))"
                             "#?(:clj (do Calendar Date DateTime)) File")
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  #?@(:clj [(java.time DateTime)
                                        (java.io File)])"
                             "  #?@(:clj [(java.util Date)])"
                             "  [java.util Calendar]))"
                             "#?(:clj (do Calendar Date DateTime)) File")
                     true
                     "file:///a.cljc"))
    (testing "only used required alias in specific lang"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [other.foo :as f]"
                             "  [other.beta :as b]"
                             "  [other.zeta :as z]))"
                             "#?(:clj f/foo)"
                             "z/o")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [other.foo :as f]"
                             "  [other.zeta :as z]))"
                             "#?(:clj f/foo)"
                             "z/o")
                     true
                     "file:///a.cljc"))
    (testing "only used required refer in specific lang"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [other.foo :refer [f]]"
                             "  [other.beta :refer [b]]"
                             "  [other.zeta :refer [z]]))"
                             "#?(:clj f)"
                             "z")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [other.foo :refer [f]]"
                             "  [other.zeta :refer [z]]))"
                             "#?(:clj f)"
                             "z")
                     true
                     "file:///a.cljc"))
    (testing "only used import in specific lang"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  [other.foo F]"
                             "  [other.beta B]"
                             "  [other.zeta Z]))"
                             "#?(:clj F)"
                             "Z")
                     (h/code "(ns foo.bar"
                             " (:import"
                             "  [other.foo F]"
                             "  [other.zeta Z]))"
                             "#?(:clj F)"
                             "Z")
                     true
                     "file:///a.cljc"))
    (testing "sort reader conditionals before normal requires"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns a"
                             "  (:require"
                             "   [a.b]"
                             "   [b.c]"
                             "   #?(:cljs [b.d])"
                             "   #?@(:clj [[a.h]]"
                             "       :cljs [[a.a]])"
                             "   #?(:clj [a.b])"
                             "   [c.d]))")
                     (h/code "(ns a"
                             "  (:require"
                             "   #?(:cljs [b.d])"
                             "   #?@(:clj [[a.h]]"
                             "       :cljs [[a.a]])"
                             "   #?(:clj [a.b])"
                             "   [a.b]"
                             "   [b.c]"
                             "   [c.d]))")
                     true
                     "file:///a.cljc"))
    (testing "Mixed cases"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns a"
                             "  (:require"
                             "   [c.b]"
                             "   [c.c :refer [x]]"
                             "   [c.d :refer [f]]"
                             "   [c.e :refer [b o]]"
                             "   [c.f :as cf]"
                             "   [c.g :as cg]))"
                             "#?(:clj"
                             "   (do (o)"
                             "       (f)"
                             "       cf/asd))")
                     (h/code "(ns a"
                             "  (:require"
                             "   [c.b]"
                             "   [c.d :refer [f]]"
                             "   [c.e :refer [o]]"
                             "   [c.f :as cf]))"
                             "#?(:clj"
                             "   (do (o)"
                             "       (f)"
                             "       cf/asd))")
                     true
                     "file:///a.cljc")))
  (testing "organizing and removing discard macros or comments in a :require or :import"
    (testing "comment attached to the right of a require libspec should stay attached after moving"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.set :as set] ;; important comment about set"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "set/set"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]"
                             "  [clojure.set :as set] ;; important comment about set"
                             "))"
                             "set/set"
                             "edn/read")))
    (testing "comments attached to the right of an import spec should stay attached during sort"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:import"
                             "  java.awt.Window  ; comment about Window"
                             "  java.awt.Frame   ; comment about Frame"
                             "))"
                             "Window"
                             "Frame")
                     (h/code "(ns foo"
                             " (:import"
                             "  java.awt.Frame ; comment about Frame"
                             "  java.awt.Window ; comment about Window"
                             "))"
                             "Window"
                             "Frame")))
    (testing "comment attached to the right of an unused library should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.set :as set] ;; important comment about set"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]))"
                             "edn/read")))
    (testing "sorting namespaces with attached comment, using prefix lists rather than vectors, should preserve comments"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  (clojure [set :as set]) ; info about set"
                             "  (clojure.java [io :as io])"
                             "  (clojure [edn :as edn])))"
                             "edn/read"
                             "set/set"
                             "io/file")
                     (h/code "(ns foo"
                             " (:require"
                             "  (clojure [set :as set]) ; info about set"
                             "  (clojure [edn :as edn])"
                             "  (clojure.java [io :as io])))"
                             "edn/read"
                             "set/set"
                             "io/file")))

    ;; Note: as of commit 1ac7dd3 prefix lists aren't removed by clojure-lsp, this may not be fixed
    ;; as they aren't commonly used anymore
    ;; (testing "remove unused namespaces, but using prefix lists rather than vectors"
    ;;   (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
    ;;                  (h/code "(ns foo"
    ;;                          " (:require"
    ;;                          "  ;; important comment about Z"
    ;;                          "  (Z [z :as z])"
    ;;                          "  ;; important comment about A"
    ;;                          "  (A [a :as a])"
    ;;                          "  ;; important comment about edn"
    ;;                          "  (clojure [edn :as edn])))"
    ;;                          "edn/read")
    ;;                  (h/code "(ns foo"
    ;;                          " (:require"
    ;;                          "  (clojure [edn :as edn])))"
    ;;                          "edn/read")))

    (testing "sorting namespaces with above comments at the top, using prefix lists rather than vectors, should preserve comments"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important comment about Z"
                             "  (Z [z :as z])"
                             "  ;; important comment about A"
                             "  (A [a :as a])"
                             "  ;; important comment about edn"
                             "  (clojure [edn :as edn])))"
                             "a/foo"
                             "edn/read"
                             "b/foo")
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important comment about A"
                             "  (A [a :as a])"
                             "  ;; important comment about edn"
                             "  (clojure [edn :as edn])"
                             "  ;; important comment about Z"
                             "  (Z [z :as z])))"
                             "a/foo"
                             "edn/read"
                             "b/foo")))
    (testing "sorting namespaces with above comments not at the top of require list, using prefix lists rather than vectors, should preserve comments"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  (Z [z :as z])"
                             "  ;; important comment about A"
                             "  (A [a :as a])"
                             "  ;; important comment about edn"
                             "  (clojure [edn :as edn])))"
                             "a/foo"
                             "edn/read"
                             "b/foo")
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important comment about A"
                             "  (A [a :as a])"
                             "  ;; important comment about edn"
                             "  (clojure [edn :as edn])"
                             "  (Z [z :as z])))"
                             "a/foo"
                             "edn/read"
                             "b/foo")))
    (testing "sorting namespaces with above comments, using prefix lists rather than vectors, should preserve comments"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important comment about Z"
                             "  (Z [z :as z])"
                             "  (A [a :as a])"
                             "  ;; important comment about edn"
                             "  (clojure [edn :as edn])))"
                             "edn/read"
                             "z/foo"
                             "a/foo")
                     (h/code "(ns foo"
                             " (:require"
                             "  (A [a :as a])"
                             "  ;; important comment about edn"
                             "  (clojure [edn :as edn])"
                             "  ;; important comment about Z"
                             "  (Z [z :as z])))"
                             "edn/read"
                             "z/foo"
                             "a/foo")))
    (testing "comments attached to the right of an unused libspec (but without whitespace) should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.set :as set];; important comment about set"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]))"
                             "edn/read")))
    (testing "comment attached to the right of a naked libspec should be preserved"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  clojure.set ;; important comment about set"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]"
                             "  clojure.set ;; important comment about set"
                             "))"
                             "edn/read")))
    (testing "comment above an unused libspec without whitespace at top of :require list should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important comment about set"
                             "  [clojure.set :as set]"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]))"
                             "edn/read")))
    (testing "comment attached to an unused imported package should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:import"
                             "  java.awt.Window  ; comment about Window"
                             "  java.awt.Frame   ; comment about Frame"
                             "))"
                             "Frame")
                     (h/code "(ns foo"
                             " (:import"
                             "  java.awt.Frame ; comment about Frame"
                             "))"
                             "Frame")))
    (testing "comment above an unused imported package should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:import"
                             "  ; comment about Window"
                             "  java.awt.Window"
                             "  ; comment about Frame"
                             "  java.awt.Frame"
                             "))"
                             "Frame")
                     (h/code "(ns foo"
                             " (:import"
                             "  ; comment about Frame"
                             "  java.awt.Frame))"
                             "Frame")))
    (testing "comment above an unused imported package list in a vector should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:import"
                             "  ; comment about Window"
                             "  [java.awt Window]"
                             "  ; comment about Map"
                             "  [java.util.collections Map]))"
                             "Map")
                     (h/code "(ns foo"
                             " (:import"
                             "  ; comment about Map"
                             "  [java.util.collections Map]))"
                             "Map")))
    (testing "comment above an unused imported package in a vector should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:import"
                             "  ; comment about Window"
                             "  [java.awt Window]"
                             "  ; comment about Map"
                             "  [java.util.collections Map]))"
                             "Map")
                     (h/code "(ns foo"
                             " (:import"
                             "  ; comment about Map"
                             "  [java.util.collections Map]))"
                             "Map")))
    (testing "comment above an unused imported package using a list should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:import"
                             "  ; comment about Window"
                             "  (java.awt Window)"
                             "  ; comment about Map"
                             "  (java.util.collections Map)))"
                             "Map")
                     (h/code "(ns foo"
                             " (:import"
                             "  ; comment about Map"
                             "  (java.util.collections Map)))"
                             "Map")))
    (testing "discard in import directive when there is an unrelated unused package in the package vector should be preserved"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:import"
                             "  [java.awt Window]"
                             "  #_abc"
                             "  [java.util.collections Map]))"
                             "Map")
                     (h/code "(ns foo"
                             " (:import"
                             "  #_abc"
                             "  [java.util.collections Map]))"
                             "Map")))
    (testing "discard in import directive when there is an unrelated unused imported package in a list should be preserved"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:import"
                             "  (java.awt Window)"
                             "  #_abc"
                             "  (java.util.collections Map)))"
                             "Map")
                     (h/code "(ns foo"
                             " (:import"
                             "  #_abc"
                             "  (java.util.collections Map)))"
                             "Map")))
    (testing "discard above an unused imported package in a vector should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:import"
                             "  #_xyz"
                             "  [java.awt Window]"
                             "  #_abc"
                             "  [java.util.collections Map]))"
                             "Map")
                     (h/code "(ns foo"
                             " (:import"
                             "  #_abc"
                             "  [java.util.collections Map]))"
                             "Map")))
    (testing "comment above an unused libspec without whitespace at top should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important comment about set"
                             "  [clojure.set :as set]"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]))"
                             "edn/read")))
    (testing "comment above an unused libspec without whitespace in middle should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.java.io :as io]"
                             "  ;; important comment about set"
                             "  [clojure.set :as set]"
                             "  [clojure.edn :as edn]))"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]))"
                             "edn/read")))
    (testing "multiple comments above unused libspecs in middle of list should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important comment about io"
                             "  [clojure.java.io :as io]"
                             "  ;; important comment about set"
                             "  [clojure.set :as set]"
                             "  [clojure.edn :as edn]))"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]))"
                             "edn/read")))
    (testing "comment above an unused libspec with additional whitespace should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; info about set"
                             " "
                             "  ;; important comment about set"
                             "  [clojure.set :as set]"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]))"
                             "edn/read")))
    (testing "comment above an out-of-order libspec with additional whitespace should be preserved"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; info about set"
                             " "
                             "  ;; important comment about set"
                             "  [clojure.set :as set]"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "edn/read"
                             "set/set")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]"
                             "  ;; info about set"
                             "  ;; important comment about set"
                             "  [clojure.set :as set]))"
                             "edn/read"
                             "set/set")))
    (testing "comment above an unused libspec without whitespace in middle should be removed"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.java.io :as io]"
                             "  ;; important comment about set"
                             "  [clojure.set :as set]  ; something else about set"
                             "  [clojure.edn :as edn]))"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]))"
                             "edn/read")))
    (testing "comments inside vector of multi-line require should remain"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.set :as "
                             "   set    ;; comment about set"
                             "   union] ;; important comment"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "set/set"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]"
                             "  [clojure.set :as "
                             "   set    ;; comment about set"
                             "   union] ;; important comment"
                             "))"
                             "set/set"
                             "edn/read")))
    (testing "comment on the same line as a multi-line require should stick with the require"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.set :as "
                             "   set"
                             "   union] ;; important comment"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "set/set"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]"
                             "  [clojure.set :as "
                             "   set"
                             "   union] ;; important comment"
                             "))"
                             "set/set"
                             "edn/read")))
    (testing "if a comment is at the top already sorted, it should stay at the top"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important comment about edn"
                             "  [clojure.edn :as edn]"
                             "  [clojure.java.io :as io]"
                             "  [clojure.set :as set]))"
                             "set/set"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important comment about edn"
                             "  [clojure.edn :as edn]"
                             "  [clojure.set :as set]))"
                             "set/set"
                             "edn/read")))
    (testing "empty require should not fail, but not do anything"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  []))")
                     (h/code "(ns foo"
                             " (:require"
                             "  []))")))
    (testing "if a comment is above a libspec, it should stay with the libspec - sorted to top"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.set :as set]"
                             "  [clojure.java.io :as io]"
                             "  ;; important comment about edn"
                             "  [clojure.edn :as edn]))"
                             "set/set"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important comment about edn"
                             "  [clojure.edn :as edn]"
                             "  [clojure.set :as set]))"
                             "set/set"
                             "edn/read")))
    (testing "if a comment is above a libspec, it should stay with the libspec - not sorted to top"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important comment about set"
                             "  [clojure.set :as set]"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "set/set"
                             "io/foo"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]"
                             "  [clojure.java.io :as io]"
                             "  ;; important comment about set"
                             "  [clojure.set :as set]))"
                             "set/set"
                             "io/foo"
                             "edn/read")))
    (testing "if multiple lines of comments, they should stay with the associated libspec"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important comment about set 1"
                             "  ;; important comment about set 2"
                             "  [clojure.set :as set]"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "set/set"
                             "io/file"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.edn :as edn]"
                             "  [clojure.java.io :as io]"
                             "  ;; important comment about set 1"
                             "  ;; important comment about set 2"
                             "  [clojure.set :as set]))"
                             "set/set"
                             "io/file"
                             "edn/read")))
    (testing "preserves kondo/ignore discard macros on first line"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  #_{:clj-kondo/ignore [:unused-namespace]}"
                             "  [clojure.core.async :as async]"
                             "  [clojure.set :as set] ;; important comment"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "set/set"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  #_{:clj-kondo/ignore [:unused-namespace]}"
                             "  [clojure.core.async :as async]"
                             "  [clojure.edn :as edn]"
                             "  [clojure.set :as set] ;; important comment"
                             "))"
                             "set/set"
                             "edn/read")))
    (testing "preserves kondo/ignore discard macros in the simple case with only one libspec remaining"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  #_{:clj-kondo/ignore [:unused-namespace]}"
                             "  [clojure.set :as set]"
                             "  [clojure.java.io :as io]))")
                     (h/code "(ns foo"
                             " (:require"
                             "  #_{:clj-kondo/ignore [:unused-namespace]}"
                             "  [clojure.set :as set]))")))
    (testing "preserves kondo/ignore discard macros with attached comments"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  #_{:clj-kondo/ignore [:unused-namespace]}"
                             "  [clojure.set :as set] ;; comment about set"
                             "  [clojure.java.io :as io]))")
                     (h/code "(ns foo"
                             " (:require"
                             "  #_{:clj-kondo/ignore [:unused-namespace]}"
                             "  [clojure.set :as set] ;; comment about set"
                             "))")))
    (testing "preserves indentation when removing top libspec"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :keep}}}
                     (h/code "(ns foo"
                             " (:require"
                             "      ;; important info about async"
                             "      [clojure.core.async :as async]"
                             "      #_:clj-kondo/ignore"
                             "      ;; important info about api"
                             "      clojure-lsp.api"
                             "      [clojure.string :as str]))"
                             "str/blank?")
                     (h/code "(ns foo"
                             " (:require"
                             "      #_:clj-kondo/ignore"
                             "      ;; important info about api"
                             "      clojure-lsp.api"
                             "      [clojure.string :as str]))"
                             "str/blank?")))
    (testing "preserves indentation when not removing top libspec and it has a kondo directive"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :keep}}}
                     (h/code "(ns foo"
                             " (:require"
                             "      #_:clj-kondo/ignore"
                             "      ;; important info about async"
                             "      [clojure.core.async :as async]"
                             "      #_:clj-kondo/ignore"
                             "      ;; important info about api"
                             "      clojure-lsp.api"
                             "      [clojure.string :as str]))"
                             "str/blank?")
                     (h/code "(ns foo"
                             " (:require"
                             "      #_:clj-kondo/ignore"
                             "      ;; important info about api"
                             "      clojure-lsp.api"
                             "      #_:clj-kondo/ignore"
                             "      ;; important info about async"
                             "      [clojure.core.async :as async]"
                             "      [clojure.string :as str]))"
                             "str/blank?")))
    (testing "preserves indentation when not removing top libspec and it is a comment"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :keep}}}
                     (h/code "(ns foo"
                             " (:require"
                             "      #_:clj-kondo/ignore"
                             "      ;; important info about async"
                             "      [clojure.core.async :as async]"
                             "      ;; important info about api"
                             "      clojure-lsp.api"
                             "      [clojure.string :as str]))"
                             "str/blank?")
                     (h/code "(ns foo"
                             " (:require"
                             "      ;; important info about api"
                             "      clojure-lsp.api"
                             "      #_:clj-kondo/ignore"
                             "      ;; important info about async"
                             "      [clojure.core.async :as async]"
                             "      [clojure.string :as str]))"
                             "str/blank?")))
    (testing "preserves indentation when not removing top libspec and it is a libspec (not a discard macro or comment)"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :keep}}}
                     (h/code "(ns foo"
                             " (:require"
                             "      #_:clj-kondo/ignore"
                             "      ;; important info about async"
                             "      [clojure.core.async :as async]"
                             "      clojure-lsp.api"
                             "      [clojure.string :as str]))"
                             "str/blank?")
                     (h/code "(ns foo"
                             " (:require"
                             "      clojure-lsp.api"
                             "      #_:clj-kondo/ignore"
                             "      ;; important info about async"
                             "      [clojure.core.async :as async]"
                             "      [clojure.string :as str]))"
                             "str/blank?")))
    (testing "preserves kondo/ignore discard macros together when they end up on the first line"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  #_:clj-kondo/ignore"
                             "  ;; important info about async"
                             "  [clojure.core.async :as async]"
                             "  #_:clj-kondo/ignore"
                             "  ;; important info about api"
                             "  clojure-lsp.api"
                             "  [clojure.string :as str]))"
                             "str/blank?")
                     (h/code "(ns foo"
                             " (:require"
                             "  #_:clj-kondo/ignore"
                             "  ;; important info about api"
                             "  clojure-lsp.api"
                             "  #_:clj-kondo/ignore"
                             "  ;; important info about async"
                             "  [clojure.core.async :as async]"
                             "  [clojure.string :as str]))"
                             "str/blank?")))
    (testing "preserves kondo/ignore discard macros together when discard is after comment"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important info about async"
                             "  #_:clj-kondo/ignore"
                             "  [clojure.core.async :as async]"
                             "  #_:clj-kondo/ignore"
                             "  ;; important info about api"
                             "  clojure-lsp.api"
                             "  [clojure.string :as str]))"
                             "str/blank?")
                     (h/code "(ns foo"
                             " (:require"
                             "  #_:clj-kondo/ignore"
                             "  ;; important info about api"
                             "  clojure-lsp.api"
                             "  ;; important info about async"
                             "  #_:clj-kondo/ignore"
                             "  [clojure.core.async :as async]"
                             "  [clojure.string :as str]))"
                             "str/blank?")))
    (testing "preserves multiple discard macros together (albeit on new lines)"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  #_abc #_xyz"
                             "  clojure-lsp.api"
                             "  [clojure.string :as str]))"
                             "str/blank?")
                     (h/code "(ns foo"
                             " (:require"
                             "  #_abc"
                             "  #_xyz"
                             "  clojure-lsp.api"
                             "  [clojure.string :as str]))"
                             "str/blank?")))
    (testing "preserves multiple discard macros together (albeit on new lines) with comments"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  #_abc #_xyz  ; comment about xyz"
                             "  clojure-lsp.api"
                             "  [clojure.string :as str]))"
                             "str/blank?")
                     (h/code "(ns foo"
                             " (:require"
                             "  #_abc"
                             "  #_xyz"
                             "  ; comment about xyz"
                             "  clojure-lsp.api"
                             "  [clojure.string :as str]))"
                             "str/blank?")))
    (testing "preserves kondo/ignore discard macros together when they end up on non-first lines"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  #_:clj-kondo/ignore"
                             "  ;; important info about async"
                             "  [clojure.core.async :as async]"
                             "  ;; important info about api"
                             "  clojure-lsp.api"
                             "  [clojure.string :as str]))"
                             "str/blank?")
                     (h/code "(ns foo"
                             " (:require"
                             "  ;; important info about api"
                             "  clojure-lsp.api"
                             "  #_:clj-kondo/ignore"
                             "  ;; important info about async"
                             "  [clojure.core.async :as async]"
                             "  [clojure.string :as str]))"
                             "str/blank?")))
    (testing "removes with duplicate require with different and unused alias that has comment above"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [bar :as b]"
                             "  ; this is unused"
                             "  [foo :as fa]"
                             "  [foo :as f]))"
                             "f/bar b/bar")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [bar :as b]"
                             "  [foo :as f]))"
                             "f/bar b/bar")))
    (testing "removes with duplicate require with different and unused alias that has discard and comment above"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [bar :as b]"
                             "  #_xyz ; test xyz"
                             "  [foo :as fa]"
                             "  [foo :as f]))"
                             "f/bar b/bar")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [bar :as b]"
                             "  [foo :as f]))"
                             "f/bar b/bar")))
    (testing "removes with duplicate require with different and unused alias that has comment attached"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [bar :as b]"
                             "  [foo :as fa] ; this is unused"
                             "  [foo :as f]))"
                             "f/bar b/bar")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [bar :as b]"
                             "  [foo :as f]))"
                             "f/bar b/bar")))
    (testing "removes with duplicate require with different and unused alias that has kondo discard above"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [bar :as b]"
                             "  #_xyz"
                             "  [foo :as fa] ; this is unused"
                             "  [foo :as f]))"
                             "f/bar b/bar")
                     (h/code "(ns foo.bar"
                             " (:require"
                             "  [bar :as b]"
                             "  [foo :as f]))"
                             "f/bar b/bar")))
    (testing "preserves kondo/ignore discard macros on non-first lines"
      (test-clean-ns {:settings {:clean {:ns-inner-blocks-indentation :next-line}}}
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.core.async :as async]"
                             "  #_{:clj-kondo/ignore [:unused-namespace]}"
                             "  [clojure.set :as set] ;; important comment"
                             "  [clojure.java.io :as io]"
                             "  [clojure.edn :as edn]))"
                             "async/chan"
                             "edn/read")
                     (h/code "(ns foo"
                             " (:require"
                             "  [clojure.core.async :as async]"
                             "  [clojure.edn :as edn]"
                             "  #_{:clj-kondo/ignore [:unused-namespace]}"
                             "  [clojure.set :as set] ;; important comment"
                             "))"
                             "async/chan"
                             "edn/read")))))
