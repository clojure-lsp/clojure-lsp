(ns clojure-lsp.queries-test
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(deftest internal-analysis
  (testing "when dependency-scheme is zipfile"
    (h/reset-components!)
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///b.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "zipfile:///some.jar::some-file.clj"))
    (is (= 2 (count (q/internal-analysis (h/db))))))
  (testing "when dependency-scheme is jar"
    (swap! (h/db*) shared/deep-merge {:settings {:dependency-scheme "jar"}})
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///b.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "jar:file:///some.jar!/some-file.clj"))
    (is (= 2 (count (q/internal-analysis (h/db)))))))

(deftest internal-plus-local-analysis
  (testing "when dependency-scheme is zipfile"
    (h/reset-components!)
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///b.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "zipfile:///some.jar::some-file.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "zipfile:///some.jar::other-file.clj"))
    (is (= 2 (count (q/internal-plus-local-analysis (h/db) (h/file-uri "file:///a.clj")))))
    (is (= 3 (count (q/internal-plus-local-analysis (h/db) (h/file-uri "zipfile:///some.jar::some-file.clj"))))))
  (testing "when dependency-scheme is jar"
    (swap! (h/db*) shared/deep-merge {:settings {:dependency-scheme "jar"}})
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///b.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "jar:file:///some.jar!/some-file.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "jar:file:///some.jar!/other-file.clj"))
    (is (= 2 (count (q/internal-plus-local-analysis (h/db) (h/file-uri "file:///a.clj")))))
    (is (= 3 (count (q/internal-plus-local-analysis (h/db) (h/file-uri "zipfile:///some.jar::some-file.clj")))))))

(deftest external-analysis
  (testing "when dependency-scheme is zipfile"
    (h/reset-components!)
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///b.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "zipfile:///some.jar::some-file.clj"))
    (is (= 1 (count (q/external-analysis (h/db))))))
  (testing "when dependency-scheme is jar"
    (h/reset-components!)
    (swap! (h/db*) shared/deep-merge {:settings {:dependency-scheme "jar"}})
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///b.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "jar:file:///some.jar!/some-file.clj"))
    (is (= 1 (count (q/external-analysis (h/db)))))))

(deftest ns-analysis
  (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
  (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///b.clj"))
  (h/load-code-and-locs "(ns foo.baz)" (h/file-uri "file:///c.clj"))
  (is (= 2 (count (q/ns-analysis (h/db) 'foo.bar)))))

(deftest ns-dependents-analysis
  (h/load-code-and-locs "(ns aaa (:require [bbb] [ccc]))" (h/file-uri "file:///aaa.clj"))
  (h/load-code-and-locs "(ns bbb (:require [ccc]))" (h/file-uri "file:///bbb.clj"))
  (h/load-code-and-locs "(ns ccc)" (h/file-uri "file:///ccc.clj"))
  (is (= 0 (count (q/ns-dependents-analysis (h/db) 'aaa))))
  (is (= 1 (count (q/ns-dependents-analysis (h/db) 'bbb))))
  (is (= 2 (count (q/ns-dependents-analysis (h/db) 'ccc)))))

(deftest ns-and-dependents-analysis
  (h/load-code-and-locs "(ns aaa (:require [bbb] [ccc]))" (h/file-uri "file:///aaa.clj"))
  (h/load-code-and-locs "(ns bbb (:require [ccc]))" (h/file-uri "file:///bbb.clj"))
  (h/load-code-and-locs "(ns ccc)" (h/file-uri "file:///ccc.clj"))
  (is (= 1 (count (q/ns-and-dependents-analysis (h/db) 'aaa))))
  (is (= 2 (count (q/ns-and-dependents-analysis (h/db) 'bbb))))
  (is (= 3 (count (q/ns-and-dependents-analysis (h/db) 'ccc)))))

(deftest ns-dependencies-analysis
  (h/load-code-and-locs "(ns aaa (:require [bbb] [ccc]))" (h/file-uri "file:///aaa.clj"))
  (h/load-code-and-locs "(ns bbb (:require [ccc]))" (h/file-uri "file:///bbb.clj"))
  (h/load-code-and-locs "(ns ccc)" (h/file-uri "file:///ccc.clj"))
  (is (= 2 (count (q/ns-dependencies-analysis (h/db) 'aaa))))
  (is (= 1 (count (q/ns-dependencies-analysis (h/db) 'bbb))))
  (is (= 0 (count (q/ns-dependencies-analysis (h/db) 'ccc)))))

(deftest nses-analysis
  (h/load-code-and-locs "(ns aaa)" (h/file-uri "file:///aaa.clj"))
  (h/load-code-and-locs "(ns bbb)" (h/file-uri "file:///bbb.clj"))
  (h/load-code-and-locs "(ns ccc)" (h/file-uri "file:///ccc.clj"))
  (is (= 1 (count (q/nses-analysis (h/db) '#{aaa}))))
  (is (= 2 (count (q/nses-analysis (h/db) '#{aaa bbb}))))
  (is (= 3 (count (q/nses-analysis (h/db) '#{aaa bbb ccc})))))

(deftest nses-and-dependents-analysis
  (h/load-code-and-locs "(ns aaa (:require [bbb] [ccc]))" (h/file-uri "file:///aaa.clj"))
  (h/load-code-and-locs "(ns bbb (:require [ccc]))" (h/file-uri "file:///bbb.clj"))
  (h/load-code-and-locs "(ns ccc)" (h/file-uri "file:///ccc.clj"))
  (is (= 1 (count (q/nses-and-dependents-analysis (h/db) '#{aaa}))))
  (is (= 2 (count (q/nses-and-dependents-analysis (h/db) '#{bbb}))))
  (is (= 3 (count (q/nses-and-dependents-analysis (h/db) '#{ccc}))))
  (is (= 2 (count (q/nses-and-dependents-analysis (h/db) '#{aaa bbb})))))

(deftest uri-dependents-analysis
  (h/load-code-and-locs "(ns aaa (:require [bbb] [ccc]))" (h/file-uri "file:///aaa.clj"))
  (h/load-code-and-locs "(ns bbb (:require [ccc]))" (h/file-uri "file:///bbb.clj"))
  (h/load-code-and-locs "(ns ccc)" (h/file-uri "file:///ccc.clj"))
  (is (= 0 (count (q/uri-dependents-analysis (h/db) (h/file-uri "file:///aaa.clj")))))
  (is (= 1 (count (q/uri-dependents-analysis (h/db) (h/file-uri "file:///bbb.clj")))))
  (is (= 2 (count (q/uri-dependents-analysis (h/db) (h/file-uri "file:///ccc.clj"))))))

(deftest uri-dependencies-analysis
  (h/load-code-and-locs "(ns aaa (:require [bbb] [ccc]))" (h/file-uri "file:///aaa.clj"))
  (h/load-code-and-locs "(ns bbb (:require [ccc]))" (h/file-uri "file:///bbb.clj"))
  (h/load-code-and-locs "(ns ccc)" (h/file-uri "file:///ccc.clj"))
  (is (= 2 (count (q/uri-dependencies-analysis (h/db) (h/file-uri "file:///aaa.clj")))))
  (is (= 1 (count (q/uri-dependencies-analysis (h/db) (h/file-uri "file:///bbb.clj")))))
  (is (= 0 (count (q/uri-dependencies-analysis (h/db) (h/file-uri "file:///ccc.clj"))))))

(deftest find-last-order-by-project-analysis
  (testing "with pred that applies for both project and external analysis"
    (h/reset-components!)
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "zipfile:///some.jar::some-file.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (let [element (#'q/find-last-order-by-project-analysis
                   (comp q/xf-analysis->namespace-definitions
                         (q/xf-same-name 'foo.bar))
                   (h/db))]
      (is (= (h/file-uri "file:///a.clj") (:uri element)))))
  (testing "with pred that applies for both project and external analysis with multiple on project"
    (h/reset-components!)
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "zipfile:///some.jar::some-file.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///b.clj"))
    (let [element (#'q/find-last-order-by-project-analysis
                   (comp q/xf-analysis->namespace-definitions
                         (q/xf-same-name 'foo.bar))
                   (h/db))]
      (is (= (h/file-uri "file:///b.clj") (:uri element))))))

(deftest find-element-under-cursor
  (let [code (str "(ns a.b.c (:require [d.e.f :as |f-alias]))\n"
                  "(defn x [file|name] filename)\n"
                  "|x\n"
                  "un|known")
        [[alias-r alias-c]
         [param-r param-c]
         [x-r x-c]
         [unknown-r unknown-c]] (h/load-code-and-locs code)
        db (h/db)]
    (h/assert-submap
      '{:alias f-alias}
      (q/find-element-under-cursor db (h/file-uri "file:///a.clj") alias-r alias-c))
    (h/assert-submap
      '{:name x}
      (q/find-element-under-cursor db (h/file-uri "file:///a.clj") x-r x-c))
    (h/assert-submap
      '{:name filename}
      (q/find-element-under-cursor db (h/file-uri "file:///a.clj") param-r param-c))
    (h/assert-submap
      '{:name unknown}
      (q/find-element-under-cursor db (h/file-uri "file:///a.clj") unknown-r unknown-c))))

(deftest find-references-from-cursor
  (let [a-code (h/code "(ns a.b.c (:require [d.e.f :as |f-alias]))"
                       "(defn |x [|filename] |filename |f-alias/foo)"
                       "|x |unknown unknown")
        [[alias-r alias-c]
         [x-r x-c]
         [param-r param-c]
         [param-use-r param-use-c]
         [alias-use-r alias-use-c]
         [x-use-r x-use-c]
         [unknown-r unknown-c]] (h/load-code-and-locs a-code)
        [[a-foo-kw-r a-foo-kw-c]] (h/load-code-and-locs "|:foo-kw" (h/file-uri "file:///b.clj"))
        [[b-foo-kw-r b-foo-kw-c]
         [c-foo-kw-r c-foo-kw-c]
         [d-foo-kw-r d-foo-kw-c]] (h/load-code-and-locs (h/code "|:foo-kw"
                                                                "(let [{:keys [|foo-kw]} {|:foo-kw 1}]"
                                                                "  foo-kw)") (h/file-uri "file:///c.clj"))
        [[a-baz-kw-r a-baz-kw-c]
         [b-baz-kw-r b-baz-kw-c]] (h/load-code-and-locs (h/code "(ns baz)"
                                                                "|::foo/foo-kw"
                                                                "|::baz-kw") (h/file-uri "file:///baz.clj"))
        db (h/db)]
    (h/assert-submaps
      [{:name 'x :name-row x-r :name-col x-c}
       {:name 'x :name-row x-use-r :name-col x-use-c}]
      (q/find-references-from-cursor db (h/file-uri "file:///a.clj") x-r x-c true))
    (h/assert-submaps
      [{:name 'filename :name-row param-r :name-col param-c}
       {:name 'filename :name-row param-use-r :name-col param-use-c}]
      (q/find-references-from-cursor db (h/file-uri "file:///a.clj") param-r param-c true))
    (h/assert-submaps
      ['{:name unknown}]
      (q/find-references-from-cursor db (h/file-uri "file:///a.clj") unknown-r unknown-c true))
    (h/assert-submaps
      [{:alias 'f-alias :name-row alias-r :name-col alias-c}
       {:alias 'f-alias :name 'foo :name-row alias-use-r :name-col alias-use-c}]
      (q/find-references-from-cursor db (h/file-uri "file:///a.clj") alias-r alias-c true))
    (h/assert-submaps
      [{:name "foo-kw" :name-row a-foo-kw-r :name-col a-foo-kw-c}
       {:name "foo-kw" :name-row b-foo-kw-r :name-col b-foo-kw-c}
       {:name "foo-kw" :name-row d-foo-kw-r :name-col d-foo-kw-c}
       {:name "foo-kw" :name-row c-foo-kw-r :name-col c-foo-kw-c}]
      (q/find-references-from-cursor db (h/file-uri "file:///c.clj") b-foo-kw-r b-foo-kw-c true))
    (h/assert-submaps
      [{:name "foo-kw" :name-row a-baz-kw-r :name-col a-baz-kw-c :ns :clj-kondo/unknown-namespace}]
      (q/find-references-from-cursor db (h/file-uri "file:///baz.clj") a-baz-kw-r a-baz-kw-c true))
    (h/assert-submaps
      [{:name "baz-kw" :name-row b-baz-kw-r :name-col b-baz-kw-c :ns 'baz}]
      (q/find-references-from-cursor db (h/file-uri "file:///baz.clj") b-baz-kw-r b-baz-kw-c true))))

(deftest find-references-from-cursor-cljc
  (let [code (str "(ns a.b.c (:require [d.e.f :as |f-alias]))\n"
                  "(defn |x [|filename] |filename |f-alias/foo)\n"
                  "|x |unknown unknown")
        [[alias-r alias-c]
         [x-r x-c]
         [param-r param-c]
         [param-use-r param-use-c]
         [alias-use-r alias-use-c]
         [x-use-r x-use-c]
         [unknown-r unknown-c]] (h/load-code-and-locs code (h/file-uri "file:///a.cljc"))
        db (h/db)]
    (h/assert-submaps
      [{:name 'x :name-row x-r :name-col x-c}
       {:name 'x :name-row x-use-r :name-col x-use-c}]
      (q/find-references-from-cursor db (h/file-uri "file:///a.cljc") x-r x-c true))
    (h/assert-submaps
      [{:name 'filename :name-row param-r :name-col param-c}
       {:name 'filename :name-row param-use-r :name-col param-use-c}]
      (q/find-references-from-cursor db (h/file-uri "file:///a.cljc") param-r param-c true))
    (h/assert-submaps
      ['{:name unknown}]
      (q/find-references-from-cursor db (h/file-uri "file:///a.cljc") unknown-r unknown-c true))
    (h/assert-submaps
      [{:alias 'f-alias :name-row alias-r :name-col alias-c}
       {:alias 'f-alias :name 'foo :name-row alias-use-r :name-col alias-use-c}]
      (q/find-references-from-cursor db (h/file-uri "file:///a.cljc") alias-r alias-c true))))

(deftest find-references-from-namespace-definition
  (testing "clj"
    (h/reset-components!)
    (let [[[ns-def-r ns-def-c]] (h/load-code-and-locs (h/code "(ns |some.cool-ns) (def foo 1)"))
          _ (h/load-code-and-locs (h/code "(ns |other.cool-ns"
                                          " (:require [some.cool-ns :as s])) s/foo")
                                  (h/file-uri "file:///b.clj"))
          _ (h/load-code-and-locs (h/code "(ns |another.cool-ns) :some.cool-ns/bar")
                                  (h/file-uri "file:///c.clj"))
          common-references [{:uri (h/file-uri "file:///b.clj")
                              :bucket :namespace-usages
                              :name 'some.cool-ns
                              :from 'other.cool-ns}
                             {:uri (h/file-uri "file:///c.clj")
                              :bucket :keyword-usages
                              :from 'another.cool-ns
                              :name "bar"
                              :ns 'some.cool-ns}]]
      (testing "from ns definition"
        (h/assert-submaps
          common-references
          (q/find-references-from-cursor (h/db) (h/file-uri "file:///a.clj") ns-def-r ns-def-c false)))
      (testing "Including definition"
        (h/assert-submaps
          (concat [{:uri (h/file-uri "file:///a.clj")
                    :name 'some.cool-ns
                    :bucket :namespace-definitions}]
                  common-references)
          (q/find-references-from-cursor (h/db) (h/file-uri "file:///a.clj") ns-def-r ns-def-c true)))))
  (testing "cljc"
    (h/reset-components!)
    (let [[[ns-def-r ns-def-c]] (h/load-code-and-locs (h/code "(ns |some.cool-ns) (def foo 1)") (h/file-uri "file:///a.cljc"))
          _ (h/load-code-and-locs (h/code "(ns |other.cool-ns"
                                          " (:require [some.cool-ns :as s])) s/foo")
                                  (h/file-uri "file:///b.cljc"))
          _ (h/load-code-and-locs (h/code "(ns |another.cool-ns) :some.cool-ns/bar")
                                  (h/file-uri "file:///c.cljc"))
          common-references [{:uri (h/file-uri "file:///b.cljc")
                              :bucket :namespace-usages
                              :name 'some.cool-ns
                              :from 'other.cool-ns}
                             {:uri (h/file-uri "file:///c.cljc")
                              :bucket :keyword-usages
                              :from 'another.cool-ns
                              :name "bar"
                              :ns 'some.cool-ns}]]
      (testing "from ns definition"
        (h/assert-submaps
          common-references
          (q/find-references-from-cursor (h/db) (h/file-uri "file:///a.cljc") ns-def-r ns-def-c false)))
      (testing "Including definition"
        (h/assert-submaps
          (concat [{:uri (h/file-uri "file:///a.cljc")
                    :name 'some.cool-ns
                    :bucket :namespace-definitions}]
                  common-references)
          (q/find-references-from-cursor (h/db) (h/file-uri "file:///a.cljc") ns-def-r ns-def-c true))))))

(deftest find-references-from-namespace-usage
  (let [_ (h/load-code-and-locs (h/code "(ns some.cool-ns) (def foo 1)"))
        [[usage-r usage-c]] (h/load-code-and-locs (h/code "(ns other.cool-ns"
                                                          " (:require [|some.cool-ns :as s])) s/foo")
                                                  (h/file-uri "file:///b.clj"))
        _ (h/load-code-and-locs (h/code "(ns another.cool-ns) :some.cool-ns/bar")
                                (h/file-uri "file:///c.clj"))]
    (testing "from ns usage"
      (h/assert-submaps
        [{:uri (h/file-uri "file:///b.clj")
          :bucket :namespace-usages
          :name 'some.cool-ns
          :from 'other.cool-ns}
         {:uri (h/file-uri "file:///c.clj")
          :bucket :keyword-usages
          :from 'another.cool-ns
          :name "bar"
          :ns 'some.cool-ns}]
        (q/find-references-from-cursor (h/db) (h/file-uri "file:///b.clj") usage-r usage-c false)))))

(deftest find-references-from-var-definition-to-symbols
  (let [[[def-r def-c]] (h/load-code-and-locs (h/code "(ns some.cool-ns) (def |foo 1)"))
        _ (h/load-code-and-locs (h/code "(ns other.cool-ns"
                                        " (:require [some.cool-ns :as s]))"
                                        "s/foo"
                                        "'some.cool-ns/foo"
                                        "#'some.cool-ns/foo")
                                (h/file-uri "file:///b.clj"))]
    (testing "from var-definition"
      (h/assert-submaps
        [{:uri (h/file-uri "file:///b.clj")
          :bucket :var-usages
          :name-row 3
          :name 'foo
          :from 'other.cool-ns}
         {:uri (h/file-uri "file:///b.clj")
          :bucket :var-usages
          :name 'foo
          :name-row 5
          :from 'other.cool-ns}
         {:uri (h/file-uri "file:///b.clj")
          :bucket :symbols
          :from 'other.cool-ns
          :name 'foo
          :name-row 4
          :to 'some.cool-ns}]
        (q/find-references-from-cursor (h/db) h/default-uri def-r def-c false)))))

(deftest find-references-from-defrecord
  (let [code (str "(defrecord |MyRecord [])\n"
                  "(|MyRecord)"
                  "(|->MyRecord)"
                  "(|map->MyRecord)")
        [[def-r def-c]
         [raw-r raw-c]
         [to-r to-c]
         [map-to-r map-to-c]] (h/load-code-and-locs code (h/file-uri "file:///a.clj"))
        db (h/db)]
    (h/assert-submaps
      [{:name 'MyRecord :bucket :var-usages :name-row raw-r :name-col raw-c}
       {:name '->MyRecord :bucket :var-usages :name-row to-r :name-col to-c}
       {:name 'map->MyRecord :bucket :var-usages :name-row map-to-r :name-col map-to-c}]
      (q/find-references-from-cursor db (h/file-uri "file:///a.clj") def-r def-c false))))

(deftest find-references-excluding-function-different-arity
  (let [a-code (h/code "(ns a)"
                       "(defn foo [] (foo))"
                       "(defn |bar"
                       "  ([] (bar 1))"
                       "  ([_]))"
                       "(|bar)")
        b-code (h/code "(ns b"
                       "  (:require [a :as aa]))"
                       "(defn bar []"
                       "  (|aa/bar))")
        [[bar-def-r bar-def-c]
         [bar-usa-r bar-usa-c]] (h/load-code-and-locs a-code (h/file-uri "file:///a.clj"))
        [[bar-usa-b-r bar-usa-b-c]] (h/load-code-and-locs b-code (h/file-uri "file:///b.clj"))
        db (h/db)]
    (testing "from definition"
      (h/assert-submaps
        #{{:name-row 6
           :name 'bar
           :uri (h/file-uri "file:///a.clj")
           :name-col 2
           :bucket :var-usages}
          {:name-row 4
           :name 'bar
           :uri (h/file-uri "file:///b.clj")
           :from 'b
           :name-col 4
           :from-var 'bar}}

        (q/find-references-from-cursor db (h/file-uri "file:///a.clj") bar-def-r bar-def-c false)))
    (testing "from usage"
      (h/assert-contains-submaps
        #{{:name-row 6
           :name 'bar
           :uri (h/file-uri "file:///a.clj")
           :name-col 2
           :bucket :var-usages}
          {:name-row 4
           :name 'bar
           :uri (h/file-uri "file:///b.clj")
           :from 'b
           :name-col 4
           :from-var 'bar}}
        (q/find-references-from-cursor db (h/file-uri "file:///a.clj") bar-usa-r bar-usa-c false)))
    (testing "from other ns"
      (h/assert-contains-submaps
        #{{:name-row 6
           :name 'bar
           :uri (h/file-uri "file:///a.clj")
           :name-col 2
           :bucket :var-usages}
          {:name-row 4
           :name 'bar
           :uri (h/file-uri "file:///b.clj")
           :from 'b
           :name-col 4
           :from-var 'bar}}
        (q/find-references-from-cursor db (h/file-uri "file:///b.clj") bar-usa-b-r bar-usa-b-c false)))))

(deftest find-references-from-protocol-impl
  (h/load-code-and-locs (h/code "(ns a)"
                                "(defprotocol Foo"
                                "  (something []))"))
  (h/load-code-and-locs (h/code "(ns b (:require [a :as f]))"
                                "(defrecord FooImpl1 []"
                                " f/Foo"
                                " (something [_] 123))"
                                "(defrecord FooImpl2 []"
                                " f/Foo"
                                " (^void something [_] 456))"
                                "(f/something (->FooImpl1))"
                                "(f/something (->FooImpl2))") (h/file-uri "file:///b.clj"))
  (testing "from defrecord method name"
    (h/assert-submaps
      [{:row 8 :col 1 :end-row 8 :end-col 27
        :name-row 8 :name-end-col 13 :name-col 2 :name-end-row 8
        :name 'something
        :uri (h/file-uri "file:///b.clj")
        :alias 'f
        :from 'b
        :bucket :var-usages
        :to 'a}
       {:name-col 2 :name-row 9 :name-end-row 9 :name-end-col 13
        :name 'something
        :uri (h/file-uri "file:///b.clj")
        :alias 'f
        :from 'b
        :row 9 :col 1 :end-row 9 :end-col 27
        :bucket :var-usages
        :to 'a}]
      (q/find-references-from-cursor (h/db) (h/file-uri "file:///b.clj") 7 9 false))))

(deftest find-references-for-defmulti
  (let [[[defmulti-r defmulti-c]]
        (h/load-code-and-locs (h/code "(ns a)"
                                      "(defmulti |my-multi :some-key)"))
        [[defmethod-r defmethod-c]
         [usage-r usage-c]]
        (h/load-code-and-locs (h/code "(ns b (:require [a :as f]))"
                                      "(defmethod |f/my-multi :some-value"
                                      " [_]"
                                      " :foo)"
                                      "(|f/my-multi {:some-value 123})") (h/file-uri "file:///b.clj"))
        references [;; defmethod
                    {:name-row 2 :name-col 12 :name-end-row 2 :name-end-col 22
                     :row 2 :col 12 :end-row 2 :end-col 22
                     :name 'my-multi
                     :uri (h/file-uri "file:///b.clj")
                     :alias 'f
                     :from 'b
                     :bucket :var-usages
                     :defmethod true
                     :to 'a}
                    ;; usage
                    {:name-row 5 :name-col 2 :name-end-row 5 :name-end-col 12
                     :row 5 :col 1 :end-row 5 :end-col 31
                     :name 'my-multi
                     :uri (h/file-uri "file:///b.clj")
                     :alias 'f
                     :from 'b
                     :arity 1
                     :bucket :var-usages
                     :to 'a}]]
    (testing "from defmulti method name"
      (h/assert-submaps
        references
        (q/find-references-from-cursor (h/db) (h/file-uri "file:///a.clj") defmulti-r defmulti-c false)))
    (testing "from defmethod method name"
      (h/assert-submaps
        references
        (q/find-references-from-cursor (h/db) (h/file-uri "file:///b.clj") defmethod-r defmethod-c false)))
    (testing "from usage name"
      (h/assert-submaps
        references
        (q/find-references-from-cursor (h/db) (h/file-uri "file:///b.clj") usage-r usage-c false)))))

(deftest find-references-for-defmulti-without-usages
  (let [[[defmulti-r defmulti-c]]
        (h/load-code-and-locs (h/code "(ns a)"
                                      "(defmulti |my-multi :some-key)"))
        [[defmethod-r defmethod-c]]
        (h/load-code-and-locs (h/code "(ns b (:require [a :as f]))"
                                      "(defmethod |f/my-multi :some-value"
                                      " [_]"
                                      " :foo)") (h/file-uri "file:///b.clj"))
        references [;; defmethod
                    {:name-row 2 :name-col 12 :name-end-row 2 :name-end-col 22
                     :row 2 :col 12 :end-row 2 :end-col 22
                     :name 'my-multi
                     :uri (h/file-uri "file:///b.clj")
                     :alias 'f
                     :from 'b
                     :bucket :var-usages
                     :defmethod true
                     :to 'a}]]
    (testing "from defmulti method name"
      (h/assert-submaps
        references
        (q/find-references-from-cursor (h/db) (h/file-uri "file:///a.clj") defmulti-r defmulti-c false)))
    (testing "from defmethod method name"
      (h/assert-submaps
        references
        (q/find-references-from-cursor (h/db) (h/file-uri "file:///b.clj") defmethod-r defmethod-c false)))))

(deftest find-references-from-declare
  (let [[[declare-r declare-c]
         [def-r def-c]
         [usage-r usage-c]]
        (h/load-code-and-locs (h/code "(ns a)"
                                      "(declare |my-declared)"
                                      "(def |my-declared 1)"
                                      "(inc |my-declared)"))
        usage-element {:name-row 4 :name-col 6 :name-end-row 4 :name-end-col 17
                       :row 4 :col 6 :end-row 4 :end-col 17
                       :name 'my-declared
                       :uri (h/file-uri "file:///a.clj")
                       :from 'a
                       :bucket :var-usages
                       :to 'a}]
    (testing "from declare"
      (h/assert-submaps
        [usage-element]
        (q/find-references-from-cursor (h/db) (h/file-uri "file:///a.clj") declare-r declare-c false)))
    (testing "from def"
      (h/assert-submaps
        [usage-element]
        (q/find-references-from-cursor (h/db) (h/file-uri "file:///a.clj") def-r def-c false)))
    (testing "from usage name"
      (h/assert-submaps
        [usage-element]
        (q/find-references-from-cursor (h/db) (h/file-uri "file:///a.clj") usage-r usage-c false)))))

(deftest find-references-from-declare-without-usages
  (let [[[declare-r declare-c]
         [def-r def-c]]
        (h/load-code-and-locs (h/code "(ns a)"
                                      "(declare |my-declared)"
                                      "(def |my-declared 1)"))]
    (testing "from declare"
      (h/assert-submaps
        '[]
        (q/find-references-from-cursor (h/db) (h/file-uri "file:///a.clj") declare-r declare-c false)))
    (testing "from def"
      (h/assert-submaps
        '[]
        (q/find-references-from-cursor (h/db) (h/file-uri "file:///b.clj") def-r def-c false)))))

(deftest find-references-in-edn
  (h/load-code-and-locs (h/code "(ns b)"
                                "{:a 1}") (h/file-uri "file:///b.clj"))
  (let [[[kwd-r kwd-c]]
        (h/load-code-and-locs (h/code "{:a 1"
                                      " :b {|:a :foo}}") (h/file-uri "file:///a.edn"))]

    (testing "from keyword outside source-path consider source-paths + uri analysis"
      (h/assert-submaps
        [{:name "a" :name-row 2 :name-col 2 :name-end-row 2 :name-end-col 4 :bucket :keyword-usages :uri (h/file-uri "file:///b.clj")}
         {:name "a" :name-row 1 :name-col 2 :name-end-row 1 :name-end-col 4 :bucket :keyword-usages :uri (h/file-uri "file:///a.edn")}
         {:name "a" :name-row 2 :name-col 6 :name-end-row 2 :name-end-col 8 :bucket :keyword-usages :uri (h/file-uri "file:///a.edn")}]
        (q/find-references-from-cursor (h/db) (h/file-uri "file:///a.edn") kwd-r kwd-c false)))))

(deftest find-definition-from-cursor
  (let [code (str "(ns a.b.c (:require [d.e.f :as |f-alias]))\n"
                  "(defn |x [|filename] |filename |f-alias/foo)\n"
                  "|x |unknown unknown")
        [[alias-r alias-c]
         [x-r x-c]
         [param-r param-c]
         [param-use-r param-use-c]
         [alias-use-r alias-use-c]
         [x-use-r x-use-c]
         [unknown-r unknown-c]] (h/load-code-and-locs code)
        _ (h/load-code-and-locs "(ns d.e.f) (def foo 1)" (h/file-uri "file:///b.clj"))
        db (h/db)]
    (h/assert-submap
      {:name 'x :name-row x-r :name-col x-c}
      (q/find-definition-from-cursor db (h/file-uri "file:///a.clj") x-use-r x-use-c))
    (h/assert-submap
      {:name 'filename :name-row param-r :name-col param-c}
      (q/find-definition-from-cursor db (h/file-uri "file:///a.clj") param-use-r param-use-c))
    (is (= nil
           (q/find-definition-from-cursor db (h/file-uri "file:///a.clj") unknown-r unknown-c)))
    (h/assert-submap
      {:name 'foo :uri (h/file-uri "file:///b.clj") :ns 'd.e.f}
      (q/find-definition-from-cursor db (h/file-uri "file:///a.clj") alias-use-r alias-use-c))
    (h/assert-submap
      {:name 'd.e.f :bucket :namespace-definitions}
      (q/find-definition-from-cursor db (h/file-uri "file:///a.clj") alias-r alias-c))))

(deftest find-definition-from-cursor-with-quoted-symbol
  (h/load-code-and-locs "(ns foo) (defn foo [])" (h/file-uri "file:///foo.clj"))
  (h/load-code-and-locs "(ns bar) (defn bar [])" (h/file-uri "file:///bar.clj"))
  (let [code (h/code "(ns a (:require [foo :as f]))"
                     "'foo/foo|"
                     "'f/foo|"
                     "'bar/bar|")
        [[foo-r foo-c] [foo-alias-r foo-alias-c] [bar-r bar-c]]
        (h/load-code-and-locs code (h/file-uri "file:///a.clj"))
        db (h/db)]
    (h/assert-submap
      {:name 'foo :ns 'foo :col 10}
      (q/find-definition-from-cursor db (h/file-uri "file:///a.clj") foo-r foo-c))
    (h/assert-submap
      {:name 'foo :ns 'foo :col 10}
      (q/find-definition-from-cursor db (h/file-uri "file:///a.clj") foo-alias-r foo-alias-c))
    (h/assert-submap
      {:name 'bar :ns 'bar :col 10}
      (q/find-definition-from-cursor db (h/file-uri "file:///a.clj") bar-r bar-c))))

(deftest find-definition-from-cursor-with-symbol-in-edn-file
  (h/load-code-and-locs "(ns exec-ns) (defn foo [])" (h/file-uri "file:///exec_ns.clj"))
  (let [code (h/code "{:aliases {:foo {:exec-fn |exec-ns/foo}}}")
        [[exec-fn-r exec-fn-c]]
        (h/load-code-and-locs code (h/file-uri "file:///deps.edn"))
        db (h/db)]
    (h/assert-submap
      {:name 'foo :ns 'exec-ns :row 1 :col 14}
      (q/find-definition-from-cursor db (h/file-uri "file:///deps.edn") exec-fn-r exec-fn-c))))

(deftest find-definition-from-cursor-when-duplicate-from-external-analysis
  (let [_ (h/load-code-and-locs (h/code "(ns foo) (def bar)") "zipfile:///some.jar::some-jar.clj")
        _ (h/load-code-and-locs (h/code "(ns foo) (def bar)") (h/file-uri "file:///a.clj"))
        [[bar-r bar-c]] (h/load-code-and-locs (h/code "(ns baz (:require [foo :as f]))"
                                                      "|f/bar") (h/file-uri "file:///b.clj"))
        db (h/db)]
    (h/assert-submap
      {:name 'bar :uri (h/file-uri "file:///a.clj")}
      (q/find-definition-from-cursor db (h/file-uri "file:///b.clj") bar-r bar-c))))

(defn assert-find-definition-from-cursor-when-it-has-same-namespace-from-clj-and-cljs []
  (testing "when on a clj file"
    (let [[[bar-r bar-c]] (h/load-code-and-locs (h/code "(ns baz (:require [foo :as f]))"
                                                        "|f/bar") (h/file-uri "file:///b.clj"))
          db (h/db)]
      (h/assert-submap
        {:name 'bar :uri (h/file-uri "zipfile:///some.jar::some-jar.clj")}
        (q/find-definition-from-cursor db (h/file-uri "file:///b.clj") bar-r bar-c)))
    (testing "fallback to cljs file"
      (let [[[bar-r bar-c]] (h/load-code-and-locs (h/code "(ns baz (:require [foo :as-alias f]))"
                                                          "|f/cljs-only") (h/file-uri "file:///b.clj"))
            db (h/db)]
        (h/assert-submap
          {:ns 'foo, :name 'cljs-only, :uri (h/file-uri "zipfile:///some.jar::other-jar.cljs")}
          (q/find-definition-from-cursor db (h/file-uri "file:///b.clj") bar-r bar-c)))))
  (testing "when on a cljs file"
    (let [[[bar-r bar-c]] (h/load-code-and-locs (h/code "(ns baz (:require [foo :as f]))"
                                                        "|f/bar") (h/file-uri "file:///b.cljs"))
          db (h/db)]
      (h/assert-submap
        {:name 'bar :uri (h/file-uri "zipfile:///some.jar::other-jar.cljs")}
        (q/find-definition-from-cursor db (h/file-uri "file:///b.cljs") bar-r bar-c))))
  (testing "when on a cljc file"
    (let [[[bar-r bar-c]] (h/load-code-and-locs (h/code "(ns baz (:require [foo :as f]))"
                                                        "|f/bar") (h/file-uri "file:///b.cljc"))
          db (h/db)]
      (h/assert-submap
        {:name 'bar :uri (h/file-uri "zipfile:///some.jar::some-jar.clj")}
        (q/find-definition-from-cursor db (h/file-uri "file:///b.cljc") bar-r bar-c))))
  (testing "when on a cljc file with multiple langs available"
    (let [[[bar-r-clj bar-c-clj]
           [bar-r-cljs bar-c-cljs]] (h/load-code-and-locs (h/code "(ns baz #?(:clj (:require [foo :as fc]) :cljs (:require [foo :as fs])))"
                                                                  "|fc/bar"
                                                                  "|fs/bar") (h/file-uri "file:///b.cljc"))
          db (h/db)]
      (h/assert-submap
        {:name 'bar :uri (h/file-uri "zipfile:///some.jar::some-jar.clj")}
        (q/find-definition-from-cursor db (h/file-uri "file:///b.cljc") bar-r-clj bar-c-clj))
      (h/assert-submap
        {:name 'bar :uri (h/file-uri "zipfile:///some.jar::other-jar.cljs")}
        (q/find-definition-from-cursor db (h/file-uri "file:///b.cljc") bar-r-cljs bar-c-cljs))))
  (testing "when a macro is require-macros on cljs, being the var-definition on a clj/cljc file."
    (h/reset-components!)
    (h/load-code-and-locs (h/code "(ns some.foo) (defmacro bar [& body] @body)") (h/file-uri "file:///some/foo.clj"))
    (h/load-code-and-locs (h/code "(ns some.foo (:require-macros [some.foo])) (def baz)") (h/file-uri "file:///some/foo.cljs"))
    (let [[[bar-r bar-c]] (h/load-code-and-locs (h/code "(ns a (:require [some.foo :as f])) (f/|bar 1)") (h/file-uri "file:///a.cljs"))
          db (h/db)]
      (h/assert-submap
        {:name 'bar :uri (h/file-uri "file:///some/foo.clj")}
        (q/find-definition-from-cursor db (h/file-uri "file:///a.cljs") bar-r bar-c)))))

(deftest find-definition-from-cursor-when-it-has-same-namespace-from-clj-and-cljs
  (h/load-code-and-locs (h/code "(ns foo) (def bar)") (h/file-uri "zipfile:///some.jar::some-jar.clj"))
  (h/load-code-and-locs (h/code "(ns foo) (def bar) (def cljs-only)") (h/file-uri "zipfile:///some.jar::other-jar.cljs"))
  (assert-find-definition-from-cursor-when-it-has-same-namespace-from-clj-and-cljs))

(deftest find-definition-from-cursor-when-it-has-same-namespace-from-clj-and-cljs-in-other-load-order
  (h/load-code-and-locs (h/code "(ns foo) (def bar) (def cljs-only)") (h/file-uri "zipfile:///some.jar::other-jar.cljs"))
  (h/load-code-and-locs (h/code "(ns foo) (def bar)") (h/file-uri "zipfile:///some.jar::some-jar.clj"))
  (assert-find-definition-from-cursor-when-it-has-same-namespace-from-clj-and-cljs))

(deftest find-definition-from-cursor-when-declared
  (testing "declared, then defined"
    (let [[[usage-r usage-c]
           [defn-r _]] (h/load-code-and-locs
                         (h/code "(ns foo)"
                                 "(declare bar)"
                                 "(|bar)"
                                 "(defn |bar [] 1)") (h/file-uri "file:///a.clj"))
          db (h/db)]
      (h/assert-submap
        {:name 'bar :uri (h/file-uri "file:///a.clj") :defined-by 'clojure.core/defn :row defn-r}
        (q/find-definition-from-cursor db (h/file-uri "file:///a.clj") usage-r usage-c))))
  (testing "only declared"
    (let [[[decl-r _]
           [usage-r usage-c]] (h/load-code-and-locs
                                (h/code "(ns foo)"
                                        "(declare |bar)"
                                        "(|bar)") (h/file-uri "file:///a.clj"))
          db (h/db)]
      (h/assert-submap
        {:name 'bar :uri (h/file-uri "file:///a.clj") :defined-by 'clojure.core/declare :row decl-r}
        (q/find-definition-from-cursor db (h/file-uri "file:///a.clj") usage-r usage-c))))
  (testing "navigating from declare to definition"
    (let [positions (h/load-code-and-locs
                      (h/code "(ns foo)"
                              "(declare |bar)"
                              "(defn something [] (bar))"
                              "(defn |bar [] 1)") (h/file-uri "file:///a.clj"))
          [decl-r decl-c] (first positions)
          definition (q/find-definition-from-cursor (h/db) (h/file-uri "file:///a.clj") decl-r decl-c)]
      (h/assert-submap
        {:name 'bar :uri (h/file-uri "file:///a.clj") :defined-by 'clojure.core/defn}
        definition))))

(deftest find-definition-from-namespace-alias
  (h/load-code-and-locs (h/code "(ns foo.bar) (def a 1)") (h/file-uri "file:///a.clj"))
  (let [[[foob-r foob-c]] (h/load-code-and-locs (h/code "(ns foo.baz (:require [foo.bar :as |foob]))") (h/file-uri "file:///b.clj"))
        db (h/db)]
    (h/assert-submap
      {:name-end-col 12 :name-end-row 1 :name-row 1 :name 'foo.bar :uri (h/file-uri "file:///a.clj") :col 1 :name-col 5 :bucket :namespace-definitions :row 1}
      (q/find-definition-from-cursor db (h/file-uri "file:///b.clj") foob-r foob-c))))

(deftest find-definition-from-cursor-when-on-potemkin
  (h/load-code-and-locs (h/code "(ns foo.impl) (def bar)") (h/file-uri "file:///b.clj"))
  (let [[[bar-r bar-c]] (h/load-code-and-locs
                          (h/code "(ns foo.api"
                                  "  (:require [potemkin :refer [import-vars]]"
                                  "            [foo.impl :as impl]))"
                                  "(import-vars |impl/bar)") (h/file-uri "file:///a.clj"))
        db (h/db)]
    (h/assert-submap
      {:name 'bar :uri (h/file-uri "file:///b.clj") :defined-by 'clojure.core/def :row 1 :col 15}
      (q/find-definition-from-cursor db (h/file-uri "file:///a.clj") bar-r bar-c))))

(deftest find-definition-from-keyword-usage
  (h/load-code-and-locs (h/code "(ns bbb (:require [re-frame.core :as r]))"
                                "(r/reg-event-fx :namespaced/kw (fn []))"
                                "(r/reg-event-fx :simple-kw (fn []))")
                        (h/file-uri "file:///bbb.clj"))
  (let [[[ns-kw-r ns-kw-c]
         [simple-kw-r simple-kw-c]
         [unreg-kw-r unreg-kw-c]]
        (h/load-code-and-locs (h/code "(ns aaa)"
                                      "|:namespaced/kw"
                                      "|:simple-kw"
                                      "|:unregistered-kw")
                              (h/file-uri "file:///aaa.clj"))
        db (h/db)]
    (h/assert-submap
      {:ns 'namespaced, :name "kw", :uri (h/file-uri "file:///bbb.clj"), :bucket :keyword-definitions}
      (q/find-definition-from-cursor db (h/file-uri "file:///aaa.clj") ns-kw-r ns-kw-c))
    (h/assert-submap
      {:ns nil, :name "simple-kw", :uri (h/file-uri "file:///bbb.clj"), :bucket :keyword-definitions}
      (q/find-definition-from-cursor db (h/file-uri "file:///aaa.clj") simple-kw-r simple-kw-c))
    (h/assert-submap
      {:ns nil, :name "unregistered-kw", :uri (h/file-uri "file:///aaa.clj"), :bucket :keyword-usages}
      (q/find-definition-from-cursor db (h/file-uri "file:///aaa.clj") unreg-kw-r unreg-kw-c))))

(deftest find-clojure-definition-of-imported-java-class-usage
  (h/load-code-and-locs (h/code "(ns my.fabulous-namespace)"
                                "(defrecord SomeRecord [foo bar])"
                                "(defprotocol SomeProtocol"
                                "  (do-stuff [this]))") (h/file-uri "file:///project/my/fabulous_namespace.clj"))

  (testing "Finding defrecord definition from imported java class"
    (let [[[from-ns-row from-ns-col]
           [from-defrecord-row from-defrecord-col]]
          (h/load-code-and-locs
            (h/code "(ns my.other-namespace"
                    "  (:require [my.fabulous-namespace :as foobar])"
                    "  (:import [my.fabulous_namespace |SomeRecord]))"
                    ""
                    "(extend-type |SomeRecord"
                    "  foobar/SomeProtocol "
                    "  (do-stuff [this]"
                    "    42))") (h/file-uri "file:///project/my/other_namespace.clj"))
          expected {:ns 'my.fabulous-namespace
                    :name 'SomeRecord
                    :defined-by 'clojure.core/defrecord
                    :uri (h/file-uri "file:///project/my/fabulous_namespace.clj")
                    :bucket :var-definitions}]
      (h/assert-submap
        expected
        (q/find-definition-from-cursor (h/db) (h/file-uri "file:///project/my/other_namespace.clj") from-ns-row from-ns-col))
      (h/assert-submap
        expected
        (q/find-definition-from-cursor (h/db) (h/file-uri "file:///project/my/other_namespace.clj") from-defrecord-row from-defrecord-col)))))

(deftest find-declaration-from-cursor
  (h/load-code-and-locs (h/code "(ns foo.baz) (def other 123)"))
  (h/load-code-and-locs (h/code "(ns colors) (def orange 123)"))
  (let [[[something-r something-c]
         [orange-r orange-c]
         [other-r other-c]] (h/load-code-and-locs
                              (h/code "(ns sample"
                                      "  (:require [foo.bar :as foob]"
                                      "            [colors :refer [orange]]"
                                      "            [foo.baz :refer :all]))"
                                      "foob/som|ething"
                                      "|orange"
                                      "|other")
                              (h/file-uri "file:///b.clj"))
        db (h/db)]
    (testing "from usage with alias"
      (h/assert-submap
        {:alias 'foob
         :from 'sample
         :bucket
         :namespace-alias
         :to 'foo.bar}
        (q/find-declaration-from-cursor db (h/file-uri "file:///b.clj") something-r something-c)))
    (testing "from usage with refer"
      (h/assert-submap
        '{:from sample
          :to colors
          :name orange
          :bucket :var-usages
          :refer true}
        (q/find-declaration-from-cursor db (h/file-uri "file:///b.clj") orange-r orange-c)))
    (testing "from usage with refer all"
      (h/assert-submap
        {:from 'sample
         :bucket
         :namespace-usages
         :name 'foo.baz}
        (q/find-declaration-from-cursor db (h/file-uri "file:///b.clj") other-r other-c)))))

(deftest find-declaration-from-cursor-in-cljc
  (let [[[clj-ns-r clj-ns-c]
         [cljs-ns-r cljs-ns-c]
         [clj-usage-r clj-usage-c]
         [cljs-usage-r cljs-usage-c]] (h/load-code-and-locs
                                        (h/code "(ns sample"
                                                "  (:require #?(:clj |foo"
                                                "               :cljs |foo)))"
                                                "#?(:clj foo/som|ething"
                                                "   :cljs foo/som|ething)")
                                        (h/file-uri "file:///b.cljc"))
        db (h/db)]
    (testing "from clj usage"
      (h/assert-submap
        {:name 'foo
         :from 'sample
         :bucket :namespace-usages
         :row clj-ns-r
         :col clj-ns-c}
        (q/find-declaration-from-cursor db (h/file-uri "file:///b.cljc") clj-usage-r clj-usage-c)))
    (testing "from cljs usage"
      (h/assert-submap
        {:name 'foo
         :from 'sample
         :bucket :namespace-usages
         :row cljs-ns-r
         :col cljs-ns-c}
        (q/find-declaration-from-cursor db (h/file-uri "file:///b.cljc") cljs-usage-r cljs-usage-c)))))

(deftest find-implementations-from-cursor-protocols
  (h/load-code-and-locs (h/code "(ns a)"
                                "(defprotocol Foo"
                                "  (something []))"))
  (h/load-code-and-locs (h/code "(ns b (:require [a :as f]))"
                                "(defrecord FooImpl1 []"
                                " f/Foo"
                                " (something [_] 123))"
                                "(defrecord FooImpl2 []"
                                " f/Foo"
                                " (^void something [_] 456))"
                                "(f/something (->FooImpl1))"
                                "(f/something (->FooImpl2))"
                                "(defn make-foo [] (reify f/Foo (something [_] 123)))") (h/file-uri "file:///b.clj"))
  (testing "from protocol name definition"
    (h/assert-submaps
      '[{:name Foo
         :name-row 3 :name-col 2 :name-end-row 3 :name-end-col 7
         :row 3 :col 2 :end-row 3 :end-col 7
         :alias f
         :from b
         :bucket :var-usages
         :to a}
        {:name-row 6 :name-col 2 :name-end-row 6 :name-end-col 7
         :row 6 :col 2 :end-row 6 :end-col 7
         :alias f
         :name Foo
         :from b
         :bucket :var-usages
         :to a}
        {:name-row 10 :name-col 26 :name-end-row 10 :name-end-col 31
         :row 10 :col 26 :end-col 31 :end-row 10
         :name Foo
         :alias f
         :from b
         :context {}
         :from-var make-foo
         :bucket :var-usages
         :to a}]
      (q/find-implementations-from-cursor (h/db) (h/file-uri "file:///a.clj") 2 16)))
  (testing "from protocol method definitions"
    (h/assert-submaps
      [{:impl-ns 'b
        :protocol-ns 'a
        :method-name 'something
        :name-row 4 :name-col 3 :name-end-row 4 :name-end-col 12
        :row 4 :col 2 :end-row 4 :end-col 21
        :defined-by 'clojure.core/defrecord
        :protocol-name 'Foo
        :bucket :protocol-impls}
       {:impl-ns 'b
        :name-row 7 :name-col 9 :name-end-row 7 :name-end-col 18
        :row 7 :col 2 :end-row 7 :end-col 27
        :protocol-ns 'a
        :method-name 'something
        :defined-by 'clojure.core/defrecord
        :protocol-name 'Foo
        :bucket :protocol-impls}
       {:impl-ns 'b
        :row 10 :col 32 :end-row 10 :end-col 51
        :name-row 10 :name-col 33 :name-end-row 10 :name-end-col 42
        :protocol-ns 'a
        :method-name 'something
        :defined-by 'clojure.core/reify
        :protocol-name 'Foo
        :uri (h/file-uri "file:///b.clj")
        :bucket :protocol-impls}]
      (q/find-implementations-from-cursor (h/db) (h/file-uri "file:///a.clj") 3 4)))
  (testing "from implementation usage"
    (h/assert-submaps
      [{:impl-ns 'b
        :protocol-ns 'a
        :method-name 'something
        :name-row 4 :name-col 3 :name-end-row 4 :name-end-col 12
        :row 4 :col 2 :end-row 4 :end-col 21
        :defined-by 'clojure.core/defrecord
        :protocol-name 'Foo
        :bucket :protocol-impls}
       {:impl-ns 'b
        :name-row 7 :name-col 9 :name-end-row 7 :name-end-col 18
        :row 7 :col 2 :end-row 7 :end-col 27
        :protocol-ns 'a
        :method-name 'something
        :defined-by 'clojure.core/defrecord
        :protocol-name 'Foo
        :bucket :protocol-impls}
       {:impl-ns 'b
        :row 10 :col 32 :end-row 10 :end-col 51
        :name-row 10 :name-col 33 :name-end-row 10 :name-end-col 42
        :protocol-ns 'a
        :method-name 'something
        :defined-by 'clojure.core/reify
        :protocol-name 'Foo
        :uri (h/file-uri "file:///b.clj")
        :bucket :protocol-impls}]
      (q/find-implementations-from-cursor (h/db) (h/file-uri "file:///b.clj") 9 2))))

(deftest find-implementations-from-cursor-defmulti
  (h/load-code-and-locs (h/code "(ns a)"
                                "(defmulti foo :some-key)"))
  (h/load-code-and-locs (h/code "(ns b (:require [a :as f]))"
                                "(defmethod f/foo :some-value"
                                "  [_]"
                                "  1)"
                                "(defmethod f/foo :default"
                                "  [_]"
                                "  2)"
                                "(f/foo {:some-key :some-value})"
                                "(f/foo {})") (h/file-uri "file:///b.clj"))
  (testing "from defmulti definition"
    (h/assert-submaps
      '[{:name foo
         :name-row 2 :name-col 12 :name-end-row 2 :name-end-col 17
         :row 2 :col 12 :end-row 2 :end-col 17
         :defmethod true
         :alias f
         :from b
         :bucket :var-usages
         :to a}
        {:name-row 5 :name-col 12 :name-end-row 5 :name-end-col 17
         :row 5 :col 12 :end-row 5 :end-col 17
         :name foo
         :defmethod true
         :alias f
         :from b
         :bucket :var-usages
         :to a}]
      (q/find-implementations-from-cursor (h/db) (h/file-uri "file:///a.clj") 2 12)))
  (testing "from defmethod declaration"
    (h/assert-submaps
      '[{:name foo
         :name-row 2 :name-col 12 :name-end-row 2 :name-end-col 17
         :row 2 :col 12 :end-row 2 :end-col 17
         :defmethod true
         :alias f
         :from b
         :bucket :var-usages
         :to a}
        {:name-row 5 :name-col 12 :name-end-row 5 :name-end-col 17
         :row 5 :col 12 :end-row 5 :end-col 17
         :name foo
         :defmethod true
         :alias f
         :from b
         :bucket :var-usages
         :to a}]
      (q/find-implementations-from-cursor (h/db) (h/file-uri "file:///b.clj") 2 13)))
  (testing "from defmethod usage"
    (h/assert-submaps
      '[{:name foo
         :name-row 2 :name-col 12 :name-end-row 2 :name-end-col 17
         :row 2 :col 12 :end-row 2 :end-col 17
         :defmethod true
         :alias f
         :from b
         :bucket :var-usages
         :to a}
        {:name-row 5 :name-col 12 :name-end-row 5 :name-end-col 17
         :row 5 :col 12 :end-row 5 :end-col 17
         :name foo
         :defmethod true
         :alias f
         :from b
         :bucket :var-usages
         :to a}]
      (q/find-implementations-from-cursor (h/db) (h/file-uri "file:///b.clj") 8 2))))

(deftest find-unused-aliases
  (testing "clj"
    (testing "used require via alias"
      (h/load-code-and-locs "(ns a (:require [x :as f])) f/foo")
      (is (= '#{}
             (q/find-unused-aliases (h/db) (h/file-uri "file:///a.clj")))))
    (testing "used require via full-ns"
      (h/load-code-and-locs "(ns a (:require [x :as f])) x/foo")
      (is (= '#{}
             (q/find-unused-aliases (h/db) (h/file-uri "file:///a.clj")))))
    (testing "full-ns require"
      (h/load-code-and-locs "(ns a (:require [x] y)) foo")
      (is (= '#{}
             (q/find-unused-aliases (h/db) (h/file-uri "file:///a.clj")))))
    (testing "single unused-alias"
      (h/load-code-and-locs "(ns a (:require [x :as f]))")
      (is (= '#{x}
             (q/find-unused-aliases (h/db) (h/file-uri "file:///a.clj")))))
    (testing "used and unused aliases"
      (h/load-code-and-locs "(ns a (:require [x :as f] [foo] x [bar :as b] [y :refer [m]] [z :refer [o i]])) o")
      (is (= '#{y bar}
             (q/find-unused-aliases (h/db) (h/file-uri "file:///a.clj"))))))
  (testing "cljc"
    (testing "used require via alias"
      (h/load-code-and-locs "(ns a (:require [x :as f])) f/foo" (h/file-uri "file:///a.cljc"))
      (is (= '#{}
             (q/find-unused-aliases (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "used require via full-ns"
      (h/load-code-and-locs "(ns a (:require [x :as f])) x/foo" (h/file-uri "file:///a.cljc"))
      (is (= '#{}
             (q/find-unused-aliases (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "full-ns require"
      (h/load-code-and-locs "(ns a (:require [x] y)) foo" (h/file-uri "file:///a.cljc"))
      (is (= '#{}
             (q/find-unused-aliases (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "single unused-alias"
      (h/load-code-and-locs "(ns a (:require [x :as f]))" (h/file-uri "file:///a.cljc"))
      (is (= '#{x}
             (q/find-unused-aliases (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "used and unused aliases"
      (h/load-code-and-locs "(ns a (:require [x :as f] [foo] x [bar :as b] [y :refer [m]] [z :refer [o i]])) o" (h/file-uri "file:///a.cljc"))
      (is (= '#{y bar}
             (q/find-unused-aliases (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "used alias in a reader conditional"
      (h/load-code-and-locs "(ns a (:require [y :as o] [x :as f])) #?(:clj f/foo)" (h/file-uri "file:///a.cljc"))
      (is (= '#{y}
             (q/find-unused-aliases (h/db) (h/file-uri "file:///a.cljc")))))))

(deftest find-unused-refers
  (testing "clj"
    (testing "used require via refer"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo]])) foo")
      (is (= '#{}
             (q/find-unused-refers (h/db) (h/file-uri "file:///a.clj")))))
    (testing "multiple used refers"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo bar baz]])) foo bar baz")
      (is (= '#{}
             (q/find-unused-refers (h/db) (h/file-uri "file:///a.clj")))))
    (testing "single unused refer"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo]]))")
      (is (= '#{x/foo}
             (q/find-unused-refers (h/db) (h/file-uri "file:///a.clj")))))
    (testing "multiple unused refer"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo bar]]))")
      (is (= '#{x/foo x/bar}
             (q/find-unused-refers (h/db) (h/file-uri "file:///a.clj")))))
    (testing "multiple unused refer and used"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo bar baz]])) bar")
      (is (= '#{x/foo x/baz}
             (q/find-unused-refers (h/db) (h/file-uri "file:///a.clj"))))))
  (testing "cljc"
    (testing "used require via refer"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo]])) foo" (h/file-uri "file:///a.cljc"))
      (is (= '#{}
             (q/find-unused-refers (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "multiple used refers"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo bar baz]])) foo bar baz" (h/file-uri "file:///a.cljc"))
      (is (= '#{}
             (q/find-unused-refers (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "single unused refer"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo]]))" (h/file-uri "file:///a.cljc"))
      (is (= '#{x/foo}
             (q/find-unused-refers (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "multiple unused refer"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo bar]]))" (h/file-uri "file:///a.cljc"))
      (is (= '#{x/foo x/bar}
             (q/find-unused-refers (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "multiple unused refer and used"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo bar baz]])) bar" (h/file-uri "file:///a.cljc"))
      (is (= '#{x/foo x/baz}
             (q/find-unused-refers (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "used refer in a reader conditional"
      (h/load-code-and-locs "(ns a (:require [y :refer [o]] [x :refer [f]])) #?(:clj f)" (h/file-uri "file:///a.cljc"))
      (is (= '#{y/o}
             (q/find-unused-refers (h/db) (h/file-uri "file:///a.cljc")))))))

(deftest find-unused-imports
  (testing "clj"
    (testing "single used full import"
      (h/load-code-and-locs "(ns a (:import java.util.Date)) Date.")
      (is (= '#{}
             (q/find-unused-imports (h/db) (h/file-uri "file:///a.clj")))))
    (testing "single unused full import"
      (h/load-code-and-locs "(ns a (:import java.util.Date))")
      (is (= '#{java.util.Date}
             (q/find-unused-imports (h/db) (h/file-uri "file:///a.clj")))))
    (testing "multiple unused full imports"
      (h/load-code-and-locs "(ns a (:import java.util.Date java.util.Calendar java.time.LocalDateTime))")
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (h/db) (h/file-uri "file:///a.clj")))))
    (testing "multiple unused package imports"
      (h/load-code-and-locs "(ns a (:import [java.util Date Calendar] [java.time LocalDateTime]))")
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (h/db) (h/file-uri "file:///a.clj")))))
    (testing "multiple unused and used imports"
      (h/load-code-and-locs "(ns a (:import [java.util Date Calendar] [java.time LocalTime LocalDateTime])) LocalTime.")
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (h/db) (h/file-uri "file:///a.clj"))))))
  (testing "cljc"
    (testing "single used full import"
      (h/load-code-and-locs "(ns a (:import java.util.Date)) Date." (h/file-uri "file:///a.cljc"))
      (is (= '#{}
             (q/find-unused-imports (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "single unused full import"
      (h/load-code-and-locs "(ns a (:import java.util.Date))" (h/file-uri "file:///a.cljc"))
      (is (= '#{java.util.Date}
             (q/find-unused-imports (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "multiple unused full imports"
      (h/load-code-and-locs "(ns a (:import java.util.Date java.util.Calendar java.time.LocalDateTime))" (h/file-uri "file:///a.cljc"))
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "multiple unused package imports"
      (h/load-code-and-locs "(ns a (:import [java.util Date Calendar] [java.time LocalDateTime]))" (h/file-uri "file:///a.cljc"))
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "multiple unused and used imports"
      (h/load-code-and-locs "(ns a (:import [java.util Date Calendar] [java.time LocalTime LocalDateTime])) LocalTime." (h/file-uri "file:///a.cljc"))
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (h/db) (h/file-uri "file:///a.cljc")))))
    (testing "used import in a reader conditional"
      (h/load-code-and-locs "(ns a (:import [java.util Date Calendar] [java.time LocalTime LocalDateTime])) #?(:clj LocalTime.)" (h/file-uri "file:///a.cljc"))
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (h/db) (h/file-uri "file:///a.cljc")))))))

(deftest find-local-usages-defined-outside-form
  (testing "inside let"
    (let [[[sum-pos-r sum-pos-c]
           [sum-end-pos-r sum-end-pos-c]]
          (h/load-code-and-locs "(ns a) (let [a 2 b 1] |(+ 2 b)| (- 2 a))")]
      (h/assert-submaps
        [{:name 'b}]
        (q/find-local-usages-defined-outside-form (h/db) (h/file-uri "file:///a.clj")
                                                  {:row sum-pos-r, :col sum-pos-c
                                                   :end-row sum-end-pos-r, :end-col sum-end-pos-c}))))
  (testing "inside defn"
    (let [[[let-pos-r let-pos-c]
           [let-end-pos-r let-end-pos-c]]
          (h/load-code-and-locs "(ns a) (defn ab [b] |(let [a 1] (b a))|) (defn other [c] c)")]
      (h/assert-submaps
        [{:name 'b}]
        (q/find-local-usages-defined-outside-form (h/db) (h/file-uri "file:///a.clj")
                                                  {:row let-pos-r, :col let-pos-c
                                                   :end-row let-end-pos-r, :end-col let-end-pos-c})))))

(deftest find-local-usages-under-form
  (let [[[start-r start-c]
         [end-r end-c]]
        (h/load-code-and-locs "(let [a 2] |(let [b 1] (+ 2 b) (- 2 a))|)")]
    (h/assert-submaps
      [{:name 'b}
       {:name 'a}]
      (q/find-local-usages-under-form (h/db) (h/file-uri "file:///a.clj")
                                      {:row start-r, :col start-c
                                       :end-row end-r, :end-col end-c}))))

(deftest find-all-project-namespace-definitions
  (h/load-code-and-locs "(ns a)" (h/file-uri "file:///src/a.cljc"))
  (h/load-code-and-locs "(ns a)" (h/file-uri "file:///test/a.clj"))
  (h/load-code-and-locs "(ns b)" (h/file-uri "file:///test/b.clj"))

  (h/assert-submaps
    [{:uri (h/file-uri "file:///src/a.cljc")}
     {:uri (h/file-uri "file:///test/a.clj")}]
    (q/find-all-project-namespace-definitions (h/db) 'a)))

(deftest analysis-summary-test
  (h/load-code-and-locs "(ns a) (def a 1) (def b 2) (def c 3)" (h/file-uri "file:///src/a.clj"))
  (h/load-code-and-locs "(ns b) (def d 1) (def e 2) :a 1 :b 2" (h/file-uri "file:///src/b.clj"))

  (h/assert-submap
    {:internal {:var-definitions 5
                :keyword-usages 2
                :var-usages 5
                :namespace-definitions 2}
     :external {}}
    (q/analysis-summary (h/db))))
