(ns clojure-lsp.feature.project-tree-test
  (:require
   [clojure-lsp.feature.project-tree :as f.project-tree]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest]]))

(h/reset-components-before-test)

(deftest project-root-node-test
  (swap! (h/db*) shared/deep-merge {:settings {:dependency-scheme "jar"
                                               :source-paths #{(h/file-path "/user/project/src/main/clojure")
                                                               (h/file-path "/user/project/src/test/clojure")}}
                                    :project-root-uri (h/file-uri "file:///user/project")})
  (h/assert-submap
    {:name "project"
     :type :project
     :nodes [{:name (h/file-path "src/main/clojure")
              :final false
              :type :source-path}
             {:name (h/file-path "src/test/clojure")
              :final false
              :type :source-path}
             {:name "External dependencies"
              :id :external-dependencies
              :final false
              :type :library}]}
    (f.project-tree/nodes (h/db) nil)))

(deftest source-path-node-test
  (swap! (h/db*) shared/deep-merge {:settings {:dependency-scheme "jar"
                                               :source-paths #{(h/file-path "/user/project/src/main/clojure")
                                                               (h/file-path "/user/project/src/test/clojure")}}
                                    :project-root-uri (h/file-uri "file:///user/project")})
  (h/load-code-and-locs (h/code "(ns foo.bar)"
                                "(def a 1)") (h/file-uri "file:///user/project/src/main/clojure/foo/bar.clj"))
  (h/load-code-and-locs (h/code "(ns foo.baz)"
                                "(def b 1)") (h/file-uri "file:///user/project/src/main/clojure/foo/baz.clj"))
  (h/load-code-and-locs (h/code "(ns foo.bar-test)"
                                "(def c 1)") (h/file-uri "file:///user/project/src/test/clojure/foo/bar_test.clj"))
  (h/load-code-and-locs (h/code "(ns foo.bar-jar)"
                                "(def d 1)") (h/file-uri "jar:file:///path/to/some.jar!/foo/bar_jar.clj"))
  (h/assert-submap
    {:name (h/file-path "src/main/clojure")
     :type :source-path
     :nodes [{:name "foo.bar"
              :uri (h/file-uri "file:///user/project/src/main/clojure/foo/bar.clj")
              :final false
              :type :ns}
             {:name "foo.baz"
              :uri (h/file-uri "file:///user/project/src/main/clojure/foo/baz.clj")
              :final false
              :type :ns}]}
    (f.project-tree/nodes (h/db) {:name (h/file-path "src/main/clojure")
                                  :type :source-path})))

(deftest external-dependencies-node-test
  (swap! (h/db*) shared/deep-merge {:settings {:dependency-scheme "jar"
                                               :source-paths #{(h/file-path "/user/project/src/main/clojure")
                                                               (h/file-path "/user/project/src/test/clojure")}}
                                    :project-root-uri (h/file-uri "file:///user/project")})
  (h/load-code-and-locs (h/code "(ns foo.bar)"
                                "(def a 1)") (h/file-uri "file:///user/project/src/main/clojure/foo/bar.clj"))
  (h/load-code-and-locs (h/code "(ns foo.bar-jar)"
                                "(def b 1)") (h/file-uri "jar:file:///path/to/asd.some-2-3.jar!/foo/bar_jar.clj"))
  (h/load-code-and-locs (h/code "(ns baz.baz)"
                                "(def c 1)") (h/file-uri "jar:file:///path/to/asd.some-2-3.jar!/baz/baz.clj"))
  (h/load-code-and-locs (h/code "(ns bar.another-lib)"
                                "(def c 1)") (h/file-uri "jar:file:///path/to/another/lib.jar!/bar/another_lib.clj"))
  (h/assert-submap
    {:name "External dependencies"
     :id "external-dependencies"
     :type :library
     :nodes [{:name "asd.some-2-3.jar"
              :detail (h/file-uri "jar:file:///path/to/asd.some-2-3.jar")
              :uri (h/file-uri "jar:file:///path/to/asd.some-2-3.jar")
              :final false
              :type :jar}
             {:name "lib.jar"
              :detail (h/file-uri "jar:file:///path/to/another/lib.jar")
              :uri (h/file-uri "jar:file:///path/to/another/lib.jar")
              :final false
              :type :jar}]}
    (f.project-tree/nodes (h/db) {:name "External dependencies"
                                  :id "external-dependencies"
                                  :type :library})))

(deftest jar-node-test
  (swap! (h/db*) shared/deep-merge {:settings {:dependency-scheme "jar"
                                               :source-paths #{(h/file-path "/user/project/src/main/clojure")
                                                               (h/file-path "/user/project/src/test/clojure")}}
                                    :project-root-uri (h/file-uri "file:///user/project")})
  (h/load-code-and-locs (h/code "(ns foo.bar)"
                                "(def a 1)") (h/file-uri "file:///user/project/src/main/clojure/foo/bar.clj"))
  (h/load-code-and-locs (h/code "(ns foo.bar-jar)"
                                "(def b 1)"
                                "(def c 1)") (h/file-uri "jar:file:///path/to/some.jar!/foo/bar_jar.clj"))
  (h/load-code-and-locs (h/code "(ns baz.baz)"
                                "(def d 1)") (h/file-uri "jar:file:///path/to/some.jar!/baz/baz.clj"))
  (h/load-code-and-locs (h/code "(ns bar.another-lib)"
                                "(def e 1)") (h/file-uri "jar:file:///path/to/another/lib.jar!/bar/another_lib.clj"))
  (h/assert-submap
    {:name "some.jar"
     :type :jar
     :detail (h/file-uri "jar:file:///path/to/some.jar")
     :uri (h/file-uri "jar:file:///path/to/some.jar")
     :nodes [{:name "foo.bar-jar"
              :uri (h/file-uri "jar:file:///path/to/some.jar!/foo/bar_jar.clj")
              :final false
              :type :ns}
             {:name "baz.baz"
              :uri (h/file-uri "jar:file:///path/to/some.jar!/baz/baz.clj")
              :final false
              :type :ns}]}
    (f.project-tree/nodes (h/db) {:name "some.jar"
                                  :detail (h/file-uri "jar:file:///path/to/some.jar")
                                  :uri (h/file-uri "jar:file:///path/to/some.jar")
                                  :type :jar})))

(deftest ns-node-test
  (swap! (h/db*) shared/deep-merge {:settings {:dependency-scheme "jar"
                                               :source-paths #{(h/file-path "/user/project/src/main/clojure")
                                                               (h/file-path "/user/project/src/test/clojure")}}
                                    :project-root-uri (h/file-uri "file:///user/project")})
  (h/load-code-and-locs (h/code "(ns foo.bar (:require [re-frame.core :as r]))"
                                "(def a 1)"
                                "(def ^:private b 2)"
                                "(defn c [] 3)"
                                "(definterface Foo)"
                                "(r/reg-sub ::foo (fn [_]))") (h/file-uri "file:///user/project/src/main/clojure/foo/bar.clj"))
  (h/assert-submap
    {:name "foo.bar"
     :type :ns
     :uri (h/file-uri "file:///user/project/src/main/clojure/foo/bar.clj")
     :nodes [{:name "a"
              :uri (h/file-uri "file:///user/project/src/main/clojure/foo/bar.clj")
              :range {:start {:line 1 :character 5} :end {:line 1 :character 6}}
              :final true
              :type :variable}
             {:name "b"
              :uri (h/file-uri "file:///user/project/src/main/clojure/foo/bar.clj")
              :range {:start {:line 2 :character 15} :end {:line 2 :character 16}}
              :final true
              :type :variable
              :detail "private"}
             {:name "c"
              :uri (h/file-uri "file:///user/project/src/main/clojure/foo/bar.clj")
              :range {:start {:line 3 :character 6} :end {:line 3 :character 7}}
              :final true
              :type :function}
             {:name "Foo"
              :uri (h/file-uri "file:///user/project/src/main/clojure/foo/bar.clj")
              :range {:start {:line 4 :character 14} :end {:line 4 :character 17}}
              :final true
              :type :interface}
             {:name "foo"
              :uri (h/file-uri "file:///user/project/src/main/clojure/foo/bar.clj")
              :range {:start {:line 5 :character 11} :end {:line 5 :character 16}}
              :final true
              :type :function
              :detail "reg-sub"}]}
    (f.project-tree/nodes (h/db) {:name "foo.bar"
                                  :uri (h/file-uri "file:///user/project/src/main/clojure/foo/bar.clj")
                                  :final false
                                  :type :ns})))
