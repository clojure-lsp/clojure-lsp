(ns clojure-lsp.queries-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [taoensso.timbre :as log]))

(h/reset-db-after-test)

(deftest project-analysis
  (testing "when dependency-scheme is zip"
    (h/clean-db!)
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///b.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "jar:file:///some.jar!/some-file.clj"))
    (is (= 2 (count (q/filter-project-analysis (:analysis @db/db) db/db)))))
  (testing "when dependency-scheme is jar"
    (swap! db/db shared/deep-merge {:settings {:dependency-scheme "jar"}})
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///b.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "jar:file:///some.jar!/some-file.clj"))
    (is (= 2 (count (q/filter-project-analysis (:analysis @db/db) db/db))))))

(deftest external-analysis
  (testing "when dependency-scheme is zip"
    (h/clean-db!)
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///b.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "jar:file:///some.jar!/some-file.clj"))
    (is (= 1 (count (q/filter-external-analysis (:analysis @db/db) db/db)))))
  (testing "when dependency-scheme is jar"
    (swap! db/db shared/deep-merge {:settings {:dependency-scheme "jar"}})
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///b.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "jar:file:///some.jar!/some-file.clj"))
    (is (= 1 (count (q/filter-external-analysis (:analysis @db/db) db/db))))))

(deftest find-last-order-by-project-analysis
  (testing "with pred that applies for both project and external analysis"
    (h/clean-db!)
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "jar:file:///some.jar!/some-file.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (let [element (#'q/find-last-order-by-project-analysis #(= 'foo.bar (:name %)) (:analysis @db/db) db/db)]
      (is (= (h/file-path "/a.clj") (:filename element)))))
  (testing "with pred that applies for both project and external analysis with multiple on project"
    (h/clean-db!)
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "jar:file:///some.jar!/some-file.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///b.clj"))
    (let [element (#'q/find-last-order-by-project-analysis #(= 'foo.bar (:name %)) (:analysis @db/db) db/db)]
      (is (= (h/file-path "/b.clj") (:filename element))))))

(deftest find-element-under-cursor
  (let [code (str "(ns a.b.c (:require [d.e.f :as |f-alias]))\n"
                  "(defn x [file|name] filename)\n"
                  "|x\n"
                  "un|known")
        [[alias-r alias-c]
         [param-r param-c]
         [x-r x-c]
         [unknown-r unknown-c]] (h/load-code-and-locs code)
        ana (:analysis @db/db)]
    (h/assert-submap
      '{:alias f-alias}
      (q/find-element-under-cursor ana (h/file-path "/a.clj") alias-r alias-c))
    (h/assert-submap
      '{:name x}
      (q/find-element-under-cursor ana (h/file-path "/a.clj") x-r x-c))
    (h/assert-submap
      '{:name filename}
      (q/find-element-under-cursor ana (h/file-path "/a.clj") param-r param-c))
    (h/assert-submap
      '{:name unknown}
      (q/find-element-under-cursor ana (h/file-path "/a.clj") unknown-r unknown-c))))

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
        ana (:analysis @db/db)]
    (h/assert-submaps
      [{:name 'x :name-row x-r :name-col x-c}
       {:name 'x :name-row x-use-r :name-col x-use-c}]
      (q/find-references-from-cursor ana (h/file-path "/a.clj") x-r x-c true db/db))
    (h/assert-submaps
      [{:name 'filename :name-row param-r :name-col param-c}
       {:name 'filename :name-row param-use-r :name-col param-use-c}]
      (q/find-references-from-cursor ana (h/file-path "/a.clj") param-r param-c true db/db))
    (h/assert-submaps
      ['{:name unknown}]
      (q/find-references-from-cursor ana (h/file-path "/a.clj") unknown-r unknown-c true db/db))
    (h/assert-submaps
      [{:alias 'f-alias :name-row alias-r :name-col alias-c}
       {:alias 'f-alias :name 'foo :name-row alias-use-r :name-col alias-use-c}]
      (q/find-references-from-cursor ana (h/file-path "/a.clj") alias-r alias-c true db/db))
    (h/assert-submaps
      [{:name "foo-kw" :name-row a-foo-kw-r :name-col a-foo-kw-c}
       {:name "foo-kw" :name-row b-foo-kw-r :name-col b-foo-kw-c}
       {:name "foo-kw" :name-row d-foo-kw-r :name-col d-foo-kw-c}
       {:name "foo-kw" :name-row c-foo-kw-r :name-col c-foo-kw-c}]
      (q/find-references-from-cursor ana (h/file-path "/c.clj") b-foo-kw-r b-foo-kw-c true db/db))))

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
        ana (:analysis @db/db)]
    (h/assert-submaps
      [{:name 'x :name-row x-r :name-col x-c}
       {:name 'x :name-row x-use-r :name-col x-use-c}]
      (q/find-references-from-cursor ana (h/file-path "/a.cljc") x-r x-c true db/db))
    (h/assert-submaps
      [{:name 'filename :name-row param-r :name-col param-c}
       {:name 'filename :name-row param-use-r :name-col param-use-c}]
      (q/find-references-from-cursor ana (h/file-path "/a.cljc") param-r param-c true db/db))
    (h/assert-submaps
      ['{:name unknown}]
      (q/find-references-from-cursor ana (h/file-path "/a.cljc") unknown-r unknown-c true db/db))
    (h/assert-submaps
      [{:alias 'f-alias :name-row alias-r :name-col alias-c}
       {:alias 'f-alias :name 'foo :name-row alias-use-r :name-col alias-use-c}]
      (q/find-references-from-cursor ana (h/file-path "/a.cljc") alias-r alias-c true db/db))))

(deftest find-references-from-defrecord
  (let [code (str "(defrecord |MyRecord [])\n"
                  "(|MyRecord)"
                  "(|->MyRecord)"
                  "(|map->MyRecord)")
        [[def-r def-c]
         [raw-r raw-c]
         [to-r to-c]
         [map-to-r map-to-c]] (h/load-code-and-locs code (h/file-uri "file:///a.clj"))
        ana (:analysis @db/db)]
    (h/assert-submaps
      [{:name 'MyRecord :bucket :var-usages :name-row raw-r :name-col raw-c}
       {:name '->MyRecord :bucket :var-usages :name-row to-r :name-col to-c}
       {:name 'map->MyRecord :bucket :var-usages :name-row map-to-r :name-col map-to-c}]
      (q/find-references-from-cursor ana (h/file-path "/a.clj") def-r def-c false db/db))))

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
        ana (:analysis @db/db)]
    (testing "from definition"
      (h/assert-submaps
        '[{:name-row 6
           :name bar
           :filename "/a.clj"
           :name-col 2
           :bucket :var-usages}
          {:name-row 4
           :name bar
           :filename "/b.clj"
           :from b
           :name-col 4
           :from-var bar}]
        (q/find-references-from-cursor ana (h/file-path "/a.clj") bar-def-r bar-def-c false db/db)))
    (testing "from usage"
      (h/assert-submaps
        '[{:name-row 6
           :name bar
           :filename "/a.clj"
           :name-col 2
           :bucket :var-usages}
          {:name-row 4
           :name bar
           :filename "/b.clj"
           :from b
           :name-col 4
           :from-var bar}]
        (q/find-references-from-cursor ana (h/file-path "/a.clj") bar-usa-r bar-usa-c false db/db)))
    (testing "from other ns"
      (h/assert-submaps
        '[{:name-row 6
           :name bar
           :filename "/a.clj"
           :name-col 2
           :bucket :var-usages}
          {:name-row 4
           :name bar
           :filename "/b.clj"
           :from b
           :name-col 4
           :from-var bar}]
        (q/find-references-from-cursor ana (h/file-path "/b.clj") bar-usa-b-r bar-usa-b-c false db/db)))))

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
        ana (:analysis @db/db)]
    (h/assert-submap
      {:name 'x :name-row x-r :name-col x-c}
      (q/find-definition-from-cursor ana (h/file-path "/a.clj") x-use-r x-use-c db/db))
    (h/assert-submap
      {:name 'filename :name-row param-r :name-col param-c}
      (q/find-definition-from-cursor ana (h/file-path "/a.clj") param-use-r param-use-c db/db))
    (is (= nil
           (q/find-definition-from-cursor ana (h/file-path "/a.clj") unknown-r unknown-c db/db)))
    (h/assert-submap
      {:name 'foo :filename (h/file-path "/b.clj") :ns 'd.e.f}
      (q/find-definition-from-cursor ana (h/file-path "/a.clj") alias-use-r alias-use-c db/db))
    (h/assert-submap
      {:alias 'f-alias :name-row alias-r :name-col alias-c}
      (q/find-definition-from-cursor ana (h/file-path "/a.clj") alias-r alias-c db/db))))

(deftest find-definition-from-cursor-when-duplicate-from-external-analysis
  (let [_ (h/load-code-and-locs (h/code "(ns foo) (def bar)") "jar:file:///some.jar!/some-jar.clj")
        _ (h/load-code-and-locs (h/code "(ns foo) (def bar)") (h/file-uri "file:///a.clj"))
        [[bar-r bar-c]] (h/load-code-and-locs (h/code "(ns baz (:require [foo :as f]))"
                                                      "|f/bar") (h/file-uri "file:///b.clj"))
        ana (:analysis @db/db)]
    (h/assert-submap
      {:name 'bar :filename (h/file-path "/a.clj")}
      (q/find-definition-from-cursor ana (h/file-path "/b.clj") bar-r bar-c db/db))))

(deftest find-definition-from-cursor-when-it-has-same-namespace-from-clj-and-cljs
  (h/load-code-and-locs (h/code "(ns foo) (def bar)") (h/file-uri "jar:file:///some.jar!/some-jar.clj"))
  (h/load-code-and-locs (h/code "(ns foo) (def bar)") (h/file-uri "jar:file:///some.jar!/other-jar.cljs"))
  (testing "when on a clj file"
    (let [[[bar-r bar-c]] (h/load-code-and-locs (h/code "(ns baz (:require [foo :as f]))"
                                                        "|f/bar") (h/file-uri "file:///b.clj"))
          ana (:analysis @db/db)]
      (h/assert-submap
        {:name 'bar :filename (h/file-path "/some.jar:some-jar.clj")}
        (q/find-definition-from-cursor ana (h/file-path "/b.clj") bar-r bar-c db/db))))
  (testing "when on a cljs file"
    (let [[[bar-r bar-c]] (h/load-code-and-locs (h/code "(ns baz (:require [foo :as f]))"
                                                        "|f/bar") (h/file-uri "file:///b.cljs"))
          ana (:analysis @db/db)]
      (h/assert-submap
        {:name 'bar :filename (h/file-path "/some.jar:other-jar.cljs")}
        (q/find-definition-from-cursor ana (h/file-path "/b.cljs") bar-r bar-c db/db))))
  (testing "when on a cljc file"
    (let [[[bar-r bar-c]] (h/load-code-and-locs (h/code "(ns baz (:require [foo :as f]))"
                                                        "|f/bar") (h/file-uri "file:///b.cljc"))
          ana (:analysis @db/db)]
      (h/assert-submap
        {:name 'bar :filename (h/file-path "/some.jar:some-jar.clj")}
        (q/find-definition-from-cursor ana (h/file-path "/b.cljc") bar-r bar-c db/db))))
  (testing "when on a cljc file with multiple langs available"
    (let [[[bar-r-clj bar-c-clj]
           [bar-r-cljs bar-c-cljs]] (h/load-code-and-locs (h/code "(ns baz #?(:clj (:require [foo :as fc]) :cljs (:require [foo :as fs])))"
                                                                  "|fc/bar"
                                                                  "|fs/bar") (h/file-uri "file:///b.cljc"))
          ana (:analysis @db/db)]
      (h/assert-submap
        {:name 'bar :filename (h/file-path "/some.jar:some-jar.clj")}
        (q/find-definition-from-cursor ana (h/file-path "/b.cljc") bar-r-clj bar-c-clj db/db))
      (h/assert-submap
        {:name 'bar :filename (h/file-path "/some.jar:other-jar.cljs")}
        (q/find-definition-from-cursor ana (h/file-path "/b.cljc") bar-r-cljs bar-c-cljs db/db)))))

(deftest find-definition-from-cursor-when-declared
  (let [[[bar-r bar-c]] (h/load-code-and-locs
                          (h/code "(ns foo)"
                                  "(declare bar)"
                                  "(|bar)"
                                  "(defn bar [] 1)") (h/file-uri "file:///a.clj"))
        ana (:analysis @db/db)]
    (h/assert-submap
      {:name 'bar :filename (h/file-path "/a.clj") :defined-by 'clojure.core/defn :row 4}
      (q/find-definition-from-cursor ana (h/file-path "/a.clj") bar-r bar-c db/db))))

(deftest find-definition-from-cursor-when-on-potemkin
  (h/load-code-and-locs (h/code "(ns foo.impl) (def bar)") (h/file-uri "file:///b.clj"))
  (let [[[bar-r bar-c]] (h/load-code-and-locs
                          (h/code "(ns foo.api"
                                  "  (:require [potemkin :refer [import-vars]]"
                                  "            [foo.impl]))"
                                  "(import-vars |impl/bar)") (h/file-uri "file:///a.clj"))
        ana (:analysis @db/db)]
    (h/assert-submap
      {:name 'bar :filename (h/file-path "/b.clj") :defined-by 'clojure.core/def :row 1 :col 15}
      (q/find-definition-from-cursor ana (h/file-path "/a.clj") bar-r bar-c db/db))))

(deftest find-unused-aliases
  (testing "clj"
    (testing "used require via alias"
      (h/load-code-and-locs "(ns a (:require [x :as f])) f/foo")
      (is (= '#{}
             (q/find-unused-aliases (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj")))))
    (testing "used require via full-ns"
      (h/load-code-and-locs "(ns a (:require [x :as f])) x/foo")
      (is (= '#{}
             (q/find-unused-aliases (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj")))))
    (testing "full-ns require"
      (h/load-code-and-locs "(ns a (:require [x] y)) foo")
      (is (= '#{}
             (q/find-unused-aliases (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj")))))
    (testing "single unused-alias"
      (h/load-code-and-locs "(ns a (:require [x :as f]))")
      (is (= '#{x}
             (q/find-unused-aliases (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj")))))
    (testing "used and unused aliases"
      (h/load-code-and-locs "(ns a (:require [x :as f] [foo] x [bar :as b] [y :refer [m]] [z :refer [o i]])) o")
      (is (= '#{y bar}
             (q/find-unused-aliases (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj"))))))
  (testing "cljc"
    (testing "used require via alias"
      (h/load-code-and-locs "(ns a (:require [x :as f])) f/foo" (h/file-uri "file:///a.cljc"))
      (is (= '#{}
             (q/find-unused-aliases (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "used require via full-ns"
      (h/load-code-and-locs "(ns a (:require [x :as f])) x/foo" (h/file-uri "file:///a.cljc"))
      (is (= '#{}
             (q/find-unused-aliases (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "full-ns require"
      (h/load-code-and-locs "(ns a (:require [x] y)) foo" (h/file-uri "file:///a.cljc"))
      (is (= '#{}
             (q/find-unused-aliases (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "single unused-alias"
      (h/load-code-and-locs "(ns a (:require [x :as f]))" (h/file-uri "file:///a.cljc"))
      (is (= '#{x}
             (q/find-unused-aliases (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "used and unused aliases"
      (h/load-code-and-locs "(ns a (:require [x :as f] [foo] x [bar :as b] [y :refer [m]] [z :refer [o i]])) o" (h/file-uri "file:///a.cljc"))
      (is (= '#{y bar}
             (q/find-unused-aliases (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "used alias in a reader conditional"
      (h/load-code-and-locs "(ns a (:require [y :as o] [x :as f])) #?(:clj f/foo)" (h/file-uri "file:///a.cljc"))
      (is (= '#{y}
             (q/find-unused-aliases (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))))

(deftest find-unused-refers
  (testing "clj"
    (testing "used require via refer"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo]])) foo")
      (is (= '#{}
             (q/find-unused-refers (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj")))))
    (testing "multiple used refers"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo bar baz]])) foo bar baz")
      (is (= '#{}
             (q/find-unused-refers (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj")))))
    (testing "single unused refer"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo]]))")
      (is (= '#{x/foo}
             (q/find-unused-refers (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj")))))
    (testing "multiple unused refer"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo bar]]))")
      (is (= '#{x/foo x/bar}
             (q/find-unused-refers (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj")))))
    (testing "multiple unused refer and used"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo bar baz]])) bar")
      (is (= '#{x/foo x/baz}
             (q/find-unused-refers (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj"))))))
  (testing "cljc"
    (testing "used require via refer"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo]])) foo" (h/file-uri "file:///a.cljc"))
      (is (= '#{}
             (q/find-unused-refers (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "multiple used refers"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo bar baz]])) foo bar baz" (h/file-uri "file:///a.cljc"))
      (is (= '#{}
             (q/find-unused-refers (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "single unused refer"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo]]))" (h/file-uri "file:///a.cljc"))
      (is (= '#{x/foo}
             (q/find-unused-refers (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "multiple unused refer"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo bar]]))" (h/file-uri "file:///a.cljc"))
      (is (= '#{x/foo x/bar}
             (q/find-unused-refers (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "multiple unused refer and used"
      (h/load-code-and-locs "(ns a (:require [x :refer [foo bar baz]])) bar" (h/file-uri "file:///a.cljc"))
      (is (= '#{x/foo x/baz}
             (q/find-unused-refers (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "used refer in a reader conditional"
      (h/load-code-and-locs "(ns a (:require [y :refer [o]] [x :refer [f]])) #?(:clj f)" (h/file-uri "file:///a.cljc"))
      (is (= '#{y/o}
             (q/find-unused-refers (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))))

(deftest find-unused-imports
  (testing "clj"
    (testing "single used full import"
      (h/load-code-and-locs "(ns a (:import java.util.Date)) Date.")
      (is (= '#{}
             (q/find-unused-imports (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj")))))
    (testing "single unused full import"
      (h/load-code-and-locs "(ns a (:import java.util.Date))")
      (is (= '#{java.util.Date}
             (q/find-unused-imports (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj")))))
    (testing "multiple unused full imports"
      (h/load-code-and-locs "(ns a (:import java.util.Date java.util.Calendar java.time.LocalDateTime))")
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj")))))
    (testing "multiple unused package imports"
      (h/load-code-and-locs "(ns a (:import [java.util Date Calendar] [java.time LocalDateTime]))")
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj")))))
    (testing "multiple unused and used imports"
      (h/load-code-and-locs "(ns a (:import [java.util Date Calendar] [java.time LocalTime LocalDateTime])) LocalTime.")
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (:analysis @db/db) (:findings @db/db) (h/file-path "/a.clj"))))))
  (testing "cljc"
    (testing "single used full import"
      (h/load-code-and-locs "(ns a (:import java.util.Date)) Date." (h/file-uri "file:///a.cljc"))
      (is (= '#{}
             (q/find-unused-imports (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "single unused full import"
      (h/load-code-and-locs "(ns a (:import java.util.Date))" (h/file-uri "file:///a.cljc"))
      (is (= '#{java.util.Date}
             (q/find-unused-imports (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "multiple unused full imports"
      (h/load-code-and-locs "(ns a (:import java.util.Date java.util.Calendar java.time.LocalDateTime))" (h/file-uri "file:///a.cljc"))
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "multiple unused package imports"
      (h/load-code-and-locs "(ns a (:import [java.util Date Calendar] [java.time LocalDateTime]))" (h/file-uri "file:///a.cljc"))
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "multiple unused and used imports"
      (h/load-code-and-locs "(ns a (:import [java.util Date Calendar] [java.time LocalTime LocalDateTime])) LocalTime." (h/file-uri "file:///a.cljc"))
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))
    (testing "used import in a reader conditional"
      (h/load-code-and-locs "(ns a (:import [java.util Date Calendar] [java.time LocalTime LocalDateTime])) #?(:clj LocalTime.)" (h/file-uri "file:///a.cljc"))
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (q/find-unused-imports (:analysis @db/db) (:findings @db/db) (h/file-path "/a.cljc")))))))

(deftest find-local-usages-under-cursor
  (testing "inside let"
    (let [[[sum-pos-r sum-pos-c]
           [sum-end-pos-r sum-end-pos-c]]
          (h/load-code-and-locs "(ns a) (let [a 2 b 1] |(+ 2 b)| (- 2 a))")]
      (h/assert-submaps
        [{:name 'b}]
        (q/find-local-usages-under-form (:analysis @db/db) (h/file-path "/a.clj") sum-pos-r sum-pos-c sum-end-pos-r sum-end-pos-c))))
  (testing "inside defn"
    (let [[[let-pos-r let-pos-c]
           [let-end-pos-r let-end-pos-c]]
          (h/load-code-and-locs "(ns a) (defn ab [b] |(let [a 1] (b a))|) (defn other [c] c)")]
      (h/assert-submaps
        [{:name 'b}]
        (q/find-local-usages-under-form (:analysis @db/db) (h/file-path "/a.clj") let-pos-r let-pos-c let-end-pos-r let-end-pos-c)))))
