(ns clojure-lsp.queries-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.queries :as q]
    [clojure-lsp.test-helper :as h]
    [clojure.test :refer [deftest is testing]]
    [taoensso.timbre :as log]))

(h/reset-db-after-test)

(deftest project-analysis
  (testing "when dependency-scheme is zip"
    (reset! db/db {})
    (h/load-code-and-locs "(ns foo.bar)" "file:///a.clj")
    (h/load-code-and-locs "(ns foo.bar)" "file:///b.clj")
    (h/load-code-and-locs "(ns foo.bar)" "file:///some.jar:some-file.clj")
    (is (= 2 (count (q/filter-project-analysis (:analysis @db/db))))))
  (testing "when dependency-scheme is jar"
    (swap! db/db merge {:settings {:dependency-scheme "jar"}})
    (h/load-code-and-locs "(ns foo.bar)" "file:///a.clj")
    (h/load-code-and-locs "(ns foo.bar)" "file:///b.clj")
    (h/load-code-and-locs "(ns foo.bar)" "file:///some.jar:some-file.clj")
    (is (= 2 (count (q/filter-project-analysis (:analysis @db/db)))))))

(deftest external-analysis
  (testing "when dependency-scheme is zip"
    (reset! db/db {})
    (h/load-code-and-locs "(ns foo.bar)" "file:///a.clj")
    (h/load-code-and-locs "(ns foo.bar)" "file:///b.clj")
    (h/load-code-and-locs "(ns foo.bar)" "file:///some.jar:some-file.clj")
    (is (= 1 (count (q/filter-external-analysis (:analysis @db/db))))))
  (testing "when dependency-scheme is jar"
    (swap! db/db merge {:settings {:dependency-scheme "jar"}})
    (h/load-code-and-locs "(ns foo.bar)" "file:///a.clj")
    (h/load-code-and-locs "(ns foo.bar)" "file:///b.clj")
    (h/load-code-and-locs "(ns foo.bar)" "file:///some.jar:some-file.clj")
    (is (= 1 (count (q/filter-external-analysis (:analysis @db/db)))))))

(deftest find-first-order-by-project-analysis
  (testing "with pred that applies for both project and external analysis"
    (reset! db/db {})
    (h/load-code-and-locs "(ns foo.bar)" "file:///some.jar:some-file.clj")
    (h/load-code-and-locs "(ns foo.bar)" "file:///a.clj")
    (let [element (#'q/find-first-order-by-project-analysis #(= 'foo.bar (:name %)) (:analysis @db/db))]
      (is (= "/a.clj" (:filename element)))))
  (testing "with pred that applies for both project and external analysis with multiple on project"
    (reset! db/db {})
    (h/load-code-and-locs "(ns foo.bar)" "file:///some.jar:some-file.clj")
    (h/load-code-and-locs "(ns foo.bar)" "file:///a.clj")
    (h/load-code-and-locs "(ns foo.bar)" "file:///b.clj")
    (let [element (#'q/find-first-order-by-project-analysis #(= 'foo.bar (:name %)) (:analysis @db/db))]
      (is (= "/a.clj" (:filename element))))))

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
      (q/find-element-under-cursor ana "/a.clj" alias-r alias-c))
    (h/assert-submap
      '{:name x}
      (q/find-element-under-cursor ana "/a.clj" x-r x-c))
    (h/assert-submap
      '{:name filename}
      (q/find-element-under-cursor ana "/a.clj" param-r param-c))
    (h/assert-submap
      '{:name unknown}
      (q/find-element-under-cursor ana "/a.clj" unknown-r unknown-c))))

(deftest find-references-from-cursor
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
        ana (:analysis @db/db)]
    (h/assert-submaps
      [{:name 'x :name-row x-r :name-col x-c}
       {:name 'x :name-row x-use-r :name-col x-use-c}]
      (q/find-references-from-cursor ana "/a.clj" x-r x-c true))
    (h/assert-submaps
      [{:name 'filename :name-row param-r :name-col param-c}
       {:name 'filename :name-row param-use-r :name-col param-use-c}]
      (q/find-references-from-cursor ana "/a.clj" param-r param-c true))
    (h/assert-submaps
      ['{:name unknown}]
      (q/find-references-from-cursor ana "/a.clj" unknown-r unknown-c true))
    (h/assert-submaps
      [{:alias 'f-alias :name-row alias-r :name-col alias-c}
       {:alias 'f-alias :name 'foo :name-row alias-use-r :name-col alias-use-c}]
      (q/find-references-from-cursor ana "/a.clj" alias-r alias-c true))))

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
         [unknown-r unknown-c]] (h/load-code-and-locs code "file:///a.cljc")
        ana (:analysis @db/db)]
    (h/assert-submaps
      [{:name 'x :name-row x-r :name-col x-c}
       {:name 'x :name-row x-use-r :name-col x-use-c}]
      (q/find-references-from-cursor ana "/a.cljc" x-r x-c true))
    (h/assert-submaps
      [{:name 'filename :name-row param-r :name-col param-c}
       {:name 'filename :name-row param-use-r :name-col param-use-c}]
      (q/find-references-from-cursor ana "/a.cljc" param-r param-c true))
    (h/assert-submaps
      ['{:name unknown}]
      (q/find-references-from-cursor ana "/a.cljc" unknown-r unknown-c true))
    (h/assert-submaps
      [{:alias 'f-alias :name-row alias-r :name-col alias-c}
       {:alias 'f-alias :name 'foo :name-row alias-use-r :name-col alias-use-c}]
      (q/find-references-from-cursor ana "/a.cljc" alias-r alias-c true))))

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
        _ (h/load-code-and-locs "(ns d.e.f) (def foo 1)" "file:///b.clj")
        ana (:analysis @db/db)]
    (h/assert-submap
      {:name 'x :name-row x-r :name-col x-c}
      (q/find-definition-from-cursor ana "/a.clj" x-use-r x-use-c))
    (h/assert-submap
      {:name 'filename :name-row param-r :name-col param-c}
      (q/find-definition-from-cursor ana "/a.clj" param-use-r param-use-c))
    (is (= nil
           (q/find-definition-from-cursor ana "/a.clj" unknown-r unknown-c)))
    (h/assert-submap
      {:name 'foo :filename "/b.clj" :ns 'd.e.f}
      (q/find-definition-from-cursor ana "/a.clj" alias-use-r alias-use-c))
    (h/assert-submap
      {:alias 'f-alias :name-row alias-r :name-col alias-c}
      (q/find-definition-from-cursor ana "/a.clj" alias-r alias-c))))

(deftest find-definition-from-cursor-when-duplicate-from-external-analysis
  (let [_ (h/load-code-and-locs (h/code "(ns foo) (def bar)") "file:///some.jar:some-jar.clj")
        _ (h/load-code-and-locs (h/code "(ns foo) (def bar)") "file:///a.clj")
        [[bar-r bar-c]] (h/load-code-and-locs (h/code "(ns baz (:require [foo :as f]))"
                                                      "|f/bar") "file:///b.clj")
        ana (:analysis @db/db)]
    (h/assert-submap
     {:name 'bar :filename "/a.clj"}
     (q/find-definition-from-cursor ana "/b.clj" bar-r bar-c))))

(deftest find-unused-aliases
  (testing "used require via alias"
    (h/load-code-and-locs "(ns a (:require [x :as f])) f/foo")
    (is (= '#{}
           (q/find-unused-aliases (:findings @db/db) "/a.clj"))))
  (testing "used require via full-ns"
    (h/load-code-and-locs "(ns a (:require [x :as f])) x/foo")
    (is (= '#{}
           (q/find-unused-aliases (:findings @db/db) "/a.clj"))))
  (testing "full-ns require"
    (h/load-code-and-locs "(ns a (:require [x] y)) foo")
    (is (= '#{}
           (q/find-unused-aliases (:findings @db/db) "/a.clj"))))
  (testing "single unused-alias"
    (h/load-code-and-locs "(ns a (:require [x :as f]))")
    (is (= '#{x}
           (q/find-unused-aliases (:findings @db/db) "/a.clj"))))
  (testing "used and unused aliases"
    (h/load-code-and-locs "(ns a (:require [x :as f] [foo] x [bar :as b] [y :refer [m]] [z :refer [o i]])) o")
    (is (= '#{y bar}
           (q/find-unused-aliases (:findings @db/db) "/a.clj")))))

(deftest find-unused-refers
  (testing "used require via refer"
    (h/load-code-and-locs "(ns a (:require [x :refer [foo]])) foo")
    (is (= '#{}
           (q/find-unused-refers (:findings @db/db) "/a.clj"))))
  (testing "multiple used refers"
    (h/load-code-and-locs "(ns a (:require [x :refer [foo bar baz]])) foo bar baz")
    (is (= '#{}
           (q/find-unused-refers (:findings @db/db) "/a.clj"))))
  (testing "single unused refer"
    (h/load-code-and-locs "(ns a (:require [x :refer [foo]]))")
    (is (= '#{x/foo}
           (q/find-unused-refers (:findings @db/db) "/a.clj"))))
  (testing "multiple unused refer"
    (h/load-code-and-locs "(ns a (:require [x :refer [foo bar]]))")
    (is (= '#{x/foo x/bar}
           (q/find-unused-refers (:findings @db/db) "/a.clj"))))
  (testing "multiple unused refer and used"
    (h/load-code-and-locs "(ns a (:require [x :refer [foo bar baz]])) bar")
    (is (= '#{x/foo x/baz}
           (q/find-unused-refers (:findings @db/db) "/a.clj")))))

(deftest find-unused-imports
  (testing "single used full import"
    (h/load-code-and-locs "(ns a (:import java.util.Date)) Date.")
    (is (= '#{}
           (q/find-unused-imports (:findings @db/db) "/a.clj"))))
  (testing "single unused full import"
    (h/load-code-and-locs "(ns a (:import java.util.Date))")
    (is (= '#{java.util.Date}
           (q/find-unused-imports (:findings @db/db) "/a.clj"))))
  (testing "multiple unused full imports"
    (h/load-code-and-locs "(ns a (:import java.util.Date java.util.Calendar java.time.LocalDateTime))")
    (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
           (q/find-unused-imports (:findings @db/db) "/a.clj"))))
  (testing "multiple unused package imports"
    (h/load-code-and-locs "(ns a (:import [java.util Date Calendar] [java.time LocalDateTime]))")
    (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
           (q/find-unused-imports (:findings @db/db) "/a.clj"))))
  (testing "multiple unused and used imports"
    (h/load-code-and-locs "(ns a (:import [java.util Date Calendar] [java.time LocalTime LocalDateTime])) LocalTime.")
    (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
           (q/find-unused-imports (:findings @db/db) "/a.clj")))))

(deftest find-local-usages-under-cursor
  (testing "inside let"
    (let [[[sum-pos-r sum-pos-c]
           [sum-end-pos-r sum-end-pos-c]]
          (h/load-code-and-locs "(ns a) (let [a 2 b 1] |(+ 2 b)| (- 2 a))")]
      (h/assert-submaps
        [{:name 'b}]
        (q/find-local-usages-under-form (:analysis @db/db) "/a.clj" sum-pos-r sum-pos-c sum-end-pos-r sum-end-pos-c))))
  (testing "inside defn"
    (let [[[let-pos-r let-pos-c]
           [let-end-pos-r let-end-pos-c]]
          (h/load-code-and-locs "(ns a) (defn ab [b] |(let [a 1] (b a))|) (defn other [c] c)")]
      (h/assert-submaps
        [{:name 'b}]
        (q/find-local-usages-under-form (:analysis @db/db) "/a.clj" let-pos-r let-pos-c let-end-pos-r let-end-pos-c)))))
