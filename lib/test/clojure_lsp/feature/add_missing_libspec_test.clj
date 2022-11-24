(ns clojure-lsp.feature.add-missing-libspec-test
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(h/reset-components-before-test)

(deftest resolve-best-alias-suggestions-test
  (testing "alias not exists"
    (is (= nil
           (#'f.add-missing-libspec/find-namespace-suggestions "foo" {})))
    (is (= [{:ns "clojure.string" :alias "string"}
            {:ns "clojure.string"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "clojure.string" {"clojure.string" nil})))
    (is (= [{:ns "clojure.set" :alias "s" :count 1}]
           (#'f.add-missing-libspec/find-namespace-suggestions "clojure.set" {"clojure.set" "s"})))
    (is (= [{:ns "clojure.spec.alpha" :alias "spec"} {:ns "clojure.spec.alpha"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "spec" {"clojure.spec.alpha" nil})))
    (is (= [{:ns "clojure.spec.alpha" :alias "s" :count 1}]
           (#'f.add-missing-libspec/find-namespace-suggestions "spec" {"clojure.spec.alpha" "s"})))
    (is (= [{:ns "clojure.data.json" :alias "json"}
            {:ns "clojure.data.json"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "clojure.data.json" {"clojure.data.json" nil}))))
  (testing "alias already exists"
    (is (= [{:ns "bar" :alias "foo" :count 1}] (#'f.add-missing-libspec/find-namespace-suggestions "foo" {"bar" "foo"})))
    (is (= [{:ns "clojure.string"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "clojure.string" {"clojure.string" nil "utils.string" "string"})))
    (is (= [{:alias "data.json" :ns "clojure.data.json"}
            {:ns "clojure.data.json"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "clojure.data.json" {"clojure.data.json" nil "something.else" "json"})))
    (is (= [{:alias "data.edn", :count 3, :ns "clojure.data.edn.impl"}
            {:alias "edn.impl", :count 2, :ns "clojure.data.edn.impl"}
            {:alias "impl", :count 1, :ns "clojure.data.edn.impl"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "clojure.data.edn.impl"
                                                               [["clojure.data.edn.impl" "impl"]
                                                                ["clojure.data.edn.impl" "data.edn"]
                                                                ["clojure.data.edn.impl" "data.edn"]
                                                                ["clojure.data.edn.impl" "data.edn"]
                                                                ["clojure.data.edn.impl" "edn.impl"]
                                                                ["clojure.data.edn.impl" "edn.impl"]])))

    (is (= [{:alias "data.edn.impl", :ns "clojure.data.edn.impl"}
            {:ns "clojure.data.edn.impl"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "clojure.data.edn.impl"
                                                               [["clojure.data.edn.impl" nil]
                                                                ["other.data.edn.impl" "impl"]
                                                                ["other.data.edn.impl" "data.edn"]
                                                                ["other.data.edn.impl" "data.edn"]
                                                                ["other.data.edn.impl" "data.edn"]
                                                                ["other.data.edn.impl" "edn.impl"]
                                                                ["other.data.edn.impl" "edn.impl"]]))))
  (testing "core ns"
    (is (= [{:ns "medley.core" :alias "medley"} {:ns "medley.core"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "medley.core" {"medley.core" nil})))
    (is (= [{:ns "foo.bar.core" :alias "bar"} {:ns "foo.bar.core"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "foo.bar.core" {"foo.bar.core" nil})))
    (is (= [{:ns "foo.bar.core" :alias "foo.bar"} {:ns "foo.bar.core"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "foo.bar.core" {"foo.bar.core" nil "something.else" "bar"}))))

  (testing "when alias segments match namespaces in the order"
    (is (= [{:ns "foo.dar.zas", :alias "d.zas"}]
           (#'f.add-missing-libspec/find-namespace-suggestions
            "d.z" {"foo.bar.baz" nil "foo.dar.zas" nil})))
    (is (= [{:alias "da.zas", :ns "foo.dar.zas"}]
           (#'f.add-missing-libspec/find-namespace-suggestions
            "da.zas" {"foo.bar.baz" nil "foo.dar.zas" nil})))
    (is (= nil
           (#'f.add-missing-libspec/find-namespace-suggestions
            "dai.zas" {"foo.bar.baz" nil "foo.dar.zas" nil})))
    (is (= [{:alias "d.zas", :ns "foo.dar.zas"} {:alias "d.zsr", :ns "foo.dow.zsr"}]
           (#'f.add-missing-libspec/find-namespace-suggestions
            "d.z" {"foo.dar.zas" nil "foo.bar.baz" nil "foo.dow.zsr" nil})))
    (is (= [{:alias "zas", :ns "foo.dar.zas"}
            {:alias "zsr", :ns "foo.dow.zsr"}
            {:ns "foo.dar.zas"}
            {:ns "foo.dow.zsr"}]
           (#'f.add-missing-libspec/find-namespace-suggestions
            "f.d.z" {"foo.dar.zas" nil "baz.dar.zas" nil "zaz.dar.zas" nil "foo.bar.baz" nil "foo.dow.zsr" nil}))))
  (testing "use cases"
    (is (= nil
           (#'f.add-missing-libspec/find-namespace-suggestions "clojure.data.json" {})))
    (is (= [{:ns "taoensso.timbre" :alias "log" :count 1}]
           (#'f.add-missing-libspec/find-namespace-suggestions "log" {"taoensso.timbre" "log" "clojure.tools.logging" nil})))
    (is (= [{:ns "clojure.tools.logging" :alias "logging"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "log" {"clojure.tools.logging" nil})))
    (is (= [{:ns "clojure.data.json" :alias "json"}
            {:ns "clojure.data.json"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "clojure.data.json" {"clojure.data.json" nil})))
    (is (= [{:ns "clojure.data.json" :alias "data.json"}
            {:ns "clojure.data.json"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "clojure.data.json" {"clojure.data.json" nil "cheshire.core" "json"})))
    (is (= [{:ns "cheshire.core" :alias "cheshire"}
            {:ns "cheshire.core"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "cheshire.core" {"cheshire.core" nil})))
    (is (= [{:ns "clojure.data.json" :alias "x" :count 1}]
           (#'f.add-missing-libspec/find-namespace-suggestions "clojure.data.json" {"clojure.data.json" "x"})))
    (is (= [{:ns "clojure.data.json" :alias "x" :count 1}]
           (#'f.add-missing-libspec/find-namespace-suggestions "c.d.j" {"clojure.data.json" "x"})))
    (is (= [{:ns "clojure.data.json" :alias "x" :count 1}
            {:ns "clojure.delta.json" :alias "delta.json"}
            {:ns "clojure.delta.json"}]
           (#'f.add-missing-libspec/find-namespace-suggestions "c.d.j" {"clojure.data.json" "x"
                                                                        "clojure.delta.json" nil
                                                                        "cheshire.core" "json"}))))
  (testing "super fuzzy matching"
    (is (= (set [{:alias "log", :ns "project.tools.log"}
                 {:alias "logging", :ns "clojure.tools.logging"}
                 {:alias "logging", :ns "clojure.tools.internal.logging"}])
           (set (#'f.add-missing-libspec/find-namespace-suggestions
                 "l"
                 {"clojure.tools.logging" nil "clojure.tools.internal.logging" nil "project.tools.log" nil}))))
    (is (= [{:alias "log", :ns "project.tools.log"}
            {:alias "logging", :ns "clojure.tools.internal.logging"}
            {:alias "logging", :ns "clojure.tools.logging"}]
           (#'f.add-missing-libspec/find-namespace-suggestions
            "log"
            {"clojure.tools.logging" nil "clojure.tools.internal.logging" nil "project.tools.log" nil})))
    (is (= [{:alias "t.logging", :ns "clojure.tools.logging"}
            {:alias "t.log", :ns "project.tools.log"}]
           (#'f.add-missing-libspec/find-namespace-suggestions
            "t.l"
            {"clojure.tools.logging" nil "clojure.tools.internal.logging" nil "project.tools.log" nil})))
    (is (= [{:alias "t.log", :ns "project.tools.log"}
            {:alias "t.logging", :ns "clojure.tools.logging"}]
           (#'f.add-missing-libspec/find-namespace-suggestions
            "t.log"
            {"clojure.tools.logging" nil "clojure.tools.internal.logging" nil "project.tools.log" nil})))
    (is (= [{:alias "log", :ns "project.tools.log"}
            {:ns "project.tools.log"}]
           (#'f.add-missing-libspec/find-namespace-suggestions
            "p.t.l"
            {"clojure.tools.logging" nil "clojure.tools.internal.logging" nil "project.tools.log" nil})))
    (is (= [{:alias "logging", :ns "clojure.tools.logging"}
            {:ns "clojure.tools.logging"}]
           (#'f.add-missing-libspec/find-namespace-suggestions
            "c.t.l"
            {"clojure.tools.logging" nil "clojure.tools.internal.logging" nil "project.tools.log" nil})))))

(defn find-require-suggestions [code]
  (f.add-missing-libspec/find-require-suggestions (h/load-code-and-zloc code) "file:///a.clj" (h/db)))

(deftest find-require-suggestions-test
  (testing "Suggested namespaces"
    (h/load-code-and-locs "(ns project.some.cool.namespace)" "file:///d.clj")
    (h/load-code-and-locs "(ns other-project.some.coolio.namespace)" "file:///b.clj")
    (h/load-code-and-locs "(ns project.some.cool.namespace-test)" "file:///c.clj")
    (h/assert-submaps
      [{:ns "other-project.some.coolio.namespace" :alias "s.cool.namespace"}
       {:ns "project.some.cool.namespace" :alias "s.cool.namespace"}]
      (find-require-suggestions "|s.cool.namespace/foo")))
  (testing "Suggested alias"
    (h/load-code-and-locs "(ns project.some.cool.namespace)")
    (h/load-code-and-locs "(ns other-project.some.coolio.namespace)" "file:///b.clj")
    (h/assert-submaps
      [{:ns "project.some.cool.namespace" :alias "namespace"}
       {:ns "project.some.cool.namespace"}]
      (find-require-suggestions "|project.some.cool.namespace/foo")))
  (testing "Suggested refers"
    (h/load-code-and-locs "(ns project.some.cool.namespace) (def bla 1) (def blow 2)" "file:///c.clj")
    (h/load-code-and-locs "(ns other-project.some.coolio.namespace) (def bli)" "file:///b.clj")
    (h/assert-submaps
      [{:ns "project.some.cool.namespace"
        :refer "blow"}]
      (find-require-suggestions "|blow")))
  (testing "Suggested core"
    (h/assert-submaps
      [{:ns "clojure.set" :alias "set"}
       {:ns "clojure.set"}]
      (find-require-suggestions "|set/intersection")))
  (testing "Suggested common"
    (h/assert-submaps
      [{:ns "clojure.core.async" :alias "async"} {:ns "clojure.core.async"}]
      (find-require-suggestions "|async/go-loop")))
  (testing "Suggested common refer"
    (h/assert-submaps
      [{:ns "clojure.core.async" :refer "go-loop"}]
      (find-require-suggestions "|go-loop")))
  (testing "Invalid location"
    (h/assert-submaps
      []
      (find-require-suggestions "|;; comment"))))

(defn ^:private add-missing-libspec [code]
  (f.add-missing-libspec/add-missing-libspec (h/load-code-and-zloc code) "file:///a.clj" (h/db)))

(defn ^:private as-sexp [[{:keys [loc]} :as locs]]
  (assert (= 1 (count locs)))
  (z/sexpr loc))

(defn ^:private as-str [[{:keys [loc]} :as locs]]
  (assert (= 1 (count locs)))
  (z/string loc))

(defn ^:private as-root-str [[{:keys [loc]} :as locs]]
  (assert (= 1 (count locs)))
  (z/root-string loc))

(deftest add-missing-libspec-test
  (testing "aliases"
    (testing "known aliases in project"
      (h/reset-components!)
      (h/load-code-and-locs "(ns a (:require [foo.s :as s]))" "file:///b.clj")
      (is (= '(ns foo (:require [foo.s :as s]))
             (-> "(ns foo) |s/thing"
                 add-missing-libspec
                 as-sexp))))
    (testing "Do not add from wrong language"
      (h/reset-components!)
      (h/load-code-and-locs "(ns a (:require [foo.s :as s]))" "file:///b.cljs")
      (h/load-code-and-locs "(ns foo.s)" "file:///c.cljs")
      (is (= nil
             (-> "(ns foo) |s/thing"
                 add-missing-libspec))))
    (testing "common ns aliases"
      (h/reset-components!)
      (is (= '(ns foo (:require [clojure.set :as set]))
             (-> "(ns foo) |set/subset?"
                 add-missing-libspec
                 as-sexp))))
    (testing "Don't add an alias that already exists"
      (h/reset-components!)
      (is (= nil
             (-> "(ns foo (:require [foo.set :as set])) |set/subset?"
                 add-missing-libspec))))
    (testing "Don't add a namespace that already exists, but fix alias."
      (h/reset-components!)
      (is (= "s/subset?"
             (-> "(ns foo (:require [clojure.set :as s])) |set/subset?"
                 add-missing-libspec
                 as-str))))
    (testing "Don't add a namespace that already exists, but fix alias."
      (h/reset-components!)
      (is (= "s/subset?"
             (-> "(ns foo (:require [clojure.set :as s])) |c.s/subset?"
                 add-missing-libspec
                 as-str))))
    (testing "with ns-inner-blocks-indentation :same-line"
      (testing "we add first require without spaces"
        (swap! (h/db*) shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
        (is (= (h/code "(ns foo "
                       "  (:require [clojure.set :as set]))")
               (-> "(ns foo) |set/subset?"
                   add-missing-libspec
                   as-str))))
      (testing "next requires follow the same pattern"
        (swap! (h/db*) shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
        (is (= (h/code "(ns foo "
                       "  (:require [clojure.set :as set]"
                       "            [foo :as bar]))")
               (-> (h/code "(ns foo "
                           "  (:require [foo :as bar])) |set/subset?")
                   add-missing-libspec
                   as-str)))))
    (testing "do not clean if disbled"
      (swap! (h/db*) shared/deep-merge {:settings {:clean {:automatically-after-ns-refactor false}}})
      (is (= (h/code "(ns foo "
                     "  (:require [foo :as bar]"
                     "            [clojure.set :as set]))")
             (-> (h/code "(ns foo "
                         "  (:require [foo :as bar])) |set/subset?")
                 add-missing-libspec
                 as-str))))
    (testing "with deprecated keep-require-at-start?"
      (testing "we add first require without spaces"
        (swap! (h/db*) shared/deep-merge {:settings {:clean {:automatically-after-ns-refactor true
                                                             :ns-inner-blocks-indentation :same-line}}})
        (is (= (h/code "(ns foo "
                       "  (:require [clojure.set :as set]))")
               (-> "(ns foo) |set/subset?"
                   add-missing-libspec
                   as-str))))
      (testing "next requires follow the same pattern"
        (swap! (h/db*) shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
        (is (= (h/code "(ns foo "
                       "  (:require [clojure.set :as set]"
                       "            [foo :as bar]))")
               (-> (h/code "(ns foo "
                           "  (:require [foo :as bar])) |set/subset?")
                   add-missing-libspec
                   as-str))))))
  (testing "common refers"
    (testing "when require doesn't exists"
      (h/reset-components!)
      (is (= '(ns foo (:require [clojure.test :refer [deftest]]))
             (-> "(ns foo) |deftest"
                 add-missing-libspec
                 as-sexp))))
    (testing "when already exists another require"
      (h/reset-components!)
      (is (= '(ns foo (:require
                       [clojure.set :refer [subset?]]
                       [clojure.test :refer [deftest]]))
             (-> "(ns foo (:require [clojure.set :refer [subset?]])) |deftest subset?"
                 add-missing-libspec
                 as-sexp))))
    (testing "when already exists that ns with alias and no refers"
      (h/reset-components!)
      (is (= '(ns foo (:require [clojure.test :as t :refer [testing]]))
             (-> "(ns foo (:require [clojure.test :as t])) |testing t/deftest"
                 add-missing-libspec
                 as-sexp))))
    (testing "when already exists that ns with another refer"
      (h/reset-components!)
      (is (= '(ns foo (:require [clojure.test :refer [deftest testing]]))
             (-> "(ns foo (:require [clojure.test :refer [deftest]])) |testing deftest"
                 add-missing-libspec
                 as-sexp))))
    (testing "we don't add existing refers"
      (h/reset-components!)
      (is (nil? (add-missing-libspec "(ns foo (:require [clojure.test :refer [testing]])) |testing"))))
    (testing "we can add multiple refers"
      (h/reset-components!)
      (is (= '(ns foo (:require
                       [clojure.test :refer [deftest is testing]]))
             (-> "(ns foo (:require [clojure.test :refer [deftest testing]])) |is deftest testing"
                 add-missing-libspec
                 as-sexp)))))
  (testing "when on invalid location"
    (h/reset-components!)
    (is (nil? (-> "(ns foo) |;; comment"
                  add-missing-libspec)))))

(defn add-import-to-namespace [code import-name & [settings]]
  (h/reset-components!)
  (swap! (h/db*) shared/deep-merge {:settings (merge
                                                {:clean {:automatically-after-ns-refactor false}}
                                                settings)})

  (h/load-java-path (str (fs/canonicalize (io/file "test" "fixtures" "java_interop" "File.java"))))
  (f.add-missing-libspec/add-missing-import (h/load-code-and-zloc code) "file:///a.clj" import-name (h/db)))

(deftest add-import-to-namespace-test
  (testing "when there is no :import form"
    (is (= (h/code "(ns foo.bar "
                   "  (:import"
                   "    [java.util Date]))")
           (-> "(ns foo.bar) |Date."
               (add-import-to-namespace "java.util.Date")
               as-root-str))))
  (testing "when there is no :import form with ns-inner-blocks-indentation :same-line"
    (is (= (h/code "(ns foo.bar "
                   "  (:import [java.util Date]))")
           (-> "(ns foo.bar) |Date."
               (add-import-to-namespace "java.util.Date"  {:clean {:ns-inner-blocks-indentation :same-line}})
               as-root-str))))
  (testing "when there is no :import form with deprecated :keep-require-at-start?"
    (is (= (h/code "(ns foo.bar "
                   "  (:import [java.util Date]))")
           (-> "(ns foo.bar) |Date."
               (add-import-to-namespace "java.util.Date"  {:keep-require-at-start? true})
               as-root-str))))
  (testing "when there is a :import form already as full package import"
    (is (= (h/code "(ns foo.bar "
                   "  (:import "
                   "    [java.util Calendar Date]))")
           (-> (h/code "(ns foo.bar "
                       "  (:import "
                       "    java.util.Calendar)) |Date.")
               (add-import-to-namespace "java.util.Date")
               as-root-str))))
  (testing "when there is more than one :import form already as full package import"
    (is (= (h/code "(ns foo.bar "
                   "  (:import "
                   "    java.util.Calendar"
                   "    java.util.GregorianCalendar"
                   "    java.util.Date))")
           (-> (h/code "(ns foo.bar "
                       "  (:import "
                       "    java.util.Calendar"
                       "    java.util.GregorianCalendar)) |Date.")
               (add-import-to-namespace "java.util.Date")
               as-root-str))))
  (testing "when there is a :import form already as vector import"
    (is (= (h/code "(ns foo.bar "
                   "  (:import "
                   "    [java.util Calendar Date]))")
           (-> (h/code "(ns foo.bar "
                       "  (:import "
                       "    [java.util Calendar])) |Date.")
               (add-import-to-namespace "java.util.Date")
               as-root-str))))
  (testing "when there is a :import form already as list import"
    (is (= (h/code "(ns foo.bar "
                   "  (:import "
                   "    (java.util Calendar Date)))")
           (-> (h/code "(ns foo.bar "
                       "  (:import "
                       "    (java.util Calendar))) |Date.")
               (add-import-to-namespace "java.util.Date")
               as-root-str))))
  (testing "when there is already that :import imported"
    (is (= nil
           (-> (h/code "(ns foo.bar "
                       "  (:import "
                       "    [java.util Date])) |Date.")
               (add-import-to-namespace "java.util.Date")))))
  (testing "when there is only a :require form"
    (is (= (h/code "(ns foo.bar"
                   "  (:require"
                   "    [foo.baz :as baz]) "
                   "  (:import"
                   "    [java.util Date]))")
           (-> (h/code "(ns foo.bar"
                       "  (:require"
                       "    [foo.baz :as baz])) |Date.")
               (add-import-to-namespace "java.util.Date")
               as-root-str))))
  (testing "when there is a :require form and :import form"
    (is (= (h/code "(ns foo.bar"
                   "  (:require"
                   "    [foo.baz :as baz])"
                   "  (:import"
                   "    [java.util Calendar Date]))")
           (-> (h/code "(ns foo.bar"
                       "  (:require"
                       "    [foo.baz :as baz])"
                       "  (:import"
                       "    java.util.Calendar)) |Date.")
               (add-import-to-namespace "java.util.Date")
               as-root-str))))
  (testing "when on an invalid location"
    (is (= (h/code "(ns foo.bar "
                   "  (:import"
                   "    [java.util Date])) ;; comment")
           (-> (h/code "(ns foo.bar) |;; comment")
               (add-import-to-namespace "java.util.Date")
               (h/changes->code (h/db)))))))

(deftest add-known-import-to-namespace-test
  (testing "when we known the import"
    (is (= (h/code "(ns foo.bar "
                   "  (:import"
                   "    [java.io File]))")
           (-> "(ns foo.bar) |File."
               (add-import-to-namespace nil)
               as-root-str))))
  (testing "when we don't known the import"
    (is (nil? (add-import-to-namespace "(ns foo.bar) |MyClass." nil))))
  (testing "when on invalid location"
    (is (nil? (add-import-to-namespace "(ns foo.bar) |;; comment" nil)))))

(defn add-require-suggestion [code chosen-ns chosen-alias chosen-refer]
  (f.add-missing-libspec/add-require-suggestion (h/zloc-from-code code) "file:///a.clj" chosen-ns chosen-alias chosen-refer (h/db)))

(deftest add-require-suggestion-test
  (h/load-code-and-locs (h/code "(ns clojure.string) (defn split [])" "file:///clojure/string.clj"))
  (testing "alias"
    (testing "on empty ns"
      (is (= (h/code "(ns foo.bar "
                     "  (:require"
                     "   [clojure.string :as str]))")
             (-> (h/code "(ns foo.bar)"
                         "|str/a")
                 (add-require-suggestion "clojure.string" "str" nil)
                 as-root-str))))
    (testing "changing alias"
      (let [[ns-edit form-edit] (-> (h/code "(ns foo.bar)"
                                            "|clojure.string/a")
                                    ;; The code actions will suggest clojure.string or string but it's possible
                                    ;; to have a custom alias if invoked directly.
                                    (add-require-suggestion "clojure.string" "my-str" nil))]
        (is (= (h/code "(ns foo.bar "
                       "  (:require"
                       "   [clojure.string :as my-str]))")
               (z/root-string (:loc ns-edit))))
        ;; If we are aliasing to something other than the full namespace, we change
        ;; uses of the namespace to the alias.
        (is (= (h/code "my-str/a")
               (z/string (:loc form-edit))))))
    (testing "on non empty ns"
      (is (= (h/code "(ns foo.bar"
                     "  (:require"
                     "   [clojure.java.io :as io]"
                     "   [clojure.string :as str]))")
             (-> (h/code "(ns foo.bar"
                         "  (:require"
                         "   [clojure.java.io :as io]))"
                         "|str/a")
                 (add-require-suggestion "clojure.string" "str" nil)
                 as-root-str))))
    (testing "on invalid location"
      (is (nil? (-> (h/code "(ns foo.bar)"
                            "|;; comment")
                    (add-require-suggestion "clojure.string" "str" nil))))))
  (testing "refer"
    (testing "on empty ns"
      (is (= (h/code "(ns foo.bar "
                     "  (:require"
                     "   [clojure.string :refer [split]]))")
             (-> (h/code "(ns foo.bar)"
                         "|split")
                 (add-require-suggestion "clojure.string" nil "split")
                 as-root-str))))
    (testing "on non empty ns"
      (is (= (h/code "(ns foo.bar"
                     "  (:require"
                     "   [clojure.java.io :as io]"
                     "   [clojure.string :refer [split]]))")
             (-> (h/code "(ns foo.bar"
                         "  (:require"
                         "   [clojure.java.io :as io]))"
                         "|split")
                 (add-require-suggestion "clojure.string" nil "split")
                 as-root-str))))
    (testing "on existing ns with alias"
      (is (= (h/code "(ns foo.bar"
                     "  (:require"
                     "   [clojure.string :as str :refer [split]]))")
             (-> (h/code "(ns foo.bar"
                         "  (:require"
                         "   [clojure.string :as str]))"
                         "|split")
                 (add-require-suggestion "clojure.string" nil "split")
                 as-root-str))))
    (testing "on existing ns with refers"
      (is (= (h/code "(ns foo.bar"
                     "  (:require"
                     "   [clojure.string :refer [join split]]))")
             (-> (h/code "(ns foo.bar"
                         "  (:require"
                         "   [clojure.string :refer [join]]))"
                         "|split")
                 (add-require-suggestion "clojure.string" nil "split")
                 as-root-str))))
    (testing "on invalid location"
      (is (nil? (-> (h/code "(ns foo.bar)"
                            "|;; comment")
                    (add-require-suggestion "clojure.string" nil "split")))))))

(defn- find-missing-imports [code]
  (f.add-missing-libspec/find-missing-imports (h/zloc-from-code code) (h/db)))

(deftest find-missing-import-test
  (h/load-java-path (str (fs/canonicalize (io/file "test" "fixtures" "java_interop" "File.java"))))
  (testing "when usage is a java class"
    (is (= '["java.io.File"] (find-missing-imports "(ns a) |File"))))
  (testing "when usage is a java constructor"
    (is (= '["java.io.File"] (find-missing-imports "(ns a) (|File.)"))))
  (testing "when usage is a java ns"
    (is (= '["java.io.File"] (find-missing-imports "(ns a) (|File/of \"foo\")"))))
  (testing "when usage is invalid"
    (is (nil? (find-missing-imports "(ns a) |;; comment")))))
