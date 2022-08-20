(ns clojure-lsp.feature.add-missing-libspec-test
  (:require
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

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

(defn find-require-suggestions [code components]
  (f.add-missing-libspec/find-require-suggestions (h/zloc-from-code code)
                                                  h/default-uri
                                                  (h/db components)))

(deftest find-require-suggestions-test
  (testing "Suggested namespaces"
    (let [components (h/make-components)]
      (h/load-code "(ns project.some.cool.namespace)" "file:///d.clj" components)
      (h/load-code "(ns other-project.some.coolio.namespace)" "file:///b.clj" components)
      (h/load-code "(ns project.some.cool.namespace-test)" "file:///c.clj" components)
      (h/assert-submaps
        [{:ns "other-project.some.coolio.namespace" :alias "s.cool.namespace"}
         {:ns "project.some.cool.namespace" :alias "s.cool.namespace"}]
        (find-require-suggestions "|s.cool.namespace/foo" components))))
  (testing "Suggested alias"
    (let [components (h/make-components)]
      (h/load-code "(ns project.some.cool.namespace)" "file:///c.clj" components)
      (h/load-code "(ns other-project.some.coolio.namespace)" "file:///b.clj" components)
      (h/assert-submaps
        [{:ns "project.some.cool.namespace" :alias "namespace"}
         {:ns "project.some.cool.namespace"}]
        (find-require-suggestions "|project.some.cool.namespace/foo" components))))
  (testing "Suggested refers"
    (let [components (h/make-components)]
      (h/load-code "(ns project.some.cool.namespace) (def bla 1) (def blow 2)" "file:///c.clj" components)
      (h/load-code "(ns other-project.some.coolio.namespace) (def bli)" "file:///b.clj" components)
      (h/assert-submaps
        [{:ns "project.some.cool.namespace"
          :refer "blow"}]
        (find-require-suggestions "|blow" components))))
  (testing "Suggested core"
    (h/assert-submaps
      [{:ns "clojure.set" :alias "set"}
       {:ns "clojure.set"}]
      (find-require-suggestions "|set/intersection" (h/make-components))))
  (testing "Suggested common"
    (h/assert-submaps
      [{:ns "clojure.core.async" :alias "async"} {:ns "clojure.core.async"}]
      (find-require-suggestions "|async/go-loop" (h/make-components))))
  (testing "Suggested common refer"
    (h/assert-submaps
      [{:ns "clojure.core.async" :refer "go-loop"}]
      (find-require-suggestions "|go-loop" (h/make-components))))
  (testing "Invalid location"
    (h/assert-submaps
      []
      (find-require-suggestions "|;; comment" (h/make-components)))))

(defn ^:private add-missing-libspec [code components]
  (f.add-missing-libspec/add-missing-libspec (h/load-code-and-zloc code h/default-uri components)
                                             h/default-uri
                                             (h/db components)))

(defn ^:private as-sexp [[{:keys [loc]} :as locs]]
  (is (= 1 (count locs)))
  (z/sexpr loc))

(defn ^:private as-str [[{:keys [loc]} :as locs]]
  (is (= 1 (count locs)))
  (z/string loc))

(defn ^:private as-root-str [[{:keys [loc]} :as locs]]
  (is (= 1 (count locs)))
  (z/root-string loc))

(deftest add-missing-libspec-test
  (testing "aliases"
    (testing "known aliases in project"
      (let [components (h/make-components)]
        (h/load-code "(ns a (:require [foo.s :as s]))" "file:///b.clj" components)
        (is (= '(ns foo (:require [foo.s :as s]))
               (-> "(ns foo) |s/thing"
                   (add-missing-libspec components)
                   as-sexp)))))
    (testing "Do not add from wrong language"
      (let [components (h/make-components)]
        (h/load-code "(ns a (:require [foo.s :as s]))" "file:///b.cljs" components)
        (h/load-code "(ns foo.s)" "file:///c.cljs" components)
        (is (= nil
               (-> "(ns foo) |s/thing"
                   (add-missing-libspec components))))))
    (testing "common ns aliases"
      (let [components (h/make-components)]
        (is (= '(ns foo (:require [clojure.set :as set]))
               (-> "(ns foo) |set/subset?"
                   (add-missing-libspec components)
                   as-sexp)))))
    (testing "Don't add an alias that already exists"
      (let [components (h/make-components)]
        (is (= nil
               (-> "(ns foo (:require [foo.set :as set])) |set/subset?"
                   (add-missing-libspec components))))))
    (testing "Don't add a namespace that already exists, but fix alias."
      (let [components (h/make-components)]
        (is (= "s/subset?"
               (-> "(ns foo (:require [clojure.set :as s])) |set/subset?"
                   (add-missing-libspec components)
                   as-str)))))
    (testing "Don't add a namespace that already exists, but fix alias."
      (let [components (h/make-components)]
        (is (= "s/subset?"
               (-> "(ns foo (:require [clojure.set :as s])) |c.s/subset?"
                   (add-missing-libspec components)
                   as-str)))))
    (testing "with ns-inner-blocks-indentation :same-line"
      (testing "we add first require without spaces"
        (let [components (h/make-components {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})]
          (is (= (h/code "(ns foo "
                         "  (:require [clojure.set :as set]))")
                 (-> "(ns foo) |set/subset?"
                     (add-missing-libspec components)
                     as-str)))))
      (testing "next requires follow the same pattern"
        (let [components (h/make-components {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})]
          (is (= (h/code "(ns foo "
                         "  (:require [clojure.set :as set]"
                         "            [foo :as bar]))")
                 (-> (h/code "(ns foo "
                             "  (:require [foo :as bar])) |set/subset?")
                     (add-missing-libspec components)
                     as-str))))))
    (testing "do not clean if disbled"
      (let [components (h/make-components {:settings {:clean {:automatically-after-ns-refactor false}}})]
        (is (= (h/code "(ns foo "
                       "  (:require [foo :as bar]"
                       "            [clojure.set :as set]))")
               (-> (h/code "(ns foo "
                           "  (:require [foo :as bar])) |set/subset?")
                   (add-missing-libspec components)
                   as-str)))))
    (testing "with deprecated keep-require-at-start?"
      (testing "we add first require without spaces"
        (let [components (h/make-components {:settings {:clean {:automatically-after-ns-refactor true
                                                                :ns-inner-blocks-indentation :same-line}}})]
          (is (= (h/code "(ns foo "
                         "  (:require [clojure.set :as set]))")
                 (-> "(ns foo) |set/subset?"
                     (add-missing-libspec components)
                     as-str)))))
      (testing "next requires follow the same pattern"
        (let [components (h/make-components {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})]
          (is (= (h/code "(ns foo "
                         "  (:require [clojure.set :as set]"
                         "            [foo :as bar]))")
                 (-> (h/code "(ns foo "
                             "  (:require [foo :as bar])) |set/subset?")
                     (add-missing-libspec components)
                     as-str)))))))
  (testing "common refers"
    (testing "when require doesn't exists"
      (let [components (h/make-components)]
        (is (= '(ns foo (:require [clojure.test :refer [deftest]]))
               (-> "(ns foo) |deftest"
                   (add-missing-libspec components)
                   as-sexp)))))
    (testing "when already exists another require"
      (let [components (h/make-components)]
        (is (= '(ns foo (:require
                         [clojure.set :refer [subset?]]
                         [clojure.test :refer [deftest]]))
               (-> "(ns foo (:require [clojure.set :refer [subset?]])) |deftest subset?"
                   (add-missing-libspec components)
                   as-sexp)))))
    (testing "when already exists that ns with alias and no refers"
      (let [components (h/make-components)]
        (is (= '(ns foo (:require [clojure.test :as t :refer [testing]]))
               (-> "(ns foo (:require [clojure.test :as t])) |testing t/deftest"
                   (add-missing-libspec components)
                   as-sexp)))))
    (testing "when already exists that ns with another refer"
      (let [components (h/make-components)]
        (is (= '(ns foo (:require [clojure.test :refer [deftest testing]]))
               (-> "(ns foo (:require [clojure.test :refer [deftest]])) |testing deftest"
                   (add-missing-libspec components)
                   as-sexp)))))
    (testing "we don't add existing refers"
      (let [components (h/make-components)]
        (is (nil? (-> "(ns foo (:require [clojure.test :refer [testing]])) |testing"
                      (add-missing-libspec components))))))
    (testing "we can add multiple refers"
      (let [components (h/make-components)]
        (is (= '(ns foo (:require
                         [clojure.test :refer [deftest is testing]]))
               (-> "(ns foo (:require [clojure.test :refer [deftest testing]])) |is deftest testing"
                   (add-missing-libspec components)
                   as-sexp))))))
  (testing "when on invalid location"
    (let [components (h/make-components)]
      (is (nil? (-> "(ns foo) |;; comment"
                    (add-missing-libspec components)))))))

(defn make-import-components
  ([] (make-import-components {}))
  ([settings]
   (h/make-components {:settings (merge
                                   {:clean {:automatically-after-ns-refactor false}}
                                   settings)})))

(defn add-import-to-namespace
  ([code import-name] (add-import-to-namespace code import-name (make-import-components)))
  ([code import-name components]
   (let [zloc (h/load-code-and-zloc code h/default-uri components)]
    (f.add-missing-libspec/add-missing-import zloc h/default-uri import-name (h/db components)))))

(deftest add-import-to-namespace-test
  (testing "when there is no :import form"
    (is (= (h/code "(ns foo.bar "
                   "  (:import"
                   "    java.util.Date))")
           (-> "(ns foo.bar) |Date."
               (add-import-to-namespace "java.util.Date")
               as-root-str))))
  (testing "when there is no :import form with ns-inner-blocks-indentation :same-line"
    (is (= (h/code "(ns foo.bar "
                   "  (:import java.util.Date))")
           (-> "(ns foo.bar) |Date."
               (add-import-to-namespace "java.util.Date" (make-import-components {:clean {:ns-inner-blocks-indentation :same-line}}))
               as-root-str))))
  (testing "when there is no :import form with deprecated :keep-require-at-start?"
    (is (= (h/code "(ns foo.bar "
                   "  (:import java.util.Date))")
           (-> "(ns foo.bar) |Date."
               (add-import-to-namespace "java.util.Date" (make-import-components {:keep-require-at-start? true}))
               as-root-str))))
  (testing "when there is a :import form already"
    (is (= (h/code "(ns foo.bar "
                   "  (:import "
                   "    java.util.Calendar"
                   "    java.util.Date))")
           (-> (h/code "(ns foo.bar "
                       "  (:import "
                       "    java.util.Calendar)) |Date.")
               (add-import-to-namespace "java.util.Date")
               as-root-str))))
  (testing "when there is already that :import imported"
    (is (= nil
           (-> (h/code "(ns foo.bar "
                       "  (:import "
                       "    java.util.Date)) |Date.")
               (add-import-to-namespace "java.util.Date")))))
  (testing "when there is only a :require form"
    (is (= (h/code "(ns foo.bar"
                   "  (:require"
                   "    [foo.baz :as baz]) "
                   "  (:import"
                   "    java.util.Date))")
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
                   "    java.util.Calendar"
                   "    java.util.Date))")
           (-> (h/code "(ns foo.bar"
                       "  (:require"
                       "    [foo.baz :as baz])"
                       "  (:import"
                       "    java.util.Calendar)) |Date.")
               (add-import-to-namespace "java.util.Date")
               as-root-str))))
  (testing "when on an invalid location"
    (let [components (make-import-components)]
      (is (= (h/code "(ns foo.bar "
                     "  (:import"
                     "    java.util.Date)) ;; comment")
             (-> (h/code "(ns foo.bar) |;; comment")
                 (add-import-to-namespace "java.util.Date" components)
                 (h/changes->code (h/db components))))))))

(deftest add-common-import-to-namespace-test
  (testing "when we known the import"
    (is (= (h/code "(ns foo.bar "
                   "  (:import"
                   "    java.util.Date))")
           (-> "(ns foo.bar) |Date."
               (add-import-to-namespace nil)
               as-root-str))))
  (testing "when we don't known the import"
    (is (nil? (add-import-to-namespace "(ns foo.bar) |MyClass." nil))))
  (testing "when on invalid location"
    (is (nil? (add-import-to-namespace "(ns foo.bar) |;; comment" nil)))))

(defn add-require-suggestion [code components chosen-ns chosen-alias chosen-refer]
  (f.add-missing-libspec/add-require-suggestion (h/zloc-from-code code) h/default-uri chosen-ns chosen-alias chosen-refer (h/db components)))

(deftest add-require-suggestion-test
  (let [components (h/make-components)]
    (h/load-code (h/code "(ns clojure.string) (defn split [])") "file:///clojure/string.clj" components)
    (testing "alias"
      (testing "on empty ns"
        (is (= (h/code "(ns foo.bar "
                       "  (:require"
                       "   [clojure.string :as str]))")
               (-> (h/code "(ns foo.bar)"
                           "|str/a")
                   (add-require-suggestion components "clojure.string" "str" nil)
                   as-root-str))))
      (testing "changing alias"
        (let [[ns-edit form-edit] (-> (h/code "(ns foo.bar)"
                                              "|clojure.string/a")
                                    ;; The code actions will suggest clojure.string or string but it's possible
                                    ;; to have a custom alias if invoked directly.
                                      (add-require-suggestion components "clojure.string" "my-str" nil))]
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
                   (add-require-suggestion components "clojure.string" "str" nil)
                   as-root-str))))
      (testing "on invalid location"
        (is (nil? (-> (h/code "(ns foo.bar)"
                              "|;; comment")
                      (add-require-suggestion components "clojure.string" "str" nil))))))
    (testing "refer"
      (testing "on empty ns"
        (is (= (h/code "(ns foo.bar "
                       "  (:require"
                       "   [clojure.string :refer [split]]))")
               (-> (h/code "(ns foo.bar)"
                           "|split")
                   (add-require-suggestion components "clojure.string" nil "split")
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
                   (add-require-suggestion components "clojure.string" nil "split")
                   as-root-str))))
      (testing "on existing ns with alias"
        (is (= (h/code "(ns foo.bar"
                       "  (:require"
                       "   [clojure.string :as str :refer [split]]))")
               (-> (h/code "(ns foo.bar"
                           "  (:require"
                           "   [clojure.string :as str]))"
                           "|split")
                   (add-require-suggestion components "clojure.string" nil "split")
                   as-root-str))))
      (testing "on existing ns with refers"
        (is (= (h/code "(ns foo.bar"
                       "  (:require"
                       "   [clojure.string :refer [join split]]))")
               (-> (h/code "(ns foo.bar"
                           "  (:require"
                           "   [clojure.string :refer [join]]))"
                           "|split")
                   (add-require-suggestion components "clojure.string" nil "split")
                   as-root-str))))
      (testing "on invalid location"
        (is (nil? (-> (h/code "(ns foo.bar)"
                              "|;; comment")
                      (add-require-suggestion components "clojure.string" nil "split"))))))))

(defn- find-missing-import [code]
  (f.add-missing-libspec/find-missing-import (h/zloc-from-code code)))

(deftest find-missing-import-test
  (testing "when usage is a java constructor"
    (is (= 'java.util.Date (find-missing-import "(ns a) |Date."))))
  (testing "when usage is a java ns"
    (is (= 'java.util.Date (find-missing-import "(ns a) |Date/parse"))))
  (testing "when usage is invalid"
    (is (nil? (find-missing-import "(ns a) |;; comment")))))
