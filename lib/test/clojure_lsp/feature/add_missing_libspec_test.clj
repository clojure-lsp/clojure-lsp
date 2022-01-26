(ns clojure-lsp.feature.add-missing-libspec-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(h/reset-db-after-test)

(deftest resolve-best-alias-suggestions-test
  (testing "alias not exists"
    (is (= #{"foo"} (#'f.add-missing-libspec/resolve-best-alias-suggestions "foo" '#{bar})))
    (is (= #{"string"} (#'f.add-missing-libspec/resolve-best-alias-suggestions "clojure.string" '#{foo bar})))
    (is (= #{"json"} (#'f.add-missing-libspec/resolve-best-alias-suggestions "clojure.data.json" '#{foo bar}))))
  (testing "alias already exists"
    (is (= #{"foo"} (#'f.add-missing-libspec/resolve-best-alias-suggestions "foo" '#{foo bar})))
    (is (= #{"string" "clojure.string"} (#'f.add-missing-libspec/resolve-best-alias-suggestions "clojure.string" '#{foo bar string})))
    (is (= #{"json" "data.json"} (#'f.add-missing-libspec/resolve-best-alias-suggestions "clojure.data.json" '#{foo bar json})))
    (is (= #{"impl" "edn.impl"} (#'f.add-missing-libspec/resolve-best-alias-suggestions "clojure.data.edn.impl" '#{foo bar json impl edn.impl}))))
  (testing "core ns"
    (is (= #{"medley"} (#'f.add-missing-libspec/resolve-best-alias-suggestions "medley.core" '#{})))
    (is (= #{"bar"} (#'f.add-missing-libspec/resolve-best-alias-suggestions "foo.bar.core" '#{})))
    (is (= #{"bar"} (#'f.add-missing-libspec/resolve-best-alias-suggestions "foo.bar.core" '#{bar})))))

(deftest resolve-best-namespaces-suggestions-test
  (testing "when alias segments match namespaces in the order"
    (is (= #{"foo.dar.zas"} (#'f.add-missing-libspec/resolve-best-namespaces-suggestions
                             "d.z" '#{foo.bar.baz
                                      foo.dar.zas})))
    (is (= #{"foo.dar.zas"} (#'f.add-missing-libspec/resolve-best-namespaces-suggestions
                             "da.zas" '#{foo.bar.baz
                                         foo.dar.zas})))
    (is (= #{} (#'f.add-missing-libspec/resolve-best-namespaces-suggestions
                "dai.zas" '#{foo.bar.baz
                             foo.dar.zas})))
    (is (= #{"foo.dar.zas"
             "foo.dow.zsr"} (#'f.add-missing-libspec/resolve-best-namespaces-suggestions
                             "d.z" '#{foo.dar.zas
                                      foo.bar.baz
                                      foo.dow.zsr})))
    (is (= #{"foo.dar.zas"
             "foo.dow.zsr"} (#'f.add-missing-libspec/resolve-best-namespaces-suggestions
                             "f.d.z" '#{foo.dar.zas
                                        baz.dar.zas
                                        zaz.dar.zas
                                        foo.bar.baz
                                        foo.dow.zsr})))
    (is (= #{"foo.bar"} (#'f.add-missing-libspec/resolve-best-namespaces-suggestions
                         "foo.bar" '#{foo.bar.zas
                                      foo.bar
                                      foo.bar-test})))))

(defn find-require-suggestions [code]
  (f.add-missing-libspec/find-require-suggestions (h/zloc-from-code code) [] db/db))

(deftest find-require-suggestions-test
  (testing "Suggested namespaces"
    (h/load-code-and-locs "(ns project.some.cool.namespace)")
    (h/load-code-and-locs "(ns other-project.some.coolio.namespace)" "file:///b.clj")
    (h/load-code-and-locs "(ns project.some.cool.namespace-test)" "file:///c.clj")
    (h/assert-submaps
      [{:ns "project.some.cool.namespace"
        :alias "s.cool.namespace"}
       {:ns "other-project.some.coolio.namespace"
        :alias "s.cool.namespace"}]
      (find-require-suggestions "|s.cool.namespace/foo")))
  (testing "Suggested alias"
    (h/load-code-and-locs "(ns project.some.cool.namespace)")
    (h/load-code-and-locs "(ns other-project.some.coolio.namespace)" "file:///b.clj")
    (h/assert-submaps
      [{:ns "project.some.cool.namespace"
        :alias "namespace"}]
      (find-require-suggestions "|project.some.cool.namespace/foo")))
  (testing "Suggested refers"
    (h/load-code-and-locs "(ns project.some.cool.namespace) (def bla 1) (def blow 2)")
    (h/load-code-and-locs "(ns other-project.some.coolio.namespace) (def bli)" "file:///b.clj")
    (h/assert-submaps
      [{:ns "project.some.cool.namespace"
        :refer "blow"}]
      (find-require-suggestions "|blow")))
  (testing "Invalid location"
    (h/assert-submaps
      []
      (find-require-suggestions "|;; comment"))))

(defn ^:private add-missing-libspec [code]
  (f.add-missing-libspec/add-missing-libspec (h/zloc-from-code code) db/db))

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
      (h/load-code-and-locs "(ns a (:require [foo.s :as s]))")
      (is (= '(ns foo (:require [foo.s :as s]))
             (-> "(ns foo) |s/thing"
                 add-missing-libspec
                 as-sexp))))
    (testing "common ns aliases"
      (h/clean-db!)
      (is (= '(ns foo (:require [clojure.set :as set]))
             (-> "(ns foo) |set/subset?"
                 add-missing-libspec
                 as-sexp))))
    (testing "with ns-inner-blocks-indentation :same-line"
      (testing "we add first require without spaces"
        (swap! db/db shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
        (is (= (h/code "(ns foo "
                       "  (:require [clojure.set :as set]))")
               (-> "(ns foo) |set/subset?"
                   add-missing-libspec
                   as-str))))
      (testing "next requires follow the same pattern"
        (swap! db/db shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
        (is (= (h/code "(ns foo "
                       "  (:require [foo :as bar]"
                       "            [clojure.set :as set]))")
               (-> (h/code "(ns foo "
                           "  (:require [foo :as bar])) |set/subset?")
                   add-missing-libspec
                   as-str)))))
    (testing "with deprecated keep-require-at-start?"
      (testing "we add first require without spaces"
        (swap! db/db shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
        (is (= (h/code "(ns foo "
                       "  (:require [clojure.set :as set]))")
               (-> "(ns foo) |set/subset?"
                   add-missing-libspec
                   as-str))))
      (testing "next requires follow the same pattern"
        (swap! db/db shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
        (is (= (h/code "(ns foo "
                       "  (:require [foo :as bar]"
                       "            [clojure.set :as set]))")
               (-> (h/code "(ns foo "
                           "  (:require [foo :as bar])) |set/subset?")
                   add-missing-libspec
                   as-str))))))
  (testing "common refers"
    (testing "when require doesn't exists"
      (h/clean-db!)
      (is (= '(ns foo (:require [clojure.test :refer [deftest]]))
             (-> "(ns foo) |deftest"
                 add-missing-libspec
                 as-sexp))))
    (testing "when already exists another require"
      (h/clean-db!)
      (is (= '(ns foo (:require [clojure.set :refer [subset?]]
                                [clojure.test :refer [deftest]]))
             (-> "(ns foo (:require [clojure.set :refer [subset?]])) |deftest"
                 add-missing-libspec
                 as-sexp))))
    (testing "when already exists that ns with alias and no refers"
      (h/clean-db!)
      (is (= '(ns foo (:require [clojure.test :as t :refer [testing]]))
             (-> "(ns foo (:require [clojure.test :as t])) |testing"
                 add-missing-libspec
                 as-sexp))))
    (testing "when already exists that ns with another refer"
      (h/clean-db!)
      (is (= '(ns foo (:require [clojure.test :refer [deftest testing]]))
             (-> "(ns foo (:require [clojure.test :refer [deftest]])) |testing"
                 add-missing-libspec
                 as-sexp))))
    (testing "we don't add existing refers"
      (h/clean-db!)
      (is (nil? (add-missing-libspec "(ns foo (:require [clojure.test :refer [testing]])) |testing"))))
    (testing "we can add multiple refers"
      (h/clean-db!)
      (is (= '(ns foo (:require [clojure.test :refer [deftest testing is]]))
             (-> "(ns foo (:require [clojure.test :refer [deftest testing]])) |is"
                 add-missing-libspec
                 as-sexp)))))
  (testing "when on invalid location"
    (h/clean-db!)
    (is (nil? (-> "(ns foo) |;; comment"
                  add-missing-libspec)))))

(defn add-import-to-namespace [code import-name]
  (f.add-missing-libspec/add-import-to-namespace (h/zloc-from-code code) import-name db/db))

(deftest add-import-to-namespace-test
  (testing "when there is no :import form"
    (h/clean-db!)
    (is (= (h/code "(ns foo.bar "
                   "  (:import"
                   "    java.util.Date))")
           (-> "(ns foo.bar) |Date."
               (add-import-to-namespace "java.util.Date")
               as-root-str))))
  (testing "when there is no :import form with ns-inner-blocks-indentation :same-line"
    (swap! db/db shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
    (is (= (h/code "(ns foo.bar "
                   "  (:import java.util.Date))")
           (-> "(ns foo.bar) |Date."
               (add-import-to-namespace "java.util.Date")
               as-root-str))))
  (testing "when there is no :import form with deprecated :keep-require-at-start?"
    (swap! db/db shared/deep-merge {:settings {:keep-require-at-start? true}})
    (is (= (h/code "(ns foo.bar "
                   "  (:import java.util.Date))")
           (-> "(ns foo.bar) |Date."
               (add-import-to-namespace "java.util.Date")
               as-root-str))))
  (testing "when there is a :import form already"
    (h/clean-db!)
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
    (h/clean-db!)
    (is (= nil
           (-> (h/code "(ns foo.bar "
                       "  (:import "
                       "    java.util.Date)) |Date.")
               (add-import-to-namespace "java.util.Date")))))
  (testing "when there is only a :require form"
    (h/clean-db!)
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
    (h/clean-db!)
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
    (h/clean-db!)
    (is (nil? (-> (h/code "(ns foo.bar) |;; comment")
                  (add-import-to-namespace "java.util.Date"))))))

(defn add-common-import-to-namespace [code]
  (f.add-missing-libspec/add-common-import-to-namespace (h/zloc-from-code code) db/db))

(deftest add-common-import-to-namespace-test
  (testing "when we known the import"
    (is (= (h/code "(ns foo.bar "
                   "  (:import"
                   "    java.util.Date))")
           (-> "(ns foo.bar) |Date."
               add-common-import-to-namespace
               as-root-str))))
  (testing "when we don't known the import"
    (is (nil? (add-common-import-to-namespace "(ns foo.bar) |MyClass."))))
  (testing "when on invalid location"
    (is (nil? (add-common-import-to-namespace "(ns foo.bar) |;; comment")))))

(defn add-require-suggestion [code chosen-ns chosen-alias chosen-refer]
  (f.add-missing-libspec/add-require-suggestion (h/zloc-from-code code) chosen-ns chosen-alias chosen-refer db/db))

(deftest add-require-suggestion-test
  (h/load-code-and-locs (h/code "(ns clojure.string) (defn split [])" "file:///clojure/string.clj"))
  (testing "alias"
    (testing "on empty ns"
      (is (= (h/code "(ns foo.bar "
                     "  (:require"
                     "    [clojure.string :as str]))")
             (-> (h/code "(ns foo.bar)"
                         "|str/a")
                 (add-require-suggestion "clojure.string" "str" nil)
                 as-root-str))))
    ;; TODO: the UI doesn't offer a way to provide a custom alias, so this test
    ;; is a bit silly.
    (testing "changing alias"
      (let [[ns-edit form-edit] (-> (h/code "(ns foo.bar)"
                                            "|clojure.string/a")
                                    (add-require-suggestion "clojure.string" "my-str" nil))]
        (is (= (h/code "(ns foo.bar "
                       "  (:require"
                       "    [clojure.string :as my-str]))")
               (z/root-string (:loc ns-edit))))
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
                     "    [clojure.string :refer [split]]))")
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

(defn- find-missing-import [code]
  (f.add-missing-libspec/find-missing-import (h/zloc-from-code code)))

(deftest find-missing-import-test
  (testing "when usage is a java constructor"
    (is (= 'java.util.Date (find-missing-import "(ns a) |Date."))))
  (testing "when usage is a java ns"
    (is (= 'java.util.Date (find-missing-import "(ns a) |Date/parse"))))
  (testing "when usage is invalid"
    (is (nil? (find-missing-import "(ns a) |;; comment")))))
