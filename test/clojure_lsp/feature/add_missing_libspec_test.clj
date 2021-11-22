(ns clojure-lsp.feature.add-missing-libspec-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(h/reset-db-after-test)

(deftest resolve-best-alias-suggestions
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

(deftest resolve-best-namespaces-suggestions
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

(deftest find-require-suggestions
  (testing "Suggested namespaces"
    (h/load-code-and-locs "(ns project.some.cool.namespace)")
    (h/load-code-and-locs "(ns other-project.some.coolio.namespace)" "file:///b.clj")
    (h/load-code-and-locs "(ns project.some.cool.namespace-test)" "file:///c.clj")
    (h/assert-submaps
      [{:ns "project.some.cool.namespace"
        :alias "s.cool.namespace"}
       {:ns "other-project.some.coolio.namespace"
        :alias "s.cool.namespace"}]
      (f.add-missing-libspec/find-require-suggestions (z/of-string "s.cool.namespace/foo") [] db/db)))
  (testing "Suggested alias"
    (h/load-code-and-locs "(ns project.some.cool.namespace)")
    (h/load-code-and-locs "(ns other-project.some.coolio.namespace)" "file:///b.clj")
    (h/assert-submaps
      [{:ns "project.some.cool.namespace"
        :alias "namespace"}]
      (f.add-missing-libspec/find-require-suggestions (z/of-string "project.some.cool.namespace/foo") [] db/db)))
  (testing "Suggested refers"
    (h/load-code-and-locs "(ns project.some.cool.namespace) (def bla 1) (def blow 2)")
    (h/load-code-and-locs "(ns other-project.some.coolio.namespace) (def bli)" "file:///b.clj")
    (h/assert-submaps
      [{:ns "project.some.cool.namespace"
        :refer "blow"}]
      (f.add-missing-libspec/find-require-suggestions (z/of-string "blow") [] db/db))))

(deftest add-missing-libspec
  (testing "aliases"
    (testing "known aliases in project"
      (h/load-code-and-locs "(ns a (:require [foo.s :as s]))")
      (let [zloc (-> (z/of-string "(ns foo) s/thing") z/rightmost)
            [{:keys [loc range]}] (f.add-missing-libspec/add-missing-libspec zloc db/db)]
        (is (some? range))
        (is (= '(ns foo (:require [foo.s :as s])) (z/sexpr loc)))))
    (testing "common ns aliases"
      (h/clean-db!)
      (let [zloc (-> (z/of-string "(ns foo) set/subset?") z/rightmost)
            [{:keys [loc range]}] (f.add-missing-libspec/add-missing-libspec zloc db/db)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.set :as set])) (z/sexpr loc)))))
    (testing "with ns-inner-blocks-indentation :same-line"
      (testing "we add first require without spaces"
        (swap! db/db shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
        (let [zloc (-> (z/of-string "(ns foo) set/subset?") z/rightmost)
              [{:keys [loc range]}] (f.add-missing-libspec/add-missing-libspec zloc db/db)]
          (is (some? range))
          (is (= (h/code "(ns foo "
                         "  (:require [clojure.set :as set]))") (z/string loc)))))
      (testing "next requires follow the same pattern"
        (swap! db/db shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
        (let [zloc (-> (z/of-string "(ns foo \n  (:require [foo :as bar])) set/subset?") z/rightmost)
              [{:keys [loc range]}] (f.add-missing-libspec/add-missing-libspec zloc db/db)]
          (is (some? range))
          (is (= (h/code "(ns foo "
                         "  (:require [foo :as bar]"
                         "            [clojure.set :as set]))") (z/string loc))))))
    (testing "with deprecated keep-require-at-start?"
      (testing "we add first require without spaces"
        (swap! db/db shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
        (let [zloc (-> (z/of-string "(ns foo) set/subset?") z/rightmost)
              [{:keys [loc range]}] (f.add-missing-libspec/add-missing-libspec zloc db/db)]
          (is (some? range))
          (is (= (h/code "(ns foo "
                         "  (:require [clojure.set :as set]))") (z/string loc)))))
      (testing "next requires follow the same pattern"
        (swap! db/db shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
        (let [zloc (-> (z/of-string "(ns foo \n  (:require [foo :as bar])) set/subset?") z/rightmost)
              [{:keys [loc range]}] (f.add-missing-libspec/add-missing-libspec zloc db/db)]
          (is (some? range))
          (is (= (h/code "(ns foo "
                         "  (:require [foo :as bar]"
                         "            [clojure.set :as set]))") (z/string loc)))))))
  (testing "common refers"
    (testing "when require doesn't exists"
      (h/clean-db!)
      (let [zloc (-> (z/of-string "(ns foo) deftest") z/rightmost)
            [{:keys [loc range]}] (f.add-missing-libspec/add-missing-libspec zloc db/db)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.test :refer [deftest]])) (z/sexpr loc)))))
    (testing "when already exists another require"
      (h/clean-db!)
      (let [zloc (-> (z/of-string "(ns foo (:require [clojure.set :refer [subset?]])) deftest") z/rightmost)
            [{:keys [loc range]}] (f.add-missing-libspec/add-missing-libspec zloc db/db)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.set :refer [subset?]]
                                  [clojure.test :refer [deftest]])) (z/sexpr loc)))))
    (testing "when already exists that ns with alias and no refers"
      (h/clean-db!)
      (let [zloc (-> (z/of-string "(ns foo (:require [clojure.test :as t])) testing") z/rightmost)
            [{:keys [loc range]}] (f.add-missing-libspec/add-missing-libspec zloc db/db)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.test :as t :refer [testing]])) (z/sexpr loc)))))
    (testing "when already exists that ns with another refer"
      (h/clean-db!)
      (let [zloc (-> (z/of-string "(ns foo (:require [clojure.test :refer [deftest]])) testing") z/rightmost)
            [{:keys [loc range]}] (f.add-missing-libspec/add-missing-libspec zloc db/db)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.test :refer [deftest testing]])) (z/sexpr loc)))))
    (testing "we don't add existing refers"
      (h/clean-db!)
      (let [zloc (-> (z/of-string "(ns foo (:require [clojure.test :refer [testing]])) testing") z/rightmost)]
        (is (= nil (f.add-missing-libspec/add-missing-libspec zloc db/db)))))
    (testing "we can add multiple refers"
      (h/clean-db!)
      (let [zloc (-> (z/of-string "(ns foo (:require [clojure.test :refer [deftest testing]])) is") z/rightmost)
            [{:keys [loc range]}] (f.add-missing-libspec/add-missing-libspec zloc db/db)]
        (is (some? range))
        (is (= '(ns foo (:require [clojure.test :refer [deftest testing is]])) (z/sexpr loc)))))))

(deftest add-import-to-namespace-test
  (testing "when there is no :import form"
    (h/clean-db!)
    (let [zloc (-> (z/of-string "(ns foo.bar) Date.") (z/find-value z/next 'Date.))
          [{:keys [loc range]}] (f.add-missing-libspec/add-import-to-namespace zloc "java.util.Date" db/db)]
      (is (some? range))
      (is (= (h/code "(ns foo.bar "
                     "  (:import"
                     "    java.util.Date))") (z/root-string loc)))))
  (testing "when there is no :import form with ns-inner-blocks-indentation :same-line"
    (swap! db/db shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
    (let [zloc (-> (z/of-string "(ns foo.bar) Date.") (z/find-value z/next 'Date.))
          [{:keys [loc range]}] (f.add-missing-libspec/add-import-to-namespace zloc "java.util.Date" db/db)]
      (is (some? range))
      (is (= (h/code "(ns foo.bar "
                     "  (:import java.util.Date))") (z/root-string loc)))))
  (testing "when there is no :import form with deprecated :keep-require-at-start?"
    (swap! db/db shared/deep-merge {:settings {:clean {:ns-inner-blocks-indentation :same-line}}})
    (let [zloc (-> (z/of-string "(ns foo.bar) Date.") (z/find-value z/next 'Date.))
          [{:keys [loc range]}] (f.add-missing-libspec/add-import-to-namespace zloc "java.util.Date" db/db)]
      (is (some? range))
      (is (= (h/code "(ns foo.bar "
                     "  (:import java.util.Date))") (z/root-string loc)))))
  (testing "when there is a :import form already"
    (h/clean-db!)
    (let [zloc (-> (z/of-string (h/code "(ns foo.bar "
                                        "  (:import "
                                        "    java.util.Calendar)) Date.")) (z/find-value z/next 'Date.))
          [{:keys [loc range]}] (f.add-missing-libspec/add-import-to-namespace zloc "java.util.Date" db/db)]
      (is (some? range))
      (is (= (h/code "(ns foo.bar "
                     "  (:import "
                     "    java.util.Calendar"
                     "    java.util.Date))") (z/root-string loc)))))
  (testing "when there is already that :import imported"
    (h/clean-db!)
    (let [zloc (-> (z/of-string (h/code "(ns foo.bar "
                                        "  (:import "
                                        "    java.util.Date)) Date.")) (z/find-value z/next 'Date.))]
      (is (= nil
             (f.add-missing-libspec/add-import-to-namespace zloc "java.util.Date" db/db)))))
  (testing "when there is only a :require form"
    (h/clean-db!)
    (let [zloc (-> (z/of-string (h/code "(ns foo.bar"
                                        "  (:require"
                                        "    [foo.baz :as baz])) Date.")) (z/find-value z/next 'Date.))
          [{:keys [loc range]}] (f.add-missing-libspec/add-import-to-namespace zloc "java.util.Date" db/db)]
      (is (some? range))
      (is (= (h/code "(ns foo.bar"
                     "  (:require"
                     "    [foo.baz :as baz]) "
                     "  (:import"
                     "    java.util.Date))") (z/root-string loc)))))
  (testing "when there is a :require form and :import form"
    (h/clean-db!)
    (let [zloc (-> (z/of-string (h/code "(ns foo.bar"
                                        "  (:require"
                                        "    [foo.baz :as baz])"
                                        "  (:import"
                                        "    java.util.Calendar)) Date.")) (z/find-value z/next 'Date.))
          [{:keys [loc range]}] (f.add-missing-libspec/add-import-to-namespace zloc "java.util.Date" db/db)]
      (is (some? range))
      (is (= (h/code "(ns foo.bar"
                     "  (:require"
                     "    [foo.baz :as baz])"
                     "  (:import"
                     "    java.util.Calendar"
                     "    java.util.Date))") (z/root-string loc))))))

(deftest add-common-import-to-namespace-test
  (testing "when we known the import"
    (let [zloc (-> (z/of-string "(ns foo.bar) Date.") (z/find-value z/next 'Date.))
          [{:keys [loc range]}] (f.add-missing-libspec/add-common-import-to-namespace zloc db/db)]
      (is (some? range))
      (is (= (h/code "(ns foo.bar "
                     "  (:import"
                     "    java.util.Date))") (z/root-string loc)))))
  (testing "when we don't known the import"
    (let [zloc (-> (z/of-string "(ns foo.bar) MyClass.") (z/find-value z/next 'MyClass.))]
      (is (= nil (f.add-missing-libspec/add-common-import-to-namespace zloc db/db))))))

(deftest add-require-suggestion-test
  (h/load-code-and-locs (h/code "(ns clojure.string) (defn split [])" "file:///clojure/string.clj"))
  (testing "alias"
    (testing "on empty ns"
      (let [zloc (-> (z/of-string "(ns foo.bar)\nstr/a") (z/find-value z/next 'str/a))
            [{:keys [loc range]}] (f.add-missing-libspec/add-require-suggestion zloc "clojure.string" "str" nil db/db)]
        (is (some? range))
        (is (= (h/code "(ns foo.bar "
                       "  (:require"
                       "    [clojure.string :as str]))")
               (z/root-string loc)))))
    (testing "on non empty ns"
      (let [zloc (-> (z/of-string "(ns foo.bar\n  (:require\n   [clojure.java.io :as io]))\nstr/a") (z/find-value z/next 'str/a))
            [{:keys [loc range]}] (f.add-missing-libspec/add-require-suggestion zloc "clojure.string" "str" nil db/db)]
        (is (some? range))
        (is (= (h/code "(ns foo.bar"
                       "  (:require"
                       "   [clojure.java.io :as io]"
                       "   [clojure.string :as str]))")
               (z/root-string loc))))))
  (testing "refer"
    (testing "on empty ns"
      (let [zloc (-> (z/of-string "(ns foo.bar)\nsplit") (z/find-value z/next 'split))
            [{:keys [loc range]}] (f.add-missing-libspec/add-require-suggestion zloc "clojure.string" nil "split" db/db)]
        (is (some? range))
        (is (= (h/code "(ns foo.bar "
                       "  (:require"
                       "    [clojure.string :refer [split]]))")
               (z/root-string loc)))))
    (testing "on non empty ns"
      (let [zloc (-> (z/of-string "(ns foo.bar\n  (:require\n   [clojure.java.io :as io]))\nsplit") (z/find-value z/next 'split))
            [{:keys [loc range]}] (f.add-missing-libspec/add-require-suggestion zloc "clojure.string" nil "split" db/db)]
        (is (some? range))
        (is (= (h/code "(ns foo.bar"
                       "  (:require"
                       "   [clojure.java.io :as io]"
                       "   [clojure.string :refer [split]]))")
               (z/root-string loc)))))
    (testing "on existing ns with alias"
      (let [zloc (-> (z/of-string "(ns foo.bar\n  (:require\n   [clojure.string :as str]))\nsplit") (z/find-value z/next 'split))
            [{:keys [loc range]}] (f.add-missing-libspec/add-require-suggestion zloc "clojure.string" nil "split" db/db)]
        (is (some? range))
        (is (= (h/code "(ns foo.bar"
                       "  (:require"
                       "   [clojure.string :as str :refer [split]]))")
               (z/root-string loc)))))
    (testing "on existing ns with refers"
      (let [zloc (-> (z/of-string "(ns foo.bar\n  (:require\n   [clojure.string :refer [join]]))\nsplit") (z/find-value z/next 'split))
            [{:keys [loc range]}] (f.add-missing-libspec/add-require-suggestion zloc "clojure.string" nil "split" db/db)]
        (is (some? range))
        (is (= (h/code "(ns foo.bar"
                       "  (:require"
                       "   [clojure.string :refer [join split]]))")
               (z/root-string loc)))))))

(deftest find-missing-import
  (testing "when usage is a java constructor"
    (let [zloc (-> (z/of-string "(ns a) Date.") z/rightmost)
          full-package (f.add-missing-libspec/find-missing-import zloc)]
      (is (= 'java.util.Date full-package))))
  (testing "when usage is a java constructor"
    (let [zloc (-> (z/of-string "(ns a) Date/parse") z/rightmost)
          full-package (f.add-missing-libspec/find-missing-import zloc)]
      (is (= 'java.util.Date full-package)))))
