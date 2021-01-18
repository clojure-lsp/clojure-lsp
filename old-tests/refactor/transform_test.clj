(ns clojure-lsp.refactor.transform-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.references :as f.references]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.refactor.edit :as edit]
    [clojure-lsp.refactor.transform :as transform]
    [clojure.string :as string]
    [clojure.test :refer :all]
    [rewrite-clj.zip :as z]
    [clojure-lsp.test-helper :as h]
    [clojure-lsp.queries :as q]
    [clojure-lsp.shared :as shared]))

(defn code [& strings] (string/join "\n" strings))

(defn test-clean-ns
  ([db input-code expected-code]
   (test-clean-ns db input-code expected-code true))
  ([db input-code expected-code in-form]
   (with-redefs [slurp (constantly input-code)]
     (reset! db/db db)
     (let [zloc (when in-form
                  (-> (z/of-string input-code) z/down z/right z/right))
           [{:keys [loc range]}] (transform/clean-ns zloc "file://a.clj")]
       (is (some? range))
       (is (= expected-code
              (z/root-string loc)))))))

(deftest clean-ns-test
  (testing "without keep-require-at-start?"
    (test-clean-ns {:settings {:keep-require-at-start? false}}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] [bar :as b] baz [z] ))"
                         "(s/defn func []"
                         "  (f/some))")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f]"
                         "   [z]"
                         "   baz))"
                         "(s/defn func []"
                         "  (f/some))")))
  (testing "with keep-require-at-start?"
    (test-clean-ns {:settings {:keep-require-at-start? true}}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] [bar :as b] baz [z] ))"
                         "(s/defn func []"
                         "  (f/some))")
                   (code "(ns foo.bar"
                         " (:require [foo  :as f]"
                         "           [z]"
                         "           baz))"
                         "(s/defn func []"
                         "  (f/some))")))
  (testing "with first require as unused"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] [bar :as b] baz [z] ))"
                         "(defn func []"
                         "  (b/some))")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :as b]"
                         "   [z]"
                         "   baz))"
                         "(defn func []"
                         "  (b/some))")))
  (testing "with single unused require on ns"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] ))"
                         "(defn func []"
                         "  (b/some))")
                   (code "(ns foo.bar)"
                         "(defn func []"
                         "  (b/some))")))
  (testing "with single used require on ns"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] ))"
                         "(defn func []"
                         "  (f/some))")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f]))"
                         "(defn func []"
                         "  (f/some))")))
  (testing "with multiple unused requires on ns"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f]"
                         "   [bar :as b]))")
                   (code "(ns foo.bar)")))
  #_(testing "with refer at require"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] [bar :refer [some]] baz [z] ))"
                         "(defn func []"
                         "  (f/some))")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f]"
                         "   [z]"
                         "   baz))"
                         "(defn func []"
                         "  (f/some))")))
  #_(testing "with refer as single require"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :refer [some]]))")
                   (code "(ns foo.bar)")))
  #_(testing "in any form"
    (let [to-clean (code "(ns foo.bar"
                         " (:require"
                         "   [foo  :as f] [bar :refer [some]] baz [z] ))"
                         ""
                         "(defn func []"
                         "  (f/some))")]
      (test-clean-ns {:documents {"file://a.clj" {:text to-clean}}}
                     to-clean
                     (code "(ns foo.bar"
                           " (:require"
                           "   [foo  :as f]"
                           "   [z]"
                           "   baz))"
                           ""
                           "(defn func []"
                           "  (f/some))")
                     false)))
  #_(testing "with first require as a refer"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :refer [some] ] [foo :as f]))"
                         ""
                         "(defn func []"
                         "  (some))")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :refer [some] ]))"
                         ""
                         "(defn func []"
                         "  (some))")))
  #_(testing "with first require as a refer with alias"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :as b :refer [some] ] [foo :as f]))"
                         ""
                         "(defn func []"
                         "  b/some"
                         "  (some))")
                   (code "(ns foo.bar"
                         " (:require"
                         "   [bar :as b :refer [some] ]))"
                         ""
                         "(defn func []"
                         "  b/some"
                         "  (some))")))
  #_(testing "unused refer from multiple refers"
      (test-clean-ns {}
                     (code "(ns foo.bar"
                           " (:require"
                           "   [bar :refer [some other] ]))"
                           "(some)")
                     (code "(ns foo.bar"
                           " (:require"
                           "   [bar :refer [some] ]))"
                           "(some)")))
  #_(testing "unused middle refer from multiple refers"
      (test-clean-ns {}
                     (code "(ns foo.bar"
                           " (:require"
                           "   [bar :refer [some other baz another] ]))"
                           "(some)"
                           "(another)"
                           "(baz)")
                     (code "(ns foo.bar"
                           " (:require"
                           "   [bar :refer [another baz some] ]))"
                           "(some)"
                           "(another)"
                           "(baz)")))
  #_(testing "unused refer and alias"
      (test-clean-ns {}
                     (code "(ns foo.bar"
                           " (:require"
                           "   [bar :refer [some] ]"
                           "   [baz :as b]))")
                     (code "(ns foo.bar)")))
  #_(testing "single unused full package import"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import"
                         "  java.util.Date))")
                   (code "(ns foo.bar)")))
  #_(testing "single unused package import"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Date]))")
                   (code "(ns foo.bar)")))
  #_(testing "unused full package imports"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import "
                         "  java.util.Date java.util.Calendar java.util.List))"
                         "Calendar.")
                   (code "(ns foo.bar"
                         " (:import"
                         "  java.util.Calendar))"
                         "Calendar.")))
  #_(testing "unused package imports"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import "
                         "  [java.util Date Calendar List Map]))"
                         "Calendar."
                         "Map.")
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Calendar Map]))"
                         "Calendar."
                         "Map.")))
  #_(testing "unused package imports with keep-at-start?"
    (test-clean-ns {:settings {:keep-require-at-start? true}}
                   (code "(ns foo.bar"
                         " (:import [java.util Date Calendar List Map]))"
                         "Calendar."
                         "Map.")
                   (code "(ns foo.bar"
                         " (:import [java.util Calendar Map]))"
                         "Calendar."
                         "Map.")))
  #_(testing "unused package imports with single import"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Date List]"
                         "  java.util.Calendar))"
                         "Calendar."
                         "List.")
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util List]"
                         "  java.util.Calendar))"
                         "Calendar."
                         "List.")))
  #_(testing "unused package imports spacing"
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Date"
                         "             Calendar"
                         "             List]))"
                         "Date."
                         "List.")
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Date"
                         "             List]))"
                         "Date."
                         "List."))
    (test-clean-ns {}
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Date"
                         "             List]))"
                         "Date."
                         "List.")
                   (code "(ns foo.bar"
                         " (:import"
                         "  [java.util Date"
                         "             List]))"
                         "Date."
                         "List."))))

(deftest extract-function-test
  (let [code "(defn a [b] (let [c 1] (b c)))"
        usages (parser/find-usages code :clj {})
        zloc (z/find-value (z/of-string code) z/next 'let)
        results (transform/extract-function
                  zloc
                  "foo"
                  (parser/usages-in-form zloc usages))]
    (is (= (z/string (:loc (first results))) "(defn foo [b]\n  (let [c 1] (b c)))"))
    (is (= (z/string (:loc (last results))) "(foo b)"))))

(deftest inline-symbol
  (testing "simple let"
    (h/load-code-and-locs "(let [something 1] something something)")
    (let [results (transform/inline-symbol "file:///a.clj" 1 7)
          a-results (get results "file:///a.clj")]
      (is (map? results))
      (is (= 1 (count results)))
      (is (= 3 (count a-results)))
      (is (= [nil "1" "1"] (map (comp z/string :loc) a-results)))))
  #_(testing "def in another file"
      (let [a-code "(ns a) (def something (1 * 60))"
            b-code "(ns b (:require a)) (inc a/something)"
            a-usages (parser/find-usages a-code :clj {})
            b-usages (parser/find-usages b-code :clj {})]
        (reset! db/db {:documents {"file:///a.clj" {:text a-code}
                                   "file:///b.clj" {:text b-code}}
                       :file-envs {"file:///a.clj" a-usages
                                   "file:///b.clj" b-usages}})
        (let [zloc (z/find-value (z/of-string b-code) z/next 'a/something)
              pos (meta (z/node zloc))
              definition (f.definition/definition-usage "file:///b.clj" (:row pos) (:col pos))
              references (f.references/reference-usages "file:///b.clj" (:row pos) (:col pos))
              results (transform/inline-symbol definition references)
              a-results (get results "file:///a.clj")
              b-results (get results "file:///b.clj")]
          (is (map? results))
          (is (= 2 (count results)))
          (is (= [nil] (map (comp z/string :loc) a-results)))
          (is (= ["(1 * 60)"] (map (comp z/string :loc) b-results)))))))
