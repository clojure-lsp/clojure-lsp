(ns clojure-lsp.features.code-actions-test
  (:require
   [clojure-lsp.feature.code-actions :as f.code-actions]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(defn zloc-of [uri]
  (parser/safe-zloc-of-file (h/db) uri))

(deftest add-alias-suggestion-code-actions
  (h/load-code-and-locs "(ns clojure.set)" (h/file-uri "file:///clojure.core.clj"))
  (h/load-code-and-locs "(ns medley.core)" (h/file-uri "file:///medley.core.clj"))
  (h/load-code-and-locs "(ns clojure.data.json)" (h/file-uri "file:///clojure.data.json.clj"))
  (h/load-code-and-locs "(ns some (:require [chesire :as json]))" (h/file-uri "file:///some.clj"))
  (h/load-code-and-locs (h/code "(ns some)"
                                "(clojure.set/union #{} #{})"
                                "(medley.core/foo 1 2)"
                                "(clojure.data.json/bar 1 2)"))
  (testing "simple ns"
    (h/assert-contains-submaps
      [{:title "Add require '[clojure.set :as set]'"
        :command {:command "add-require-suggestion"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                          (h/file-uri "file:///a.clj")
                          2
                          4
                          [{:code  "unresolved-namespace"
                            :range {:start {:line 1 :character 3}}}] {}
                          (h/db))))
  (testing "core ns"
    (h/assert-contains-submaps
      [{:title "Add require '[medley.core :as medley]'"
        :command {:command "add-require-suggestion"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                          (h/file-uri "file:///a.clj")
                          3
                          4
                          [{:code  "unresolved-namespace"
                            :range {:start {:line 2 :character 3}}}] {}
                          (h/db))))
  (testing "already used alias, we add proper suggestion"
    (h/assert-contains-submaps
      [{:title "Add require '[clojure.data.json :as data.json]'"
        :command {:command "add-require-suggestion"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                          (h/file-uri "file:///a.clj")
                          4
                          4
                          [{:code  "unresolved-namespace"
                            :range {:start {:line 3 :character 3}}}] {}
                          (h/db)))))

(deftest add-refer-suggestion-code-actions
  (h/load-code-and-locs "(ns clojure.set) (defn union [])" (h/file-uri "file:///clojure.core.clj"))
  (h/load-code-and-locs "(ns medley.core) (def unit) (defn uni [])" (h/file-uri "file:///medley.core.clj"))
  (h/load-code-and-locs "(ns some (:require [clojure.set :refer [union]]))" (h/file-uri "file:///some.clj"))
  (h/load-code-and-locs (h/code "(ns a)"
                                "(unit 1)"
                                "(union #{} #{})"))
  (testing "single suggestion"
    (h/assert-contains-submaps
      [{:title "Add require '[medley.core :refer [unit]]'"
        :command {:command "add-require-suggestion"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                          (h/file-uri "file:///a.clj")
                          2
                          3
                          [{:code    "unresolved-symbol"
                            :message "Unresolved symbol: unit"
                            :range   {:start {:line 1 :character 2}}}] {}
                          (h/db))))
  (testing "multiple suggestions"
    (h/assert-contains-submaps
      [{:title   "Add require '[clojure.set :refer [union]]'"
        :command {:command "add-require-suggestion"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                          (h/file-uri "file:///a.clj")
                          3
                          3
                          [{:code    "unresolved-symbol"
                            :message "Unresolved symbol: union"
                            :range   {:start {:line 2 :character 2}}}] {}
                          (h/db)))))

(deftest add-missing-namespace-code-actions
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        (h/file-uri "file:///a.clj"))
  (h/load-code-and-locs (str "(ns other-ns (:require [some-ns :as sns]))\n"
                             "(def bar 1)\n"
                             "(defn baz []\n"
                             "  bar)")
                        (h/file-uri "file:///b.clj"))
  (h/load-code-and-locs (str "(ns another-ns)\n"
                             "(def bar ons/bar)\n"
                             "(def foo sns/foo)\n"
                             "(deftest some-test)\n"
                             "MyClass.\n"
                             "Date.\n"
                             "Date/parse")
                        (h/file-uri "file:///c.clj"))
  (testing "when it has not unresolved-namespace diagnostic"
    (is (not-any? #(string/starts-with? (:title %) "Add require")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///c.clj"))
                                      (h/file-uri "file:///c.clj")
                                      2
                                      10
                                      [] {}
                                      (h/db)))))
  (testing "when it has unresolved-namespace and can find namespace"
    (h/assert-contains-submaps
      [{:title "Add require '[some-ns :as sns]' × 1"
        :command {:command "add-require-suggestion"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///c.clj"))
                          (h/file-uri "file:///c.clj")
                          3
                          11
                          [{:code  "unresolved-namespace"
                            :range {:start {:line 2 :character 10}}}] {}
                          (h/db))))
  (testing "when it has unresolved-namespace but cannot find namespace"
    (is (not-any? #(string/starts-with? (:title %) "Add require")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///c.clj"))
                                      (h/file-uri "file:///c.clj")
                                      2
                                      11
                                      [{:code "unresolved-namespace"
                                        :range {:start {:line 1 :character 10}}}] {}
                                      (h/db)))))
  (testing "when it has unresolved-symbol and it's a known refer"
    (h/assert-contains-submaps
      [{:title "Add require '[clojure.test :refer [deftest]]'"
        :command {:command "add-require-suggestion"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///c.clj"))
                          (h/file-uri "file:///c.clj")
                          4
                          2
                          [{:code  "unresolved-namespace"
                            :range {:start {:line 3 :character 1}}}] {}
                          (h/db))))
  (testing "when it has unresolved-symbol but it's not a known refer"
    (is (not-any? #(string/starts-with? (:title %) "Add require")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///c.clj"))
                                      (h/file-uri "file:///c.clj")
                                      4
                                      11
                                      [{:code "unresolved-symbol"
                                        :message "Unresolved symbol: foo"
                                        :range {:start {:line 3 :character 15}}}] {}
                                      (h/db))))))

(deftest add-common-missing-import-code-action
  (h/load-code-and-locs (str "(ns another-ns)\n"
                             "(def bar ons/bar)\n"
                             "(def foo sns/foo)\n"
                             "(deftest some-test)\n"
                             "MyClass.\n"
                             "Date.\n"
                             "Date/parse")
                        (h/file-uri "file:///c.clj"))
  (testing "when it has no unresolved-symbol diagnostic"
    (is (not-any? #(string/starts-with? (:title %) "Add import")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///c.clj"))
                                      (h/file-uri "file:///c.clj")
                                      5
                                      2
                                      [] {}
                                      (h/db)))))

  (testing "when it has unresolved-symbol but it's not a common import"
    (is (not-any? #(string/starts-with? (:title %) "Add import")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///c.clj"))
                                      (h/file-uri "file:///c.clj")
                                      5
                                      2
                                      [{:code "unresolved-symbol"
                                        :message "Unresolved symbol: foo"
                                        :range {:start {:line 4 :character 2}}}] {}
                                      (h/db)))))
  (testing "when it has unresolved-symbol and it's a common import"
    (h/assert-contains-submaps
      [{:title "Add import 'java.util.Date'"
        :command {:command "add-missing-import"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///c.clj"))
                          (h/file-uri "file:///c.clj")
                          6
                          2
                          [{:code    "unresolved-symbol"
                            :message "Unresolved symbol: foo"
                            :range   {:start {:line 5 :character 2}}}] {}
                          (h/db))))
  (testing "when it has unresolved-namespace and it's a common import via method"
    (h/assert-contains-submaps
      [{:title "Add import 'java.util.Date'"
        :command {:command "add-missing-import"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///c.clj"))
                          (h/file-uri "file:///c.clj")
                          7
                          2
                          [{:code  "unresolved-namespace"
                            :range {:start {:line 6 :character 2}}}] {}
                          (h/db)))))

(deftest inline-symbol-code-action
  (h/load-code-and-locs (str "(ns other-ns (:require [some-ns :as sns]))\n"
                             "(def bar 1)\n"
                             "(defn baz []\n"
                             "  bar)")
                        (h/file-uri "file:///b.clj"))
  (testing "when in not a let/def symbol"
    (is (not-any? #(= (:title %) "Inline symbol")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///b.clj"))
                                      (h/file-uri "file:///b.clj")
                                      4
                                      8
                                      [] {}
                                      (h/db)))))
  (testing "when in let/def symbol"
    (h/assert-contains-submaps
      [{:title "Inline symbol"
        :command {:command "inline-symbol"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///b.clj"))
                          (h/file-uri "file:///b.clj")
                          4
                          5
                          [] {}
                          (h/db)))))

(deftest change-coll-code-action
  (h/load-code-and-locs (h/code "\"some string\""
                                "(some-function 1 2)"
                                "{:some :map}"
                                "[:some :vector]"
                                "#{:some :set}")
                        (h/file-uri "file:///b.clj"))
  (testing "when in not a coll"
    (is (not-any? #(= (:title %) "Change coll to")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///b.clj"))
                                      (h/file-uri "file:///b.clj")
                                      1
                                      1
                                      [] {}
                                      (h/db)))))
  (testing "when in a list"
    (h/assert-contains-submaps
      [{:title "Change coll to map"
        :command {:command "change-coll"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///b.clj")) (h/file-uri "file:///b.clj") 2 1 [] {} (h/db))))
  (testing "when in a map"
    (h/assert-contains-submaps
      [{:title   "Change coll to vector"
        :command {:command "change-coll"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///b.clj")) (h/file-uri "file:///b.clj") 3 1 [] {} (h/db))))
  (testing "when in a vector"
    (h/assert-contains-submaps
      [{:title "Change coll to set"
        :command {:command "change-coll"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///b.clj")) (h/file-uri "file:///b.clj") 4 1 [] {} (h/db))))
  (testing "when in a set"
    (h/assert-contains-submaps
      [{:title "Change coll to list"
        :command {:command "change-coll"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///b.clj")) (h/file-uri "file:///b.clj") 5 1 [] {} (h/db)))))

(deftest introduce-let-code-action
  (h/load-code-and-locs (h/code "(+ (- 10 3) 2)")
                        (h/file-uri "file:///a.clj"))
  (testing "when a valid zloc"
    (h/assert-contains-submaps
      [{:title "Introduce let"
        :command {:command "introduce-let"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                          (h/file-uri "file:///a.clj")
                          1
                          4
                          [] {}
                          (h/db))))
  (testing "when not a valid zloc"
    (is (not-any? #(= (:title %) "Introduce let")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///b.clj"))
                                      (h/file-uri "file:///b.clj")
                                      4
                                      14
                                      [] {}
                                      (h/db))))))

(deftest move-to-let-code-action
  (h/load-code-and-locs (h/code "(let [a 1"
                                "      b 2]"
                                "  (+ 1 2))"
                                "(+ 1 2)")
                        (h/file-uri "file:///b.clj"))
  (testing "when a valid zloc"
    (is (not-any? #(= (:title %) "Move to let")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///b.clj"))
                                      (h/file-uri "file:///b.clj")
                                      4
                                      14
                                      [] {}
                                      (h/db)))))
  (testing "when inside let form"
    (h/assert-contains-submaps
      [{:title "Move to let"
        :command {:command "move-to-let"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///b.clj"))
                          (h/file-uri "file:///b.clj")
                          3
                          3
                          [] {}
                          (h/db)))))

(deftest cycle-privacy-code-action
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        (h/file-uri "file:///a.clj"))
  (testing "when non function location"
    (is (not-any? #(= (:title %) "Cycle privacy")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                                      (h/file-uri "file:///a.clj")
                                      1
                                      5
                                      [] {}
                                      (h/db)))))
  (testing "when on function location"
    (h/assert-contains-submaps
      [{:title "Cycle privacy"
        :command {:command "cycle-privacy"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                          (h/file-uri "file:///a.clj")
                          2
                          5
                          [] {}
                          (h/db)))))

(deftest destructure-keys-code-action
  (let [[[non-local-r non-local-c]
         [local-r local-c]]
        (h/load-code-and-locs (h/code "(ns some-ns)"
                                      "(def |foo)"
                                      "(defn bar [|shape]"
                                      "  (:shape/type shape))")
                              (h/file-uri "file:///a.clj"))]
    (testing "when not on local"
      (is (not-any? #(= (:title %) "Destructure keys")
                    (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                                        (h/file-uri "file:///a.clj")
                                        non-local-r
                                        non-local-c
                                        [] {}
                                        (h/db)))))
    (testing "when on local"
      (h/assert-contains-submaps
        [{:title "Destructure keys"
          :command {:command "destructure-keys"}}]
        (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                            (h/file-uri "file:///a.clj")
                            local-r
                            local-c
                            [] {}
                            (h/db))))))

(deftest extract-function-code-action
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        (h/file-uri "file:///a.clj"))
  (testing "when non function location"
    (is (not-any? #(= (:title %) "Extract function")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                                      (h/file-uri "file:///a.clj")
                                      1
                                      5
                                      [] {}
                                      (h/db)))))
  (testing "when on function location"
    (h/assert-contains-submaps
      [{:title "Extract function"
        :command {:command "extract-function"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                          (h/file-uri "file:///a.clj")
                          2
                          5
                          [] {}
                          (h/db)))))

(deftest extract-to-def-code-action
  (h/load-code-and-locs "{:a 1}"
                        (h/file-uri "file:///a.clj"))
  (h/assert-contains-submaps
    [{:title "Extract to def"
      :command {:command "extract-to-def"}}]
    (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                        (h/file-uri "file:///a.clj")
                        1
                        1
                        [] {}
                        (h/db))))

(deftest create-private-function-code-action
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                "(def foo (+ 1 2))"
                                "(def bar (some-func 1 2))"))
  (testing "when not in a unresolved symbol"
    (is (not-any? #(= (:title %) "Create private function")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                                      (h/file-uri "file:///a.clj")
                                      1
                                      10
                                      [] {}
                                      (h/db)))))
  (testing "when in a unresolved symbol"
    (h/assert-contains-submaps
      [{:title "Create private function 'some-func'"
        :command {:command "create-function"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                          (h/file-uri "file:///a.clj")
                          2
                          10
                          [{:code    "unresolved-symbol"
                            :message "Unresolved symbol: some-func"
                            :range   {:start {:line 2 :character 11}}}] {}
                          (h/db)))))

(deftest thread-first-all-action
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                "(def foo)"
                                "(- (+ 1 1) 2)")
                        (h/file-uri "file:///a.clj"))
  (testing "when in a ns or :require"
    (is (not-any? #(= (:title %) "Thread first all")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 1 1 [] {} (h/db)))))
  (testing "when in a def similar location"
    (is (not-any? #(= (:title %) "Thread first all")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 2 1 [] {} (h/db)))))
  (testing "when on a def non-list node"
    (is (not-any? #(= (:title %) "Thread first all")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 2 2 [] {} (h/db)))))
  (testing "when on a valid function that can be threaded"
    (h/assert-contains-submaps
      [{:title "Thread first all"
        :command {:command "thread-first-all"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 3 1 [] {} (h/db))))
  (testing "when on a non-list node"
    (h/assert-contains-submaps
      [{:title "Thread first all"
        :command {:command "thread-first-all"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 3 3 [] {} (h/db)))))

(deftest thread-last-all-action
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                "(def foo)"
                                "(- (+ 1 1) 2)")
                        (h/file-uri "file:///a.clj"))
  (testing "when in a ns or :require"
    (is (not-any? #(= (:title %) "Thread last all")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 1 1 [] {} (h/db)))))
  (testing "when in a def similar location"
    (is (not-any? #(= (:title %) "Thread last all")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 2 1 [] {} (h/db)))))
  (testing "when on a def non-list node"
    (is (not-any? #(= (:title %) "Thread last all")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 2 1 [] {} (h/db)))))
  (testing "when on a valid function that can be threaded"
    (h/assert-contains-submaps
      [{:title "Thread last all"
        :command {:command "thread-last-all"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 3 1 [] {} (h/db))))
  (testing "when on a non-list node"
    (h/assert-contains-submaps
      [{:title "Thread last all"
        :command {:command "thread-last-all"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 3 3 [] {} (h/db)))))

(deftest unwind-thread-action
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                "(def foo)"
                                "(->> (+ 0 1)"
                                "     (+ 2)"
                                "     (+ 3))")
                        (h/file-uri "file:///a.clj"))
  (testing "when not in a thread"
    (is (not-any? #(= (:title %) "Unwind thread once")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 1 1 [] {} (h/db)))))
  (testing "when inside thread call"
    (h/assert-contains-submaps
      [{:title "Unwind thread once"
        :command {:command "unwind-thread"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 3 1 [] {} (h/db))))
  (testing "when inside thread symbol"
    (h/assert-contains-submaps
      [{:title "Unwind thread once"
        :command {:command "unwind-thread"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 3 2 [] {} (h/db))))
  (testing "when inside any threading call"
    (h/assert-contains-submaps
      [{:title "Unwind thread once"
        :command {:command "unwind-thread"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 5 7 [] {} (h/db)))))

(deftest clean-ns-code-actions
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo)")
                        (h/file-uri "file:///a.clj"))
  (h/load-code-and-locs (str "(ns other-ns (:require [some-ns :as sns]))\n"
                             "(def bar 1)\n"
                             "(defn baz []\n"
                             "  bar)")
                        (h/file-uri "file:///b.clj"))
  (h/load-code-and-locs (str "(ns another-ns)\n"
                             "(def bar ons/bar)\n"
                             "(def foo sns/foo)\n"
                             "(deftest some-test)\n"
                             "MyClass.\n"
                             "Date.\n"
                             "Date/parse")
                        (h/file-uri "file:///c.clj"))
  (testing "without workspace edit client capability"
    (is (not-any? #(= (:title %) "Clean namespace")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///b.clj"))
                                      (h/file-uri "file:///b.clj")
                                      2
                                      2
                                      [] {}
                                      (h/db)))))

  (testing "with workspace edit client capability"
    (swap! (h/db*) assoc-in [:client-capabilities :workspace :workspace-edit] true)
    (h/assert-contains-submaps
      [{:title "Clean namespace"
        :command {:command "clean-ns"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///b.clj"))
                          (h/file-uri "file:///b.clj")
                          2
                          2
                          [] {:workspace {:workspace-edit true}}
                          (h/db)))))

(deftest resolve-macro-as-code-actions
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                "(defmacro foo [name & body] @body)"
                                "(foo my-fn)"
                                "(+ 1 2)"))
  (testing "when inside a macro usage"
    (h/assert-contains-submaps
      [{:title "Resolve macro 'some-ns/foo' as..."
        :command {:command "resolve-macro-as"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 3 7 [] {} (h/db))))
  (testing "when not inside a macro usage"
    (is (not-any? #(= (:title %) "Resolve macro 'some-ns/foo' as...")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj")) (h/file-uri "file:///a.clj") 4 4 [] {} (h/db))))))

(deftest suppress-diagnostic-code-actions
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                ""
                                "(def ^:private a 1)"))
  (testing "unused-private-var"
    (h/assert-contains-submaps
      [{:title "Suppress 'unused-private-var' diagnostic"
        :command {:command "suppress-diagnostic"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                          (h/file-uri "file:///a.clj")
                          3
                          3
                          [{:code    "unused-private-var"
                            :message "Unused private var: a"
                            :range   {:start {:line 3 :character 11}}}]
                          {:workspace {:workspace-edit true}}
                          (h/db)))))

(deftest sort-map-actions
  (swap! (h/db*) shared/deep-merge {:client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                ""
                                "(defn foo []"
                                "  {:g 2 :s 3 :kj 3 :a 5})"))
  (testing "on map bracket"
    (h/assert-contains-submaps
      [{:title "Sort map"
        :command {:command "sort-map"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                          (h/file-uri "file:///project/src/some_ns.clj")
                          4
                          3
                          []
                          {:workspace {:workspace-edit true}}
                          (h/db))))
  (testing "On map's key"
    (h/assert-contains-submaps
      [{:title "Sort map"
        :command {:command "sort-map"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                          (h/file-uri "file:///project/src/some_ns.clj")
                          4
                          5
                          []
                          {:workspace {:workspace-edit true}}
                          (h/db))))
  (testing "not on map"
    (is (not-any? #(= (:title %) "Sort map")
                  (f.code-actions/all (zloc-of (h/file-uri "file:///a.clj"))
                                      (h/file-uri "file:///project/src/some_ns.clj")
                                      3
                                      7
                                      []
                                      {:workspace {:workspace-edit true}}
                                      (h/db))))))

(deftest create-test-code-actions
  (swap! (h/db*) shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src") (h/file-path "/project/test")}}
                                  :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                                  :project-root-uri (h/file-uri "file:///project")})
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                ""
                                "(defn foo [] 1)")
                        "file:///project/src/some_ns.clj")
  (testing "inside function"
    (h/assert-contains-submaps
      [{:title "Create test for 'foo'"
        :command {:command "create-test"}}]
      (f.code-actions/all (zloc-of (h/file-uri "file:///project/src/some_ns.clj"))
                          (h/file-uri "file:///project/src/some_ns.clj")
                          3
                          6
                          []
                          {:workspace {:workspace-edit true}}
                          (h/db)))))

(deftest promote-fn-actions
  (let [[[row col]] (h/load-code-and-locs (h/code "|(fn [] (+ 1 2))"))]
    (h/assert-contains-submaps
      [{:title "Promote fn to defn"
        :command {:command "promote-fn"}}]
      (f.code-actions/all (zloc-of h/default-uri)
                          h/default-uri
                          row
                          col
                          []
                          {:workspace {:workspace-edit true}}
                          (h/db))))
  (let [[[row col]] (h/load-code-and-locs (h/code "|#(+ 1 2)"))]
    (h/assert-contains-submaps
      [{:title "Promote #() to fn"
        :command {:command "promote-fn"}}]
      (f.code-actions/all (zloc-of h/default-uri)
                          h/default-uri
                          row
                          col
                          []
                          {:workspace {:workspace-edit true}}
                          (h/db)))))

(deftest demote-fn-actions
  (let [[[row col]] (h/load-code-and-locs (h/code "|(fn [] (+ 1 2))"))]
    (h/assert-contains-submaps
      [{:title "Demote fn to #()"
        :command {:command "demote-fn"}}]
      (f.code-actions/all (zloc-of h/default-uri)
                          h/default-uri
                          row
                          col
                          []
                          {:workspace {:workspace-edit true}}
                          (h/db)))))
