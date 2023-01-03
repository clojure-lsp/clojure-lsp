(ns clojure-lsp.feature.cycle-keyword-test
  (:require
   [clojure-lsp.feature.cycle-keyword :as f.cycle-keyword]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(defn ^:private cycle-keyword-auto-resolve [code]
  (h/load-code-and-locs code)
  (h/edits-as-strings
    (f.cycle-keyword/cycle-keyword-auto-resolve
      (h/zloc-from-code code)
      h/default-uri
      (h/db)
      (h/components))))

(defn ^:private cycle-keyword-namespace-alias-status [code]
  (:status (f.cycle-keyword/cycle-keyword-auto-resolve-status
             (h/zloc-from-code code))))

(deftest cycle-keyword-auto-resolve-status-test
  (is (= nil (cycle-keyword-namespace-alias-status
               "(ns foo) |:bar")))
  (is (= :from-auto-resolve-to-namespace (cycle-keyword-namespace-alias-status
                                           "(ns foo) |::bar")))
  (is (= :from-namespace-to-auto-resolve (cycle-keyword-namespace-alias-status
                                           "(ns foo) |:foo/bar")))
  (is (= nil (cycle-keyword-namespace-alias-status
               "(ns foo) |:baz/bar")))
  (is (= nil (cycle-keyword-namespace-alias-status
               "(ns foo (:require [baz :as b])) |::b/bar"))))

(deftest cycle-keyword-auto-resolve-test
  (testing "single keyword usages"
    (testing "converting from a auto-resolved keyword to a namespaced keyword"
      (is (= [":foo/bar"]
             (cycle-keyword-auto-resolve
               "(ns foo) |::bar"))))
    (testing "converting from namespaced local keyword to a auto-resolved keyword"
      (is (= ["::bar"]
             (cycle-keyword-auto-resolve
               "(ns foo) |:foo/bar"))))
    (testing "converting from namespaced not local keyword to a auto-resolved keyword"
      (is (= []
             (cycle-keyword-auto-resolve
               "(ns foo) |:baz/bar")))))
  (testing "multiple keyword usages on current namespace"
    (testing "converting from a auto-resolved keyword to a namespaced keyword when user agree"
      (with-redefs [f.cycle-keyword/ask-to-replace-all-ns-changes (constantly true)]
        (is (= [":foo/bar" ":foo/bar"]
               (cycle-keyword-auto-resolve
                 "(ns foo) |::bar ::bar")))))
    (testing "converting from a auto-resolved keyword to a namespaced keyword when user disagree"
      (with-redefs [f.cycle-keyword/ask-to-replace-all-ns-changes (constantly false)]
        (is (= [":foo/bar"]
               (cycle-keyword-auto-resolve
                 "(ns foo) ::bar |::bar")))))
    (testing "converting from a namespaced keyword to auto-resolved when user agree"
      (with-redefs [f.cycle-keyword/ask-to-replace-all-ns-changes (constantly true)]
        (is (= ["::bar" "::bar"]
               (cycle-keyword-auto-resolve
                 "(ns foo) :foo/bar |:foo/bar")))))
    (testing "converting from a namespaced keyword to auto-resolved when user disagree"
      (with-redefs [f.cycle-keyword/ask-to-replace-all-ns-changes (constantly false)]
        (is (= ["::bar"]
               (cycle-keyword-auto-resolve
                 "(ns foo) :foo/bar |:foo/bar")))))))
