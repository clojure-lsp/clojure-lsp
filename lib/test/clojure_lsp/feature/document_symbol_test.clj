(ns clojure-lsp.feature.document-symbol-test
  (:require
   [clojure-lsp.feature.document-symbol :as f.document-symbol]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(deftest document-symbols-test
  (testing "edn files"
    (testing "empty edn"
      (h/load-code-and-locs
        (h/code "{}")
        (h/file-uri "file:///a.edn"))
      (is (= []
             (f.document-symbol/document-symbols
               (h/db)
               (h/file-uri "file:///a.edn")))))
    (testing "nested maps"
      (h/load-code-and-locs
        (h/code "{:a 1 :b {:c {:d :bla} :e \"foo\"} :f [1] :g {some/thing {:version 1} other/thing [{}]}}")
        (h/file-uri "file:///a.edn"))
      (is (= [{:name "a"
               :kind :struct
               :range {:start {:line 0 :character 1} :end {:line 0 :character 3}}
               :selection-range {:start {:line 0 :character 1} :end {:line 0 :character 3}}
               :tags []}
              {:name "b"
               :kind :struct
               :range {:start {:line 0 :character 6} :end {:line 0 :character 8}}
               :selection-range {:start {:line 0 :character 6} :end {:line 0 :character 8}}
               :tags []
               :children
               [{:name "c"
                 :kind :struct
                 :range {:start {:line 0 :character 10} :end {:line 0 :character 12}}
                 :selection-range {:start {:line 0 :character 10} :end {:line 0 :character 12}}
                 :tags []
                 :children
                 [{:name "d"
                   :kind :field
                   :range {:start {:line 0 :character 14} :end {:line 0 :character 16}}
                   :selection-range {:start {:line 0 :character 14} :end {:line 0 :character 16}}
                   :tags []}]}
                {:name "e"
                 :kind :string
                 :range {:start {:line 0 :character 23} :end {:line 0 :character 25}}
                 :selection-range {:start {:line 0 :character 23} :end {:line 0 :character 25}}
                 :tags []}]}
              {:name "f"
               :kind :array
               :range {:start {:line 0 :character 33} :end {:line 0 :character 35}}
               :selection-range {:start {:line 0 :character 33} :end {:line 0 :character 35}}
               :tags []}
              {:name "g"
               :kind :struct
               :range
               {:start {:line 0 :character 40} :end {:line 0 :character 42}}
               :selection-range
               {:start {:line 0 :character 40} :end {:line 0 :character 42}}
               :tags []
               :children
               [{:name "some/thing"
                 :kind :struct
                 :range
                 {:start {:line 0 :character 44} :end {:line 0 :character 54}}
                 :selection-range
                 {:start {:line 0 :character 44} :end {:line 0 :character 54}}
                 :tags []
                 :children
                 [{:name "version"
                   :kind :struct
                   :range
                   {:start {:line 0 :character 56}
                    :end {:line 0 :character 64}}
                   :selection-range
                   {:start {:line 0 :character 56}
                    :end {:line 0 :character 64}}
                   :tags []}]}
                {:name "other/thing"
                 :kind :array
                 :range
                 {:start {:line 0 :character 68} :end {:line 0 :character 79}}
                 :selection-range
                 {:start {:line 0 :character 68} :end {:line 0 :character 79}}
                 :tags []}]}]
             (f.document-symbol/document-symbols
               (h/db)
               (h/file-uri "file:///a.edn")))))))
