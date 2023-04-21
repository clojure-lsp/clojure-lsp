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
        (h/code (str '{:a 1
                       :b {:c {:d :bla}
                           :e "foo"}
                       :f [1]
                       :g {some/thing {:version 1}
                           other/thing [{}]}}))
        (h/file-uri "file:///a.edn"))
      (is (= [{:name "a"
               :kind :struct
               :range
               {:start {:line 0 :character 1} :end {:line 0 :character 3}}
               :selection-range
               {:start {:line 0 :character 1} :end {:line 0 :character 3}}
               :tags []}
              {:name "b"
               :kind :struct
               :range
               {:start {:line 0 :character 7} :end {:line 0 :character 9}}
               :selection-range
               {:start {:line 0 :character 7} :end {:line 0 :character 9}}
               :tags []
               :children
               [{:name "c"
                 :kind :struct
                 :range
                 {:start {:line 0 :character 11} :end {:line 0 :character 13}}
                 :selection-range
                 {:start {:line 0 :character 11} :end {:line 0 :character 13}}
                 :tags []
                 :children
                 [{:name "d"
                   :kind :field
                   :range
                   {:start {:line 0 :character 15} :end {:line 0 :character 17}}
                   :selection-range
                   {:start {:line 0 :character 15} :end {:line 0 :character 17}}
                   :tags []}]}
                {:name "e"
                 :kind :string
                 :range
                 {:start {:line 0 :character 25} :end {:line 0 :character 27}}
                 :selection-range
                 {:start {:line 0 :character 25} :end {:line 0 :character 27}}
                 :tags []}]}
              {:name "f"
               :kind :array
               :range
               {:start {:line 0 :character 36} :end {:line 0 :character 38}}
               :selection-range
               {:start {:line 0 :character 36} :end {:line 0 :character 38}}
               :tags []}
              {:name "g"
               :kind :struct
               :range
               {:start {:line 0 :character 44} :end {:line 0 :character 46}}
               :selection-range
               {:start {:line 0 :character 44} :end {:line 0 :character 46}}
               :tags []
               :children
               [{:name "some/thing"
                 :kind :struct
                 :range
                 {:start {:line 0 :character 48} :end {:line 0 :character 58}}
                 :selection-range
                 {:start {:line 0 :character 48} :end {:line 0 :character 58}}
                 :tags []
                 :children
                 [{:name "version"
                   :kind :struct
                   :range
                   {:start {:line 0 :character 60} :end {:line 0 :character 68}}
                   :selection-range
                   {:start {:line 0 :character 60} :end {:line 0 :character 68}}
                   :tags []}]}
                {:name "other/thing"
                 :kind :array
                 :range
                 {:start {:line 0 :character 73} :end {:line 0 :character 84}}
                 :selection-range
                 {:start {:line 0 :character 73} :end {:line 0 :character 84}}
                 :tags []}]}]
             (f.document-symbol/document-symbols
               (h/db)
               (h/file-uri "file:///a.edn")))))
    (testing "vector root"
      (h/load-code-and-locs
        (h/code "[{:a 1 :b [{:c 2} {:d 3}]}]")
        (h/file-uri "file:///a.edn"))
      (is (= [{:name "a"
               :kind :struct
               :range
               {:start {:line 0 :character 2} :end {:line 0 :character 4}}
               :selection-range
               {:start {:line 0 :character 2} :end {:line 0 :character 4}}
               :tags []}
              {:name "b"
               :kind :array
               :range
               {:start {:line 0 :character 7} :end {:line 0 :character 9}}
               :selection-range
               {:start {:line 0 :character 7} :end {:line 0 :character 9}}
               :tags []
               :children
               [{:name "c"
                 :kind :struct
                 :range
                 {:start {:line 0 :character 12} :end {:line 0 :character 14}}
                 :selection-range
                 {:start {:line 0 :character 12} :end {:line 0 :character 14}}
                 :tags []}
                {:name "d"
                 :kind :struct
                 :range
                 {:start {:line 0 :character 19} :end {:line 0 :character 21}}
                 :selection-range
                 {:start {:line 0 :character 19} :end {:line 0 :character 21}}
                 :tags []}]}]
             (f.document-symbol/document-symbols
               (h/db)
               (h/file-uri "file:///a.edn")))))))
