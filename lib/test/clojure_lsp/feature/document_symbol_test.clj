(ns clojure-lsp.feature.document-symbol-test
  (:require
   [clojure-lsp.feature.document-symbol :as f.document-symbol]
   [clojure-lsp.test-helper :as h]
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :as properties]))

(s/def ::edn
  (s/or :keyword (s/and keyword? #(= 1 (count (re-seq #":" (str %)))))
        :number number?
        :string (s/and string? #(re-matches #"[a-z].*" (str %)))
        :simple-symbol simple-symbol?
        ;;TODO fail tests because of namespaced maps using symbols
        ;; :symbol symbol?
        :nil nil?
        :boolean boolean?
        :vector (s/coll-of ::edn :kind vector? :gen-max 3)
        :set (s/coll-of ::edn :kind set?  :gen-max 3)
        :list (s/coll-of ::edn :kind list?  :gen-max 3)
        :map (s/map-of ::edn ::edn  :gen-max 3)))

(h/reset-components-before-test)

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
               :kind :number
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
                   :kind :number
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
               :kind :number
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
                 :kind :number
                 :range
                 {:start {:line 0 :character 12} :end {:line 0 :character 14}}
                 :selection-range
                 {:start {:line 0 :character 12} :end {:line 0 :character 14}}
                 :tags []}
                {:name "d"
                 :kind :number
                 :range
                 {:start {:line 0 :character 19} :end {:line 0 :character 21}}
                 :selection-range
                 {:start {:line 0 :character 19} :end {:line 0 :character 21}}
                 :tags []}]}]
             (f.document-symbol/document-symbols
               (h/db)
               (h/file-uri "file:///a.edn")))))
    (testing "invalid code being typed"
      (h/load-code-and-locs
        (h/code "[{:}]")
        (h/file-uri "file:///a.edn"))
      (is (= nil
             (f.document-symbol/document-symbols
               (h/db)
               (h/file-uri "file:///a.edn")))))
    (testing "nested vectors"
      (h/load-code-and-locs
        (h/code "{:a [[:inner 2]]}")
        (h/file-uri "file:///a.edn"))
      (is (= [{:name "a"
               :kind :array
               :range {:start {:line 0 :character 1} :end {:line 0 :character 3}}
               :selection-range {:start {:line 0 :character 1} :end {:line 0 :character 3}}
               :tags []}]
             (f.document-symbol/document-symbols
               (h/db)
               (h/file-uri "file:///a.edn")))))
    (testing "simple symbols"
      (h/load-code-and-locs
        (h/code (str '{:a {foo {:b 2}}}))
        (h/file-uri "file:///a.edn"))
      (is (= [{:name "a"
               :kind :struct
               :range {:start {:line 0 :character 1} :end {:line 0 :character 3}}
               :selection-range {:start {:line 0 :character 1} :end {:line 0 :character 3}}
               :tags []
               :children
               [{:name "foo"
                 :kind :struct
                 :range {:start {:line 0 :character 0} :end {:line 0 :character 0}}
                 :selection-range {:start {:line 0 :character 0} :end {:line 0 :character 0}}
                 :tags []
                 :children
                 [{:name "b"
                   :kind :number
                   :range {:start {:line 0 :character 10} :end {:line 0 :character 12}}
                   :selection-range {:start {:line 0 :character 10} :end {:line 0 :character 12}}
                   :tags []}]}]}]
             (f.document-symbol/document-symbols
               (h/db)
               (h/file-uri "file:///a.edn"))))
      (testing "primitive values at root"
        (h/load-code-and-locs
          (h/code (str 'something))
          (h/file-uri "file:///a.edn"))
        (is (= nil
               (f.document-symbol/document-symbols
                 (h/db)
                 (h/file-uri "file:///a.edn")))))
      (testing "empty colls"
        (h/load-code-and-locs
          (h/code (str '#{nil [] :a 5 #{}}))
          (h/file-uri "file:///a.edn"))
        (is (= []
               (f.document-symbol/document-symbols
                 (h/db)
                 (h/file-uri "file:///a.edn")))))
      (testing "colls size less than a pair"
        (h/load-code-and-locs
          (h/code (str '#{#{#{}}}))
          (h/file-uri "file:///a.edn"))
        (is (= []
               (f.document-symbol/document-symbols
                 (h/db)
                 (h/file-uri "file:///a.edn")))))
      (testing "nil as keys"
        (h/load-code-and-locs
          (h/code (str '{nil #{}}))
          (h/file-uri "file:///a.edn"))
        (is (= [{:name "nil"
                 :kind :array
                 :range {:start {:line 0 :character 0} :end {:line 0 :character 0}}
                 :selection-range {:start {:line 0 :character 0} :end {:line 0 :character 0}}
                 :tags []}]
               (f.document-symbol/document-symbols
                 (h/db)
                 (h/file-uri "file:///a.edn")))))
      (testing "nil as keys"
        (h/load-code-and-locs
          (h/code (str '#{#{-8 ag}}))
          (h/file-uri "file:///a.edn"))
        (is (= [{:name "-8"
                 :kind :struct
                 :range {:start {:line 0 :character 0} :end {:line 0 :character 0}}
                 :selection-range {:start {:line 0 :character 0} :end {:line 0 :character 0}}
                 :tags []}]
               (f.document-symbol/document-symbols
                 (h/db)
                 (h/file-uri "file:///a.edn"))))))))

(defspec edn-files 1000
  (properties/for-all [value (s/gen ::edn)]
    (h/load-code (str value) (h/file-uri "file:///a.edn"))
    (try
      (f.document-symbol/document-symbols
        (h/db)
        (h/file-uri "file:///a.edn"))
      (catch Exception e
        (println "Generative test failed. Sample:" (pr-str value))
        (throw e)))
    (is true)))
