(ns clojure-lsp.features.workspace-symbols-test
  (:require
   [clojure-lsp.feature.workspace-symbols :as f.workspace-symbols]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(defn as-ranges [positions]
  (->> positions
       (partition 2)
       (map (fn [[start end]] (h/->range start end)))))

(deftest workspace-symbols
  (let [components (h/make-components)
        [a-ns
         a-defonce
         a-def
         a-defn
         a-defmulti]
        (as-ranges
          (h/load-code (h/code "|(ns foo.alpaca.ns (:require [clojure.string :as string]))|"
                               "|(defonce my-alpapapaca (atom {}))|"
                               "|(def alpac 1)|"
                               "|(defn alpacas [a b] alpac)|"
                               "|(defmulti llama identity)|")
                       h/default-uri components))
        [b-ns
         b-defn
         b-a-defmethod]
        (as-ranges
         (h/load-code (h/code "|(ns foo.goat.ns (:require [foo.alpaca.ns :as a]))|"
                              "|(defn goats-from-alpacas [alpacas] (map inc alpacas))|"
                              "(defmethod |a/llama| \"wooly\")")
                      (h/file-uri "file:///b.clj") components))]
    (testing "querying all symbols"
      (is (= [;; a.clj
              {:name "foo.alpaca.ns"
               :kind :namespace
               :location
               {:uri (h/file-uri "file:///a.clj")
                :range a-ns}}
              {:name "my-alpapapaca"
               :kind :variable
               :location
               {:uri (h/file-uri "file:///a.clj")
                :range a-defonce}}
              {:name "alpac"
               :kind :variable
               :location
               {:uri (h/file-uri "file:///a.clj")
                :range a-def}}
              {:name "alpacas"
               :kind :function
               :location
               {:uri (h/file-uri "file:///a.clj")
                :range a-defn}}
              {:name "llama",
               :kind :variable,
               :location
               {:uri (h/file-uri "file:///a.clj"),
                :range a-defmulti}}
              ;; b.clj
              {:kind :namespace,
               :location {:range b-ns,
                          :uri (h/file-uri "file:///b.clj")},
               :name "foo.goat.ns"}
              {:kind :function,
               :location {:range b-defn,
                          :uri (h/file-uri "file:///b.clj")},
               :name "goats-from-alpacas"}
              {:name "llama \"wooly\"",
               :kind :variable,
               :location {:range b-a-defmethod,
                          :uri (h/file-uri "file:///b.clj")}}]
             (f.workspace-symbols/workspace-symbols "" (h/db components)))))
    (testing "querying a specific function using fuzzy search"
      (is (= [;; a.clj
              {:name "foo.alpaca.ns"
               :kind :namespace
               :location
               {:uri (h/file-uri "file:///a.clj")
                :range a-ns}}
              ;; later in file, but better search score
              {:name "alpacas"
               :kind :function
               :location
               {:uri (h/file-uri "file:///a.clj")
                :range a-defn}}
              {:name "my-alpapapaca"
               :kind :variable
               :location
               {:uri (h/file-uri "file:///a.clj")
                :range a-defonce}}
              ;; b.clj
              {:kind :function,
               :location {:range b-defn,
                          :uri (h/file-uri "file:///b.clj")},
               :name "goats-from-alpacas"}]
             (f.workspace-symbols/workspace-symbols "alpaca" (h/db components)))))))
