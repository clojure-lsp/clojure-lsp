(ns clojure-lsp.features.workspace-symbols-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.workspace-symbols :as f.workspace-symbols]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(deftest workspace-symbols
  (h/load-code-and-locs (h/code "(ns foo.alpaca.ns (:require [clojure.string :as string]))"
                                "(defonce my-alpapapaca (atom {}))"
                                "(def alpac 1)"
                                "(defn alpacas [a b] alpac)"
                                "(defmulti llama identity)"))
  (h/load-code-and-locs (h/code "(ns foo.goat.ns (:require [foo.alpaca.ns :as a]))"
                                "(defn goats-from-alpacas [alpacas] (map inc alpacas))"
                                "(defmethod a/llama \"wooly\")")
                        (h/file-uri "file:///b.clj"))
  (testing "querying all symbols"
    (is (= [;; a.clj
            {:name "foo.alpaca.ns"
             :kind :namespace
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 0 :character 0} :end {:line 0 :character 57}}}}
            {:name "my-alpapapaca"
             :kind :variable
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 1 :character 0} :end {:line 1 :character 33}}}}
            {:name "alpac"
             :kind :variable
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 2 :character 0} :end {:line 2 :character 13}}}}
            {:name "alpacas"
             :kind :function
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 3 :character 0} :end {:line 3 :character 26}}}}
            {:name "llama",
             :kind :variable,
             :location
             {:uri (h/file-uri "file:///a.clj"),
              :range {:start {:line 4, :character 0}, :end {:line 4, :character 25}}}}
            ;; b.clj
            {:kind :namespace,
             :location {:range {:start {:line 0, :character 0}, :end {:line 0, :character 49}},
                        :uri (h/file-uri "file:///b.clj")},
             :name "foo.goat.ns"}
            {:kind :function,
             :location {:range {:start {:line 1, :character 0}, :end {:line 1, :character 53}},
                        :uri (h/file-uri "file:///b.clj")},
             :name "goats-from-alpacas"}
            {:name "llama \"wooly\"",
             :kind :variable,
             :location {:range {:start {:line 2, :character 11}, :end {:line 2, :character 18}},
                        :uri (h/file-uri "file:///b.clj")}}]
           (f.workspace-symbols/workspace-symbols "" @db/db*))))
  (testing "querying a specific function using fuzzy search"
    (is (= [;; a.clj
            {:name "foo.alpaca.ns"
             :kind :namespace
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 0 :character 0} :end {:line 0 :character 57}}}}
            ;; later in file, but better search score
            {:name "alpacas"
             :kind :function
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 3 :character 0} :end {:line 3 :character 26}}}}
            {:name "my-alpapapaca"
             :kind :variable
             :location
             {:uri (h/file-uri "file:///a.clj")
              :range {:start {:line 1 :character 0} :end {:line 1 :character 33}}}}
            ;; b.clj
            {:kind :function,
             :location {:range {:start {:line 1, :character 0}, :end {:line 1, :character 53}},
                        :uri (h/file-uri "file:///b.clj")},
             :name "goats-from-alpacas"}]
           (f.workspace-symbols/workspace-symbols "alpaca" @db/db*)))))
