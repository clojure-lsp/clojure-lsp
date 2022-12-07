(ns clojure-lsp.feature.workspace-symbols-test
  (:require
   [clojure-lsp.feature.workspace-symbols :as f.workspace-symbols]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(deftest workspace-symbols
  (h/load-code-and-locs (h/code "(ns ns-def (:require [clojure.string :as string]))"
                                "(defonce var-defonce (atom {}))"
                                "(def var-def 1)"
                                "(defn fn-defn-fixed [a b] a)"
                                "(defn fn-defn-multi ([a] a) ([a b] a))"
                                "(defn fn-defn-varargs [a & args] a)"
                                "(defmacro fn-defmacro [a] ``(inc a))"
                                "(defmulti interface-defmulti identity)"
                                "(defmethod interface-defmulti :fn-defmethod [x] x)"
                                "(defprotocol InterfaceDefprotocol"
                                "  (fn-defprotocol [this a]))"
                                "(definterface InterfaceDefinterface"
                                "  (^String fn-definterface [a]))"))
  (h/load-code-and-locs (h/code "(ns ns-def-2 (:require [ns-def :as ns-alias]))"
                                "(defmethod ns-alias/interface-defmulti :fn-defmethod-other-ns [x] x)"
                                "(defrecord ClassDefrecord []"
                                "  ns-alias/InterfaceDefprotocol"
                                "  (fn-defprotocol [this a] a)"
                                "  ns-alias/InterfaceDefinterface"
                                "  (fn-definterface [this a] a))"
                                "(deftype ClassDeftype [])")
                        (h/file-uri "file:///b.clj"))
  (testing "querying all symbols"
    (let [all-symbols (f.workspace-symbols/workspace-symbols "" (h/db))]
      (testing "sets location"
        (is (= {:name "ns-def",
                :kind :namespace,
                :location {:uri (h/file-uri "file:///a.clj"),
                           :range {:start {:line 0, :character 0}, :end {:line 0, :character 50}}}}
               (first all-symbols))))
      (testing "set correct kind"
        (is (= [;; a.clj
                ["ns-def"                                    :namespace]
                ["var-defonce"                               :variable]
                ["var-def"                                   :variable]
                ["fn-defn-fixed"                             :function]
                ["fn-defn-multi"                             :function]
                ["fn-defn-varargs"                           :function]
                ["fn-defmacro"                               :function]
                ["interface-defmulti"                        :interface]
                ["InterfaceDefprotocol"                      :interface]
                ["fn-defprotocol"                            :function]
                ["InterfaceDefinterface"                     :interface]
                ["fn-definterface"                           :function]
                ;; TODO: should defmethods be sorted by line number?
                ["interface-defmulti :fn-defmethod"          :function]
                ;; b.clj
                ["ns-def-2"                                  :namespace]
                ;; TODO: should implementations of protocol and interface methods be included?
                ["ClassDefrecord"                            :class]
                ["->ClassDefrecord"                          :function]
                ["map->ClassDefrecord"                       :function]
                ["ClassDeftype"                              :class]
                ["->ClassDeftype"                            :function]
                ;; TODO: should defmethods be sorted by line number?
                ["interface-defmulti :fn-defmethod-other-ns" :function]]
               (map (juxt :name :kind)
                    all-symbols))))))
  (testing "querying a specific function using fuzzy search"
    (is (= [["fn-defn-fixed" 3]
            ["fn-defn-multi" 4]
            ["fn-defn-varargs" 5]
            ["fn-defmacro" 6]
            ["fn-defprotocol" 10]
            ["fn-definterface" 12]
            ;; earlier in file, but worse search score
            ["var-defonce" 1]
            ["InterfaceDefinterface" 11]]
           (map (juxt :name #(get-in % [:location :range :start :line]))
                (f.workspace-symbols/workspace-symbols "fn" (h/db)))))))
